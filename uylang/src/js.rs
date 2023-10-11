use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt::Write,
};

use anyhow::{anyhow, Result};

use crate::ir::tree::{
    Block, Expr, FnDef, IfBlock, Import, Literal, MatchPattern, Module, ObjectEntry, Stmt,
};

/// Compiles all code to JS.
pub fn build_js<'a>(
    module: &Module,
    import_map: Option<&'a HashMap<&'static str, &'static str>>,
) -> Result<String> {
    JsWriter {
        import_map,
        ..Default::default()
    }
    .compile_module(module)
}

/// Compiles only global variables and dynamic components used in the browser.
pub fn build_js_bundle<'a>(
    module: &Module,
    import_map: Option<&'a HashMap<&'static str, &'static str>>,
) -> Result<String> {
    JsWriter {
        browser_bundle: true,
        import_map,
        ..Default::default()
    }
    .compile_module(module)
}

#[derive(Default)]
struct JsWriter<'a> {
    level: usize,
    browser_bundle: bool,
    import_map: Option<&'a HashMap<&'static str, &'static str>>,
    has_jsx: bool,
}

impl<'a> JsWriter<'a> {
    /// Write spaces to match current ident level.
    fn ident(&mut self, out: &mut String) {
        Self::with_ident(out, self.level)
    }

    /// Write spaces for the provided ident level.
    fn with_ident(out: &mut String, level: usize) {
        write!(out, "{:width$}", "", width = level * 2).unwrap();
    }

    fn compile_module(&mut self, module: &Module) -> Result<String> {
        let mut out = String::new();

        for import in &module.imports {
            out.push_str(&self.compile_import(import));
        }

        if !module.imports.is_empty() {
            out.push('\n');
        }

        // Go through a list of functions and make a list of those that need to be hydrated
        let mut fns_to_hydrate = HashSet::new();
        for (name, f) in &module.fns {
            if f.meta.needs_hydration {
                fns_to_hydrate.insert(name);
                fns_to_hydrate.extend(&f.meta.global_vars);
            }
        }

        for (name, f) in &module.fns {
            // Skip compiling non-dynamic functions for browser bundle
            if self.browser_bundle && !fns_to_hydrate.contains(name) {
                continue;
            }

            out.push_str(&self.compile_fn(name, f));
        }

        for stmt in &module.statements {
            out.push_str(&self.compile_stmt(stmt));
        }

        if self.has_jsx {
            let preact_import = self
                .import_map
                .ok_or(anyhow!("Missing import map"))?
                .get("preact")
                .ok_or(anyhow!("Preact import is not mapped"))?;

            out.insert_str(
                0,
                &format!(
                    "import {{ h }} from \"{preact_import}\";\n{}",
                    if module.imports.is_empty() { "\n" } else { "" }
                ),
            );
        }

        Ok(out)
    }

    fn resolve_import<'b, 'c>(&mut self, from: &'c str) -> Cow<'b, str>
    where
        'c: 'b,
    {
        if let Some(import_map) = self.import_map {
            // First check if our import is present in the map
            if let Some(v) = import_map.get(from) {
                return (*v).into();
            }

            // Now find the best matching alias
            let mut imports = import_map.iter().collect::<Vec<_>>();
            // Sort imports by longest alias first
            imports.sort_by(|a, b| b.0.len().cmp(&a.0.len()));

            for (alias, v) in import_map {
                // Now resolve the full URL by appending the rest of the alias to the value
                if let Some(rest) = from.strip_prefix(alias) {
                    return format!("{v}{rest}").into();
                }
            }
        }

        from.into()
    }

    fn compile_import(&mut self, import: &Import) -> String {
        let from = self.resolve_import(import.from.as_str());
        format!(
            r#"import {{ {} }} from "{from}";
"#,
            import.refs.join(", "),
        )
    }

    fn compile_fn(&mut self, name: &str, f: &FnDef) -> String {
        let mut out = String::new();
        self.ident(&mut out);

        // We want to export this dynamic component so that hydration script could reference it
        let exported = f.exported || self.browser_bundle && f.meta.needs_hydration;
        if exported {
            out.push_str("export ");
        }

        out.push_str("function ");
        out.push_str(name);
        out.push('(');

        for (i, arg) in f.args.iter().enumerate() {
            if i > 0 {
                out.push_str(", ");
            }

            out.push_str(arg);
        }

        out.push_str(") ");
        out.push_str(&self.compile_block(&f.body));
        // After a function definition keep empty line
        out.push_str("\n\n");

        out
    }

    fn compile_block(&mut self, block: &Block) -> String {
        let mut out = String::new();
        out.push_str("{\n");
        self.level += 1;

        for (name, f) in &block.fns {
            out.push_str(&self.compile_fn(name, f));
        }

        for stmt in &block.statements {
            out.push_str(&self.compile_stmt(stmt));
        }

        // Return statement
        if let Some(e) = &block.out {
            self.ident(&mut out);
            let value = self.compile_expr(e);

            out.push_str("return ");
            out.push_str(&value);
            out.push_str(";\n");
        }

        self.level -= 1;
        self.ident(&mut out);
        out.push_str("}");
        out
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> String {
        match stmt {
            Stmt::LineComment(c) => {
                let mut out = String::new();
                self.ident(&mut out);
                out.push_str("// ");
                out.push_str(&c);
                out.push_str("\n");
                out
            }
            Stmt::VarDef(v) => {
                let mut out = String::new();
                self.ident(&mut out);

                if v.exported {
                    out.push_str("export ");
                }

                out.push_str("let ");
                out.push_str(&self.compile_pattern(&v.pattern));
                out.push_str(" = ");
                out.push_str(&self.compile_expr(&v.def));
                out.push_str(";\n");
                out
            }
            Stmt::Expr(e) => {
                let mut out = String::new();
                self.ident(&mut out);
                out.push_str(&self.compile_expr(e));
                out.push_str(";\n");
                out
            }
        }
    }

    fn compile_pattern(&self, pattern: &MatchPattern) -> String {
        match pattern {
            MatchPattern::Empty => "".into(),
            MatchPattern::Ident(name) => name.into(),
            MatchPattern::Slice { items, rest } => {
                let mut out = String::new();
                out.push_str("[");
                for (i, p) in items.iter().enumerate() {
                    out.push_str(&self.compile_pattern(p));
                    if (i + 1) < items.len() {
                        out.push_str(", ");
                    }
                }

                if let Some(r) = rest {
                    out.push_str(", ...");
                    out.push_str(&r);
                }

                out.push_str("]");
                out
            }
        }
    }

    fn compile_expr(&mut self, e: &Expr) -> String {
        match e {
            Expr::Literal(l) => match l {
                Literal::Null => "null".to_string(),
                Literal::Bool(b) => b.to_string(),
                Literal::Num(n) => n.to_string(),
                // TODO: escape quotes
                Literal::Str(s) => format!(r#""{s}""#,),
            },
            Expr::VarRef(v) => v.to_string(),
            Expr::Grouped(e) => format!("({})", self.compile_expr(e)),
            Expr::List(list) => {
                let mut out = String::new();
                out.push('[');

                if list.len() == 1 {
                    out.push_str(&self.compile_expr(&list[0]));
                } else {
                    out.push('\n');
                    self.level += 1;
                    for child in list {
                        self.ident(&mut out);
                        out.push_str(&self.compile_expr(child));
                        out.push_str(",\n");
                    }
                    self.level -= 1;

                    self.ident(&mut out);
                }

                out.push(']');
                out
            }
            Expr::Object(obj) => {
                let mut out = String::new();
                out.push_str("{\n");

                for entry in obj {
                    self.level += 1;
                    self.ident(&mut out);

                    match entry {
                        ObjectEntry::KeyValue(key, e) => {
                            let value = self.compile_expr(&e);
                            self.level -= 1;
                            out.push('"');
                            out.push_str(&key);
                            out.push_str("\": ");
                            out.push_str(&value);
                            out.push_str(",\n");
                        }
                        ObjectEntry::Rest(name) => {
                            self.level -= 1;
                            out.push_str("...");
                            out.push_str(&name);
                            out.push_str(",\n");
                        }
                    }
                }

                self.ident(&mut out);
                out.push('}');
                out
            }
            Expr::Field(e, field) => format!("{}.{field}", self.compile_expr(e)),
            Expr::Index(left, right) => {
                format!("{}[{}]", self.compile_expr(left), self.compile_expr(right))
            }
            Expr::Unary(unary) => {
                let e = self.compile_expr(&unary.expr);
                format!("{}{e}", unary.op.as_str())
            }
            Expr::Binary(binary) => {
                let left = self.compile_expr(&binary.left);
                let right = self.compile_expr(&binary.right);

                format!("{left} {} {right}", binary.op.as_str())
            }
            Expr::Block(b) => self.compile_block(b),
            Expr::If(if_) => self.compile_if_block(if_, IfBlockMode::Normal),
            Expr::Jsx(jsx) => {
                self.has_jsx = true;

                let mut props = String::new();
                let props_level = self.level + 1;

                // Properties
                for (key, e) in &jsx.props {
                    Self::with_ident(&mut props, props_level);
                    // TODO: escape quotes
                    props.push_str(&format!(r#""{key}": {},"#, self.compile_expr(e)));
                    props.push('\n');
                }

                // Children
                if !jsx.children.is_empty() {
                    self.level += 1;
                    let list = self.compile_expr(&Expr::List(jsx.children.clone()));
                    self.level -= 1;

                    Self::with_ident(&mut props, props_level);
                    props.push_str(&format!(r#""children": {},"#, list));
                }

                let mut out = String::new();
                out.push_str("h(");

                // This is a rough hack to figure out if our JSX node is a string or a function reference.
                if jsx.name.to_lowercase() == jsx.name {
                    // An HTML element
                    out.push('"');
                    out.push_str(&jsx.name);
                    out.push('"');
                } else {
                    // A component reference
                    out.push_str(&jsx.name);
                }
                if !props.is_empty() {
                    out.push_str(", {");
                    out.push('\n');
                    out.push_str(&props);
                    out.push('\n');
                    self.ident(&mut out);
                    out.push('}');
                }
                out.push_str(")");
                out
            }
            Expr::FnCall(f) => {
                let mut out = String::new();
                out.push_str(&self.compile_expr(&f.fn_name));
                out.push('(');

                for (i, arg) in f.args.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    out.push_str(&self.compile_expr(arg));
                }

                out.push(')');
                out
            }
            Expr::Lambda(fdef) => {
                let mut out = String::new();
                out.push('(');
                for arg in &fdef.args {
                    out.push_str(arg);
                    out.push_str(", ");
                }
                out.push(')');
                out.push_str(" => ");

                // Try to format lambda function on the same line
                match &fdef.body.out {
                    Some(e) if fdef.body.statements.len() == 0 => match **e {
                        Expr::Object(_) => {
                            // Object must be wrapped inside parenthesis not to be confused with multi-line function body.
                            out.push('(');
                            out.push_str(&self.compile_expr(e));
                            out.push(')');
                        }
                        _ => {
                            out.push_str(&self.compile_expr(e));
                        }
                    },
                    _ => {
                        out.push_str(&self.compile_block(&fdef.body));
                    }
                }

                out
            }
        }
    }

    fn compile_if_block(&mut self, if_: &IfBlock, mode: IfBlockMode) -> String {
        let mut out = String::new();

        // If-block cases:
        // 1) if or else blocks have statements and return expression
        //    --> wrap if statement inside a lambda function
        // 2) if or else blocks have statements and no return expression
        //    --> a normal if statement
        // 3) if and else have return expression and no statements
        //    --> use inline if
        // 4) we are inside else if branch
        //    --> do not inline and do not wrap inside a function

        let can_inline = mode == IfBlockMode::Normal;

        let inline = can_inline
            && if_.block.statements.is_empty()
            && if_
                .r#else
                .as_ref()
                .map(|e| {
                    if let Expr::Block(ref b) = **e {
                        b.statements.is_empty()
                    } else {
                        false
                    }
                })
                .unwrap_or(true);

        // Inline if-block
        if inline {
            let cond = self.compile_expr(&if_.cond);

            out.push_str("(");
            out.push_str(&cond);
            out.push_str("\n");
            self.level += 1;

            self.ident(&mut out);
            self.level += 1;
            out.push_str("? ");
            if let Some(e) = &if_.block.out {
                out.push_str(&self.compile_expr(e));
            } else {
                out.push_str("null");
            }
            out.push_str("\n");

            self.level -= 1;
            self.ident(&mut out);
            self.level += 1;
            out.push_str(": ");

            let else_expr = if_.r#else.as_ref().and_then(|e| {
                if let Expr::Block(ref b) = **e {
                    b.out.as_ref()
                } else {
                    None
                }
            });
            if let Some(ref e) = else_expr {
                out.push_str(&self.compile_expr(e));
            } else {
                out.push_str("null");
            }

            self.level -= 2;
            out.push_str("\n");
            self.ident(&mut out);
            out.push_str(")");
            return out;
        }

        let should_wrap = if_.block.out.is_some() && mode != IfBlockMode::ElseIf;
        if should_wrap {
            // Here we convert our if-block expression into a self-calling anonymous function.
            // Alternatively, we could defined a variable and assign the value to it but it requires more logic,
            // so sticking with simpler solution for now.
            out.push_str("(() => {\n");
            self.level += 1;
            self.ident(&mut out);
        }

        let cond = self.compile_expr(&if_.cond);
        out.push_str("if (");
        out.push_str(&cond);
        out.push_str(") ");
        out.push_str(&self.compile_block(&if_.block));

        if let Some(else_) = &if_.r#else {
            out.push_str(" else ");
            match **else_ {
                Expr::If(ref if_) => {
                    out.push_str(&self.compile_if_block(if_, IfBlockMode::ElseIf));
                }
                _ => {
                    out.push_str(&self.compile_expr(else_));
                }
            }
        }

        if should_wrap {
            // Finish our anonymous function
            out.push_str("\n");
            self.level -= 1;
            self.ident(&mut out);
            out.push_str("})()");
        }

        out
    }
}

#[derive(PartialEq)]
enum IfBlockMode {
    Normal,
    ElseIf,
}
