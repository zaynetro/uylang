use std::collections::HashMap;

use anyhow::{anyhow, Result};
use sha2::{Digest, Sha256};

use crate::eval::{EvaluatedModule, FnValue, Scope, Value, ValueRef};

pub struct Renderer<'a> {
    exports: &'a EvaluatedModule,
    scope: Scope,
    import_map: &'a HashMap<&'static str, &'static str>,
}

#[derive(Default)]
struct RenderCtx {
    hydrated: bool,
    inject_attrs: Vec<(String, String)>,
}

impl RenderCtx {
    fn hydrated(&self, id: String) -> Self {
        Self {
            hydrated: true,
            inject_attrs: vec![("id".into(), id)],
        }
    }
}

impl<'a> Renderer<'a> {
    pub fn new(
        exports: &'a EvaluatedModule,
        import_map: &'a HashMap<&'static str, &'static str>,
    ) -> Self {
        Self {
            scope: exports.build_scope(),
            exports,
            import_map,
        }
    }

    pub fn run(&self, value: &ValueRef) -> Result<String> {
        let mut ctx = RenderCtx::default();
        self.render(&mut ctx, &value)
    }

    /// Render the value into HTML
    fn render(&self, ctx: &mut RenderCtx, value: &ValueRef) -> Result<String> {
        let res = match *value.0.borrow() {
            Value::Null => "".to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Num(n) => n.to_string(),
            Value::Str(ref s) => s.clone(),
            Value::List(ref list) => {
                let mut res = String::new();
                for child in list.iter() {
                    res.push_str(&self.render(ctx, child)?);
                }

                res
            }
            Value::Object(ref obj) => {
                let mut res = String::new();

                let typ = if let Some(typ) = obj.get("type") {
                    match *typ.0.borrow() {
                        Value::Str(ref s) => s.clone(),
                        _ => {
                            return Err(anyhow!("Unsupported JSX type {:?}", typ));
                        }
                    }
                } else {
                    tracing::warn!("Unsupported JSX type: {:?}", obj.get("type"));
                    return Ok(String::new());
                };

                // Check if typ is a defined function. Call it and render result instead.
                let props_value = obj.get("props");
                {
                    if let Some(v) = self.exports.var_ref(&typ) {
                        match &*v.0.borrow() {
                            Value::Fn(f) => {
                                let needs_hydration = match **f {
                                    FnValue::Native(_) => false,
                                    FnValue::User(ref def) => {
                                        def.meta.needs_hydration && !ctx.hydrated
                                    }
                                };
                                let args = if let Some(props) = props_value {
                                    vec![props.clone()]
                                } else {
                                    vec![]
                                };

                                let component = EvaluatedModule::do_call_fn(&self.scope, f, &args)?;

                                if needs_hydration {
                                    // TODO: generate this id deterministically
                                    let hydration_id = "test".to_string();
                                    let mut child_ctx = ctx.hydrated(hydration_id.clone());
                                    let mut out = self.render(&mut child_ctx, &component)?;

                                    out.push_str(&self.inject_hydration_script(
                                        &typ,
                                        props_value,
                                        &hydration_id,
                                    )?);

                                    return Ok(out);
                                } else {
                                    return self.render(ctx, &component);
                                }
                            }
                            _ => {}
                        }
                    }
                }

                // This is a fragment --> render only children
                let is_fragment = typ.is_empty();

                if !is_fragment {
                    // Opening tag
                    res.push('<');
                    res.push_str(&typ);
                }

                let maybe_children = if let Some(pv) = props_value {
                    match *pv.0.borrow() {
                        Value::Object(ref props) => {
                            let maybe_children = props.get("children");

                            if !is_fragment {
                                for (key, val) in ctx.inject_attrs.drain(..) {
                                    res.push_str(&format!(r#" {key}="{val}""#));
                                }

                                for (key, prop) in props.iter() {
                                    if key == "children" {
                                        continue;
                                    }

                                    match *prop.0.borrow() {
                                        Value::Null | Value::Bool(false) | Value::Fn(_) => {
                                            // Skip this prop
                                            continue;
                                        }
                                        _ => {}
                                    }

                                    res.push(' ');
                                    res.push_str(&key);
                                    res.push('=');
                                    res.push('"');
                                    res.push_str(&self.render(ctx, prop)?);
                                    res.push('"');
                                }
                            }

                            maybe_children.cloned()
                        }
                        _ => None,
                    }
                } else {
                    None
                };

                let is_void = is_void_element(&typ);

                if is_void {
                    if !is_fragment {
                        // No children --> self-closing tag
                        // Technically, HTML expects no slash but using it is XHTML-compatible and more readable.
                        res.push_str(" />");
                    }
                } else {
                    if !is_fragment {
                        res.push('>');
                    }

                    if let Some(children) = maybe_children {
                        // We expect children to always be an array
                        match *children.0.borrow() {
                            Value::List(ref list) if !list.is_empty() => {
                                // TODO: pass ctx that hydration has been done already
                                res.push_str(&self.render(ctx, &children)?);
                            }
                            _ => {
                                // TODO: print warning
                            }
                        }
                    };

                    if !is_fragment {
                        // Closing tag
                        res.push_str("</");
                        res.push_str(&typ);
                        res.push('>');
                    }
                }

                res
            }
            Value::Fn(_) => {
                panic!("Can't render Fn");
            }
        };
        Ok(res)
    }

    fn inject_hydration_script(
        &self,
        tag: &str,
        props_value: Option<&ValueRef>,
        id: &str,
    ) -> Result<String> {
        let preact_import = self
            .import_map
            .get("preact")
            .ok_or(anyhow!("Preact import is not mapped"))?;

        // TODO: get bundle location from config
        let common = format!(
            r#"<script type="module">
import {{ hydrate, h }} from "{preact_import}";
import {{ {tag} }} from "/assets/uy-bundle.js";
"#
        );

        let props = if let Some(pv) = props_value {
            match *pv.0.borrow() {
                Value::Object(ref props) => {
                    let mut entries = vec![];

                    for (key, prop) in props.iter() {
                        if key == "type" || key == "children" {
                            continue;
                        }

                        let json = to_json(prop);
                        if !json.is_empty() {
                            entries.push(format!(r#"  "{key}": {json}"#));
                        }
                    }

                    let count = entries.len();
                    let mut res = entries.join(",\n");
                    if count > 0 {
                        res.push_str(",\n");
                    }
                    res
                }
                _ => "".to_string(),
            }
        } else {
            "".to_string()
        };

        // TODO: serialize children
        let component = format!(
            r#"h({tag}, {{
{props}  children: [],
}})"#
        );

        Ok(format!(
            r##"
{common}
hydrate({component}, document.querySelector('#{id}').parentNode);
</script>
"##
        ))
    }

    fn gen_id(data: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(data.as_bytes());
        let hash = hasher.finalize();
        base16ct::lower::encode_string(&hash)
            .chars()
            .take(8)
            .collect::<String>()
    }
}

/// Void elements are elements that cannot have any child nodes.
/// Ref: https://developer.mozilla.org/en-US/docs/Glossary/Void_element
fn is_void_element(tag: &str) -> bool {
    match tag {
        "area" | "base" | "br" | "col" | "embed" | "hr" | "img" | "input" | "link" | "meta"
        | "param" | "source" | "track" | "wbr" => true,
        _ => false,
    }
}

fn to_json(value: &ValueRef) -> String {
    match &*value.0.borrow() {
        Value::Null => "null".into(),
        Value::Bool(b) => {
            format!("{b}")
        }
        Value::Num(n) => {
            format!("{n}")
        }
        Value::Str(s) => {
            // TODO: escape quotes
            format!(r#""{s}""#)
        }
        Value::List(list) => {
            let mut out = String::new();
            out.push_str("[");

            for v in list.iter() {
                let json = to_json(v);
                out.push_str(" ");
                out.push_str(&json);
                out.push_str(",");
            }

            if !list.is_empty() {
                out.push_str(" ");
            }

            out.push_str("]");
            out
        }
        Value::Object(obj) => {
            let mut out = String::new();
            out.push_str("{");

            for (key, v) in obj.iter() {
                let json = to_json(v);
                out.push_str(&format!(r#" "{key}": {json},"#));
            }

            if !obj.is_empty() {
                out.push_str(" ");
            }

            out.push_str("}");
            out
        }
        Value::Fn(_) => "".into(),
    }
}
