use ::std::{collections::HashMap, fs::File, io::Read, path::PathBuf};

use anyhow::{anyhow, Result};
use ariadne::{Color, Label, Report, ReportKind, Source};
use chumsky::Parser;
pub use eval::EvaluatedModule;
use ir::tree::Module;
use render::Renderer;

mod eval;
mod ir;
mod js;
mod parser;
mod render;
mod std;

/// Global configuration for parsing and interpreting UY code.
#[derive(Default, Clone)]
pub struct Config {
    /// An import map. Specify waht imports should be resolved to.
    /// Ref: https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script/type/importmap
    pub import_map: HashMap<&'static str, &'static str>,
    // TODO: also specify where to fetch the bundle from (aka my /assets/uy-bundle.js URL)
    pub bundle_path: Option<PathBuf>,
}

/// Read module from file.
pub fn import(s: impl Into<PathBuf>, conf: Config) -> Result<ParsedModule> {
    let p = s.into();
    let mut src = String::new();
    let mut file = File::open(&p)?;
    file.read_to_string(&mut src)?;
    let module = parse_source(&src)?;

    Ok(ParsedModule {
        path: Some(p),
        config: conf,
        module,
    })
}

pub fn import_from_str(src: &str, conf: Config) -> Result<ParsedModule> {
    let module = parse_source(src)?;
    Ok(ParsedModule {
        path: None,
        config: conf,
        module,
    })
}

pub struct ParsedModule {
    path: Option<PathBuf>,
    config: Config,
    module: Module,
}

impl ParsedModule {
    /// Call the function and render its return value to HTML.
    /// Calling this function will automatically prepent "<!doctype html>" if rendered HTML starts with "<html".
    pub fn render(&self, fn_name: &str) -> Result<String> {
        let exports = eval::eval(&self.module)?;
        let scope = exports.build_scope();
        let renderer = Renderer::new(&exports, &self.config.import_map);
        let html = renderer.run(&exports.call_fn(&scope, fn_name, &[])?)?;

        if html.trim_start().starts_with("<html") {
            Ok(format!("<!doctype html>{html}"))
        } else {
            Ok(html)
        }
    }

    /// Build JS code that will be imported by rendered HTML.
    /// Note, that this bundle will requested only if HTML includes dynamic components that should be hydrated.
    pub fn build_js_bundle(&self) -> Result<String> {
        js::build_js_bundle(&self.module, Some(&self.config.import_map))
    }

    /// Build JS code and save to disk.
    pub fn save_js_bundle(&self) -> Result<()> {
        let js = self.build_js_bundle()?;
        if !js.is_empty() {
            let bundle_path = self.config.bundle_path.as_ref().ok_or_else(|| {
                anyhow!("Can't save JS bundle: bundle_path not provided in the config")
            })?;
            ::std::fs::write(bundle_path, &js)?;
        }
        Ok(())
    }

    /// Read the module from disk.
    pub fn reload(&self) -> Result<Self> {
        let Some(ref p) = self.path else {
            return Err(anyhow!("ParsedModule was build from string and cannot be reloaded."));
        };

        let mut src = String::new();
        let mut file = File::open(p)?;
        file.read_to_string(&mut src)?;

        let raw = parse_source(&src)?;
        let module = Self {
            path: self.path.clone(),
            config: self.config.clone(),
            module: raw,
        };
        module.save_js_bundle()?;

        Ok(module)
    }
}

fn parse_source(src: &str) -> Result<Module> {
    let (parsed, errs) = parser::module_parser().parse(src).into_output_errors();
    errs.into_iter().for_each(|e| {
        Report::build(ReportKind::Error, /* filename */ (), e.span().start)
            .with_message(e.to_string())
            .with_label(
                Label::new((/* filename */ (), e.span().into_range()))
                    .with_message(e.reason().to_string())
                    .with_color(Color::Red),
            )
            .with_labels(e.contexts().map(|(label, span)| {
                Label::new((/* filename */ (), span.into_range()))
                    .with_message(format!("while parsing this {}", label))
                    .with_color(Color::Yellow)
            }))
            .finish()
            // .print(sources([(filename.clone(), src.clone())]))
            .eprint(Source::from(&src))
            .unwrap()
    });

    let raw = parsed.ok_or_else(|| anyhow!("Nothing was parsed"))?;
    let module = ir::convert(raw)?;
    Ok(module)
}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;
    use std::rc::Rc;

    use anyhow::Result;
    use ariadne::{Color, Label, Report, ReportKind, Source};
    use chumsky::prelude::Rich;
    use chumsky::Parser;
    use pretty_assertions::assert_eq;

    use crate::eval::{eval, EvaluatedModule, Value};
    use crate::import_from_str;
    use crate::ir::convert;
    use crate::js::build_js;
    use crate::parser::module_parser;

    #[test]
    fn test_hello_world() {
        let exports = parse_eval(
            r#"
fn one() {
  1
}

export let a = one();
"#,
        )
        .unwrap();

        assert_eq!(exports.var("a"), Some(Value::Num(1.)));

        let exports = parse_eval(
            r#"
export let a = { "value": 1 };
export let b = a;
b.value += 2;
"#,
        )
        .unwrap();

        let expected_val = Value::Object(Rc::new(HashMap::from([(
            "value".into(),
            Value::Num(3.).into(),
        )])));
        assert_eq!(exports.var("a"), Some(expected_val.clone()));
        assert_eq!(exports.var("b"), Some(expected_val));

        let exports = parse_eval(
            r#"
let a = { "value": 1 };
export let b = { "a": a };
a.value += 2;
"#,
        )
        .unwrap();

        let expected_val = Value::Object(Rc::new(HashMap::from([(
            "value".into(),
            Value::Num(3.).into(),
        )])));
        assert_eq!(exports.var("a"), Some(expected_val.clone()));
        assert_eq!(
            exports.var("b"),
            Some(Value::Object(Rc::new(HashMap::from([(
                "a".into(),
                expected_val.into()
            )]))))
        );

        let exports = parse_eval(
            r#"
let x = 1 - 2 * 3;
let a = 4 * 4 / 2 - 3;
let b = 4 * 4 / ( 2 - 3 ) + (-4);
"#,
        )
        .unwrap();

        assert_eq!(exports.var("x"), Some(Value::Num(-5.).into()));
        assert_eq!(exports.var("a"), Some(Value::Num(5.).into()));
        assert_eq!(exports.var("b"), Some(Value::Num(-20.).into()));

        let exports = parse_eval(
            r#"
let [a, b] = [1, 2, 3];
let [[x, y], z] = [[1, 2], 3];
let [first, ...rest] = [1, 2, 3];
"#,
        )
        .unwrap();

        assert_eq!(exports.var("a"), Some(Value::Num(1.).into()));
        assert_eq!(exports.var("b"), Some(Value::Num(2.).into()));
        assert_eq!(exports.var("x"), Some(Value::Num(1.).into()));
        assert_eq!(exports.var("y"), Some(Value::Num(2.).into()));
        assert_eq!(exports.var("z"), Some(Value::Num(3.).into()));
        assert_eq!(exports.var("first"), Some(Value::Num(1.).into()));
        assert_eq!(
            exports.var("rest"),
            Some(Value::List(Rc::new(vec![Value::Num(2.).into(), Value::Num(3.).into()])).into())
        );

        let exports = parse_eval(
            r#"
let a = if false {
  1
} else if true {
  2
} else {
  3
};
"#,
        )
        .unwrap();

        assert_eq!(exports.var("a"), Some(Value::Num(2.).into()));

        let exports = parse_eval(
            r#"
let a = { "a": 1 };
let b = { "b": 2, ...a };
"#,
        )
        .unwrap();

        assert_eq!(
            exports.var("b"),
            Some(
                Value::Object(Rc::new(HashMap::from([
                    ("b".into(), Value::Num(2.).into()),
                    ("a".into(), Value::Num(1.).into())
                ])))
                .into()
            )
        );
    }

    #[test]
    fn test_render_html() {
        let (html, js) = render_page(
            r#"
export fn Page() {
  <div class="one">
    Hello
  </div>
}
"#,
        )
        .unwrap();

        assert_eq!(html, r#"<div class="one">Hello</div>"#);
        assert_eq!(js, "");

        let (html, js) = render_page(
            r#"
export fn Page() {
  <button disabled hidden={false} class="two" />
}
"#,
        )
        .unwrap();

        assert!(
            html == r#"<button class="two" disabled="true"></button>"#
                || html == r#"<button disabled="true" class="two"></button>"#,
            "Actual: {}",
            html
        );
        assert_eq!(js, "");

        let (html, js) = render_page(
            r#"
export fn Page() {
  <div class="one">
    <One count={2} />
  </div>
}

fn One(props) {
  let dot = ".";
  <span>One: {props.count}{dot}</span>
}
"#,
        )
        .unwrap();

        assert_eq!(html, r#"<div class="one"><span>One: 2.</span></div>"#);
        assert_eq!(js, "");

        let (html, js) = render_page(
            r#"
export fn Page() {
  <div>
    {if 2 > 1 {
      <One />
    } else {
      <Two />
    }}

    {if false { <Three /> }}
  </div>
}

fn One() {
  <b>One</b>
}

fn Two() {
  <i>Two</i>
}

fn Three() {
  <u>Three</u>
}
"#,
        )
        .unwrap();

        assert_eq!(html, r#"<div><b>One</b></div>"#);
        assert_eq!(js, "");

        let (html, js) = render_page(
            r#"
export fn Page() {
  <Layout>
    <One />
    <span>two</span>
  </Layout>
}

fn Layout(props) {
  <main>{props.children}</main>
}

fn One() {
  <span>One</span>
}
"#,
        )
        .unwrap();

        assert_eq!(html, r#"<main><span>One</span><span>two</span></main>"#);
        assert_eq!(js, "");

        let (html, js) = render_page(
            r#"
export fn Page() {
  <>
    <i>one</i>
    <i>two</i>
  </>
}
"#,
        )
        .unwrap();

        assert_eq!(html, r#"<i>one</i><i>two</i>"#);
        assert_eq!(js, "");
    }

    #[test]
    fn test_compile_to_js() {
        let res = to_js(
            r#"
fn one() {
  1
}

export let a = one();
"#,
        )
        .unwrap();

        assert_eq!(
            res,
            r#"function one() {
  return 1;
}

export let a = one();
"#
        );

        let res = to_js(
            r#"
Math.round(Math.random() * 10000);

let one = || { "one": 1 };
let two = || {
  console.log("two");
  { "two": 2 }
};
"#,
        )
        .unwrap();

        assert_eq!(
            res,
            r#"Math.round(Math.random() * 10000);
let one = () => ({
  "one": 1,
});
let two = () => {
  console.log("two");
  return {
    "two": 2,
  };
};
"#
        );

        let res = to_js(
            r#"
fn one(num) {
  let a = if num > 2 {
    2
  } else {
    num += 1;
    num
  };

  a
}

export fn Hello() {
  let data = {
    "name": "UYlang",
    "arr": [
      {
        "el": if true { 1 } else { 2 },
      }
    ]
  };

  // Now I return JSX
  <main>
    {if data.name {
      <span>{one(2)}</span>
    }}

    {if data {
      let res = one(1);
      <span>{res}</span>
    } else {
      <span>nope</span>
    }}
  </main>
}
"#,
        )
        .unwrap();

        assert_eq!(
            res,
            r#"import { h } from "https://esm.sh/preact@10.17.1";

function one(num) {
  let a = (() => {
    if (num > 2) {
      return 2;
    } else {
      num += 1;
      return num;
    }
  })();
  return a;
}

export function Hello() {
  let data = {
    "name": "UYlang",
    "arr": [{
      "el": (true
        ? 1
        : 2
      ),
    }],
  };
  // Now I return JSX
  return h("main", {
    "children": [
      (data.name
        ? h("span", {
            "children": [one(2)],
          })
        : null
      ),
      (() => {
        if (data) {
          let res = one(1);
          return h("span", {
            "children": [res],
          });
        } else {
          return h("span", {
            "children": ["nope"],
          });
        }
      })(),
    ],
  });
}

"#
        );

        let res = to_js(
            r#"
if false {
  one();
} else if true {
  two();
} else {
  three();
};

let a = if false {
  1
} else if true {
  2
} else {
  3
};
"#,
        )
        .unwrap();

        assert_eq!(
            res,
            r#"if (false) {
  one();
} else if (true) {
  two();
} else {
  three();
};
let a = (() => {
  if (false) {
    return 1;
  } else if (true) {
    return 2;
  } else {
    return 3;
  }
})();
"#
        );

        let res = to_js(
            r#"
let a = { "a": 1, ...b };
"#,
        )
        .unwrap();

        assert_eq!(
            res,
            r#"let a = {
  "a": 1,
  ...b,
};
"#
        );
    }

    #[test]
    fn test_hydrate() {
        let (html, js) = render_page(
            r#"import { useState } from "preact/hooks";

export fn Page() {
  <main>
    <Button initial={0} />
  </main>
}

fn Button(props) {
  let state = useState(props.initial);
  let clicks = state[0];
  let setClicks = state[1];

  <button type="button" onClick={|| setClicks(clicks + 1)}>
    Clicks: {clicks}
  </button>
}
"#,
        )
        .unwrap();

        assert_eq!(
            html,
            r#"<main><button id="test" type="button">Clicks: 0</button>
<script type="module">
import { hydrate, h } from "https://esm.sh/preact@10.17.1";
import { Button } from "/assets/uy-bundle.js";

hydrate(h(Button, {
  "initial": 0,
  children: [],
}), document.querySelector('#test').parentNode);
</script>
</main>"#
        );

        // TODO: atm jsx and jsxs are exact same functions.
        //       jsxs will potentially optimize components with static children.
        // Esbuild adds /* @__PURE__ */ before jsx calls to signify that method call is pure.
        assert_eq!(
            js,
            r#"import { h } from "https://esm.sh/preact@10.17.1";
import { useState } from "https://esm.sh/preact@10.17.1/hooks";

export function Button(props) {
  let state = useState(props.initial);
  let clicks = state[0];
  let setClicks = state[1];
  return h("button", {
    "type": "button",
    "onClick": () => setClicks(clicks + 1),
    "children": [
      "Clicks: ",
      clicks,
    ],
  });
}

"#
        );

        // Fresh example:
        // `revive`: https://github.com/denoland/fresh/blob/main/src/runtime/entrypoints/main.ts#L67
        // It basically walkes over the DOM and finds comment nodes. Then it renders the components.

        /*
        <main class="mx-auto px-4 max-w-3xl mt-8">
            <!--frsh-sudoku_sudokuxwing_default:default:0-->
            <section class="flex gap-4 flex-col">
               ...
            </section>
            <!--/frsh-sudoku_sudokuxwing_default:default:0-->
        </main>

        <script id="__FRSH_STATE" type="application/json">
            { "v": [[{}], [[]]] }
        </script>
        <script type="module" nonce="4b60af7c12d2421f8eafbd77f9fdf417">
            const ST = document.getElementById("__FRSH_STATE").textContent;
            const STATE = JSON.parse(ST).v;
            import p0 from "/_frsh/js/085818a937a763c1b7f2748cf7cfabd537da9075/plugin-twind-main.js";
            p0(STATE[1][0]);
            import { revive } from "/_frsh/js/085818a937a763c1b7f2748cf7cfabd537da9075/main.js";
            import * as Sudoku_SudokuXWing_default from "/_frsh/js/085818a937a763c1b7f2748cf7cfabd537da9075/island-sudoku_sudokuxwing_default.js";
            revive({ sudoku_sudokuxwing_default: Sudoku_SudokuXWing_default }, STATE[0]);
        </script>
        */
    }

    // TODO: inject global vars into root scope (e.g Math, Array, globalThis, console)

    fn parse_module<'src>(
        src: &'src str,
    ) -> Result<crate::parser::ast::Module, Vec<Rich<'src, char>>> {
        let (parsed, errs) = module_parser().parse(src).into_output_errors();

        errs.iter().for_each(|e| {
            Report::build(ReportKind::Error, /* filename */ (), e.span().start)
                .with_message(e.to_string())
                .with_label(
                    Label::new((/* filename */ (), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new((/* filename */ (), span.into_range()))
                        .with_message(format!("while parsing this {}", label))
                        .with_color(Color::Yellow)
                }))
                .finish()
                // .print(sources([(filename.clone(), src.clone())]))
                .eprint(Source::from(&src))
                .unwrap();
        });

        if errs.is_empty() {
            if let Some(p) = parsed {
                Ok(p)
            } else {
                Err(vec![])
            }
        } else {
            Err(errs)
        }
    }

    fn parse_eval(src: &str) -> Result<EvaluatedModule, &'static str> {
        let raw = parse_module(src).map_err(|_| "Parsing error")?;
        let module = convert(raw).map_err(|_| "Conversion error")?;
        eval(&module).map_err(|_| "Eval error")
    }

    fn render_page(src: &str) -> Result<(String, String)> {
        let uy = import_from_str(
            src,
            crate::Config {
                import_map: HashMap::from([
                    ("preact", "https://esm.sh/preact@10.17.1"),
                    ("preact/", "https://esm.sh/preact@10.17.1/"),
                ]),
                ..Default::default()
            },
        )?;
        let js = uy.build_js_bundle()?;
        let html = uy.render("Page")?;
        Ok((html, js))
    }

    fn to_js(src: &str) -> Result<String, &'static str> {
        let raw = parse_module(src).map_err(|_| "Parsing error")?;
        let module = convert(raw).map_err(|_| "Convertion error")?;
        let js = build_js(
            &module,
            Some(&HashMap::from([(
                "preact",
                "https://esm.sh/preact@10.17.1",
            )])),
        )
        .map_err(|_| "Build JS error")?;
        Ok(js)
    }
}
