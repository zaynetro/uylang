# UY-lang

UY-lang is an embedded scripting language for Rust that makes defining HTML templates easy. 

> This is an early stage prototype. Do not use in production.

UY-lang has two primary goals:

1. Provide templating engine for your web apps
2. Add interactivity to your static HTML (only when needed)


## Features

- **Embedded**: Parse and evaluate HTML templates from your Rust application.
- **Static by default**: JS is needed only if you use interactive components on the page.
- **JSX support out-of-the-box**: Write plain HTML.
- **Human-readable JS**: Interactive components are compiled to a human-readable JavaScript.
- **Lightweight**: UY-lang brings only a couple of extra dependencies to your project.


## Example

First define your templates:

```jsx
import { useState } from "preact/hooks";

// Define our IndexPage. JSX support is built-in. You can write HTML as is!
export fn IndexPage() {
  <html>
    <head>
      <meta charset="utf-8" />
      <title>Minimal example</title>
    </head>
    <body>
      <h1>Click on the button below!</h1>
      <main>
        <Button initial={0} />
      </main>
    </body>
  </html>
}

// Define an interactive component. Up until this point our HTML was static. 
// UY-lang knows where you need interactivity and will compile only relevant parts to JS.
fn Button(props) {
  let [clicks, setClicks] = useState(props.initial);

  <button type="button" onClick={|| setClicks(clicks + 1)}>
    Clicks: {clicks}
  </button>
}
```

Now from Rust:

```rust
let conf = uylang::Config {
    import_map: HashMap::from([
        ("preact", "https://esm.sh/preact@10.17.1"),
        ("preact/", "https://esm.sh/preact@10.17.1/"),
    ]),
    bundle_path: Some("assets/uy-bundle.js".into()), // Server should serve this file
};

// Parse our source file
let module = uylang::import("index.uy", conf)?;
// Compile UY-lang code to JS and store the file on disk. 
module.save_js_bundle()?;

// Get HTML string
let html = module.render("IndexPage")?;
```


This repo has more examples. You can run them locally with:

```
cargo run -p example-axum-minimal
cargo run -p example-axum-todomvc
```


## Language features

* [ ] Blocks
    * [ ] For loop
    * [ ] Match (switch replacement)
    * [x] elseif branch (syntax sugar)
    * [ ] try/catch
* [ ] Elvis operator (?.)
* [ ] Support async/await (for HTML rendering just ignore it)
* [ ] Destructuring
    * [x] Arrays
    * [ ] Objects
    * [ ] Allow in function arguments
    * Ref: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment#rest_property>
* [ ] Spread operator (...)
    * [ ] In Arrays
    * [x] In Objects
    * Ref: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax>
* [ ] Define global methods
    * [ ] Array
    * [ ] Object
    * [ ] Number
    * [ ] Math
    * [ ] fetch/setTimeout/... (Window methods)
    * [ ] globalThis
* String
    * [ ] Template literal
    * [ ] Quotes
* JSX
    * [ ] Generate random ID when hydrating
    * [ ] to_json: Escape quotes 
    * [ ] useEffect hook
* JS compilation
    * [x] Convert if expressions to JS statements
    * [ ] Convert block expressions to JS statements
    * [ ] Escape quotes in strings
* Make semicolon optional after blocks
* Bundle JS dependencies
* JS compatibility:
    * JS doesn't allow redeclaring variables with let.
    * Keywords and reserved words
* Preact Context
* How to identify interactive components?
    * Usage of any hooks and "on.." event listener attributes?


## Future

* Multiple modules (import from other files)
* Tree-sitter parser
    * [JS parser](https://github.com/tree-sitter/tree-sitter-javascript/blob/master/grammar.js)
    * [Tree-sitter guide](https://tree-sitter.github.io/tree-sitter/creating-parsers#writing-the-grammar)
* Syntax highlighting (GitHub and editors)
* Source maps
* Hot-code reload
* CSS
* Compile time safety when calling UY-lang's functions from Rust
* Optional type-safety
* Testing
* Untie from Preact
* Stream HTML


### Potential optimizations

* Use references in EvaluatedModule
* Remove clones
* [smallvec: store small vectors on the stack](https://lib.rs/crates/smallvec)
* [smol_str: store short strings on the stack](https://lib.rs/crates/smol_str)


## References

* [Parsing binary operations](https://github.com/zesterer/chumsky/blob/main/tutorial.md#parsing-binary-operators)
* [ECMAScript syntax](https://262.ecma-international.org/#sec-modules)
    * [Keywords and reserved words](https://262.ecma-international.org/#sec-keywords-and-reserved-words)
* [Rust syntax](https://doc.rust-lang.org/reference/items.html)

* [Rhai: embeddable language in Rust](https://github.com/rhaiscript/rhai)
* [Gluon: embeddable language in Rust](https://github.com/gluon-lang/gluon)

* [Leptos islands](https://github.com/leptos-rs/leptos/releases/tag/v0.5.0)
