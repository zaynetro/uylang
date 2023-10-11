use anyhow::Result;
use axum::{
    extract::State,
    http::StatusCode,
    response::{Html, IntoResponse, Response},
    routing::get,
    Router,
};
use std::{collections::HashMap, net::SocketAddr, path::PathBuf, sync::Arc};
use tower_http::services::ServeDir;
use tracing_subscriber::EnvFilter;
use uylang::ParsedModule;

#[derive(Clone)]
struct AppState {
    uy: Arc<ParsedModule>,
}

impl AppState {
    /// Return the module
    #[cfg(not(debug_assertions))]
    pub fn get_uy(&self) -> Arc<ParsedModule> {
        self.uy.clone()
    }

    /// Reload the module (enabled only in debug builds)
    #[cfg(debug_assertions)]
    pub fn get_uy(&self) -> Arc<ParsedModule> {
        let module = self.uy.reload().expect("Reload module");
        Arc::new(module)
    }
}

fn main() -> Result<()> {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        // Default is 2MB but that's not enough for our module reloading.
        .thread_stack_size(3 * 1024 * 1024)
        .build()?
        .block_on(run())
}

async fn run() -> Result<()> {
    // Configure logging
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::TRACE)
        .with_env_filter(EnvFilter::new("info,uylang=info"))
        .init();

    let dir = PathBuf::from("examples").join("axum-todomvc");
    let assets_dir = dir.join("assets");

    let conf = uylang::Config {
        import_map: HashMap::from([
            ("preact", "https://esm.sh/preact@10.17.1"),
            ("preact/", "https://esm.sh/preact@10.17.1/"),
        ]),
        bundle_path: Some(assets_dir.join("uy-bundle.js")),
    };

    // Parse our source file
    let module = uylang::import(dir.join("components").join("index.uy"), conf)?;

    // Compile UYlang code to JS and store the file on disk.
    module.save_js_bundle()?;

    // Build the state
    let state = AppState {
        uy: Arc::new(module),
    };

    // Build the router
    let app = Router::new()
        .route("/", get(index))
        .nest_service("/assets", ServeDir::new(&assets_dir))
        .with_state(state);

    // Start the server
    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    tracing::info!("Listening on {addr}");
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await?;

    Ok(())
}

/// This is our index page handler.
/// We call exported Index function and render the return value to HTML.
async fn index(State(state): State<AppState>) -> Result<impl IntoResponse, AppError> {
    let html = state.get_uy().render("Index").map_err(AppError)?;
    Ok(Html(html))
}

struct AppError(anyhow::Error);

// Tell axum how to convert `AppError` into a response.
impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        tracing::error!("Server error {:?}", self.0);
        (StatusCode::INTERNAL_SERVER_ERROR, "Something went wrong").into_response()
    }
}
