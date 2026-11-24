if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop("Install the shinylive package first: install.packages('shinylive')")
}

if (!requireNamespace("curl", quietly = TRUE)) {
  stop("Install the curl package first: install.packages('curl')")
}

# Loading curl explicitly avoids a Windows user-library discovery issue in
# dependency tooling called by shinylive::export().
library(curl)

shinylive::export(appdir = "app", destdir = "site")

message("Exported to site/. Preview with:")
message("  httpuv::runStaticServer('site')")
