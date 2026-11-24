# GAM basis-function explorer

An interactive teaching app showing how basis functions are weighted and added
to produce a smooth, and how a penalized GAM estimates a smooth from simulated
ecological data.

The repository is configured for **GitHub Pages using Shinylive**. The exported
app runs R and Shiny inside the visitor's browser through WebAssembly, so it does
not require a continuously running Shiny server.

## Features

- display 4–8 B-spline building blocks;
- edit every basis coefficient and inspect their pointwise sum;
- enter an exact evaluation point or select one by clicking a plot;
- generate noisy data from known ecological relationships;
- fit a penalized GAM and change its basis dimension;
- compare the known curve, estimated smooth and confidence interval;
- inspect the actual `mgcv` basis matrix and coefficient-weighted contributions
  used to reconstruct the fitted curve.

## Repository structure

```text
.
├── .github/workflows/deploy-pages.yml  # automatic Pages deployment
├── app/app.R                           # Shiny application
├── export-local.R                      # optional local Shinylive export
├── .gitignore
└── README.md
```

## Publish on GitHub Pages

1. Create an empty GitHub repository, for example `gam-basis-explorer`.
2. Copy the contents of this folder to the repository root.
3. Commit and push to the `main` branch:

```bash
git init
git add .
git commit -m "Add GAM basis-function explorer"
git branch -M main
git remote add origin https://github.com/YOUR-USER/gam-basis-explorer.git
git push -u origin main
```

4. On GitHub, open **Settings → Pages**.
5. Under **Build and deployment**, select **GitHub Actions** as the source.
6. Open the **Actions** tab and wait for “Deploy Shinylive app to GitHub Pages”
   to finish. If necessary, run it manually using **Run workflow**.

The published address will normally be:

```text
https://YOUR-USER.github.io/gam-basis-explorer/
```

Every subsequent push to `main` rebuilds and republishes the app.

## Test as an ordinary local Shiny app

From the repository root:

```r
shiny::runApp("app")
```

## Test the serverless version locally

Install the exporter once:

```r
install.packages(c("shinylive", "httpuv"))
```

Then, from the repository root:

```r
source("export-local.R")
httpuv::runStaticServer("site")
```

Do not open `site/index.html` directly from the filesystem. Shinylive uses web
workers and must be viewed through an HTTP server.

## Notes about browser execution

- The initial visit downloads the WebAssembly R runtime and package assets, so
  startup is slower than an ordinary static webpage.
- Computation occurs locally in the visitor's browser.
- The app requires JavaScript and a modern browser.
- `shiny` and `mgcv` are declared in `app/app.R` so that the exporter discovers
  both dependencies.

## Source application

The app is written in R using `shiny`, `splines` and `mgcv`. No course data or
personal information are included in the repository.
