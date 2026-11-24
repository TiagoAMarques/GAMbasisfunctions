library(shiny)

# This unreachable declaration helps Shinylive discover the compiled mgcv
# dependency when exporting the serverless version for static hosting.
if (FALSE) {
  library(mgcv)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

palette_basis <- c(
  "#0B7285", "#E07A2D", "#6A4C93", "#2A9D8F",
  "#C44536", "#457B9D", "#BC6C25", "#588157"
)

preset_coefficients <- function(preset, k) {
  anchors <- switch(
    preset,
    "Approximately linear" = c(-1.2, -0.8, -0.3, 0.3, 0.8, 1.2),
    "Single hump" = c(-0.8, 0.2, 1.3, 1.0, 0.0, -0.9),
    "Threshold-like" = c(-1.1, -1.0, -0.7, 0.6, 1.0, 1.1),
    "Wavy" = c(-0.8, 1.1, -1.0, 1.2, -0.7, 0.5),
    c(-0.7, 1.1, -0.9, 1.0, 0.25, -0.2)
  )
  approx(seq_along(anchors), anchors,
         xout = seq(1, length(anchors), length.out = k))$y
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { color: #213547; background: #fbfcfa; }
      h1, h2, h3 { color: #17324d; }
      .title-band { border-left: 10px solid #e07a2d; padding-left: 18px; margin-bottom: 22px; }
      .well { background: #eef3f4; border: 0; border-radius: 0; }
      .concept-box { background: #eef5f5; border-left: 7px solid #0b7285;
                     padding: 12px 16px; margin-bottom: 18px; }
      .warning-box { background: #f1edf6; border-left: 7px solid #6a4c93;
                     padding: 10px 14px; margin-top: 14px; }
      .sum-value { font-size: 1.35em; color: #17324d; font-weight: 650; }
      table { background: white; }
    "))
  ),

  div(class = "title-band",
      h1("Building a smooth from basis functions"),
      p("An interactive introduction to the construction behind a GAM smooth")),

  div(class = "concept-box",
      HTML("A smooth is a weighted sum: <b>f(x) = Σ θ<sub>j</sub>b<sub>j</sub>(x)</b>. ",
           "The basis functions <i>b<sub>j</sub>(x)</i> supply simple shapes; ",
           "the coefficients θ<sub>j</sub> determine how much each shape contributes.")),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Controls"),
      selectInput(
        "preset", "Starting shape",
        choices = c("Teaching example", "Approximately linear", "Single hump",
                    "Threshold-like", "Wavy")
      ),
      sliderInput("k", "Number of basis functions (k)",
                  min = 4, max = 8, value = 5, step = 1),
      numericInput("x_focus", "Evaluate the basis at x", value = 0.55,
                   min = 0, max = 1, step = 0.01),
      helpText("Enter an exact value, or click either plot to choose x."),
      actionButton("randomise", "Random coefficients", class = "btn-primary"),
      actionButton("reset", "Reset preset"),
      hr(),
      h3("Basis weights"),
      p("Move a coefficient through zero and watch its contribution reverse direction."),
      uiOutput("coefficient_controls"),
      div(class = "warning-box",
          strong("Important: "),
          "individual basis functions and coefficients normally have no direct ecological interpretation. The combined smooth does.")
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          "Build the smooth",
          br(),
          fluidRow(
            column(6, h3("1. Available basis functions"),
                   plotOutput("basis_plot", height = "380px",
                              click = "basis_click")),
            column(6, h3("2. Weighted contributions and their sum"),
                   plotOutput("contribution_plot", height = "380px",
                              click = "contribution_click"))
          ),
          fluidRow(
            column(7, h3("What is being added at the selected x?"),
                   tableOutput("decomposition_table")),
            column(5, h3("Pointwise sum"),
                   uiOutput("sum_statement"),
                   p("The table contributions add vertically to the black point on the smooth."))
          )
        ),
        tabPanel(
          "Fit simulated data",
          br(),
          fluidRow(
            column(
              3,
              wellPanel(
                h3("Generate data"),
                selectInput("true_shape", "Underlying ecological relationship",
                            choices = c("Single optimum", "Threshold", "Wavy", "Linear")),
                sliderInput("sim_n", "Number of observations", 30, 250, 90, step = 10),
                sliderInput("sim_noise", "Observation noise (SD)", 0.05, 1.2, 0.35,
                            step = 0.05),
                numericInput("sim_seed", "Random seed", value = 2026, min = 1, step = 1),
                actionButton("regenerate", "Generate new data", class = "btn-primary"),
                hr(),
                h3("Fit the GAM"),
                sliderInput("fit_k", "Basis dimension k", 4, 15, 7, step = 1),
                checkboxInput("compare_k", "Compare k = 4, selected k, and k = 12", FALSE)
              )
            ),
            column(
              9,
              h3("Known curve, observations and estimated smooth"),
              plotOutput("simulation_fit_plot", height = "510px"),
              div(class = "concept-box",
                  "The dashed curve generated the data. The GAM sees only the points. ",
                  "Its solid curve is estimated from penalized basis functions; the ribbon ",
                  "shows an approximate 95% confidence interval for the mean.")
            )
          ),
          h2("How this fitted smooth was assembled"),
          fluidRow(
            column(6, h3("1. Intuitive raw B-spline building blocks"),
                   plotOutput("raw_fitted_basis_plot", height = "390px")),
            column(6, h3("2. Centred estimation basis used by mgcv"),
                   plotOutput("fitted_basis_plot", height = "390px"))
          ),
          h3("3. Weighted estimation-basis contributions and reconstructed smooth"),
          plotOutput("fitted_contribution_plot", height = "430px"),
          div(class = "concept-box",
              strong("Why do the two basis panels differ? "),
              "The first shows familiar localized B-spline bumps as an intuitive coordinate system. ",
              "The second shows the basis after mgcv has centred and reparameterized it for ",
              "identifiability, penalization and numerical stability. The fitted coefficients apply ",
              "to this second basis. In panel 3, the black curve is the intercept plus the pointwise ",
              "sum of those coloured weighted contributions, and therefore matches the fitted smooth ",
              "in the main plot."),
          fluidRow(
            column(6, h3("Fit summary"), tableOutput("simulation_summary")),
            column(6, h3("Questions to ask"),
                   tags$ul(
                     tags$li("Does the fitted smooth recover the broad ecological pattern?"),
                     tags$li("What happens to uncertainty when sample size decreases?"),
                     tags$li("Does increasing k force a more wiggly fitted curve?"),
                     tags$li("Which features are supported by data, and which are noise?")
                   ))
          )
        ),
        tabPanel(
          "How to read this",
          br(),
          h2("What the two plots mean"),
          tags$ol(
            tags$li(strong("Basis functions: "),
                    "overlapping building blocks spanning the predictor range."),
            tags$li(strong("Coefficients: "),
                    "weights that scale and possibly reverse each building block."),
            tags$li(strong("Contributions: "),
                    "the products θⱼ bⱼ(x), drawn as coloured curves."),
            tags$li(strong("Smooth: "),
                    "the pointwise sum of all coloured contributions, drawn in black.")
          ),
          h2("Where penalization enters"),
          p("This app lets you set the coefficients freely. In a fitted GAM, the data estimate them while a smoothness penalty discourages combinations that create unnecessary wiggles. The smoothing parameter controls the strength of that penalty."),
          h2("What k does—and does not do"),
          p("The basis dimension k limits the space of shapes available to the model. Increasing k supplies more building blocks, but it does not force the fitted GAM to use all the available complexity. Penalization can shrink the effective complexity."),
          div(class = "concept-box",
              strong("Teaching prompt: "),
              "Set one coefficient to zero. Then reverse its sign. Finally increase k without changing the broad preset. Which changes affect the final smooth, and which only change its representation?")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  coefficient_state <- reactiveVal(preset_coefficients("Teaching example", 5))

  observeEvent(list(input$preset, input$k), {
    coefficient_state(preset_coefficients(input$preset, input$k))
  }, ignoreInit = TRUE)

  observeEvent(input$reset, {
    coefficient_state(preset_coefficients(input$preset, input$k))
  })

  observeEvent(input$randomise, {
    coefficient_state(round(rnorm(input$k, sd = 0.9), 2))
  })

  output$coefficient_controls <- renderUI({
    values <- coefficient_state()
    tagList(lapply(seq_len(input$k), function(j) {
      sliderInput(
        paste0("theta_", j),
        HTML(paste0("θ<sub>", j, "</sub>")),
        min = -2, max = 2,
        value = values[j] %||% 0,
        step = 0.05
      )
    }))
  })

  theta <- reactive({
    defaults <- coefficient_state()
    vapply(seq_len(input$k), function(j) {
      input[[paste0("theta_", j)]] %||% defaults[j] %||% 0
    }, numeric(1))
  })

  observeEvent(input$basis_click, {
    req(input$basis_click$x)
    updateNumericInput(session, "x_focus",
                       value = round(min(1, max(0, input$basis_click$x)), 3))
  })

  observeEvent(input$contribution_click, {
    req(input$contribution_click$x)
    updateNumericInput(session, "x_focus",
                       value = round(min(1, max(0, input$contribution_click$x)), 3))
  })

  construction <- reactive({
    x <- seq(0, 1, length.out = 401)
    basis <- splines::bs(x, df = input$k, degree = 3, intercept = TRUE,
                         Boundary.knots = c(0, 1))
    contributions <- sweep(basis, 2, theta(), `*`)
    focus_basis <- as.numeric(predict(basis, input$x_focus))
    focus_contributions <- focus_basis * theta()
    list(
      x = x,
      basis = basis,
      contributions = contributions,
      smooth = rowSums(contributions),
      focus_basis = focus_basis,
      focus_contributions = focus_contributions,
      focus_sum = sum(focus_contributions)
    )
  })

  output$basis_plot <- renderPlot({
    z <- construction()
    matplot(z$x, z$basis, type = "l", lty = 1, lwd = 3,
            col = palette_basis[seq_len(input$k)],
            xlab = "Predictor x", ylab = expression(b[j](x)), bty = "l")
    abline(v = input$x_focus, col = "#17324D", lty = 3, lwd = 2)
    legend("topright", paste0("b", seq_len(input$k), "(x)"),
           col = palette_basis[seq_len(input$k)], lty = 1, lwd = 3,
           bty = "n", ncol = if (input$k > 5) 2 else 1, cex = 0.8)
  }, res = 110)

  output$contribution_plot <- renderPlot({
    z <- construction()
    yr <- range(c(z$contributions, z$smooth, 0))
    pad <- diff(yr) * 0.08
    matplot(z$x, z$contributions, type = "l", lty = 1, lwd = 2.5,
            col = adjustcolor(palette_basis[seq_len(input$k)], alpha.f = 0.72),
            xlab = "Predictor x", ylab = "Contribution to f(x)",
            ylim = yr + c(-pad, pad), bty = "l")
    abline(h = 0, col = "#9AA6AD", lty = 3)
    abline(v = input$x_focus, col = "#17324D", lty = 3, lwd = 2)
    points(rep(input$x_focus, input$k), z$focus_contributions,
           pch = 21, bg = palette_basis[seq_len(input$k)], col = "white", cex = 1.25)
    lines(z$x, z$smooth, col = "#17324D", lwd = 5)
    points(input$x_focus, z$focus_sum, pch = 21, bg = "#17324D",
           col = "white", cex = 1.8, lwd = 2)
    legend("bottomright", c("weighted basis contributions", "their sum: f(x)"),
           col = c("#6C7A86", "#17324D"), lty = 1, lwd = c(2.5, 5),
           bty = "n", cex = 0.82)
  }, res = 110)

  output$decomposition_table <- renderTable({
    z <- construction()
    data.frame(
      Basis = paste0("b", seq_len(input$k)),
      `Basis value bⱼ(x)` = round(z$focus_basis, 3),
      `Coefficient θⱼ` = round(theta(), 3),
      `Contribution θⱼ bⱼ(x)` = round(z$focus_contributions, 3),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$sum_statement <- renderUI({
    z <- construction()
    terms <- paste(format(round(z$focus_contributions, 3), nsmall = 3), collapse = " + ")
    div(
      class = "sum-value",
      HTML(paste0("f(", sprintf("%.2f", input$x_focus), ") = ",
                  terms, " = ", sprintf("%.3f", z$focus_sum)))
    )
  })

  true_function <- function(x, shape) {
    switch(
      shape,
      "Single optimum" = 1.6 * exp(-((x - 0.56) / 0.19)^2) - 0.45,
      "Threshold" = 1.6 / (1 + exp(-14 * (x - 0.52))) - 0.8,
      "Wavy" = 0.85 * sin(2 * pi * x) + 0.35 * cos(4 * pi * x),
      "Linear" = 1.7 * x - 0.85
    )
  }

  simulated_data <- reactive({
    # Depend on every data-generating control. The button changes the seed and
    # therefore requests a new realization under the current assumptions.
    input$regenerate
    set.seed(input$sim_seed + input$regenerate)
    x <- sort(runif(input$sim_n))
    truth <- true_function(x, input$true_shape)
    data.frame(x = x, truth = truth,
               y = truth + rnorm(input$sim_n, sd = input$sim_noise))
  })

  simulation_fit <- reactive({
    mgcv::gam(y ~ s(x, k = input$fit_k), data = simulated_data(), method = "REML")
  })

  fitted_basis_decomposition <- reactive({
    model <- simulation_fit()
    grid <- data.frame(x = seq(0, 1, length.out = 401))
    design <- predict(model, grid, type = "lpmatrix")
    smooth_index <- model$smooth[[1]]$first.para:model$smooth[[1]]$last.para
    parametric_index <- setdiff(seq_along(coef(model)), smooth_index)
    basis <- design[, smooth_index, drop = FALSE]
    coefficients <- coef(model)[smooth_index]
    contributions <- sweep(basis, 2, coefficients, `*`)
    parametric_part <- if (length(parametric_index)) {
      as.numeric(design[, parametric_index, drop = FALSE] %*%
                   coef(model)[parametric_index])
    } else {
      rep(0, nrow(grid))
    }
    list(
      x = grid$x,
      basis = basis,
      coefficients = coefficients,
      contributions = contributions,
      parametric_part = parametric_part,
      fitted = parametric_part + rowSums(contributions)
    )
  })

  output$simulation_fit_plot <- renderPlot({
    d <- simulated_data()
    model <- simulation_fit()
    grid <- data.frame(x = seq(0, 1, length.out = 401))
    pr <- predict(model, grid, se.fit = TRUE)
    truth_grid <- true_function(grid$x, input$true_shape)

    yr <- range(c(d$y, truth_grid, pr$fit - 1.96 * pr$se.fit,
                  pr$fit + 1.96 * pr$se.fit))
    plot(d$x, d$y, pch = 16, col = adjustcolor("#6C7A86", 0.48),
         xlab = "Environmental gradient x", ylab = "Response y",
         ylim = yr, bty = "l")
    polygon(c(grid$x, rev(grid$x)),
            c(pr$fit - 1.96 * pr$se.fit, rev(pr$fit + 1.96 * pr$se.fit)),
            col = adjustcolor("#E07A2D", 0.18), border = NA)
    lines(grid$x, truth_grid, col = "#0B7285", lwd = 3, lty = 2)
    lines(grid$x, pr$fit, col = "#E07A2D", lwd = 4)

    if (isTRUE(input$compare_k)) {
      comparison_k <- unique(c(4, input$fit_k, 12))
      comparison_k <- comparison_k[comparison_k != input$fit_k]
      comparison_cols <- c("#6A4C93", "#2A9D8F")
      for (j in seq_along(comparison_k)) {
        m_alt <- mgcv::gam(y ~ s(x, k = comparison_k[j]), data = d, method = "REML")
        lines(grid$x, predict(m_alt, grid), col = comparison_cols[j],
              lwd = 2.5, lty = 3)
      }
      legend_labels <- c("true relationship", paste0("GAM, k = ", input$fit_k),
                         paste0("GAM, k = ", comparison_k))
      legend_cols <- c("#0B7285", "#E07A2D", comparison_cols[seq_along(comparison_k)])
      legend_lty <- c(2, 1, rep(3, length(comparison_k)))
      legend_lwd <- c(3, 4, rep(2.5, length(comparison_k)))
    } else {
      legend_labels <- c("true relationship", paste0("GAM, k = ", input$fit_k))
      legend_cols <- c("#0B7285", "#E07A2D")
      legend_lty <- c(2, 1)
      legend_lwd <- c(3, 4)
    }
    legend("topright", legend_labels, col = legend_cols, lty = legend_lty,
           lwd = legend_lwd, bty = "n", cex = 0.82)
  }, res = 110)

  output$raw_fitted_basis_plot <- renderPlot({
    x <- seq(0, 1, length.out = 401)
    raw_basis <- splines::bs(x, df = input$fit_k, degree = 3,
                             intercept = TRUE, Boundary.knots = c(0, 1))
    basis_colours <- grDevices::colorRampPalette(palette_basis)(ncol(raw_basis))
    matplot(x, raw_basis, type = "l", lty = 1, lwd = 2.5,
            col = basis_colours, xlab = "Environmental gradient x",
            ylab = expression(B[j](x)), bty = "l")
    legend("topright", paste0("B", seq_len(ncol(raw_basis)), "(x)"),
           col = basis_colours, lty = 1, lwd = 2.5, bty = "n",
           ncol = if (ncol(raw_basis) > 7) 2 else 1, cex = 0.68)
  }, res = 110)

  output$fitted_basis_plot <- renderPlot({
    z <- fitted_basis_decomposition()
    basis_colours <- grDevices::colorRampPalette(palette_basis)(ncol(z$basis))
    matplot(z$x, z$basis, type = "l", lty = 1, lwd = 2.5,
            col = basis_colours, xlab = "Environmental gradient x",
            ylab = expression(b[j](x)), bty = "l")
    legend("topright", paste0("b", seq_len(ncol(z$basis)), "(x)"),
           col = basis_colours, lty = 1, lwd = 2.5, bty = "n",
           ncol = if (ncol(z$basis) > 7) 2 else 1, cex = 0.68)
  }, res = 110)

  output$fitted_contribution_plot <- renderPlot({
    z <- fitted_basis_decomposition()
    basis_colours <- grDevices::colorRampPalette(palette_basis)(ncol(z$contributions))
    yr <- range(c(z$contributions, z$parametric_part, z$fitted))
    pad <- max(diff(yr) * 0.07, 0.05)
    matplot(z$x, z$contributions, type = "l", lty = 1, lwd = 2.2,
            col = adjustcolor(basis_colours, alpha.f = 0.72),
            xlab = "Environmental gradient x",
            ylab = "Contribution to fitted response", ylim = yr + c(-pad, pad),
            bty = "l")
    abline(h = 0, col = "#9AA6AD", lty = 3)
    lines(z$x, z$parametric_part, col = "#6C7A86", lwd = 2.5, lty = 2)
    lines(z$x, z$fitted, col = "#17324D", lwd = 5)
    legend("topright",
           c("weighted basis contributions", "intercept", "sum: fitted smooth"),
           col = c("#6A4C93", "#6C7A86", "#17324D"),
           lty = c(1, 2, 1), lwd = c(2.2, 2.5, 5), bty = "n", cex = 0.75)
  }, res = 110)

  output$simulation_summary <- renderTable({
    d <- simulated_data()
    model <- simulation_fit()
    grid <- data.frame(x = seq(0, 1, length.out = 401))
    truth_grid <- true_function(grid$x, input$true_shape)
    predicted_grid <- predict(model, grid)
    s <- summary(model)
    data.frame(
      Quantity = c("Observations", "Chosen basis dimension k",
                   "Estimated effective df", "Deviance explained",
                   "RMSE from the known curve"),
      Value = c(nrow(d), input$fit_k,
                round(s$s.table[1, "edf"], 2),
                paste0(round(100 * s$dev.expl, 1), "%"),
                round(sqrt(mean((predicted_grid - truth_grid)^2)), 3)),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = FALSE, spacing = "s")
}

shinyApp(ui, server)
