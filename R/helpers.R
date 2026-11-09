# NORA-Tech: uncertainty in house-price prediction
# All assignments use =. Run from the project directory.
# Units: prices in historical US dollars; area in square metres.
options(stringsAsFactors = FALSE)
pal = c(ink = "#203D49", blue = "#2676A8", green = "#2D7A59", coral = "#BC6748",
        paper = "#FCFEFD", grey = "#536E79", mint = "#E9F7EE", sky = "#EAF4FC")
workshop_plot_style = function() {
  par(bg = pal[["paper"]], fg = pal[["ink"]], col = pal[["ink"]],
      col.axis = pal[["grey"]], col.lab = pal[["ink"]], col.main = pal[["ink"]])
}
numeric_features = c("living_area", "lot_area", "year_built", "year_remodel", "condition", "bathrooms", "bedrooms", "garage_cars")

read_ames = function(path = "data/AmesHousing.txt") {
  if (!file.exists(path)) stop("Open nora_uq_workshop.Rproj first. The data file should be in data/.")
  x = read.delim(path, check.names = FALSE)
  data.frame(property_id = x$PID, price = x$SalePrice,
    living_area = x[["Gr Liv Area"]] / 10.7639104,
    lot_area = x[["Lot Area"]] / 10.7639104,
    year_built = x[["Year Built"]], year_remodel = x[["Year Remod/Add"]],
    condition = x[["Overall Cond"]], bathrooms = x[["Full Bath"]],
    bedrooms = x[["Bedroom AbvGr"]], garage_cars = x[["Garage Cars"]],
    neighborhood = x$Neighborhood, sale_year = x[["Yr Sold"]])
}

split_ames = function(data, seed = 20260914) {
  set.seed(seed)
  index = sample.int(nrow(data))
  n_train = floor(0.6 * nrow(data)); n_cal = floor(0.2 * nrow(data))
  list(train = data[index[seq_len(n_train)], ],
       calibration = data[index[n_train + seq_len(n_cal)], ],
       test = data[index[(n_train + n_cal + 1):nrow(data)], ])
}

learn_preprocessing = function(data) {
  counts = table(data$neighborhood)
  list(medians = vapply(data[numeric_features], median, numeric(1), na.rm = TRUE),
       neighborhoods = sort(names(counts[counts >= 10])))
}

apply_preprocessing = function(data, recipe) {
  out = data[numeric_features]
  for (name in numeric_features) out[[name]][is.na(out[[name]])] = recipe$medians[[name]]
  # Unseen / rare neighborhoods share the all-zero indicator pattern.
  for (level in recipe$neighborhoods) {
    out[[paste0("neighborhood_", make.names(level))]] = as.integer(!is.na(data$neighborhood) & data$neighborhood == level)
  }
  out
}

# For the reference run, start with split_ames(read_ames()). Its seed also
# initializes R-side sampling used by quantile-forest fitting. The seed below
# controls ranger's forest construction. Refit experiments seed each resample.
fit_forest = function(train, trees = 300, seed = 42) {
  if (!requireNamespace("ranger", quietly = TRUE)) stop("Install ranger with 00_setup.R, or use cached results.")
  recipe = learn_preprocessing(train)
  x = apply_preprocessing(train, recipe)
  model = ranger::ranger(x = x, y = train$price, num.trees = trees,
    mtry = min(6L, ncol(x)), min.node.size = 10, quantreg = TRUE,
    keep.inbag = TRUE, num.threads = 1, seed = seed)
  list(model = model, recipe = recipe)
}

predict_intervals = function(fit, newdata, level = 0.90) {
  stopifnot(length(level) == 1, level > 0, level < 1)
  alpha = 1 - level
  q = predict(fit$model, data = apply_preprocessing(newdata, fit$recipe),
    type = "quantiles", quantiles = c(alpha / 2, 0.5, 1 - alpha / 2),
    num.threads = 1)$predictions
  data.frame(lower = q[, 1], median = q[, 2], upper = q[, 3])
}

interval_metrics = function(y, intervals) {
  stopifnot(length(y) == nrow(intervals), !anyNA(intervals))
  covered = y >= intervals$lower & y <= intervals$upper
  c(n = length(y), coverage = mean(covered),
    mean_width = mean(pmax(0, intervals$upper - intervals$lower)),
    median_width = median(pmax(0, intervals$upper - intervals$lower)),
    median_prediction_mae = mean(abs(y - intervals$median)),
    empty_intervals = sum(intervals$upper < intervals$lower))
}

conformal_correction = function(y_cal, q_cal, alpha = 0.10) {
  stopifnot(length(y_cal) == nrow(q_cal), alpha > 0, alpha < 1)
  scores = pmax(q_cal$lower - y_cal, y_cal - q_cal$upper)
  k = ceiling((length(scores) + 1) * (1 - alpha))
  if (k > length(scores)) return(Inf)
  # Signed CQR score: a negative correction can narrow conservative intervals.
  sort(scores)[k]
}

calibrate_intervals = function(intervals, correction) {
  intervals$lower = intervals$lower - correction
  intervals$upper = intervals$upper + correction
  intervals
}

plot_intervals = function(y, intervals, indices = seq_len(min(24, length(y))),
                          title = "Held-out house prices", level = 0.90) {
  stopifnot(length(level) == 1, is.finite(level), level > 0, level < 1)
  old_style = workshop_plot_style()
  on.exit(par(old_style))
  # Select rows independently of outcomes, then order by predicted median.
  indices = indices[order(intervals$median[indices])]
  q = intervals[indices, ]; actual = y[indices]
  covered = actual >= q$lower & actual <= q$upper
  plot(seq_along(indices), q$median / 1000, type = "n",
       ylim = range(c(q$lower, q$upper, actual)) / 1000,
       xlab = "House (ordered by predicted median)", ylab = "Price (USD thousands)", main = title)
  segments(seq_along(indices), q$lower / 1000, seq_along(indices), q$upper / 1000,
           col = pal[["green"]], lwd = 3)
  points(seq_along(indices), q$median / 1000, pch = 3, col = pal[["blue"]])
  points(seq_along(indices), actual / 1000, pch = ifelse(covered, 16, 4),
         col = pal[["coral"]], cex = 1.2)
  legend("topleft", c(paste0(format(100 * level, trim = TRUE), "% outcome interval"),
         "Predicted median", "Observed price", "Outside interval"),
         col = pal[c("green", "blue", "coral", "coral")],
         lty = c(1, NA, NA, NA), pch = c(NA, 3, 16, 4), bty = "n", cex = 0.8)
}

resample_forests = function(train, target, sizes = c(300, 1500), repeats = 20, trees = 100) {
  out = list()
  for (size in sizes) {
    a = array(NA_real_, dim = c(nrow(target), 3, repeats),
              dimnames = list(NULL, c("lower", "median", "upper"), NULL))
    for (b in seq_len(repeats)) {
      set.seed(94000 + b)
      chosen = sample.int(nrow(train), size, replace = TRUE)
      fit = fit_forest(train[chosen, ], trees = trees, seed = 42)
      a[, , b] = as.matrix(predict_intervals(fit, target))
    }
    out[[as.character(size)]] = list(predictions = a,
      summary = data.frame(property_id = target$property_id,
        prediction_sd = apply(a[, "median", , drop = FALSE], 1, sd),
        median_interval_width = apply(a[, "upper", ] - a[, "lower", ], 1, median)))
  }
  out
}

plot_refit_summary = function(refits) {
  old_style = workshop_plot_style()
  on.exit(par(old_style))
  small = refits[[1]]$summary$prediction_sd / 1000
  large = refits[[2]]$summary$prediction_sd / 1000
  bounds = range(c(0, small, large))
  plot(small, large, xlim = bounds, ylim = bounds, pch = 16, col = pal[["blue"]],
       xlab = paste("Refit SD with", names(refits)[1], "rows (USD thousands)"),
       ylab = paste("Refit SD with", names(refits)[2], "rows (USD thousands)"),
       main = "Each point is the same house at two sample sizes")
  abline(0, 1, lty = 2, col = pal[["grey"]])
}

simulate_uncertainty = function(n, noise_sd, repeats = 20, seed = 7001) {
  # Synthetic data only. The conditional median and normal noise are known here.
  # A correctly specified straight-line learner keeps this introductory
  # experiment transparent. The real-data exercises use quantile forests.
  truth = function(x) 100 + 70 * x
  grid = seq(0.05, 0.95, length.out = 41)
  curves = matrix(NA_real_, nrow = length(grid), ncol = repeats)
  for (b in seq_len(repeats)) {
    set.seed(seed + b)
    x = runif(n)
    y = truth(x) + rnorm(n, sd = noise_sd)
    fit = lm(y ~ x)
    curves[, b] = predict(fit, newdata = data.frame(x = grid))
  }
  list(n = n, noise_sd = noise_sd, grid = grid, truth = truth(grid),
       lower = truth(grid) + qnorm(0.05) * noise_sd,
       upper = truth(grid) + qnorm(0.95) * noise_sd, curves = curves,
       median_refit_sd = median(apply(curves, 1, sd)))
}

plot_simulation = function(sim, title = paste("n =", sim$n, ", noise SD =", sim$noise_sd)) {
  old_style = workshop_plot_style()
  on.exit(par(old_style))
  plot(sim$grid, sim$truth, type = "n", ylim = c(30, 230), xlab = "Synthetic property feature",
       ylab = "Synthetic price (USD thousands)", main = title)
  polygon(c(sim$grid, rev(sim$grid)), c(sim$lower, rev(sim$upper)),
          col = "#D7EFE0", border = NA)
  lines(sim$grid, sim$lower, col = pal[["green"]], lty = 2, lwd = 1.5)
  lines(sim$grid, sim$upper, col = pal[["green"]], lty = 2, lwd = 1.5)
  matlines(sim$grid, sim$curves[, 1:8], col = "#6BA8CC", lty = 1, lwd = 1)
  lines(sim$grid, sim$truth, col = pal[["ink"]], lwd = 3)
}

load_reference = function() readRDS("cache/reference_results.rds")

check_split = function(split) {
  ids = lapply(split, function(x) x$property_id)
  stopifnot(length(intersect(ids$train, ids$test)) == 0,
            length(intersect(ids$train, ids$calibration)) == 0,
            length(intersect(ids$calibration, ids$test)) == 0)
  invisible(TRUE)
}

# Bayesian illustration for 07_model_assumptions.R -----------------------
# Prices and SDs in this section use USD thousands; variances use their square.
# Settings are passed explicitly by the follow-up script.
design = function(floor_m2, degree = 1) {
  # Centering/scaling gives coefficients comparable units.
  x = (floor_m2 - 120) / 50
  if (degree == 1) return(cbind(intercept = 1, x = x))
  if (degree == 2) return(cbind(intercept = 1, x = x, x_squared = x^2))
  stop("Use degree = 1 (straight line) or 2 (quadratic curve).")
}

true_price = function(floor_m2, curved = FALSE) {
  x = (floor_m2 - 120) / 50
  200 + 75*x + if (curved) 30*x^2 else 0
}

simulate_sales = function(n, noise_sd_k, curved = FALSE) {
  floor_m2 = runif(n, 60, 200)
  data.frame(floor_m2 = floor_m2,
             price_k = true_price(floor_m2, curved) +
               rnorm(n, sd = noise_sd_k))
}

fit_noise_model = function(data, noise_sd_k, degree = 1,
                           prior_sd = 100) {
  stopifnot(is.finite(noise_sd_k), noise_sd_k > 0, prior_sd > 0)
  X = design(data$floor_m2, degree)
  p = ncol(X)
  prior_mean = c(200, rep(0, p - 1))
  prior_precision = diag(1 / prior_sd^2, p)
  # Gaussian likelihood + Gaussian prior give an exact Gaussian posterior.
  posterior_cov = chol2inv(chol(prior_precision +
                                 crossprod(X) / noise_sd_k^2))
  posterior_mean = drop(posterior_cov %*%
                         (prior_precision %*% prior_mean +
                            crossprod(X, data$price_k) / noise_sd_k^2))
  list(mean = posterior_mean, cov = posterior_cov,
       noise_sd_k = noise_sd_k, degree = degree)
}

predict_components = function(fit, floor_m2) {
  Xnew = design(floor_m2, fit$degree)
  fitted = drop(Xnew %*% fit$mean)
  aleatoric_var = rep(fit$noise_sd_k^2, length(floor_m2))
  epistemic_var = pmax(0, rowSums((Xnew %*% fit$cov) * Xnew))
  predictive_var = aleatoric_var + epistemic_var
  q = qnorm(0.95) # central 90% intervals under this Gaussian model
  data.frame(floor_m2 = floor_m2, fitted_price_k = fitted,
             aleatoric_var = aleatoric_var, epistemic_var = epistemic_var,
             predictive_var = predictive_var,
             aleatoric_sd_k = sqrt(aleatoric_var),
             epistemic_sd_k = sqrt(epistemic_var),
             predictive_sd_k = sqrt(predictive_var),
             function_lower = fitted - q*sqrt(epistemic_var),
             function_upper = fitted + q*sqrt(epistemic_var),
             prediction_lower = fitted - q*sqrt(predictive_var),
             prediction_upper = fitted + q*sqrt(predictive_var))
}

summarize_fits = function(fits, labels, true_target, target_floor_m2) {
  do.call(rbind, lapply(seq_along(fits), function(i) {
    p = predict_components(fits[[i]], target_floor_m2)
    data.frame(scenario = labels[i], p,
               true_function_k = true_target,
               function_error_k = p$fitted_price_k - true_target,
               prediction_width_k = p$prediction_upper - p$prediction_lower)
  }))
}

show_summary = function(tab, title) {
  cat("\n", title, "\n", sep = "")
  cat("Same target house:", unique(tab$floor_m2), "m^2. Prices and SDs in $1,000.\n")
  cols = c("scenario", "fitted_price_k", "aleatoric_sd_k", "epistemic_sd_k",
           "prediction_width_k")
  compact = tab[, cols]
  compact[-1] = lapply(compact[-1], round, 2)
  names(compact) = c("Scenario", "Fitted_price", "Aleatoric_SD", "Epistemic_SD",
                    "PI90_width")
  print(compact, row.names = FALSE)
}

plot_comparison = function(fits, datasets, labels, curved, heading, target_floor_m2) {
  grid = seq(min(60, target_floor_m2), max(200, target_floor_m2), length.out = 200)
  predictions = lapply(fits, predict_components, floor_m2 = grid)
  y_limits = range(unlist(lapply(predictions, function(p)
    c(p$prediction_lower, p$prediction_upper))),
    unlist(lapply(datasets, function(d) d$price_k)), true_price(grid, curved))
  old = par(mfrow = c(1, length(fits)), mar = c(4.5, 4.5, 3, 1),
            oma = c(4.4, 0, 3, 0), bg = "white", fg = "#203D49",
            col.axis = "#203D49", col.lab = "#203D49", cex = 0.95)
  on.exit(par(old))
  for (i in seq_along(fits)) {
    p = predictions[[i]]
    plot(grid, p$fitted_price_k, type = "n", ylim = y_limits,
         xlab = "Floor area (m^2)", ylab = "Synthetic price ($1,000)",
         main = labels[i])
    polygon(c(grid, rev(grid)),
            c(p$prediction_lower, rev(p$prediction_upper)),
            col = "#DDF1E5", border = NA)
    polygon(c(grid, rev(grid)), c(p$function_lower, rev(p$function_upper)),
            col = "#9BC9EA", border = NA)
    points(datasets[[i]]$floor_m2, datasets[[i]]$price_k,
           pch = 16, cex = 0.45, col = adjustcolor("#536E79", alpha.f = 0.35))
    lines(grid, p$fitted_price_k, col = "#2676A8", lwd = 2)
    lines(grid, true_price(grid, curved), col = "#203D49", lwd = 2, lty = 2)
    abline(v = target_floor_m2, lty = 3, col = "#7F8F96")
  }
  mtext(heading, outer = TRUE, side = 3, line = -0.3, font = 2, cex = 1.2)
  mtext("Green: 90% future-price interval     Blue: 90% credible band for the price function",
        outer = TRUE, side = 1, line = 1, cex = 0.95)
  mtext("Solid: fitted price function     Dashed: true price function (simulation)",
        outer = TRUE, side = 1, line = 2.3, cex = 0.9)
  mtext("Both bands are pointwise and conditional on the model assumptions.",
        outer = TRUE, side = 1, line = 3.4, cex = 0.85)
}

save_assumption_plots = function(draw, path) {
  # Cairo embeds fonts when available; both devices are supplied with base R.
  if (capabilities("cairo")) {
    cairo_pdf(path, width = 12, height = 6, family = "sans")
  } else {
    pdf(path, width = 12, height = 6)
  }
  on.exit(dev.off())
  draw()
}
