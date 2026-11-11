# OPTIONAL FOLLOW-UP: generate the workshop results from the raw Ames data.
# Open nora_uq_workshop.Rproj, then run this script from start to finish:
# source("05_generate_reference.R")
# Install the fitting package once if needed: install.packages("ranger")
# The defaults use the workshop settings. Try changing the tree counts or seed.
# The original reference used R 4.4.3 and ranger 0.18.0; versions may affect results.
source("R/helpers.R")
if (!requireNamespace("ranger", quietly = TRUE)) {
  stop('First run install.packages("ranger"), then rerun this script.')
}

# SETTINGS --------------------------------------------------------------
split_seed = 20260914
main_trees = 300
refit_trees = 100
refit_repeats = 20
output_file = "results/reference_results.rds"
# To use your generated results in the exercises, change output_file to
# "cache/reference_results.rds". That replaces the included workshop results.
stopifnot(main_trees >= 1, refit_trees >= 1, refit_repeats >= 2)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

# 1. FIT THE QUANTILE FOREST ---------------------------------------------
# Preprocessing is learned on the training split inside fit_forest().
# Its other settings are mtry = 6, minimum node size = 10 and forest seed = 42.
# split_ames() also seeds the R-side sampling used when fitting the forest.
message("Fitting the main quantile forest...")
data = read_ames()
split = split_ames(data, seed = split_seed)
check_split(split)
started = proc.time()[["elapsed"]]
fit = fit_forest(split$train, trees = main_trees, seed = 42)
q_cal = predict_intervals(fit, split$calibration)
q_test = predict_intervals(fit, split$test)
main_seconds = proc.time()[["elapsed"]] - started

# 2. CALIBRATE THE INTERVALS ---------------------------------------------
# Only calibration prices determine the correction. Test prices evaluate it.
correction = conformal_correction(split$calibration$price, q_cal, alpha = 0.10)
q_calibrated = calibrate_intervals(q_test, correction)
metrics = rbind(raw = interval_metrics(split$test$price, q_test),
               conformal = interval_metrics(split$test$price, q_calibrated))

# 3. REFIT ON RESAMPLED TRAINING SALES -----------------------------------
# Select the same 100 test houses without looking at their observed prices.
target_indices = round(seq(1, nrow(split$test), length.out = 100))
target = split$test[target_indices, ]
message("Fitting ", 2 * refit_repeats, " forests for the resampling comparison...")
started = proc.time()[["elapsed"]]
refits = resample_forests(split$train, target, sizes = c(300, 1500),
                         repeats = refit_repeats, trees = refit_trees)
refit_seconds = proc.time()[["elapsed"]] - started
# The helper samples rows WITH replacement and refits preprocessing each time.
# Resample b uses set.seed(94000 + b); every forest uses forest seed 42.
# It saves each house's 20 predicted medians and their SD at the default settings.
stability = data.frame(
  size = as.numeric(names(refits)),
  median_prediction_sd = unname(vapply(refits, function(x)
    median(x$summary$prediction_sd), numeric(1))),
  median_outcome_width = unname(vapply(refits, function(x)
    median(x$summary$median_interval_width), numeric(1))))
fraction_more_stable = mean(refits[["1500"]]$summary$prediction_sd <
                             refits[["300"]]$summary$prediction_sd)

# 4. RECREATE THE SIMULATION AND SUPPORTING SUMMARIES ---------------------
started = proc.time()[["elapsed"]]
simulations = list(small = simulate_uncertainty(50, 10),
                   large = simulate_uncertainty(500, 10),
                   noisy = simulate_uncertainty(500, 30))
simulation_seconds = proc.time()[["elapsed"]] - started
# Houses A/B/C are selected by predicted-median rank among the 100 targets.
case_positions = order(q_test$median[target_indices])[c(20, 50, 80)]
cases = cbind(label = c("A", "B", "C"), target[case_positions, ],
              q_test[target_indices[case_positions], ])
# Preserve the reference file's size-group summary; the cutoff uses training data.
larger = split$test$living_area > quantile(split$train$living_area, 0.75)
groups = rbind(
  smaller = interval_metrics(split$test$price[!larger], q_test[!larger, ]),
  larger = interval_metrics(split$test$price[larger], q_test[larger, ]))

# 5. SAVE IN THE FORMAT USED BY THE EXERCISES -----------------------------
# Everything above is generated from data/AmesHousing.txt and R/helpers.R.
# The included cache is not needed to run this script.
reference = list(
  split = split, test_intervals = q_test, calibration_intervals = q_cal,
  calibrated_intervals = q_calibrated, correction = correction, metrics = metrics,
  target = target, target_indices = target_indices, refits = refits,
  simulations = simulations, cases = cases, case_positions = case_positions,
  groups = groups, stability = stability, fraction_more_stable = fraction_more_stable,
  versions = c(R = as.character(getRversion()),
               ranger = as.character(packageVersion("ranger"))),
  times = c(main_forest = main_seconds, refits = refit_seconds,
            simulation = simulation_seconds))
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(reference, output_file)
cat("\nCQR endpoint adjustment (USD):", correction, "\n")
print(metrics[, c("coverage", "mean_width")])
print(stability[, c("size", "median_prediction_sd")], row.names = FALSE)
cat("Fraction with lower refit SD at 1,500 rows:", fraction_more_stable, "\n")
cat("Saved:", output_file, "\n")

# To inspect the new results in R:
# generated = readRDS(output_file)
# plot_intervals(generated$split$test$price, generated$test_intervals)
# plot_refit_summary(generated$refits)
