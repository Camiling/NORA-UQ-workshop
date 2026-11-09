# CORE GUIDED DEMONSTRATION: CQR.
# We calibrate the prediction intervals from Exercise 1.
# The rule and 90% target are fixed before looking at the test results.
source("R/helpers.R")
reference = load_reference() # Load pre-done calculations
split = reference$split
names(split) # Dataset divided in three 
q_cal = reference$calibration_intervals # (Uncalibrated) intervals when applying forest to calibration data
q_test = reference$test_intervals # (Uncalibrated) intervals when applying forest to test data

# Use the calibration data to choose one adjustment to the endpoints.
adjustment = conformal_correction(split$calibration$price, q_cal, alpha = 0.10) # Compute the width adjustment
q_calibrated = calibrate_intervals(q_test, adjustment) # Add adjustment to old intervals
print(adjustment)  # A negative value narrows the original intervals.

# Evaluate on the test data
comparison = rbind(before = interval_metrics(split$test$price, q_test),
                   after = interval_metrics(split$test$price, q_calibrated))
print(comparison[, c("coverage", "mean_width")])
dev.new()
plot_intervals(split$test$price, q_calibrated, title = "After split conformal calibration")
# Guarantee: marginal coverage under exchangeability, over calibration data
# and a fresh test pair (conditional on training). Observed coverage varies
# between finite test sets. Subgroups and changed markets need separate checks.
# Calibration targets coverage. An aleatoric-epistemic split needs further
# modelling assumptions.
