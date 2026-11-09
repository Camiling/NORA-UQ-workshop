# EXERCISE 2: How stable are the predicted medians? -----------------------
# Work in pairs. Open nora_uq_workshop.Rproj first.
# Run this script section by section, filling in the three NA values.
# For ready-to-run calculations, use the worked solution instead:
# source("solutions/02_refits_solution.R")
# Finish with three numbers, one finding and one question.

# A. Predict ------------------------------------------------------------
# Will using 1,500 sampled training rows rather than 300 give more stable
# median predictions? Do you expect this for every house?
# Your prediction: <TO DO>

# B. Inspect the supplied results ---------------------------------------
# At each sample size, 20 whole forests were fitted to different resamples
# drawn WITH REPLACEMENT from the same training pool (you won't have to do this).
# Each forest predicted the median price for the same 100 target houses.
# For each house, ensemble SD is the SD of its 20 predicted medians.
# We also call this "refit SD". Smaller SD means greater stability here.
source("R/helpers.R")
reference = load_reference()
refits = reference$refits # Load the results obtained by resampling
small = refits[["300"]]$summary # Small means 300 sampled training sales
large = refits[["1500"]]$summary # Large means 1,500 sampled training sales

# Each row compares the same house at both sample sizes. SDs are in USD.
head(data.frame(sd_300 = small$prediction_sd,
                sd_1500 = large$prediction_sd), 3) # Three first houses
dev.new()
plot_refit_summary(refits)
# Below the diagonal: lower refit SD with 1,500 sampled rows.

# C. Calculate three summaries across the 100 houses ---------------------
# Summarize the house-specific SDs with their median at each sample size.
median_sd_small = NA_real_ # <TO DO>
median_sd_large = NA_real_ # <TO DO>

# What fraction of houses have lower SD with 1,500 sampled rows?
fraction_more_stable = NA_real_ # <TO DO>

print(c(median_sd_300_USD = median_sd_small,
        median_sd_1500_USD = median_sd_large,
        fraction_more_stable = fraction_more_stable))

# D. Discuss ------------------------------------------------------------
# What do your results say about sensitivity to the sampled training data?
# Could all 20 forests make a similar mistake for the same house?
# Your finding: <TO DO>
# One question you would investigate next: <TO DO>

# Stop here when your three numbers, finding and question are ready.
# In the debrief we will compare the difference between the two summaries:
# - Prediction intervals describe a range for a future sale price.
# - Ensemble SD describes sensitivity of the fitted median to resampling.
# An aleatoric-epistemic split requires additional modelling assumptions.
