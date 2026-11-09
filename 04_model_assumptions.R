# OPTIONAL FOLLOW-UP: making the uncertainty sources explicit 
# Run this whole script as written. It uses only base R and synthetic prices.
# Open the workshop .Rproj first, then:
# source("04_model_assumptions.R")
# Read the printed tables and look at the plots
# No code completion, package installation or internet connection is needed.
#
# THE QUESTION
# Prediction intervals and ensemble SD answer useful, different questions.
# To define separate aleatoric and epistemic components, we now specify a
# probabilistic model for both outcome noise and the unknown price function.
# This is a small Bayesian illustration, separate from the forest exercises.
#
# MODEL ASSUMPTIONS (note that we now simulate our own data!)
# price = f(floor area) + independent Gaussian noise
# The noise SD is treated as KNOWN in each fitted model.
# The unknown coefficients of f have an independent Gaussian prior.
# Given these assumptions, posterior predictive VARIANCE is the sum of:
#   aleatoric variance = the assumed noise variance
#   epistemic variance = posterior variance of the price function
# Their standard deviations and interval widths do not add.
# Here the Gaussian model's conditional mean and median coincide.
# Epistemic uncertainty here is conditional on the chosen function class,
# prior and noise SD. It does not account for every possible model error.
#
# Try the three comparisons first. Then change one setting and rerun.
# Supplied model and plotting functions are in R/helpers.R.
source("R/helpers.R")

# SETTINGS YOU CAN CHANGE ------------------------------------------------
seed = 20261104 # Ensures reproducibility
n_small = 30
n_large = 300
target_floor_m2 = 120            # the same house in every printed comparison
true_noise_sd_k = 20            # synthetic outcome noise, in $1,000
noise_multipliers = c(0.5, 1, 2) # three assumed noise SDs for the SAME data
prior_sd_k = 100                # prior SD for each regression coefficient

stopifnot(n_small >= 3, n_large >= n_small, true_noise_sd_k > 0,
          prior_sd_k > 0, all(noise_multipliers > 0),
          is.finite(target_floor_m2)) # Ensure we do not run with illegal parameters

# 1. MORE DATA, SAME MODEL AND NOISE ASSUMPTION ----------------------------
# Predict: which component can fall when we add training observations?
set.seed(seed)
sales = simulate_sales(n_large, true_noise_sd_k)
small_sales = sales[seq_len(n_small), ] # a subset of the SAME larger dataset
fits_n = list(fit_noise_model(small_sales, true_noise_sd_k, prior_sd = prior_sd_k),
              fit_noise_model(sales, true_noise_sd_k, prior_sd = prior_sd_k))
labels_n = paste(c(n_small, n_large), "sales")
sample_size_results = summarize_fits(fits_n, labels_n,
                                    true_price(target_floor_m2), target_floor_m2)
show_summary(sample_size_results, "1. More data, same assumptions")
dev.new()
plot_comparison(fits_n, list(small_sales, sales), labels_n, FALSE,
                "1. More data, same assumptions", target_floor_m2)
cat("The noise SD stays fixed because the model treats it as known.\n",
    "More data reduce posterior uncertainty about the price function.\n", sep = "")

# 2. SAME DATA, DIFFERENT ASSUMED NOISE -----------------------------------
# Predict: does changing the noise assumption affect BOTH components?
# The data stay fixed. Only the noise SD supplied to the fitted model changes.
assumed_noise = true_noise_sd_k * noise_multipliers
fits_noise = lapply(assumed_noise, function(s)
  fit_noise_model(sales, s, prior_sd = prior_sd_k))
labels_noise = paste("Assumed noise SD", assumed_noise)
noise_assumption_results = summarize_fits(fits_noise, labels_noise,
                                         true_price(target_floor_m2), target_floor_m2)
show_summary(noise_assumption_results, "2. Same data, different assumed noise")
dev.new() 
plot_comparison(fits_noise, rep(list(sales), length(fits_noise)), labels_noise,
                FALSE, "2. Same data, different assumed noise", target_floor_m2)
cat("True simulated noise SD:", true_noise_sd_k, "($1,000).\n",
    "These components depend on the assumed noise, even with identical data.\n",
    "The model also uses that assumption when learning the coefficients.\n")

# 3. SAME CURVED DATA, DIFFERENT FUNCTION CLASSES --------------------------
# Predict: can a model report a small epistemic SD while missing the true f?
# Both models assume the same (correct) noise SD. The straight-line class
# cannot express the curved generating relationship.
set.seed(seed + 1)
curved_sales = simulate_sales(n_large, true_noise_sd_k, curved = TRUE)
fits_shape = list(
  fit_noise_model(curved_sales, true_noise_sd_k, degree = 1, prior_sd = prior_sd_k),
  fit_noise_model(curved_sales, true_noise_sd_k, degree = 2, prior_sd = prior_sd_k))
labels_shape = c("Straight-line model", "Quadratic model")
model_shape_results = summarize_fits(
  fits_shape, labels_shape, true_price(target_floor_m2, curved = TRUE),
  target_floor_m2)
show_summary(model_shape_results, "3. Same curved data, different function classes")
dev.new()
plot_comparison(fits_shape, rep(list(curved_sales), 2), labels_shape, TRUE,
                "3. Same data, different model shape", target_floor_m2)
cat("The dashed curve in the plot reveals an error we can see in a simulation.\n",
    "Posterior uncertainty describes learning WITHIN the chosen model class.\n",
    "Its adequacy needs separate checks with real data.\n", sep = "")

# EXPLORE AND DISCUSS ----------------------------------------------------
# - Change n_small to 10. What happens to each component and the two bands?
# - Change the assumed noise multipliers while keeping true_noise_sd_k fixed.
# - Change target_floor_m2 to 190 or 240. Which prediction is extrapolation?
# - Change prior_sd_k to 10. How does the prior influence the split?
# - Which assumptions would need checking for your own data?
#
# TAKE HOME
# This specified probabilistic model makes the distinction numerical.
# It treats outcome variation as noise and uncertainty about f as posterior
# uncertainty in the coefficients. Those choices determine the reported split.
# Real applications usually also need to estimate noise and assess the model.
# The posterior epistemic SD here and the forest refit SD in Exercise 2 arise
# from different constructions. Both need interpretation in their own context.
#
# REFERENCES
# Hullermeier & Waegeman (2021), sections 2-3:
# https://doi.org/10.1007/s10994-021-05946-3
# Rasmussen & Williams (2006), Gaussian Processes for Machine Learning,
# chapter 2.1, Bayesian linear regression and fixed basis functions:
# https://gaussianprocess.org/gpml/chapters/RW2.pdf
