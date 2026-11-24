## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 5
)

## ----load---------------------------------------------------------------------
library(SimplyAgree)

## ----exact_usage, eval=FALSE--------------------------------------------------
# power_agreement_exact(
#   n = NULL,           # Sample size
#   delta = NULL,       # Tolerance bound
#   mu = 0,            # Mean of differences
#   sigma = NULL,       # SD of differences
#   p0_star = 0.95,    # Central proportion (tolerance coverage)
#   power = NULL,       # Target power
#   alpha = 0.05,      # Significance level
#   m = 1,             # Paired measurements per subject
#   icc = 0            # ICC of the differences (if m > 1)
# )

## ----exact_ex1, eval=TRUE-----------------------------------------------------
# Blood pressure device comparison
result <- power_agreement_exact(
  delta = 7,          # +/-7 mmHg tolerance
  mu = 0.5,          # Expected bias
  sigma = 2.5,       # Expected SD
  p0_star = 0.95,    # 95% must be within bounds
  power = 0.80,      # 80% power
  alpha = 0.05
)
print(result)

## ----bland_usage, eval=FALSE--------------------------------------------------
# power_agreement_limit(
#   n = NULL,                        # Sample size (pairs)
#   delta = NULL,                    # Maximal allowable difference
#   mu = 0,                          # Mean difference
#   sigma = NULL,                    # SD of differences
#   power = NULL,                    # Target power
#   conf.level = 0.95,               # CI confidence level
#   agree.level = 0.95,              # LOA agreement level
#   m = 1,                           # Paired measurements per subject
#   icc = 0                          # ICC of the differences (if m > 1)
# )

## ----bland_n, eval=TRUE-------------------------------------------------------
power_agreement_limit(delta = 7, mu = 0.5, sigma = 2.5, power = 0.80)

## ----bland_curve_usage, eval=FALSE--------------------------------------------
# blandPowerCurve(
#   samplesizes = seq(10, 100, 1),  # Range of sample sizes
#   mu = 0,                          # Mean difference
#   SD,                              # SD of differences
#   delta,                           # Tolerance bound(s)
#   conf.level = 0.95,              # CI confidence level
#   agree.level = 0.95,              # LOA agreement level
#   m = 1,                           # Paired measurements per subject
#   icc = 0                          # ICC of the differences (if m > 1)
# )

## ----bland_ex1, eval=TRUE, fig.alt = "Power curve for the Bland-Altman agreement test, showing power rising with sample size from 10 to 200."----
# Generate power curve
pc <- blandPowerCurve(
  samplesizes = seq(10, 200, 1),
  mu = 0,
  SD = 3.3,
  delta = 8,
  conf.level = 0.95,
  agree.level = 0.95
)

# Plot
plot(pc, type = 1)

# Find n for 80% power
find_n(pc, power = 0.8)

## ----expected_usage, eval=FALSE-----------------------------------------------
# agree_expected_half(
#   conf.level = 0.95,    # CI confidence level
#   delta = NULL,         # Target expected half-width
#   pstar = 0.95,        # Central proportion
#   sigma = 1,           # SD of differences
#   n = NULL,            # Sample size
#   m = 1,               # Paired measurements per subject
#   icc = 0              # ICC of the differences (if m > 1)
# )

## ----expected_ex1, eval=TRUE--------------------------------------------------
# Want E[H] <= 2.5*sigma
result <- agree_expected_half(
  conf.level = 0.95,
  delta = 2.5,         # As multiple of sigma
  pstar = 0.95,
  sigma = 1            # Standardized
)
print(result)

## ----assurance_usage, eval=FALSE----------------------------------------------
# agree_assurance(
#   conf.level = 0.95,     # CI confidence level
#   assurance = 0.90,      # Target assurance probability
#   omega = NULL,          # Target half-width bound
#   pstar = 0.95,         # Central proportion
#   sigma = 1,            # SD of differences
#   n = NULL,             # Sample size
#   m = 1,                # Paired measurements per subject
#   icc = 0               # ICC of the differences (if m > 1)
# )

## ----assurance_ex1------------------------------------------------------------
# Want 90% probability that H <= 2.5*sigma
result <- agree_assurance(
  conf.level = 0.95,
  assurance = 0.90,    # 90% probability
  omega = 2.5,         # Target bound
  pstar = 0.95,
  sigma = 1
)
print(result)

## ----cluster_ex1, eval=TRUE---------------------------------------------------
# Independent pairs
n_indep <- power_agreement_exact(
  delta = 7, mu = 0.5, sigma = 2.5,
  p0_star = 0.95, power = 0.80, alpha = 0.05
)$n
n_indep

# 3 pairs per participant
result <- power_agreement_exact(
  delta = 7, mu = 0.5, sigma = 2.5,
  p0_star = 0.95, power = 0.80, alpha = 0.05,
  m = 3, icc = 0.15
)
result

## ----cluster_ex1b, eval=TRUE--------------------------------------------------
agree_expected_half(delta = 2.5, pstar = 0.95, m = 3, icc = 0.15)$n
agree_assurance(omega = 2.5, assurance = 0.90, pstar = 0.95, m = 3, icc = 0.15)$n

## ----cluster_ex2, eval=TRUE---------------------------------------------------
n_indep <- power_agreement_exact(delta = 7, mu = 0.5, sigma = 2.5,
                                 p0_star = 0.95, power = 0.80)$n
m <- 4
for (ICC in c(0, 0.05, 0.10, 0.20, 0.40)) {
  DEFF <- 1 + (m - 1) * ICC
  K_simple <- ceiling(ceiling(n_indep * DEFF) / m)
  K_exact <- power_agreement_exact(delta = 7, mu = 0.5, sigma = 2.5,
                                   p0_star = 0.95, power = 0.80,
                                   m = m, icc = ICC)$n
  cat(sprintf("ICC = %.2f: simple DEFF %2d participants, exact %2d\n",
              ICC, K_simple, K_exact))
}

## ----cluster_pilot, eval=FALSE------------------------------------------------
# fit <- lme4::lmer(diff ~ 1 + (1 | id), data = pilot)
# vc <- as.data.frame(lme4::VarCorr(fit))$vcov  # between, within
# icc <- vc[1] / sum(vc)
# sigma <- sqrt(sum(vc))

## ----cluster_complete, eval=TRUE----------------------------------------------
# Study parameters
sigma <- 3.3  # total SD of the differences
delta <- 7
m <- 4        # paired measurements per participant
ICC <- 0.15   # ICC of the differences
dropout <- 0.20 # expecting 20% dropout

# Participants needed
result <- power_agreement_exact(
  delta = delta, mu = 0, sigma = sigma,
  p0_star = 0.95, power = 0.80, alpha = 0.05,
  m = m, icc = ICC
)

# Account for dropout
K_final <- ceiling(result$n / (1 - dropout))

# Summary
cat("Participants (no dropout):", result$n, "\n")
cat("Participants to recruit:", K_final, "\n")
cat("Total paired measurements:", K_final * m, "\n")

## ----icc_power----------------------------------------------------------------
power_icc(rho0 = 0.6, rho1 = 0.8, k = 2, power = 0.80,
          model = "twoway", alternative = "greater")

## ----icc_power_k--------------------------------------------------------------
power_icc(n = 20, k = NULL, rho0 = 0.6, rho1 = 0.8, power = 0.80,
          model = "twoway", alternative = "greater")

## ----icc_power_curve, fig.alt = "Power against number of subjects, from 10 to 80, for testing ICC greater than 0.6, with lines for true ICCs of 0.75 and 0.80 and a gray reference line at 80% power."----
n_seq <- seq(10, 80, by = 5)
pw <- sapply(c(0.75, 0.80), function(r1) {
  sapply(n_seq, function(n) {
    power_icc(n = n, k = 2, rho0 = 0.6, rho1 = r1, model = "twoway",
              alternative = "greater")$power
  })
})
matplot(n_seq, pw, type = "l", lty = 1:2, col = 1, ylim = c(0, 1),
        xlab = "Number of subjects", ylab = "Power")
abline(h = 0.8, col = "grey")
legend("bottomright", legend = c("ICC = 0.75", "ICC = 0.80"), lty = 1:2)

## ----icc_precision------------------------------------------------------------
# Median width of 0.2 around an ICC of 0.8, with three trials
precision_icc(rho = 0.8, k = 3, width = 0.2, model = "twoway")

# The same width with 80% assurance
precision_icc(rho = 0.8, k = 3, width = 0.2, assurance = 0.8,
              model = "twoway")

## ----icc_average--------------------------------------------------------------
precision_icc(rho = 0.9, k = 3, width = 0.1, assurance = 0.8,
              model = "twoway", unit = "average")

## ----icc_missing--------------------------------------------------------------
power_icc(rho0 = 0.6, rho1 = 0.8, k = 2, power = 0.80, model = "twoway",
          alternative = "greater", missing = 0.15)

## ----icc_approx---------------------------------------------------------------
power_icc(rho0 = 0.6, rho1 = 0.8, k = 2, power = 0.80,
          alternative = "greater", method = "approximate")

