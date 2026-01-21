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
#   alpha = 0.05       # Significance level
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
# blandPowerCurve(
#   samplesizes = seq(10, 100, 1),  # Range of sample sizes
#   mu = 0,                          # Mean difference
#   SD,                              # SD of differences
#   delta,                           # Tolerance bound(s)
#   conf.level = 0.95,              # CI confidence level
#   agree.level = 0.95               # LOA agreement level
# )

## ----bland_ex1, eval=TRUE-----------------------------------------------------
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
#   n = NULL             # Sample size
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
#   n = NULL              # Sample size
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
# Step 1: Independent sample size
result <- power_agreement_exact(
  delta = 7, mu = 0.5, sigma = 2.5,
  p0_star = 0.95, power = 0.80, alpha = 0.05
)
n_indep <- result$n
cat("Independent pairs needed:", n_indep, "\n")

# Step 2: Apply design effect
m <- 3  # 3 measurements per participant
ICC <- 0.15  # from pilot or literature
DEFF <- 1 + (m - 1) * ICC
cat("Design effect:", round(DEFF, 3), "\n")

# Step 3: Calculate participants needed
n_ess <- ceiling(n_indep * DEFF)
K <- ceiling(n_ess / m)
cat("Total observations:", n_ess, "\n")
cat("Participants needed:", K, "\n")

## ----cluster_ex2, eval=TRUE---------------------------------------------------
# Compare different ICC values
n_indep <- 50
m <- 4

ICC_values <- c(0, 0.05, 0.10, 0.15, 0.20)
for (ICC in ICC_values) {
  DEFF <- 1 + (m - 1) * ICC
  K <- ceiling(ceiling(n_indep * DEFF) / m)
  cat(sprintf("ICC = %.2f: Need %d participants\n", ICC, K))
}

## ----cluster_complete, eval=TRUE----------------------------------------------
# Study parameters
sigma <- 3.3
delta <- 7
m <- 4  # measurements per participant
ICC <- 0.15
dropout <- 0.20

# Step 1: Independent sample size
result <- power_agreement_exact(
  delta = delta, mu = 0, sigma = sigma,
  p0_star = 0.95, power = 0.80, alpha = 0.05
)

# Step 2: Account for clustering
DEFF <- 1 + (m - 1) * ICC
n_total <- ceiling(result$n * DEFF)
K_pre <- ceiling(n_total / m)

# Step 3: Account for dropout
K_final <- ceiling(K_pre / (1 - dropout))

# Summary
cat("Independent pairs:", result$n, "\n")
cat("Design effect:", round(DEFF, 3), "\n")
cat("Participants (no dropout):", K_pre, "\n")
cat("Participants to recruit:", K_final, "\n")
cat("Total measurements:", K_final * m, "\n")

