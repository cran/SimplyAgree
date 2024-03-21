## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(SimplyAgree)
data(temps)

## -----------------------------------------------------------------------------
tolerance_limit(
  data = temps,
  x = "trec_pre", # First measure
  y = "teso_pre", # Second measure
  id = "id", # Subject ID
  condition = "tod", # Identify condition that may affect differences
  cor_type = "sym" # Set correlation structure as Compound Symmetry
)

## -----------------------------------------------------------------------------
test1 = tolerance_limit(data = temps,
                        x = "teso_pre",
                        y = "trec_pre",
                        id = "id",
                        condition = "tod")

test1

## -----------------------------------------------------------------------------
# Calc. LoA
a1 = agreement_limit(data = reps,
                     x = "x",
                     y = "y")
# print
a1

## -----------------------------------------------------------------------------
a2 = agreement_limit(x = "x",
                y = "y",
                id = "id",
                data = reps,
                data_type = "reps",
                agree.level = .8) 

a2

## -----------------------------------------------------------------------------
a3 = agreement_limit(x = "x",
                y = "y",
                id = "id",
                data = reps,
                data_type = "nest",
                loa_calc = "mover",
                agree.level = .95)
a3

## -----------------------------------------------------------------------------
res1 = tolerance_limit(
  data = temps,
  x = "trec_pre", # First measure
  y = "teso_pre", # Second measure
  id = "id", # Subject ID
  condition = "tod", # Identify condition that may affect differences
  cor_type = "sym" # Set correlation structure as Compound Symmetry
)
plot(res1, delta = .25) # Set maximal allowable difference to .25 units

## -----------------------------------------------------------------------------
test_agree = agreement_limit(x = "x",
                             y = "y",
                             data = reps)

check(test_agree)

test_tol = tolerance_limit(x = "x",
                           y = "y",
                           data = reps)

check(test_tol)

## -----------------------------------------------------------------------------

test_tol = tolerance_limit(x = "x",
                           y = "y",
                           data = reps,
                           prop_bias = TRUE)
print(test_tol)

# See effect of proportional bias on limits
plot(test_tol)

# Confirm its effects in proportional bias check plot (should be horizontal now)
check(test_tol)

## -----------------------------------------------------------------------------
tolerance_limit(
  data = temps,
  log_tf = TRUE, # natural log transformation of responses
  x = "trec_pre", # First measure
  y = "teso_pre", # Second measure
  id = "id", # Subject ID
  condition = "tod", # Identify condition that may affect differences
  cor_type = "sym" # Set correlation structure as Compound Symmetry
)

## -----------------------------------------------------------------------------
set.seed(81346)
x = rnorm(750, 100, 10)
diff = rnorm(750, 0, 1)
y = x + diff

df = data.frame(x = x,
                y = y)

a1 = agreement_limit(data = df,
                     x = "x",
                     y = "y",
                     agree.level = .95)

plot(a1,
     geom = "geom_point")

plot(a1,
     geom = "geom_bin2d")

plot(a1,
     geom = "geom_density_2d")

plot(a1,
     geom = "geom_density_2d_filled")

plot(a1,
     geom = "stat_density_2d")

