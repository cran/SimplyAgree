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

## ----eval = FALSE-------------------------------------------------------------
# # random intercept for each subject, plus AR(1) correlation of the residuals
# tolerance_limit(
#   data = temps,
#   x = "trec_pre",
#   y = "teso_pre",
#   id = "id",
#   time = "trial_num",
#   model = "lme",
#   cor_type = "ar1"
# )
# 
# # nested random intercepts: clubs within golfers
# tolerance_limit(
#   data = golf, # a data set with columns golfer, club, x, and y
#   x = "x",
#   y = "y",
#   id = c("golfer", "club"),
#   model = "lme"
# )

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

## ----fig.alt = "Differences between trec_pre and teso_pre against their average, with the bias, tolerance limits, and the maximal allowable difference of 0.25."----
res1 = tolerance_limit(
  data = temps,
  x = "trec_pre", # First measure
  y = "teso_pre", # Second measure
  id = "id", # Subject ID
  condition = "tod", # Identify condition that may affect differences
  cor_type = "sym" # Set correlation structure as Compound Symmetry
)
plot(res1, delta = .25) # Set maximal allowable difference to .25 units

## ----fig.alt = c("Diagnostic panels for agreement_limit: normality of the differences, heteroskedasticity, and proportional bias.", "Diagnostic panels for tolerance_limit: normality of the differences, heteroskedasticity, and proportional bias.")----
test_agree = agreement_limit(x = "x",
                             y = "y",
                             data = reps)

check(test_agree)

test_tol = tolerance_limit(x = "x",
                           y = "y",
                           data = reps)

check(test_tol)

## ----fig.alt = c("Tolerance limits for the reps data that change with the mean to account for proportional bias.", "Diagnostic panels for tolerance_limit with proportional bias modeled; the proportional bias panel is now flat.")----

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
tolerance_limit(
  data = temps,
  log_tf = TRUE, # natural log transformation of responses
  log_tf_display = "sympercent", # display results as sympercent
  x = "trec_pre", # First measure
  y = "teso_pre", # Second measure
  id = "id", # Subject ID
  condition = "tod", # Identify condition that may affect differences
  cor_type = "sym" # Set correlation structure as Compound Symmetry
)

## ----fig.alt = c("Bland-Altman plot of 750 simulated pairs drawn as individual points.", "Bland-Altman plot of 750 simulated pairs drawn as a heatmap of 2D bin counts.", "Bland-Altman plot of 750 simulated pairs drawn as 2D density contour lines.", "Bland-Altman plot of 750 simulated pairs drawn as filled 2D density contours.", "Bland-Altman plot of 750 simulated pairs drawn with stat_density_2d.")----
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

