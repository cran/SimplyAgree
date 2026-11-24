## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4
)
library(SimplyAgree)
has_ordinal <- requireNamespace("ordinal", quietly = TRUE)

## -----------------------------------------------------------------------------
ratings <- data.frame(
  Rater1 = c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
  Rater2 = c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5, NA, NA),
  Rater3 = c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, 3),
  Rater4 = c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA))
raters <- c("Rater1", "Rater2", "Rater3", "Rater4")

## -----------------------------------------------------------------------------
cac_test(col.names = raters, data = ratings, coef = "pa")

## -----------------------------------------------------------------------------
cac_test(col.names = raters, data = ratings)

## -----------------------------------------------------------------------------
screen <- data.frame(
  reader1 = c(rep("normal", 45), "abnormal", "normal", "normal",
              "abnormal", "normal"),
  reader2 = c(rep("normal", 45), "normal", "abnormal", "normal",
              "abnormal", "normal"))
readers <- c("reader1", "reader2")

sapply(c("pa", "gwet", "fleiss", "krippendorff"), function(coef) {
  unname(cac_test(col.names = readers, data = screen,
                  coef = coef)$estimate)
})

## -----------------------------------------------------------------------------
sapply(c("unweighted", "linear", "quadratic"), function(w) {
  unname(cac_test(col.names = raters, data = ratings, weights = w)$estimate)
})

## -----------------------------------------------------------------------------
w <- diag(5)
w[abs(row(w) - col(w)) == 1] <- 0.5
cac_test(col.names = raters, data = ratings, weights = w)$estimate

## -----------------------------------------------------------------------------
# a 6-point scale on which no subject was rated 6
cac_test(col.names = raters, data = ratings, weights = "quadratic",
         categories = 1:6)$estimate
cac_test(col.names = raters, data = ratings, weights = "quadratic")$estimate

## -----------------------------------------------------------------------------
severity <- c("none", "mild", "moderate", "severe", "extreme")
ratings_f <- as.data.frame(lapply(ratings, function(x)
  factor(severity[x], levels = severity)))
cac_test(col.names = raters, data = ratings_f,
         weights = "quadratic")$estimate

## -----------------------------------------------------------------------------
res <- cac_test(col.names = raters, data = ratings, coef = "fleiss",
                weights = "quadratic", null.value = 0.6,
                alternative = "greater")
res

## -----------------------------------------------------------------------------
summary(res)

## ----fig.alt = c("Stacked bars showing the share of ratings in each category for each rater.", "Ratings for each subject, ordered by mean category, with point size showing how many raters chose each category.")----
plot(res)
plot(res, type = "subjects")

## ----eval = has_ordinal-------------------------------------------------------
set.seed(2017)
d <- expand.grid(id = 1:60, rater = paste0("R", 1:10))
latent <- rnorm(60, sd = 1.5)[d$id] + rnorm(10, sd = 0.5)[d$rater] +
  rnorm(nrow(d))
d$rating <- cut(latent, c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), labels = FALSE)

res_km <- ord_kappa_test(rating ~ rater | id, data = d)
res_km

## ----eval = has_ordinal-------------------------------------------------------
res_km$fit[c("sigma2_subject", "sigma2_rater", "rho", "p0")]

## ----eval = has_ordinal-------------------------------------------------------
cac_test(rating ~ rater | id, data = d, coef = "fleiss")$estimate

## ----eval = has_ordinal, fig.alt = "Estimated effect of each rater on the latent scale with intervals; a dashed line at zero marks the typical rater."----
plot(res_km, type = "effects")

## ----eval = has_ordinal-------------------------------------------------------
d$experience <- ifelse(d$rater %in% paste0("R", 1:5), "experienced",
                       "novice")
novice <- d$experience == "novice"
d$rating2 <- cut(latent + novice * rnorm(10, sd = 1)[d$rater],
                 c(-Inf, -1.5, -0.5, 0.5, 1.5, Inf), labels = FALSE)

res_g <- ord_kappa_test(rating2 ~ rater | id, data = d,
                        group = "experience")
res_g

## ----eval = has_ordinal, fig.alt = "Estimated effect of each rater on the latent scale with intervals, colored by experience group; a dashed line at zero marks the typical rater."----
res_g$fit$subgroups
summary(res_g)
plot(res_g, type = "effects")

## -----------------------------------------------------------------------------
agree_coef(data = ratings, wide = TRUE, col.names = raters,
           weighted = TRUE)

