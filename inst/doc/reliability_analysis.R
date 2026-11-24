## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(SimplyAgree)

## -----------------------------------------------------------------------------
dat <- data.frame(judge1 = c(9, 6, 8, 7, 10, 6),
                  judge2 = c(2, 1, 4, 1, 5, 2),
                  judge3 = c(5, 3, 6, 2, 6, 4),
                  judge4 = c(8, 2, 8, 6, 9, 7))
judges <- c("judge1", "judge2", "judge3", "judge4")

## -----------------------------------------------------------------------------
icc_test(col.names = judges, data = dat)

## -----------------------------------------------------------------------------
icc_test(col.names = judges, data = dat, type = "agreement")

## -----------------------------------------------------------------------------
k <- length(judges)
alpha <- k / (k - 1) * (1 - sum(apply(dat, 2, var)) / var(rowSums(dat)))
c(alpha = alpha,
  icc_C_k = unname(icc_test(col.names = judges, data = dat,
                            unit = "average")$estimate))

## -----------------------------------------------------------------------------
two <- dat[c("judge1", "judge4")]
c(CCC = unname(ccc_test(x = "judge1", y = "judge4", data = two)$estimate),
  ICC_A_1 = unname(icc_test(col.names = c("judge1", "judge4"), data = two,
                            type = "agreement")$estimate))

## -----------------------------------------------------------------------------
res <- icc_test(col.names = judges, data = dat,
                null.value = 0.5, alternative = "greater")
res

## -----------------------------------------------------------------------------
summary(res)

## -----------------------------------------------------------------------------
summary(res, style = "estimation")

## -----------------------------------------------------------------------------
dat_na <- dat
dat_na$judge2[1] <- NA
icc_test(col.names = judges, data = dat_na, method = "reml")

## -----------------------------------------------------------------------------
set.seed(2026)
icc_test(col.names = judges, data = dat, boot = TRUE, replicates = 499)

## -----------------------------------------------------------------------------
set.seed(2026)
icc_test(col.names = judges, data = dat, type = "agreement", boot = TRUE,
         replicates = 499)

## -----------------------------------------------------------------------------
res_j <- icc_test(col.names = judges, data = dat)
res_n <- icc_test(col.names = judges, data = dat, se_method = "normal")
rbind(jackknife = res_j$stderr, normal = res_n$stderr)

## ----fig.width=5, fig.height=4, fig.alt = "Each subject's measurements across the judges as gray lines, with the judge means and 95% confidence intervals in blue."----
res <- icc_test(col.names = judges, data = dat)
plot(res)

## ----fig.width=5, fig.height=4, fig.alt = "Each subject's measurements, with subjects ordered by their mean, comparing the spread between subjects with the scatter within subjects."----
plot(res, type = "subjects")

## ----fig.width=9, fig.height=3.5, fig.alt = "Model diagnostic panels: normality and homogeneity of the residuals, and the within-subject SD against the subject mean."----
check(res)

## -----------------------------------------------------------------------------
weir <- data.frame(t1 = c(146, 148, 170, 90, 157, 156, 176, 205),
                   t2 = c(140, 152, 152, 99, 145, 153, 167, 218))
reli_err_test(col.names = c("t1", "t2"), data = weir)

## -----------------------------------------------------------------------------
reli_err_test(col.names = c("t1", "t2"), data = weir, null.value = 12)

## -----------------------------------------------------------------------------
reli_err_test(col.names = c("t1", "t2"), data = weir, stat = "SEP")

## -----------------------------------------------------------------------------
reli_err_test(col.names = c("t1", "t2"), data = weir, scale = "log")

## -----------------------------------------------------------------------------
reli_err_test(col.names = c("t1", "t2"), data = weir, scale = "mean")

## -----------------------------------------------------------------------------
mdc_test(col.names = c("t1", "t2"), data = weir)

## -----------------------------------------------------------------------------
mdc_test(col.names = judges, data = dat, type = "agreement")

## -----------------------------------------------------------------------------
mdc_test(col.names = c("t1", "t2"), data = weir, null.value = 30)

## -----------------------------------------------------------------------------
set.seed(2)
scans <- expand.grid(scan = 1:2, operator = c("A", "B", "C"), id = 1:20)
subj <- rnorm(20, 50, 5)
bias <- c(A = 0, B = 1.5, C = -1)
inter <- matrix(rnorm(60, 0, 1), 20, 3)
scans$thickness <- subj[scans$id] + bias[scans$operator] +
  inter[cbind(scans$id, as.integer(scans$operator))] +
  rnorm(nrow(scans), 0, 1.5)

res <- reli_facet_test(thickness ~ operator | id, data = scans)
res
summary(res)

## -----------------------------------------------------------------------------
sem_a <- reli_facet_test(thickness ~ operator | id, data = scans,
                         stat = "SEM", facet = "inter", type = "agreement")
sem_a
mdc_test(sem_a)

## -----------------------------------------------------------------------------
one_op <- reli_facet_test(thickness ~ operator | id, data = scans,
                          facet = "inter", type = "agreement", r_avg = 2)
two_ops <- reli_facet_test(thickness ~ operator | id, data = scans,
                           facet = "inter", type = "agreement", k_avg = 2)
c(two_scans_one_operator = unname(one_op$estimate),
  one_scan_two_operators = unname(two_ops$estimate))

## -----------------------------------------------------------------------------
set.seed(4)
days <- expand.grid(day = c("day1", "day2"), operator = c("A", "B", "C"),
                    id = 1:20)
subj <- rnorm(20, 50, 5)
shift <- c(day1 = 0, day2 = 1.5)                  # everyone higher on day 2
drift <- matrix(rnorm(6, 0, 0.5), 3, 2)           # rater-specific drift
change <- matrix(rnorm(40, 0, 1), 20, 2)          # subjects change between days
oi <- as.integer(days$operator); di <- as.integer(days$day)
days$thickness <- subj[days$id] + bias[days$operator] + shift[days$day] +
  drift[cbind(oi, di)] + change[cbind(days$id, di)] +
  rnorm(nrow(days), 0, 1)

test_retest <- reli_facet_test(thickness ~ operator | id, data = days,
                               occasion = "day", stat = "SEM",
                               type = "agreement")
summary(test_retest)

## -----------------------------------------------------------------------------
test_retest_c <- reli_facet_test(thickness ~ operator | id, data = days,
                                 occasion = "day", stat = "SEM",
                                 type = "consistency")
sem_row <- function(res) {
  c(SEM = unname(res$estimate), lower = res$conf.int[1],
    upper = res$conf.int[2])
}
rbind(agreement = sem_row(test_retest),
      consistency = sem_row(test_retest_c))

## -----------------------------------------------------------------------------
power_icc(rho0 = 0.6, rho1 = 0.8, k = 2, power = 0.8,
          alternative = "greater")

## -----------------------------------------------------------------------------
precision_icc(rho = 0.8, k = 3, width = 0.2, assurance = 0.8)

## -----------------------------------------------------------------------------
reli_aov(data = dat, wide = TRUE, col.names = judges)

## -----------------------------------------------------------------------------
ratings <- data.frame(
  Rater1 = c(1, 2, 3, 3, 2, 1, 4, 1, 2, NA, NA, NA),
  Rater2 = c(1, 2, 3, 3, 2, 2, 4, 1, 2, 5, NA, NA),
  Rater3 = c(NA, 3, 3, 3, 2, 3, 4, 2, 2, 5, 1, 3),
  Rater4 = c(1, 2, 3, 3, 2, 4, 4, 1, 2, 5, 1, NA))
cac_test(col.names = c("Rater1", "Rater2", "Rater3", "Rater4"),
         data = ratings, weights = "quadratic")

