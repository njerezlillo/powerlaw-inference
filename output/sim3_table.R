library(dplyr)
library(tidyr)
library(xtable)
library(patchwork)

# Case 1-2 ----------------------------------------------------------------

load("simulation/results/sim3_case1.RData")

### Bootstrap (proposal)

T1 <- cbind(
  do.call(rbind, lapply(estimates, function(x) {
    data.frame(Bias = x$bias,
               MSE = x$mse,
               CP = x$cob)})) %>% 
    mutate(n = rep(paste(n_grid), each = 2),
           par = rep(c("alpha1", "alpha2"), times = 5)) %>% 
    pivot_wider(names_from = par,
                values_from = c(Bias, MSE, CP)),
  do.call(rbind, lapply(estimates, function(x) {
    data.frame(Accu = x$accu)
  }))
)

T1 <- cbind(
  T1[, c(1, 2, 4, 6)],
  data.frame(NA),
  paste0(T1[, c(8)], "%"),
  data.frame(NA),
  T1[, c(3, 5, 7)]
)

print(xtable(T1), include.rownames = FALSE)

### MLE (original)

T1_mle <- do.call(rbind, lapply(estimates, function(x) {
  data.frame(Bias = x$bias_mle,
             MSE = x$mse_mle,
             CP = x$cob_mle)})) %>%
  mutate(n = rep(paste(n_grid), each = 2),
         par = rep(c("alpha1", "alpha2"), times = 5)) %>%
  pivot_wider(names_from = par, values_from = c(Bias, MSE, CP))

T1_mle <- cbind(
  T1_mle[, c(1, 2, 4, 6)],
  data.frame(NA),
  T1_mle[, c(3, 5, 7)]
)

print(xtable(T1_mle), include.rownames = FALSE)

# Case 3-4 ----------------------------------------------------------------

load("simulation/results/sim3_case3.RData")

### Bootstrap (proposal)

T3 <- cbind(
  do.call(rbind, lapply(estimates, function(x) {
    data.frame(Bias = x$bias,
               MSE = x$mse,
               CP = x$cob)})) %>% 
    mutate(n = rep(paste(n_grid), each = 3),
           par = rep(c("alpha1", "alpha2", "alpha3"), times = 5)) %>% 
    pivot_wider(names_from = par,
                values_from = c(Bias, MSE, CP)),
  matrix(do.call(rbind, lapply(estimates, function(x) {
    data.frame(Accu = x$accu)
  }))$Accu, ncol = 2, byrow = T, 
  dimnames = list(NULL, c("Accu1", "Accu2")))
)

T3 <- cbind(
  T3[, c(1, 2, 5, 8)],
  data.frame(NA),
  paste0(T3[, c(11)], "%"),
  data.frame(NA),
  T3[, c(3, 6, 9)],
  data.frame(NA),
  paste0(T3[, c(12)], "%"),
  data.frame(NA),
  T3[, c(4, 7, 10)]
)

print(xtable(T3), include.rownames = FALSE)

### MLE (original)

T3_mle <- do.call(rbind, lapply(estimates, function(x) {
  data.frame(Bias = x$bias_mle,
             MSE = x$mse_mle,
             CP = x$cob_mle)})) %>%
  mutate(n = rep(paste(n_grid), each = 3), 
         par = rep(c("alpha1", "alpha2", "alpha3"), times = 5)) %>%
  pivot_wider(names_from = par, values_from = c(Bias, MSE, CP))

T3_mle <- cbind(
  T3_mle[, c(1, 2, 5, 8)],
  data.frame(NA),
  T3_mle[, c(3, 6, 9)],
  data.frame(NA),
  T3_mle[, c(4, 7, 10)]
)

print(xtable(T3_mle), include.rownames = FALSE)
