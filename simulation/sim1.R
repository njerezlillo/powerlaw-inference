require(poweRlaw)
require(VGAM)
require(parallel)
require(doParallel)
require(foreach)
# devtools::install_github("njerezlillo/pldis")
require(pldis)

ok <- function(z) is.numeric(z) && length(z) == 1L && is.finite(z)

## ---- simulation ------------------------------------------------------------

n_grid <- seq(10, 100, by = 10)

FF <- function(n) {
  set.seed(2026)

  level  <- 0.95
  B      <- 5000
  lim1   <- 1.01
  lim2   <- 50

  methods <- c("MLE", "AMLE", "Fd", "Fc")
  est   <- matrix(NA_real_, B, 4, dimnames = list(NULL, methods))
  cover <- matrix(NA_real_, B, 4, dimnames = list(NULL, methods))

  o <- 0L; ite <- 0L; max_ite <- 100L * B
  n_boundary <- 0L

  while (o < B && ite < max_ite) {
    ite <- ite + 1L
    df <- poweRlaw::rpldis(n, xm, palpha)

    f_fd  <- function(z) loglik_fd_pldis(z, xm, df)
    f_fc  <- function(z) loglik_fc_pldis(z, xm, df)

    amle <- 1 + n * sum(log(df / (xm - 0.5))) ^ -1
    mle  <- fit_pldis(df, xm, penalized = FALSE)$alpha
    afd  <- fit_pldis(df, xm, penalized = TRUE)$alpha
    afc  <- try(optimize(f_fc,  c(lim1, lim2), maximum = TRUE)$maximum, silent = TRUE)

    if (!(ok(mle) && ok(amle) && ok(afd) && ok(afc))) next

    o <- o + 1L
    est[o, ] <- c(mle, amle, afd, afc)
    if (any(c(mle, afd, afc) > lim2 - 1e-3)) n_boundary <- n_boundary + 1L

    se <- c(se_expected(mle,  xm, n),
            se_expected(amle, xm, n),
            se_observed(f_fd, afd),
            se_observed(f_fc, afc))

    cc <- qnorm(1 - (1 - level) / 2)
    li <- est[o, ] - cc * se
    ls <- est[o, ] + cc * se
    cover[o, ] <- as.numeric(li <= palpha & ls >= palpha)
  }

  list(
    n          = n,
    method     = methods,
    bias       = round(colMeans(est) - palpha, 3),
    mse        = round(colMeans((est - palpha) ^ 2), 3),
    cp         = round(colMeans(cover, na.rm = TRUE), 3),
    n_reject   = ite - o,
    n_boundary = n_boundary,
    n_se_na    = sum(is.na(cover[, "Fd"]) | is.na(cover[, "Fc"]))
  )
}


# run: case 1 -------------------------------------------------------------

xm     <- 4
palpha <- 4

number_of_cores <- 4
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)
parallel::clusterEvalQ(clusters, {
  library(poweRlaw); library(VGAM); library(pldis)
})

estimates_c1 <- foreach(
  n = n_grid,
  .multicombine = TRUE,
  .packages = c("poweRlaw", "VGAM"),
  .export = c("FF", "se_expected", "se_observed", "ok")
) %dopar% FF(n)

parallel::stopCluster(clusters)

results_c1 <- do.call(rbind, lapply(estimates_c1, function(z)
  data.frame(n = z$n, method = z$method,
             Bias = z$bias, MSE = z$mse, CP = z$cp,
             row.names = NULL, stringsAsFactors = FALSE)))

results_c1$method <- factor(results_c1$method, levels = c("MLE", "AMLE", "Fd", "Fc"))

save(results_c1, file = "simulation/results/sim1_case1.RData")

# run: case 2 -------------------------------------------------------------

xm     <- 6
palpha <- 4

number_of_cores <- 4
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)
parallel::clusterEvalQ(clusters, {
  library(poweRlaw); library(VGAM); library(pldis)
})

estimates_c2 <- foreach(
  n = n_grid,
  .multicombine = TRUE,
  .packages = c("poweRlaw", "VGAM"),
  .export = c("FF", "se_expected", "se_observed", "ok")
) %dopar% FF(n)

parallel::stopCluster(clusters)

results_c2 <- do.call(rbind, lapply(estimates_c2, function(z)
  data.frame(n = z$n, method = z$method,
             Bias = z$bias, MSE = z$mse, CP = z$cp,
             row.names = NULL, stringsAsFactors = FALSE)))

results_c2$method <- factor(results_c2$method, levels = c("MLE", "AMLE", "Fd", "Fc"))

save(results_c2, file = "simulation/results/sim1_case2.RData")

# run: case 3 -------------------------------------------------------------

xm     <- 4
palpha <- 6

number_of_cores <- 4
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)
parallel::clusterEvalQ(clusters, {
  library(poweRlaw); library(VGAM); library(pldis)
})

estimates_c3 <- foreach(
  n = n_grid,
  .multicombine = TRUE,
  .packages = c("poweRlaw", "VGAM"),
  .export = c("FF", "se_expected", "se_observed", "ok")
) %dopar% FF(n)

parallel::stopCluster(clusters)

results_c3 <- do.call(rbind, lapply(estimates_c3, function(z)
  data.frame(n = z$n, method = z$method,
             Bias = z$bias, MSE = z$mse, CP = z$cp,
             row.names = NULL, stringsAsFactors = FALSE)))

results_c3$method <- factor(results_c3$method, levels = c("MLE", "AMLE", "Fd", "Fc"))

save(results_c3, file = "simulation/results/sim1_case3.RData")

# run: case 4 -------------------------------------------------------------

xm     <- 6
palpha <- 6

number_of_cores <- 4
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)
parallel::clusterEvalQ(clusters, {
  library(poweRlaw); library(VGAM); library(pldis)
})

estimates_c4 <- foreach(
  n = n_grid,
  .multicombine = TRUE,
  .packages = c("poweRlaw", "VGAM"),
  .export = c("FF", "se_expected", "se_observed", "ok")
) %dopar% FF(n)

parallel::stopCluster(clusters)

results_c4 <- do.call(rbind, lapply(estimates_c4, function(z)
  data.frame(n = z$n, method = z$method,
             Bias = z$bias, MSE = z$mse, CP = z$cp,
             row.names = NULL, stringsAsFactors = FALSE)))

results_c4$method <- factor(results_c4$method, levels = c("MLE", "AMLE", "Fd", "Fc"))

save(results_c4, file = "simulation/results/sim1_case4.RData")

# diagnostic --------------------------------------------------------------

diagnostics_c1 <- do.call(rbind, lapply(estimates_c1, function(z)
  data.frame(n = z$n, rejected = z$n_reject,
             at_boundary = z$n_boundary, se_failed = z$n_se_na)))

diagnostics_c2 <- do.call(rbind, lapply(estimates_c2, function(z)
  data.frame(n = z$n, rejected = z$n_reject,
             at_boundary = z$n_boundary, se_failed = z$n_se_na)))

diagnostics_c3 <- do.call(rbind, lapply(estimates_c3, function(z)
  data.frame(n = z$n, rejected = z$n_reject,
             at_boundary = z$n_boundary, se_failed = z$n_se_na)))

diagnostics_c4 <- do.call(rbind, lapply(estimates_c4, function(z)
  data.frame(n = z$n, rejected = z$n_reject,
             at_boundary = z$n_boundary, se_failed = z$n_se_na)))

print(diagnostics_c1)
print(diagnostics_c2)
print(diagnostics_c3)
print(diagnostics_c4)
