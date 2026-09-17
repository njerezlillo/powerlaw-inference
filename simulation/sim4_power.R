require(dgof)
require(poweRlaw)
require(parallel)
require(doParallel)
# devtools::install_github("njerezlillo/pwpldis")
require(pwpldis)

n_grid <- c(100, 250, 500, 750, 1000)

det_char <- function (coln, obj) {
  grep(coln, colnames(obj))
}

as.num <- function (z) {
  as.numeric(z)
}

FF <- function (n) {
  set.seed(2024)

  B <- 5000
  p <- c(1, 5, 10); alpha <- c(1.2, 2.2, 4)

  p.ks <- vector(length = B)
  p.cvm <- vector(length = B)

  for (ite in 1:B) {
    df <- rpwpldis(n, p, alpha)
    grid_df <- sort(unique(df))

    aux_fit <-
      fit_pwpldis(df, nbreak = 1, exclude_int = c(quantile(df, 0.9), Inf))

    hat_p <- as.num(aux_fit[, det_char("tau", aux_fit)])
    hat_a <- as.num(aux_fit[, det_char("alpha", aux_fit)])

    aux_boot <- boot_pwpldis(df, brks = hat_p[-1])
    boot <- aux_boot[, det_char("alpha", aux_boot)]

    hat_a <- as.num(2 * hat_a - apply(boot, 2, mean))

    cdf_1 <- Vectorize(function(x) ppwpldis(x, hat_p, hat_a))
    cdf_1 <- stepfun(grid_df, c(0, cdf_1(grid_df)))

    p.ks[ite] <- dgof::ks.test(df, cdf_1)$p.value
    p.cvm[ite] <- dgof::cvm.test(df, cdf_1)$p.value

    cat("| iter:", ite, "| n:", n, "|", "\n")
  }

  return(list(
    size_ks = mean(p.ks < 0.05),
    size_cvm = mean(p.cvm < 0.05)
  ))
}

number_of_cores <- 5
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)
registerDoParallel(cores = number_of_cores)
estimates <- foreach(n = n_grid,
                     .multicombine = TRUE,
                     .packages = c("poweRlaw", "VGAM", "dgof")) %dopar% FF(n)

stopCluster(clusters)

results <- cbind(do.call(rbind, lapply(estimates, function(x) {
  data.frame(KS = x$size_ks, CVM = x$size_cvm)})), n = n_grid)

save(results, file = "simulation/results/sim4_power_case3.RData")
