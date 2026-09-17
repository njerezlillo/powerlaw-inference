require(poweRlaw)
require(numDeriv)
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

  B <- 1000
  p <- c(1, 5, 10); alpha <- c(1.2, 2, 3.5)

  brk <- matrix(0, nrow = length(alpha) - 1, ncol = B)
  est <- matrix(0, nrow = length(alpha), ncol = B)
  cp <- matrix(0, nrow = length(alpha), ncol = B)
  est_mle <- matrix(0, nrow = length(alpha), ncol = B)
  cp_mle <- matrix(0, nrow = length(alpha), ncol = B)

  for (ite in 1:B) {
    # Data
    df <- rpwpldis(n, p, alpha)

    # Fit
    aux_fit <-
      fit_pwpldis(df, nbreak = 2, exclude_int = c(quantile(df, 0.9), Inf))

    hat_p <- as.num(aux_fit[, det_char("tau", aux_fit)])
    hat_a <- as.num(aux_fit[, det_char("alpha", aux_fit)])

    # Bootstrap
    aux_boot <-
      boot_pwpldis(df, brks = hat_p[-1])
    boot <-
      aux_boot[, det_char("alpha", aux_boot)]
    IC <-
      apply(boot, 2, function(x) quantile(x, c(0.025, 0.975)))

    # Asymptotic Intervals
    l <- function(z) loglik_pwpldis(z, df, hat_p)
    f <- maxLik(l, start = runif(length(hat_p), 1.5, 3.5),
                constraints = list(ineqA = diag(rep(1, length(hat_p))),
                                   ineqB = rep(-1.01, length(hat_p))))

    H <- f$hessian
    IC_mle <- matrix(
      c(hat_a - 1.96 * sqrt(diag(solve(-H))),
        hat_a + 1.96 * sqrt(diag(solve(-H)))),
      nrow = 2, byrow = T
    )

    # Save Results
    brk[, ite] <- hat_p[-1]
    est[, ite] <- as.num(2 * hat_a - apply(boot, 2, mean))
    est_mle[, ite] <- hat_a
    cp[, ite] <-
      as.num(ifelse(IC[1, ] <=  alpha & IC[2, ] >= alpha, 1, 0))
    cp_mle[, ite] <-
      as.num(ifelse(IC_mle[1, ] <=  alpha & IC_mle[2, ] >= alpha, 1, 0))

    cat("| iter:", ite, "| n:", n, "|", "\n")
  }

  accu <- apply((brk == p[-1]), 1, mean) * 100

  bias <- apply(est - alpha, 1, mean)
  mse <- apply((est - alpha)^2, 1, mean)
  cob <- apply(cp, 1, mean)

  bias_mle <- apply(est_mle - alpha, 1, mean)
  mse_mle <- apply((est_mle - alpha)^2, 1, mean)
  cob_mle <- apply(cp_mle, 1, mean)

  return(list(
    accu = sprintf(accu, fmt = '%.0f'),
    bias = sprintf(bias, fmt = '%.2f'),
    mse = sprintf(mse, fmt = '%.2f'),
    cob = sprintf(cob, fmt = '%.2f'),
    bias_mle = sprintf(bias_mle, fmt = '%.2f'),
    mse_mle = sprintf(mse_mle, fmt = '%.2f'),
    cob_mle = sprintf(cob_mle, fmt = '%.2f')
  ))
}

number_of_cores <- 5
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)
registerDoParallel(cores = number_of_cores)
estimates <- foreach(n = n_grid,
                     .multicombine = TRUE,
                     .packages = c("poweRlaw", "VGAM", "numDeriv")) %dopar% FF(n)

stopCluster(clusters)

estimates

save(n_grid, estimates, file = "simulation/results/sim3_case1.RData")
