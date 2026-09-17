require(dgof)
require(poweRlaw)
require(parallel)
require(doParallel)
# devtools::install_github("njerezlillo/pwpldis")
require(pwpldis)

pkgs <- setdiff(sub("^package:", "", grep("^package:", search(), value = TRUE)),
                c("base", "stats", "graphics", "grDevices",
                  "utils", "datasets", "methods"))

n_grid <- c(100, 250, 500, 750, 1000)

settings <- data.frame(
  dist   = c("exp", "pois"),
  xmin   = c(5,     5),
  par    = c(0.1,   2),
  nbreak = c(1,     1),
  stringsAsFactors = FALSE
)

FF <- function (n, case, settings) {

  rexpdis <- function (n, xmin, lambda) {
    u <- runif(n)
    u[u == 0] <- .Machine$double.eps
    xmin + floor(-log(u) / lambda)
  }

  rpoisdis <- function (n, xmin, mu) {
    x <- rpois(n, mu)
    while (any(x < xmin)) {
      idx <- which(x < xmin)
      x[idx] <- rpois(length(idx), mu)
    }
    x
  }

  det_char <- function (coln, obj) {
    grep(coln, colnames(obj))
  }

  as.num <- function (z) {
    as.numeric(z)
  }

  dist   <- settings$dist[case]
  xmin   <- settings$xmin[case]
  par    <- settings$par[case]
  nbreak <- settings$nbreak[case]

  gen <- function (n) {
    if (dist == "exp") rexpdis(n, xmin, par) else rpoisdis(n, xmin, par)
  }

  one_rep <- function () {
    df <- gen(n)
    grid_df <- sort(unique(df))

    if (length(grid_df) < nbreak + 2) return(rep(NA_real_, 4))

    aux_fit <-
      fit_pwpldis(df, nbreak = nbreak, exclude_int = c(quantile(df, 0.9), Inf))

    hat_p <- as.num(aux_fit[, det_char("tau", aux_fit)])
    hat_a <- as.num(aux_fit[, det_char("alpha", aux_fit)])

    if (anyNA(hat_p) || anyNA(hat_a) ||
        !all(is.finite(hat_p)) || !all(is.finite(hat_a)) ||
        is.unsorted(hat_p, strictly = TRUE)) return(rep(NA_real_, 4))

    aux_boot <- boot_pwpldis(df, brks = hat_p[-1])
    boot <- aux_boot[, det_char("alpha", aux_boot)]

    hat_a <- as.num(2 * hat_a - apply(boot, 2, mean))
    if (anyNA(hat_a) || !all(is.finite(hat_a))) return(rep(NA_real_, 4))

    cdf_1 <- Vectorize(function(x) ppwpldis(x, hat_p, hat_a))
    fx <- cdf_1(grid_df)
    if (anyNA(fx) || is.unsorted(fx)) return(rep(NA_real_, 4))

    cdf_1 <- stepfun(grid_df, c(0, fx))

    c(dgof::ks.test(df, cdf_1)$p.value,
      dgof::cvm.test(df, cdf_1, type = "W2")$p.value,
      dgof::cvm.test(df, cdf_1, type = "U2")$p.value,
      dgof::cvm.test(df, cdf_1, type = "A2")$p.value)
  }

  set.seed(2024 + 100 * case)

  B <- 500

  pv <- matrix(NA_real_, nrow = B, ncol = 4,
               dimnames = list(NULL, c("KS", "W2", "U2", "A2")))

  for (ite in 1:B) {
    pv[ite, ] <- tryCatch(one_rep(), error = function(e) rep(NA_real_, 4))
    cat("| case:", case, "| n:", n, "| iter:", ite, "|", "\n")
  }

  ok <- stats::complete.cases(pv)

  data.frame(
    case    = case,
    n       = n,
    KS      = mean(pv[ok, "KS"] < 0.05),
    W2      = mean(pv[ok, "W2"] < 0.05),
    U2      = mean(pv[ok, "U2"] < 0.05),
    A2      = mean(pv[ok, "A2"] < 0.05),
    n_valid = sum(ok),
    n_fail  = B - sum(ok)
  )
}

grid <- expand.grid(n = n_grid, case = 1:nrow(settings))

number_of_cores <- 5
clusters <- parallel::makeCluster(number_of_cores)
doParallel::registerDoParallel(clusters)

estimates <- foreach(i = 1:nrow(grid),
                     .multicombine = TRUE,
                     .packages = pkgs) %dopar%
  FF(grid$n[i], grid$case[i], settings)

stopCluster(clusters)

results <- do.call(rbind, estimates)
results <- results[order(results$case, results$n), ]

for (k in 1:nrow(settings)) {
  results_case <- results[results$case == k, -1]
  save(results_case,
       file = paste0("simulation/results/sim4_power_case", k, ".RData"))
}

results
