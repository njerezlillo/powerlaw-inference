FF <- function (z) {
  all_networks <- dir("application/icon/data")
  
  tbl <- data.frame(Data = z, n = NA, xmin = NA, alpha = NA,
                    K = NA, W2 = NA, U2 = NA, A2 = NA)
  
  etiq <- paste0("application/icon/data/", all_networks[z])
  my_data <- read.delim(etiq, sep = ",")
  x <- rep(my_data$xvalue, my_data$counts)
  x <- x[x > 0]
  
  # Strategy 1 (Fig. 5): xmin by KS minimization with the discrete
  # Fisher-penalized MLE evaluated at every candidate threshold, then alpha_Fd
  # at the selected xmin, then GOF testing (K, W2, U2, A2). `fit_pldis(x,
  # penalized = TRUE)` computes exactly this: it maximizes
  # L(alpha) + 0.5*log{I_discrete(alpha)}, i.e. alpha_Fd (Eq. 6).
  mle <- fit_pldis(x, penalized = F)
  fd  <- fit_pldis(x, penalized = T)
  
  tbl[1, 2] <- length(x)
  tbl[1, 3] <- as.character(fd$xmin)
  tbl[1, 4] <- fd$alpha
  tbl[1, 5] <- plpva(x, xmin =  mle$xmin, vec = seq(1.5, 3.5, .01))$p
  
  x_grid <- sort(unique(x[x >= fd$xmin]))
  est_cdf <- poweRlaw::ppldis(x_grid, fd$xmin, fd$alpha)
  step_aux <- stepfun(x_grid, c(0, est_cdf))
  
  tbl[1, 6] <- dgof::cvm.test(x[x >= fd$xmin], step_aux, type = "W2")$p.value
  tbl[1, 7] <- dgof::cvm.test(x[x >= fd$xmin], step_aux, type = "U2")$p.value
  tbl[1, 8] <- dgof::cvm.test(x[x >= fd$xmin], step_aux, type = "A2")$p.value
  
  cat("| iter:", z, "|", "\n")
  
  return(tbl)
}