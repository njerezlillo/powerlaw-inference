
# probability mass function -----------------------------------------------

dpoidis <- function (x, xmin, mu) {
  if (x >= xmin) {
    x_grid <- 0:(xmin - 1)
    C <- exp(mu) - sum(mu^x_grid / factorial(x_grid))
    mu^x / factorial(x) / C
  } else {
    0
  }
}

dexpdis <- function (x, xmin, lambda) {
  if (x >= xmin) {
    C <- (1 - exp(-lambda)) * exp(lambda * xmin)
    exp(-lambda * x) * C
  } else {
    0
  }
}

dyuledis <- function (x, xmin, alpha) {
  if (x >= xmin) {
    C <- (alpha - 1) * gamma(xmin + alpha - 1) / gamma(xmin)
    gamma(x) / gamma(x + alpha) * C
  } else {
    0
  }
}

dlnormdis <- function(x, xmin, mu, sigma) {
  if (x >= xmin) {
    C <- plnorm(xmin - 1, mu, sigma, lower.tail = F)
    p_x <- plnorm(x, mu, sigma)
    p_x_minus_1 <- plnorm(x - 1, mu, sigma)
    (p_x - p_x_minus_1) * C
  } else {
    0
  }
}

dpoidis <- Vectorize(dpoidis, "x")
dexpdis <- Vectorize(dexpdis, "x")
dyuledis <- Vectorize(dyuledis, "x")
dlnormdis <- Vectorize(dlnormdis, "x")

# sampler -----------------------------------------------------------------

rpoidis <- function(n, xmin, mu) {
  samples <- numeric(n)
  
  for (i in 1:n) {
    u <- runif(1)
    cum_prob <- 0
    x <- xmin
    
    while (cum_prob < u) {
      cum_prob <- cum_prob + dpoidis(x, xmin, mu)
      x <- x + 1
    }
    
    samples[i] <- x - 1
  }
  
  return(samples)
}

ryuledis <- function(n, xmin, alpha) {
  samples <- numeric(n)
  
  for (i in 1:n) {
    u <- runif(1)
    cum_prob <- 0
    x <- xmin
    
    while (cum_prob < u) {
      cum_prob <- cum_prob + dyuledis(x, xmin, alpha)
      x <- x + 1
    }
    
    samples[i] <- x - 1
  }
  
  return(samples)
}

rexpdis <- function(n, xmin, lambda) {
  samples <- numeric(n)
  
  for (i in 1:n) {
    u <- runif(1)
    cum_prob <- 0
    x <- xmin
    
    while (cum_prob < u) {
      cum_prob <- cum_prob + dexpdis(x, xmin, lambda)
      x <- x + 1
    }
    
    samples[i] <- x - 1
  }
  
  return(samples)
}

rlnormdis <- function(n, xmin, mu, sigma) {
  samples <- numeric(n)
  
  for (i in 1:n) {
    u <- runif(1)
    cum_prob <- 0
    x <- xmin
    
    while (cum_prob < u) {
      cum_prob <- cum_prob + dlnormdis(x, xmin, mu, sigma)
      x <- x + 1
    }
    
    samples[i] <- x - 1
  }
  
  return(samples)
}
