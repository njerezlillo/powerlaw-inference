library(dplyr)
library(xtable)
library(scales)
library(ggExtra)
library(stringr)
library(ggplot2)
library(reshape2)
library(poweRlaw)
# devtools::install_github("njerezlillo/pldis")
# devtools::install_github("njerezlillo/pwpldis")
library(pldis)
library(pwpldis)

tbl <- read.table("application/icon/tbl_results.txt")
tbl %>% head

# Table -------------------------------------------------------------------

print(
  xtable(
    rbind(
      tbl %>% group_by(class) %>%
        summarise(Number = n(), K = sum(K >= 0.1), W2 = sum(W2 >= 0.1),
                  U2 = sum(U2 >= 0.1), A2 = sum(A2 >= 0.1)),
      tbl %>%
        summarise(Number = n(), K = sum(K >= 0.1), W2 = sum(W2 >= 0.1),
                  U2 = sum(U2 >= 0.1), A2 = sum(A2 >= 0.1))
      %>% mutate(class = "Total")
    )
  ), include.rownames = FALSE
)

# Fig 6 -------------------------------------------------------------------

ggplot(tbl %>% filter(U2 >= 0.1), aes(x = alpha)) +
  annotate("rect", xmin = 2, xmax = 3, ymin = 0, ymax = 800,
           fill = "gray", alpha = 0.3) +
  geom_histogram(color = "black", fill = "white") +
  xlim(c(1.5, 3.5)) + ylim(c(0, 800)) + theme_classic() +
  labs(x = "Scaling parameter", y = "Frequency") +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

ggsave("application/icon/fig6.pdf", height = 3.7, width = 7)

# Fig 7 -------------------------------------------------------------------

p <-
  ggplot() +
  geom_point(data = tbl %>% filter(match == "yes"),
             aes(x = n, y = avg.degree), color = "gray80",
             alpha = 0.7, size = 2) +
  geom_point(data = tbl %>% filter(match == "no"),
             aes(x = n, y = avg.degree), alpha = 0.7,
             color = "black", size = 2, pch = 16) +
  scale_x_log10(breaks = 10 ^ (1:6),
                labels = c(expression(10^1), expression(10^2),
                           expression(10^3), expression(10^4),
                           expression(10^5), expression(10^6))) +
  scale_y_log10(breaks = c(2, 10, 100),
                labels = c(expression(10^0),
                           expression(10^1),
                           expression(10^2))) +
  labs(x = "Number of nodes", y = "Average degree") +
  theme_classic() +
  theme(legend.position = "bottom",
        plot.caption = element_text(size = 20, hjust = 0.5),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 20),
        axis.title = element_text(size = 16),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))

q <- ggMarginal(p, type = "histogram",
                xparams = list(bins = 20),
                yparams = list(bins = 10),
                fill = "dodgerblue2", alpha = 0.7)
q

ggsave(plot = q, "application/icon/fig7.pdf", height = 6, width = 10)

# Appendix ----------------------------------------------------------------

sum(tbl$U2[tbl$K >= 0.1] >= 0.1)
sum(tbl$U2[tbl$K < 0.1] < 0.1)

sum(tbl$U2[tbl$K >= 0.1] < 0.1)
sum(tbl$U2[tbl$K < 0.1] >= 0.1)

# Fig 8a ------------------------------------------------------------------

### tbl[40,]

df <- read.delim(
  "application/icon/data/Binary_interactomes_various_species_2012_M_musculus_mouse_binary_hq_Biological_Protein_interactions_n4.gmldistribution.txt",
  sep = ","
)

x <- rep(df$xvalue, df$counts)
sum(x == 1)/length(x)
x_grid <- sort(unique(x))

k <- 1
p <- 0

while (TRUE) {
  fit <- fit_pwpldis(x, nbreak = k, exclude_int = c(quantile(x_grid, 0.9), Inf))

  fit_p <- as.numeric(fit[, grep("tau", colnames(fit))])
  fit_a <- as.numeric(fit[, grep("alpha", colnames(fit))])

  cdf_pwpl <- Vectorize(function(z) ppwpldis(z, fit_p, fit_a))
  den_pwpl <- Vectorize(function(z) dpwpldis(z, fit_p, fit_a))

  UCL_pwpl <- function(t) {
    den_pwpl(t) - 1.96 * sqrt(den_pwpl(t) * (1 - den_pwpl(t))/length(x))
  }

  LCL_pwpl <- function(t) {
    den_pwpl(t) + 1.96 * sqrt(den_pwpl(t) * (1 - den_pwpl(t))/length(x))
  }

  step_aux <- stepfun(x_grid, c(0, cdf_pwpl(x_grid)))
  p <- dgof::cvm.test(x, step_aux, type = "U2")$p.value

  if (p > 0.05 | k == 3) break
  k <- k + 1
}

ggplot(df, aes(x = xvalue, y = counts/sum(counts))) +
  stat_function(fun = UCL_pwpl, linetype = 2, color = "gray30") +
  stat_function(fun = LCL_pwpl, linetype = 2, color = "gray30") +
  stat_function(fun = den_pwpl, size = 0.5, alpha = 1) +
  geom_point(size = 1.5, pch = 1) +
  annotate("text", x = Inf, y = Inf, label = "(a)",
           hjust = 1.1, vjust = 1.5, size = 5) +
  scale_x_log10(breaks = c(1, 10),
                labels = scales::trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "bl") +
  xlab("Node degree (log scale)") +
  ylab("Frequency (log scale)") +
  theme_classic() +
  theme(text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

ggsave("application/icon/fig8a.pdf", height = 4.5, width = 6, dpi = 320)

# Alternative models

xmin <- 1

m_pois  <- dispois$new(x)
m_lnorm <- dislnorm$new(x)
m_exp   <- disexp$new(x)

m_pois$setXmin(xmin)
m_lnorm$setXmin(xmin)
m_exp$setXmin(xmin)

m_pois$setPars(estimate_pars(m_pois))
m_lnorm$setPars(estimate_pars(m_lnorm))
m_exp$setPars(estimate_pars(m_exp))

# AIC de cada modelo
2 - 2 * dist_ll(m_pois)
2 - 2 * dist_ll(m_lnorm)
2 - 2 * dist_ll(m_exp)

fit$AIC

# BIC de cada modelo
log(length(x)) - 2 * dist_ll(m_pois)
log(length(x)) - 2 * dist_ll(m_lnorm)
log(length(x)) - 2 * dist_ll(m_exp)

fit$BIC

# Test de Watson

cdf_pois  <- function(z) dist_cdf(m_pois,  q = z, lower_tail = TRUE)
cdf_lnorm <- function(z) dist_cdf(m_lnorm, q = z, lower_tail = TRUE)
cdf_exp   <- function(z) dist_cdf(m_exp,   q = z, lower_tail = TRUE)

step_pois  <- stepfun(x_grid, c(0, cdf_pois(x_grid)))
step_lnorm <- stepfun(x_grid, c(0, cdf_lnorm(x_grid)))
step_exp   <- stepfun(x_grid, c(0, cdf_exp(x_grid)))

p_pois  <- dgof::cvm.test(x, step_pois,  type = "U2")$p.value
p_lnorm <- dgof::cvm.test(x, step_lnorm, type = "U2")$p.value
p_exp   <- dgof::cvm.test(x, step_exp,   type = "U2")$p.value

p; p_pois; p_lnorm; p_exp

# Fig 8b ------------------------------------------------------------------

### tbl[2236,]

df <- read.delim(
  "application/icon/data/Rat_brain_2011-2013_Rat_rattus_norvegicus_brain_1_2011_Biological_Connectome_n3.gml_multiplex6.0_directedtotaldistribution.txt",
  sep = ","
)

x <- rep(df$xvalue, df$counts)
x_grid <- sort(unique(x))

k <- 1
p <- 0

while (TRUE) {
  fit <- fit_pwpldis(x, nbreak = k, exclude_int = c(quantile(x_grid, 0.9), Inf))

  fit_p <- as.numeric(fit[, grep("tau", colnames(fit))])
  fit_a <- as.numeric(fit[, grep("alpha", colnames(fit))])

  cdf_pwpl <- Vectorize(function(z) ppwpldis(z, fit_p, fit_a))
  den_pwpl <- Vectorize(function(z) dpwpldis(z, fit_p, fit_a))

  UCL_pwpl <- function(t) {
    den_pwpl(t) - 1.96 * sqrt(den_pwpl(t) * (1 - den_pwpl(t))/length(x))
  }

  LCL_pwpl <- function(t) {
    den_pwpl(t) + 1.96 * sqrt(den_pwpl(t) * (1 - den_pwpl(t))/length(x))
  }

  step_aux <- stepfun(x_grid, c(0, cdf_pwpl(x_grid)))
  p <- dgof::cvm.test(x, step_aux, type = "U2")$p.value

  if (p > 0.1 | k == 3) break
  k <- k + 1
}

ggplot(df, aes(x = xvalue, y = counts/sum(counts))) +
  stat_function(fun = UCL_pwpl, linetype = 2, color = "gray30") +
  stat_function(fun = LCL_pwpl, linetype = 2, color = "gray30") +
  stat_function(fun = den_pwpl, size = 0.5, alpha = 1) +
  annotate("text", x = Inf, y = Inf, label = "(b)",
           hjust = 1.1, vjust = 1.5, size = 5) +
  geom_point(size = 1.5, pch = 1) +
  scale_x_log10(breaks = c(1, 10),
                labels = scales::trans_format("log10", math_format(10^.x))) +
  scale_y_log10(labels = scales::trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "bl") +
  xlab("Node degree (log scale)") +
  ylab("Frequency (log scale)") +
  theme_classic() +
  theme(text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

ggsave("application/icon/fig8b.pdf", height = 4.5, width = 6, dpi = 320)

# Alternative models

xmin <- 1

m_pois  <- dispois$new(x)
m_lnorm <- dislnorm$new(x)
m_exp   <- disexp$new(x)

m_pois$setXmin(xmin)
m_lnorm$setXmin(xmin)
m_exp$setXmin(xmin)

m_pois$setPars(estimate_pars(m_pois))
m_lnorm$setPars(estimate_pars(m_lnorm))
m_exp$setPars(estimate_pars(m_exp))

# AIC de cada modelo
2 - 2 * dist_ll(m_pois)
2 - 2 * dist_ll(m_lnorm)
2 - 2 * dist_ll(m_exp)

fit$AIC

# BIC de cada modelo
log(length(x)) - 2 * dist_ll(m_pois)
log(length(x)) - 2 * dist_ll(m_lnorm)
log(length(x)) - 2 * dist_ll(m_exp)

fit$BIC

# Test de Watson

cdf_pois  <- function(z) dist_cdf(m_pois,  q = z, lower_tail = TRUE)
cdf_lnorm <- function(z) dist_cdf(m_lnorm, q = z, lower_tail = TRUE)
cdf_exp   <- function(z) dist_cdf(m_exp,   q = z, lower_tail = TRUE)

step_pois  <- stepfun(x_grid, c(0, cdf_pois(x_grid)))
step_lnorm <- stepfun(x_grid, c(0, cdf_lnorm(x_grid)))
step_exp   <- stepfun(x_grid, c(0, cdf_exp(x_grid)))

p_pois  <- dgof::cvm.test(x, step_pois,  type = "U2")$p.value
p_lnorm <- dgof::cvm.test(x, step_lnorm, type = "U2")$p.value
p_exp   <- dgof::cvm.test(x, step_exp,   type = "U2")$p.value

p; p_pois; p_lnorm; p_exp
