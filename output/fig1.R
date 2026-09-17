library(VGAM)
library(ggplot2)

prior1 <- function (alpha) {
  sqrt((zeta(alpha, deriv = 2) / zeta(alpha, deriv = 0)) - ((
    zeta(alpha, deriv = 1) / zeta(alpha, deriv = 0)) ^ 2))
}

prior2 <- function (alpha) {
  1 / (alpha - 1)
}

ggplot(data.frame(x = c(1.1, 10)), aes(x)) +
  stat_function(fun = prior1, aes(colour = "Discrete")) +
  stat_function(fun = prior2, aes(colour = "Continuous")) +
  labs(x = expression(alpha), y = expression(pi~(alpha))) +
  scale_color_manual("", values = c("dodgerblue3", "firebrick3")) +
  ylim(c(0, 10)) + theme_classic() + 
  theme(legend.position = "top",
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10), 
        legend.title = element_blank(),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 11))

ggsave("output/fig1.pdf", height = 4.5, width = 7)

