library(dplyr)
library(ggplot2)
library(reshape2)

df <- NULL

for (k in 1:4){
  load(paste0("simulation/results/sim2_power_case", k, ".RData"))
  colnames(results) <- c("n", "K", "W2", "U2", "A2")  
  results$case <- paste("Case", k)
  
  df <- rbind(df, results)
}

df_long <- melt(df, id.vars = c("n", "case"), 
                variable.name = "pc", value.name = "value")

cols <- c(K = "#9E3F84", W2 = "#007250", U2 = "#A73C00", A2 = "#00569F")
lty  <- c(K = 3, W2 = 4, U2 = 1, A2 = 2)
lab  <- scales::parse_format()(c("K", "W^2", "U^2", "A^2"))

ggplot(df_long, aes(x = n, y = value, colour = pc, linetype = pc)) +
  facet_wrap(~case, scales = "free") +
  geom_line(linewidth = 0.8) +
  scale_linetype_manual(name = "pc", values = lty,  breaks = names(lty),  labels = lab) +
  scale_colour_manual (name = "pc", values = cols, breaks = names(cols), labels = lab) +
  labs(x = "Sample size (n)", y = "Power of the test") +
  ylim(c(0, 1)) + theme_classic() +
  theme(strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        plot.caption = element_text(size = 18, hjust = 0.5),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.width = unit(1.1, "cm"))

ggsave("output/fig4.pdf", width = 11.5, height = 9, dpi = 320)
