library(dplyr)
library(ggplot2)
library(reshape2)

df <- NULL

for (k in 1:4){
  load(paste0("simulation/results/sim2_size_case", k + 4, ".RData"))
  colnames(results) <- c("n", "K", "W2", "U2", "A2")
  results$case <- paste("Case", k)

  df <- rbind(df, results)
}

df_long <- melt(df, id.vars = c("n", "case"),
                variable.name = "pc", value.name = "value")

shp  <- c(K = 1, W2 = 2, U2 = 3, A2 = 4)
cols <- c(K = "#9E3F84", W2 = "#007250", U2 = "#A73C00", A2 = "#00569F")
lab  <- scales::parse_format()(c("K", "W^2", "U^2", "A^2"))

ggplot(df_long, aes(x = n, y = value)) +
  facet_wrap(~case, scales = "free") +
  geom_segment(x = 100, xend = 500, y = 0.0814, yend = 0.0814,
               color = "gray40", linetype = 3, lwd = 0.5) +
  geom_segment(x = 100, xend = 500, y = 0.1186, yend = 0.1186,
               color = "gray40", linetype = 3, lwd = 0.5) +
  geom_point(aes(shape = pc, colour = pc), size = 3) +
  scale_shape_manual (name = "pc", values = shp,  breaks = names(shp),  labels = lab) +
  scale_colour_manual(name = "pc", values = cols, breaks = names(cols), labels = lab) +
  ylim(0, 0.6) + labs(x = "Sample size (n)", y = "Size of the test") +
  theme_classic() +
  theme(strip.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        plot.caption = element_text(size = 18, hjust = 0.5),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15),
        strip.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 18))

ggsave("output/fig11.pdf", width = 11.5, height = 9, dpi = 320)
