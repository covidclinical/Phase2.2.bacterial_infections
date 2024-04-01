rm(list=ls())
library(ggpubr)
library(ggplot2)

load("bacterial_temporalTrend/skin_dat_combined.RData")
load("bacterial_temporalTrend/IBI_extensive_dat_combined.RData")
load("bacterial_temporalTrend/IBI_simple_dat_combined.RData")

skinPlot <- ggplot(skin) +
  geom_line(aes(x = timeToGroup, y = bacterial, colour = site)) + 
  labs(x="time",y="number of infection") +
  theme(legend.position = "right") +
  ggtitle("Skin Infections")

ibiSimplePlot <- ggplot(IBI_simple) +
  geom_line(aes(x = timeToGroup, y = bacterial, colour = site)) + 
  labs(x="time",y="number of infection") +
  theme(legend.position = "none") +
  ggtitle("Simple Invasive Bacterial Infections")

ibiExtensivePlot <- ggplot(IBI_extensive) +
  geom_line(aes(x = timeToGroup, y = bacterial, colour = site)) + 
  labs(x="time",y="number of infection") +
  theme(legend.position = "none") +
  ggtitle("Extensive Invasive Bacterial Infections")



top_row = ggarrange(ibiSimplePlot, ibiExtensivePlot, ncol = 2, labels = c("A", "B"))
bottom_row = ggarrange( skinPlot, ncol = 1, labels = c("C"))
final_plot = ggarrange(top_row, bottom_row, ncol = 1)

ggsave(filename = "./nonRespiratoryInfections.png",
       plot = final_plot, 
       width = 350, 
       height = 250, 
      units = "mm",
       dpi=700)



