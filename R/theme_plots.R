theme_plots <- theme_bw() +
  theme(strip.text = element_text(size = 5),
        axis.text.x = element_text(size = 8), 
        axis.text.y = element_text(size = 6), 
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        title = element_text(size = 10),
        plot.subtitle = element_text(size = 9, face = "italic")) 

theme_set(theme_plots)
