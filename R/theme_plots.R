theme_plots <- ggplot2::theme_bw() +
  ggplot2::theme(strip.text = ggplot2::element_text(size = 5),
        axis.text.x = ggplot2::element_text(size = 8), 
        axis.text.y = ggplot2::element_text(size = 6), 
        axis.title.x = ggplot2::element_text(size = 8),
        axis.title.y = ggplot2::element_text(size = 8),
        title = ggplot2::element_text(size = 10),
        plot.subtitle = ggplot2::element_text(size = 9, face = "italic")) 

theme_set(theme_plots)
