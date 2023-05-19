admission_plot_count <- function(df,
                                 x,
                                 y, 
                                 title,
                                 y_title,
                                 size = NULL, 
                                 color = NULL){
  plot <- ggplot(df, aes_string(x = x, y = y, color = color, size = size))  +
    geom_point() +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", limits = c(start_date_plots, end_date_plots), expand = expansion(mult = 0.01, add = 1)) +
    theme(axis.text.x=element_text(angle=60, hjust=1))+
    labs(x = "Calendar date",
         y = y_title, 
         title = title) 
  return(plot)
}