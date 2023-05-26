admission_plot_count <- function(df,
                                 x,
                                 y, 
                                 title,
                                 y_title,
                                 size = NULL, 
                                 color = NULL){
  plot <- ggplot(df, aes_string(x = x, y = y, color = color, size = size))  +
    ggplot2::geom_point() +
    ggplot2::scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", limits = c(start_date_plots, end_date_plots), expand = expansion(mult = 0.01, add = 1)) +
    ggplot2::theme(axis.text.x=element_text(angle=60, hjust=1))+
    ggplot2::labs(x = "Calendar date",
         y = y_title, 
         title = title) +
    ggplot2::geom_vline(xintercept = as.Date(pre_NPI),
               linetype = "dashed") +
    
    ggplot2::geom_vline(xintercept = as.Date(full_NPI),
                        linetype = "dashed") 
  return(plot)
}