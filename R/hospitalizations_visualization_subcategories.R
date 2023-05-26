hospitalizations_visualization_subcategories <- function( input_df, type, title_plot, subtitle_plot ){
  
  if( type == "counts"){
    output_pl <- input_df %>%  
      ggplot2::ggplot(aes(x = time_p, y= count_hosp, fill = period, group = category )) +
      ggplot2::geom_bar(stat = "identity")+
      ggplot2::geom_vline(xintercept = as.Date(pre_NPI), linetype = "dashed") +
      ggplot2::geom_vline(xintercept = as.Date(full_NPI), linetype = "dashed") +
      ggplot2::labs(x = time_period,
           y = "Hospitalization Counts",
           title = title_plot, 
           subtitle = subtitle_plot) + 
      ylim(0, NA) + 
      ggplot2::facet_wrap(. ~ category, scales = "free_y") +
      ggplot2::theme(strip.text.x = element_text(size = 10),
            axis.text.x = element_text(size = 5, angle = 90), 
            axis.text.y = element_text(size = 6), 
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 10),
            title = element_text(size = 12),
            plot.subtitle = element_text(size = 11),
            legend.position = "none") +
      ggplot2::scale_fill_manual(values = cbPalette[-1])
    
  }else if(type == "percentages"){
    output_pl <- input_df %>%  
      ggplot2::ggplot(aes(x = time_p, y= percentage_hospitalizations, fill = period, group = category )) +
      ggplot2::geom_bar(stat = "identity")+
      ggplot2::geom_vline(xintercept = as.Date(pre_NPI), linetype = "dashed") +
      ggplot2::geom_vline(xintercept = as.Date(full_NPI), linetype = "dashed") +
      ggplot2::labs(x = time_period,
                    y = "Percentage of hospitalizations",
                    title = title_plot, 
                    subtitle = subtitle_plot) + 
      ylim(0, NA) + 
      ggplot2::facet_wrap(. ~ category, scales = "free_y") +
      ggplot2::theme(strip.text.x = element_text(size = 10),
                     axis.text.x = element_text(size = 5, angle = 90), 
                     axis.text.y = element_text(size = 6), 
                     axis.title.x = element_text(size = 10),
                     axis.title.y = element_text(size = 10),
                     title = element_text(size = 12),
                     plot.subtitle = element_text(size = 11),
                     legend.position = "none") +
      ggplot2::scale_fill_manual(values = cbPalette[-1])
    
  }
  
  output_pl
  return(output_pl)
  
}
  

