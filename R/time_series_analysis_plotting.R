time_series_analysis_plotting <- function(model, df_count_bacterial, count_col, clearance_period = FALSE) {
  output_model <- broom::tidy(model)
  coeff_slope <- round(output_model[output_model$term == "periodduring_pandemic:time", "estimate"][[1]], 3)
  pvalue_slope <- round(output_model[output_model$term == "periodduring_pandemic:time", "p.value"][[1]], 3)
  annotation <- paste0("Lockdown slope: ", coeff_slope, " (pvalue: ", pvalue_slope, ")")
  pred_model <- predict(model, df_count_bacterial, type = "response", interval = "confidence") %>% as.data.frame()
  pred_model_b <- predict(model, mutate(df_count_bacterial, period = "pre-NPI"), type = "response", interval = "confidence") %>%
    as.data.frame()
  names(pred_model_b) <- paste0(names(pred_model_b), "_wo_intervention")
  pred_model <- bind_cols(df_count_bacterial, pred_model, pred_model_b)
  if (clearance_period) {
    pred_model$clearance_period <- ifelse(pred_model$time_p < as.Date(cut(start_clear_period, breaks = time_period)) ,
                                          "pre-NPI",
                                          ifelse(pred_model$time_p > as.Date(cut(end_clear_period, breaks = time_period)),
                                                 "full&partial_NPI",
                                                 "during_clearance")
    )
    pred_model$lwr <- ifelse(pred_model$clearance_period == "during_clearance", NA, pred_model$lwr)
    pred_model$upr <- ifelse(pred_model$clearance_period == "during_clearance", NA, pred_model$upr)
    pred_model$fit_wo_intervention <- ifelse(pred_model$time_p >= max(pred_model$time_p[pred_model$clearance_period == "pre-NPI"]),
                                             pred_model$fit_wo_intervention,
                                             NA)
  } else {
    pred_model$clearance_period <- pred_model$period
  }
  plot <- ggplot(pred_model, aes_string(x = "time_p", y = count_col)) +
    geom_point(aes(colour = clearance_period)) +
    geom_line(aes(x = time_p, y = fit_wo_intervention, colour = "Expected mean w/o pandemic"), linetype = "dashed") +
    geom_line(aes(x = time_p, y = fit, colour = clearance_period)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr, colour = clearance_period), alpha = 0.3, colour = NA) +
    scale_color_manual(name = "Time Series projection", values = c("pre-NPI" = "black",
                                                                   "Expected mean w/o pandemic" = "red",
                                                                   "full&partial_NPI" = "black",
                                                                   "during_clearance" = "white")) +
    annotate("text", x = as.Date("2019-01-01"), y = max(pred_model[[count_col]]) + 5, label = annotation) +
    ggplot2::theme(legend.position = "bottom")
  
  if (clearance_period) {
    plot +
      geom_vline(xintercept = as.Date(max(pred_model$time_p[pred_model$clearance_period == "pre-NPI"])), linetype = "dashed") +
      geom_vline(xintercept = as.Date(min(pred_model$time_p[pred_model$clearance_period == "full&partial_NPI"])), linetype = "dashed")
  } else {
    plot + geom_vline(xintercept = as.Date(pre_NPI), linetype = "dashed")
  }
}
