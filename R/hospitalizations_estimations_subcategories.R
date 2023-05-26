hospitalizations_estimation_subcategories <- function( input_df, subcategory ){
  
  ## check if the subcategory is one of the columns
  if( subcategory %in% c("disorder_group", "sex", "age_group", "icu_in_hosp", "dead_in_hosp")){
    print("OK")
  }else{ 
    print("subcategory not available")}
  ## identify the name of the column for the aggregation
  ncol <- which(colnames( input_df) == subcategory)
  input_df$category <- as.data.frame(input_df)[, ncol] 
  
  # get the numerator count  
  counts_subcategory_numerator <- input_df %>%
    dplyr::filter( icd_code_category == "bacterial" & 
                     time_p < end_date_plots &
                   time_p >= start_date_plots ) %>%
    dplyr::group_by( time_p, category ) %>%
    summarise(count_hosp =  ifelse(n_distinct(hospitalization_id) > obfuscation | isFALSE(obfuscation),
                                 n_distinct(hospitalization_id), 
                                 0.5)) %>%
    dplyr::mutate( period = ifelse( time_p <= pre_NPI,
                                    "pre-NPI", ifelse( time_p > full_NPI, "partial-NPI", "full-NPI"))
    )  

  # get the denominator count
  counts_subcategory_denominator <- input_df %>%
    dplyr::filter(  icd_code_category == "bacterial" & #check with joany if this is relative to bacterial or total
                    time_p < end_date_plots &
                    time_p >= start_date_plots ) %>%
    dplyr::group_by( time_p  ) %>%
    summarise(count_hosp_total =  ifelse(n_distinct(hospitalization_id) > obfuscation | isFALSE(obfuscation),
                                   n_distinct(hospitalization_id), 
                                   0.5)) %>%
    dplyr::mutate( period = ifelse( time_p <= pre_NPI,
                                    "pre-NPI", ifelse( time_p > full_NPI, "partial-NPI", "full-NPI"))
    )  
  
  
  # add the percentage estimation
  hosp_subcategory_summary <- counts_subcategory_numerator %>%
    dplyr::left_join(counts_subcategory_denominator, 
              by = c("time_p", "period")) %>% 
    tidyr::replace_na(list(count_hosp = 0)) %>%
    dplyr::mutate( percentage_hospitalizations = round(100*count_hosp/count_hosp_total, 3)) %>%
    dplyr::select( time_p, period, category, count_hosp, count_hosp_total, percentage_hospitalizations)
  
  return( hosp_subcategory_summary )
  
}
