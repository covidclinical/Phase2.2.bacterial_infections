count_helper <- function(x) {
  
  h <- 1 # h = hospitalization, start on the first one
  i <- 2 # i = index, start with index 2 (first output already counted in y)
  y <- 1 # y = output hospitalization vector, start with 1 
  
  # while index is less than total number of x (days_since_admission values)
  while (i <= length(x)){
    
    # check if the days since admission has jumped, if so, it is a new hospitalization
    if(x[i] != x[i-1]+1){
      h <- h + 1
    }
    
    # add the hospitalization number to the output vector and step up the index
    y <- c(y, h)
    i <- i + 1
  }
  
  # return output
  return(y)
}

count_hosp <- function(df){
  
  # use the group by summarise approach to:
    # 1) not lose the severe, in_icu, dead information (no need to left join later)
    # 2) ensure there are not multiple non-distinct entries for some reason  
  df2 <- df %>% 
    dplyr::group_by(cohort, patient_num, days_since_admission, in_hospital) %>% 
    dplyr::summarise(severe = paste0(severe, collapse = ','),
                     in_icu = paste0(in_icu, collapse = ','),
                     dead = paste0(dead, collapse = ','), 
                     calendar_date = paste0(calendar_date, collapse = ','))
    stopifnot(n_distinct(df2$severe) %in% c(1,2))
    stopifnot(n_distinct(df2$in_icu) %in% c(1,2))
    stopifnot(n_distinct(df2$dead) %in% c(1,2))

    # calculate sequences using helper function above
    out <- df2 %>%
      dplyr::group_by(cohort, patient_num) %>%
      dplyr::arrange(days_since_admission, .by_group = TRUE) %>% # make sure the days since admission are ordered
      dplyr::mutate(n_hospitalization = count_helper(days_since_admission)) %>%
      dplyr::group_by(cohort, patient_num, n_hospitalization) %>%
      dplyr::mutate(length_hospitalization = length(n_hospitalization))
    return(out)
  
}












