# function to get the number of admission in total and for bacterial infections
# input arguments the complete table (input_df), the category in case we want to 
# break down the counts (subcategory), the type refers to totals or bacterial infection ones, 
# and the time is to distinguish between counts per NPI period or month
a_tablesPerCohort <- function( input_df, subcategory, type, time ){
  
  ## check if the subcategory is one of the columns
  if( ! subcategory %in% c("total", "age_group", "sex", "season", 
                         "length_hospitalization_category", "icu_in_hosp", "dead_in_hosp")){
    print("subcategory not available")
  }
  
  # create a new column, called timeToGroup, based on the time argument
  if( time == "NPI period"){
    input_df$timeToGroup <- input_df$period
  }else if( time == "month"){
    input_df$timeToGroup <- input_df$time_p
  }
  
  
  ## identify the name of the column for the aggregation and create a new column called category for the counts
  if( subcategory != "total"){
    ncol <- which(colnames( input_df) == subcategory)
    input_df$category <- as.data.frame(input_df)[, ncol] 
  }
 
  # select the columns of interest (hospitalization_id, category and timeToGroup)
  # and get the counts of the distinct hospitalizations grouping by both category and timeToGroup
  # obfuscate the output
  if( subcategory != "total"){
    
    if( type == "all"){
      hospital_admissions <- input_df %>%
        dplyr::distinct(hospitalization_id, category, timeToGroup) %>% 
        dplyr::group_by(timeToGroup, category) %>%
        dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
        dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    }else if( type == "bacterial"){
      hospital_admissions <- input_df %>%
        dplyr::filter( icd_code_category == "bacterial") %>%
        dplyr::distinct(hospitalization_id, category, timeToGroup) %>% 
        dplyr::group_by(timeToGroup, category) %>%
        dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
        dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    }
  }
 
  # select the columns of interest (hospitalization_id and timeToGroup)
  # and get the counts of the distinct hospitalizations grouping by timeToGroup
  # obfuscate the output
  
  if( subcategory == "total"){
    
    if( type == "all"){
      hospital_admissions <- input_df %>%
        dplyr::distinct(hospitalization_id, timeToGroup) %>% 
        dplyr::group_by(timeToGroup) %>%
        dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
        dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    }else if( type == "bacterial"){
      hospital_admissions <- input_df %>%
        dplyr::filter( icd_code_category == "bacterial") %>%
        dplyr::distinct(hospitalization_id, timeToGroup) %>% 
        dplyr::group_by(timeToGroup) %>%
        dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
        dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    }
  }
  
  # return the counts table
  return( hospital_admissions )
  
}


# function to get the number of admission per big subgroups, `IBI_simple`, `IBI_extensive`,`Respiratory`, `Skin`
# input arguments the complete table (input_df), the category in case we want to 
# break down the counts (subcategory), and the time is to distinguish between counts per NPI period or month
a_tablesPerCohort_subgroup <- function( input_df, subcategory, time ){
  
  ## check if the subcategory is one of the columns
  if( ! subcategory %in% c("total", "age_group", "sex", "season", 
                           "length_hospitalization_category", "icu_in_hosp", "dead_in_hosp")){
    print("subcategory not available")
  }
  
  # create a new column, called timeToGroup, based on the time argument
  if( time == "NPI period"){
    input_df$timeToGroup <- input_df$period
  }else if( time == "month"){
    input_df$timeToGroup <- input_df$time_p
  }
  
  ## identify the name of the column for the aggregation
  if( subcategory != "total"){
    ncol <- which(colnames( input_df) == subcategory)
    
    # create a new column with the specific category for the breakdown
    input_df$category <- as.data.frame(input_df)[, ncol] 
    
    # select the columns of interest and transform all the subgroups from multiple columns to
    # just one column that we call subgroups
    # we only need to count the actual cases, so we can filter by value = yes after pivoting
    subset <- input_df %>%
      dplyr::select( hospitalization_id, category, timeToGroup, `IBI_simple`, `IBI_extensive`,
                     `Respiratory`, `Skin` ) %>%
      tidyr::pivot_longer( cols = c(`IBI_simple`, `IBI_extensive`,`Respiratory`, `Skin`), 
                           names_to = "subgroup", 
                           values_to = "values") %>%
      dplyr::filter( values == "yes") %>%
      dplyr::select( -values )
    
    # select the columns of interest and get the counts by category and subgroup
    hospital_subroups_admissions <- subset %>%
      dplyr::distinct(hospitalization_id, category, subgroup, timeToGroup) %>% 
      dplyr::group_by(timeToGroup, category, subgroup) %>%
      dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
      dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    
    
  }
  
  # same as above but without breaking down by any category
  if( subcategory == "total"){
    subset <- input_df %>%
      dplyr::select( hospitalization_id, timeToGroup, `IBI_simple`, `IBI_extensive`,
                     `Respiratory`, `Skin` ) %>%
      tidyr::pivot_longer( cols = c(`IBI_simple`, `IBI_extensive`,`Respiratory`, `Skin`), 
                           names_to = "subgroup", 
                           values_to = "values") %>%
      dplyr::filter( values == "yes") %>%
      dplyr::select( -values )
    
    hospital_subroups_admissions <- subset %>%
      dplyr::distinct(hospitalization_id, subgroup, timeToGroup) %>% 
      dplyr::group_by(timeToGroup, subgroup ) %>%
      dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
      dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    
  }

  return( hospital_subroups_admissions )
  
}

# function to get the number of admission per bacterial infection
# input arguments the complete table (input_df), the category in case we want to 
# break down the counts (subcategory), and the time is to distinguish between counts per NPI period or month
a_tablesPerCohort_bacterialInfection <- function( input_df, subcategory, time ){
  
  ## check if the subcategory is one of the columns
  if( ! subcategory %in% c("total", "age_group", "sex", "season", 
                           "length_hospitalization_category", "icu_in_hosp", "dead_in_hosp")){
    print("subcategory not available")
  }
  
  # create a new column, called timeToGroup, based on the time argument
  if( time == "NPI period"){
    input_df$timeToGroup <- input_df$period
  }else if( time == "month"){
    input_df$timeToGroup <- input_df$time_p
  }
  
  ## identify the name of the column for the aggregation
  if( subcategory != "total"){
    ncol <- which(colnames( input_df) == subcategory)
    input_df$category <- as.data.frame(input_df)[, ncol] 
    
    # filter by bacterial infection, since all the disorder groups are bacterial
    # count distinct hospitalizations by category (e.g. sex), disorder group (e.g., empyema) and time (e.g., month)
    hospital_bacterial_infection_admissions <- input_df %>%
      dplyr::filter( icd_code_category == "bacterial") %>%
      dplyr::distinct(hospitalization_id, category, disorder_group, timeToGroup) %>% 
      dplyr::group_by(timeToGroup, category, disorder_group) %>%
      dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
      dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    
  }
  
  # same as above but without filtering by category
  # count distinct hospitalizations by disorder group (e.g., empyema) and time (e.g., month)
  if( subcategory == "total"){
   
    hospital_bacterial_infection_admissions <- input_df %>%
      dplyr::filter( icd_code_category == "bacterial") %>%
      dplyr::distinct(hospitalization_id, disorder_group, timeToGroup) %>% 
      dplyr::group_by(timeToGroup, disorder_group ) %>%
      dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
      dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    
  }
  
  return( hospital_bacterial_infection_admissions )
  
}

