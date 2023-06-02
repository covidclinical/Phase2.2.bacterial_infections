a1_tablesPerCohort <- function( input_df, subcategory, type ){
  
  ## check if the subcategory is one of the columns
  if( subcategory %in% c("total", "age_group", "sex", "season", 
                         "length_hospitalization_category", "icu_in_hosp", "dead_in_hosp")){
    print("OK")
  }else{ 
    print("subcategory not available")}
  
  
  ## identify the name of the column for the aggregation
  if( subcategory != "total"){
    ncol <- which(colnames( input_df) == subcategory)
    input_df$category <- as.data.frame(input_df)[, ncol] 
  }
 
  
  if( subcategory != "total"){
    
    if( type == "all"){
      hospital_admissions <- input_df %>%
        dplyr::distinct(hospitalization_id, category, period) %>% 
        dplyr::group_by(period, category) %>%
        dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
        dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    }else if( type == "bacterial"){
      hospital_admissions <- input_df %>%
        dplyr::filter( icd_code_category == "bacterial") %>%
        dplyr::distinct(hospitalization_id, category, period) %>% 
        dplyr::group_by(period, category) %>%
        dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
        dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    }
  }
  
  if( subcategory == "total"){
    
    if( type == "all"){
      hospital_admissions <- input_df %>%
        dplyr::distinct(hospitalization_id, period) %>% 
        dplyr::group_by(period) %>%
        dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
        dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    }else if( type == "bacterial"){
      hospital_admissions <- input_df %>%
        dplyr::filter( icd_code_category == "bacterial") %>%
        dplyr::distinct(hospitalization_id, period) %>% 
        dplyr::group_by(period) %>%
        dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
        dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    }
  }
  
  return( hospital_admissions )
  
}


a1_tablesPerCohort_subgroup <- function( input_df, subcategory ){
  
  ## check if the subcategory is one of the columns
  if( subcategory %in% c("total", "age_group", "sex", "season", 
                         "length_hospitalization_category", "icu_in_hosp", "dead_in_hosp")){
    print("OK")
  }else{ 
    print("subcategory not available")}
  
  
  ## identify the name of the column for the aggregation
  if( subcategory != "total"){
    ncol <- which(colnames( input_df) == subcategory)
    input_df$category <- as.data.frame(input_df)[, ncol] 
    
    subset <- input_df %>%
      dplyr::select( hospitalization_id, category, period, `IBI_simple`, `IBI_extensive`,
                     `Respiratory`, `Skin` ) %>%
      tidyr::pivot_longer( cols = c(`IBI_simple`, `IBI_extensive`,`Respiratory`, `Skin`), 
                           names_to = "subgroup", 
                           values_to = "values") %>%
      dplyr::filter( values == "yes") %>%
      dplyr::select( -values )
    
    hospital_subroups_admissions <- subset %>%
      dplyr::distinct(hospitalization_id, category, subgroup, period) %>% 
      dplyr::group_by(period, category, subgroup) %>%
      dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
      dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    
    
  }
  
  if( subcategory == "total"){
    subset <- input_df %>%
      dplyr::select( hospitalization_id, period, `IBI_simple`, `IBI_extensive`,
                     `Respiratory`, `Skin` ) %>%
      tidyr::pivot_longer( cols = c(`IBI_simple`, `IBI_extensive`,`Respiratory`, `Skin`), 
                           names_to = "subgroup", 
                           values_to = "values") %>%
      dplyr::filter( values == "yes") %>%
      dplyr::select( -values )
    
    hospital_subroups_admissions <- subset %>%
      dplyr::distinct(hospitalization_id, subgroup, period) %>% 
      dplyr::group_by(period, subgroup ) %>%
      dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
      dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    
  }

  return( hospital_subroups_admissions )
  
}


a1_tablesPerCohort_bacterialInfection <- function( input_df, subcategory ){
  
  ## check if the subcategory is one of the columns
  if( subcategory %in% c("total", "age_group", "sex", "season", 
                         "length_hospitalization_category", "icu_in_hosp", "dead_in_hosp")){
    print("OK")
  }else{ 
    print("subcategory not available")}
  
  
  ## identify the name of the column for the aggregation
  if( subcategory != "total"){
    ncol <- which(colnames( input_df) == subcategory)
    input_df$category <- as.data.frame(input_df)[, ncol] 
    
    hospital_bacterial_infection_admissions <- input_df %>%
      dplyr::filter( icd_code_category == "bacterial") %>%
      dplyr::distinct(hospitalization_id, category, disorder_group, period) %>% 
      dplyr::group_by(period, category, disorder_group) %>%
      dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
      dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    
  }
  
  if( subcategory == "total"){
   
    hospital_bacterial_infection_admissions <- input_df %>%
      dplyr::filter( icd_code_category == "bacterial") %>%
      dplyr::distinct(hospitalization_id, disorder_group, period) %>% 
      dplyr::group_by(period, disorder_group ) %>%
      dplyr::summarise( count = n_distinct(hospitalization_id) ) %>%
      dplyr::mutate( count = ifelse( count > obfuscation, count, obfuscation * 0.5)  )
    
  }
  
  return( hospital_bacterial_infection_admissions )
  
}

