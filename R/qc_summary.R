qc_summary <- function(input_df, obfuscation_threshold, dir.output){
  
  print("Starting QC summary")
  
  ### check that the ICD codes follow the regular expression [A-Z][0-9][0-9AB]\.?[0-9A-TV-Z]{0,4}
  codesToReview <- input_df %>% dplyr::filter(concept_type == "DIAG-ICD10",
                                             ! grepl( "[A-Z][0-9][0-9AB]\\.?[0-9A-TV-Z]{0,4}", concept_code)) %>%
                  dplyr::ungroup() %>%
                  dplyr::select(concept_type, concept_code, calendar_date)
  print(codesToReview)
  
  ### summary of the ICD codes for QC
  diag_sum <- input_df %>%
    filter( concept_type == 'DIAG-ICD10', 
            age_time_diagnosis < 18 ) %>%
    group_by( concept_code ) %>%
    summarise( n_patients = n_distinct( patient_num ) ) %>%
    mutate( n_patients = ifelse( n_patients > obfuscation_threshold | isFALSE( obfuscation_threshold), n_patients, 0.5)) %>%
    arrange( desc(n_patients))
  
  save( diag_sum, file=paste0( dir.output, "/ICDdiagnosisCodes.RData"))
  ###
  
  outcome_summary <- input_df %>%
    group_by(patient_num) %>%
    summarise(ever_in_hospital = max(in_hospital),
              ever_in_icu = max(as.numeric(in_icu)),
              ever_dead = max(as.numeric(dead)))
  print(paste0("Total number of patients ever hospitalized: ", sum(outcome_summary$ever_in_hospital)))
  print(paste0("Total number of patients ever in icu: ", sum(outcome_summary$ever_in_icu)))
  print(paste0("Total number of patients ever dead: ", sum(outcome_summary$ever_dead)))
  
  ### total patients should be the same that total number of patients ever hospitalized
  ### QC is done after filtering by hospitalized patients 
  total_n <- length(unique(input_df$patient_num))
  print(paste0("Total patients: ", total_n))
  
  bacterialInfectionCodes <- input_df %>%
                                  dplyr::filter( concept_type == "DIAG-ICD10",
                                                 concept_code %in% icdCodes$concept_code )
  
  print( paste0("There are ", length(unique(bacterialInfectionCodes$concept_code)), " ICD10 bacterial infection codes in this site out of the total ",length(unique(icdCodes$ICD10_Code)), " in the dataset"))
  
  ### add a print of the summary of length of hospitalization
  length_hospitalization_check <- input_df %>%
    dplyr::group_by( hospitalization_id ) %>%
    dplyr::select( hospitalization_id, length_hospitalization) %>%
    unique()
  
  summary(length_hospitalization_check$length_hospitalization )
  print(paste0("The min days hospitalized is ", min( length_hospitalization_check$length_hospitalization)))
  print(paste0("The mean days hospitalized is ", round(mean( length_hospitalization_check$length_hospitalization), 2)))
  print(paste0("The max days hospitalized is ", max( length_hospitalization_check$length_hospitalization)))
  
  #number of hospitalizations of more than 3 months
  moreThan3months <- length_hospitalization_check %>%
    dplyr::filter( length_hospitalization > 90 )
  print(paste0("There are ", length( unique(moreThan3months$hospitalization_id)), " hospitalizations of length > 90 days"))
  
  #number of hospitalizations of more than 1 year
  moreThan1year <- length_hospitalization_check %>%
    dplyr::filter( length_hospitalization > 365 )
  print(paste0("There are ", length( unique(moreThan1year$hospitalization_id)), " hospitalizations of length > 365 days"))
  
}

