# df = all_codes
# use calendar_date here and not admit_date, looking at codes at a whole

summarize_bi_codes <- function(df, icdCodes){
  
  # filter to only include bacterial infection codes
  print('filter to only include bacterial infection codes')
  df2 <- all_codes %>% dplyr::filter( concept_code %in% icdCodes$concept_code )
  print(paste0('there are ', length(unique(df2$concept_code)), 'unique bacterial infection codes in the dataset'))
  
  # count the number of patients PER CODE, min and max date 
  # this will help to identify codes that were only used for specific periods 
  df3 <- df2 %>%
    dplyr::group_by( concept_code ) %>%
    dplyr::summarise( distinct_patients = ifelse(length(unique(patient_num)) > obfuscation | isFALSE(obfuscation),
                                                 length(unique(patient_num)), 
                                                 0.5),
                      distinct_observations = ifelse(length(patient_num) > obfuscation | isFALSE(obfuscation),
                                                     length(patient_num), 
                                                     0.5),                           
                      min_date = min( as.Date( calendar_date, format = dateFormat ) ), 
                      max_date = max( as.Date( calendar_date, format = dateFormat ) ))%>% 
    dplyr::arrange(desc(distinct_observations))
  
  # add the descriptions to the codes
  df4 <- df3 %>%
    dplyr::left_join( icdCodes, by="concept_code")  %>%
    dplyr::select( "disorder_group", "description", "concept_code", "distinct_patients", 
                   "distinct_observations", "min_date", "max_date")
  print('summary of patients & dates per code generated')
  
  # count the number of patients PER DISORDER GROUP and year 
  df5 <- df2 %>%
    dplyr::left_join( icdCodes, by = 'concept_code') %>%
    dplyr::mutate( year = format( as.Date(calendar_date, format = dateFormat), "%Y")) %>%
    dplyr::group_by( disorder_group, year ) %>%
    dplyr::summarise( distinct_patients = ifelse(length(unique(patient_num)) > obfuscation | isFALSE(obfuscation),
                                                 length(unique(patient_num)), 
                                                 0.5),
                      distinct_observations = ifelse(length(patient_num) > obfuscation | isFALSE(obfuscation),
                                                     length(patient_num), 
                                                     0.5),                           
                      min_date = min( as.Date( calendar_date, format = dateFormat ) ), 
                      max_date = max( as.Date( calendar_date, format = dateFormat ) ))%>% 
    dplyr::arrange(disorder_group, year, distinct_patients )
  print('summary of patients & dates per disorder group and year generated')
  
  
  # save the files for QC
  #save( df5, file = paste0(dir.output, 'bacterialCodesSummary.Rdata')) 
  save( df4, file = paste0(dir.output, 'bacterialCodesSummary_bycode.Rdata')) 
  print(paste0('bacterial infection codes summaries saved in: ', dir.output))
  
}



