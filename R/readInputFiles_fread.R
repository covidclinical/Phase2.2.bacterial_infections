checkInputFilesFread <- function( path, separator = ",", skip = 0, verbose = FALSE, ... ){

  #check that the input files needed are in the path
  if( verbose == TRUE){
    print('Checking if the files are located in the directory provided')
  }

  filesInDirectory <- list.files(path = path)
  fourcefile_names <- c(
    "LocalPatientSummary.csv",
    "LocalPatientObservations.csv",
    "LocalPatientClinicalCourse.csv"
  )
  check_files <- fourcefile_names %in% filesInDirectory
  if (all(check_files)) {
    if (verbose) {
      cat("All of ", fourcefile_names, " are in the directory, input file check passed")
    }
    return(TRUE)
  } else {
    cat("Following files not found in the file directory:\n")
    cat(fourcefile_names[!check_files])
    cat("\n")
    cat("Please check if the file name and the directory are correct")
    return(FALSE)
  }

}

  
read_delim_4ce <- function(file_name, path, separator = ",", skip = 0, verbose = FALSE) {
  print(paste0('reading: ', file_name))
  df <- data.table::fread(file.path(path, file_name), sep = separator, skip = skip)
  print( paste0( file_name, " contains: ", nrow( df ), " rows and ", ncol( df ), " columns."))
  return(df)

}
