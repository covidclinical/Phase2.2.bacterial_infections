readInputFilesFread <- function( path, separator = ",", skip = 0, verbose = FALSE, ... ){

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
      cat("All of ", fourcefile_names, " are in the directory")
    }
  } else {
    cat("Following files not found in the file directory:\n")
    cat(fourcefile_names[!check_files])
    cat("\n")
    cat("Please check if the file name and the directory are correct")
    stop()
  }

  #read the files
  if( verbose == TRUE){
    cat( 'Reading \n')
    cat(fourcefile_names)
    cat(" files")
  }

  read_delim_4ce <- function(file_name, ...) data.table::fread(file.path(path, file_name), sep = separator, skip = skip, ...)
  patientSummary <- read_delim_4ce("LocalPatientSummary.csv")
  patientObservations <- read_delim_4ce("LocalPatientObservations.csv")
  patientClinicalCourse <- read_delim_4ce("LocalPatientClinicalCourse.csv")

  if( verbose == TRUE){
    print( paste0( "LocalPatientsummary file contains: ", nrow( patientSummary ), " rows and ", ncol( patientSummary ), " columns."))
    print( paste0( "LocalPatientobservation file contains: ", nrow( patientObservations ), " rows and ", ncol( patientObservations ), " columns."))
    print( paste0( "LocalPatientClinicalCourse file contains: ", nrow( patientClinicalCourse ), " rows and ", ncol( patientClinicalCourse ), " columns."))
  }

  #return it as a list
  files <- list("patientSummary"        = patientSummary,
                "patientObservations"   = patientObservations,
                "patientClinicalCourse" = patientClinicalCourse
  )

  if( verbose == TRUE){
    print( "A list with the three data.frames read is being generated")
  }

  return( files )
}
