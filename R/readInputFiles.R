#' Function to read the Phase 2.0 input files with patient-level data
#'
#' Given the \code{path} where the files are located it will generates
#' a \code{list} with the \code{data.frame} object of the different files.
#'
#' @param path A path were the input files are located.
#' @param separator The separator between columns (by default ",").
#' @param skip In case the file does not start with the column names, add the number of lines that should be skipped at the beggining.
#' @param verbose By default \code{FALSE}. Change it to \code{TRUE} to get an on-time log from the function.
#' @return An object of class \code{list} with the \code{data.frames}.
#' @examples
#'
#' dataSet <- readInputFiles(
#'               path      = "./",
#'               separator = ",",
#'               skip      = 1,
#'              )
#' @export readInputFiles

readInputFiles <- function( path, separator = ",", skip = 0, verbose = FALSE, ... ){

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

  read_delim_4ce <- function(file_name, ...) read.delim(file.path(path, file_name), sep = separator, skip = skip, ...)
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
