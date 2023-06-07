assign_season <- function(date) {
  month <- lubridate::month(date)
  
  if ( month %in% c(12, 1, 2)) {
    season <- "Winter"
  } else if (month  %in% c(3, 4, 5)) {
    season <- "Spring"
  } else if (month  %in% c(6, 7, 8)) {
    season <- "Summer"
  } else if( month  %in% c(9, 10, 11)) {
    season <- "Autumn"
  }
  
  return(season)
}