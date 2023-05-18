# Install packages
paket <- function(pak){
  new_pak <- pak[!(pak %in% rownames(installed.packages()))]
  if (length(new_pak)) 
    install.packages(new_pak, dependencies = TRUE,repos="https://cloud.r-project.org/")
  sapply(pak, library, character.only = TRUE)
}

listOfPackages <- c("tidyverse", "RColorBrewer", "knitr", "kableExtra", "tsModel", "gridExtra", "dplyr", "lubridate")
paket(listOfPackages)