### code version
codeVersion <- "1.1.0"

## minimum days hospitalized
min_hosp_days <- 1 

## time period for the analysis
## (possible values: day, week, month )
time_period <-"month"

## cut-off dates
pre_NPI <- as.Date( "2020-02-01") 
full_NPI <- as.Date( "2021-02-01")
partial_NPI <- as.Date("2023-03-01")

## plots start and end dates
start_date_plots <- as.Date("2018-12-01")
end_date_plots <- as.Date("2023-04-01")

## Colorblind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


