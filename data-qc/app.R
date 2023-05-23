library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(dplyr)

ui <- fluidPage(
    
    theme = shinythemes::shinytheme("cosmo"),
    
    navbarPage("4CE Pediatrics, Bacterial Infections QC. Tool for exploratory purposes",
               tabPanel(icon("house"),
                        
                        fluidRow( column(
                            
                            br(),
                            p("Recently, there have been an unprecedented high level of specific types of invasive bacterial infections in children. Our aim is to assess changes in the hospitalisation rate for common bacterial infections in children before, during and after the COVID-19 pandemic "),
                            br(),
                            
                            p("The data used in this application is part of the 4CE Consortium, pediatric working group."),
                            
                            width=8)),
                        
                        hr(),
                        p(em("Developed by"),br("4CE Pediatrics working group"),style="text-align:center; font-family: times")
               ), 
               
               tabPanel("ICD QC",
                        fluidRow(p(
                                "Coding can be different across sites and countries. To review all the different ICD codes associated to MISC patients, we develop a table to compare the percentages of patients per site and code."
                        ),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                
                                sliderInput("counts",
                                            "Number of patients:",
                                            min = 10,
                                            max = 5000,
                                            value = 100),
                                br(),
                            ),
                            mainPanel(DT::dataTableOutput("table")
                            ))
                        )
               )
    )
)

######################################################################################
# Define server #
######################################################################################

server <- function(input, output) {
    sites <- list.files("./QC_counts/")
    
    #### ICD code QC
    output$table <- DT::renderDataTable(DT::datatable({
        
        ### our codes of interest 
        clinicalCodes <- read.csv("./ICD_bacterial4.csv") %>%
            mutate( ICDcode = gsub("[.]", "", ICD10_Code)) %>%
            select( -ICD10_Code, -bacterial_syndrome )
        
        ### read the ICD files from each site
        files <- list.files("./QC_counts/")

        for( i in 1:length( files )){
            site_id <- files[i]
            load( paste0("./QC_counts/", files[i], "/", files[i], "_ICDdiagnosisCodes.RData"))
            
            if( i ==1 ){
                site_codes <- diag_sum %>%
                    dplyr::mutate( n_patients = ifelse( n_patients == 0.5, 1, n_patients )) %>% #### change obfuscation from 0.5 to 1
                    dplyr::mutate( site = site_id, 
                                   ICDcode = gsub("[.]", "", concept_code)) %>%
                    #dplyr::filter( n_patients >= 10) %>%
                    dplyr::filter( n_patients >= input$counts ) %>% 
                    dplyr::select( ICDcode, n_patients )
                colnames( site_codes)[2] <- paste0( site_id, "_nPatients") 
                rm( diag_sum )
            }else{
                int_site_codes <- diag_sum %>%
                  dplyr::mutate( n_patients = ifelse( n_patients == 0.5, 1, n_patients )) %>% #### change obfuscation from 0.5 to 1
                  dplyr::mutate( site = site_id, 
                                 ICDcode = gsub("[.]", "", concept_code)) %>%
                  dplyr::filter( n_patients >= input$counts ) %>% 
                  dplyr::select( ICDcode, n_patients )
                colnames( site_codes)[2] <- paste0( site_id, "_nPatients") 
                rm( diag_sum )
                site_codes <- full_join( site_codes, int_site_codes )
            }
            
        }
        
        allCodes <- unique( c( clinicalCodes$ICDcode, site_codes$ICDcode))
        
        ### read the description of the codes
        cdc_codes <- read.delim("./icd10cm-codes-2022.txt", header = FALSE) %>%
            dplyr::mutate( V1 = gsub("\\s+", " ", V1), 
                           ICDcode = sapply( strsplit( V1, " "), '[', 1)) %>%
            dplyr::filter( ICDcode %in% allCodes ) %>%
            dplyr::group_by( ICDcode ) %>%
            dplyr::mutate( ICDdescription = paste( sapply( strsplit( V1, " "), '[', -1), collapse = " ")) %>%
            dplyr::select( -V1 ) 
        
        
        #phecode_code <- read.csv("Phecode_map_v1_2_icd10cm_beta.csv") %>%
        phecode_code <- read.csv("./phecode_icd10.csv") %>%
            dplyr::mutate( ICDcode = gsub("[.]", "", ICD10), 
                           ICDdescription = ICD10.String) %>%
            dplyr::filter( ICDcode %in% allCodes ) %>%
            dplyr::select( ICDcode, ICDdescription )
        
        #codes description missing but found in UMLS
        missingCodesPresentInUMLS <- read.delim("missingCodesFoundInUmls.dsv") %>%
            dplyr::mutate( ICDcode = gsub("[.]", "", CODE), 
                           ICDdescription = STR) %>%
            dplyr::filter( ICDcode %in% allCodes ) %>%
            dplyr::select( ICDcode, ICDdescription )
        
        #codes descriptions manually added
        manualCodesDescriptionAdded <- read.delim("manuallyAddedCodeDescription.txt") %>%
            dplyr::mutate( ICDcode = gsub("[.]", "", CODE), 
                           ICDdescription = DESCRIPTION) %>%
            dplyr::filter( ICDcode %in% allCodes ) %>%
            dplyr::select( ICDcode, ICDdescription )
        
        totalCode <- rbind( cdc_codes, phecode_code, missingCodesPresentInUMLS, manualCodesDescriptionAdded ) %>%
            unique()
        totalCode <- dplyr::full_join( totalCode, clinicalCodes )
        
        
        ### merge all the tables in one
        compareTable <- dplyr::full_join( totalCode, site_codes ) 
        compareTable <- compareTable[ order( compareTable$ICDcode, decreasing = FALSE), ]

    }, options = list("pageLength" = 50),  filter = "top", rownames = FALSE))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
