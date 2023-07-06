library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(shinycssloaders)
library(dplyr)
library(plotly)
library(lubridate)

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
                                
                                radioButtons("icd1D", "ICD category",
                                             choices = list("A00-B99 Certain infectious and parasitic diseases" = "Certain infectious and parasitic diseases", 
                                                            "C00-D49 Neoplasms" = "Neoplasms",
                                                            "D50-D89 Diseases of the blood" = "Diseases of the blood",
                                                            "E00-E89 Endocrine, nutritional and metabolic diseases" = "Endocrine, nutritional and metabolic diseases",
                                                            "F01-F99 Mental, Behavioral and Neurodevelopmental disorders" = "Mental, Behavioral and Neurodevelopmental disorders",
                                                            "G00-G99 Diseases of the nervous system" = "Diseases of the nervous system",
                                                            "H00-H59 Diseases of the eye and adnexa" = "Diseases of the eye and adnexa",
                                                            "H60-H95 Diseases of the ear and mastoid process" = "Diseases of the ear and mastoid process",
                                                            "I00-I99 Diseases of the circulatory system" = "Diseases of the circulatory system",
                                                            "J00-J99 Diseases of the respiratory system" = "Diseases of the respiratory system",
                                                            "K00-K95 Diseases of the digestive system" = "Diseases of the digestive system",
                                                            "L00-L99 Diseases of the skin and subcutaneous tissue" = "Diseases of the skin and subcutaneous tissue",
                                                            "M00-M99 Diseases of the musculoskeletal system and connective tissue" = "Diseases of the musculoskeletal system and connective tissue",
                                                            "N00-N99 Diseases of the genitourinary system" = "Diseases of the genitourinary system",
                                                            "O00-O9A Pregnancy, childbirth and the puerperium" = "Pregnancy, childbirth and the puerperium",
                                                            "P00-P96 Certain conditions originating in the perinatal period" = "Certain conditions originating in the perinatal period",
                                                            "Q00-Q99 Congenital malformations, deformations and chromosomal abnormalities" = "Congenital malformations, deformations and chromosomal abnormalities",
                                                            "R00-R99 Symptoms, signs" = "Symptoms, signs",
                                                            "S00-T88 Injury, poisoning" = "Injury, poisoning",
                                                            "U00-U85 Codes for special purposes" = "Codes for special purposes",
                                                            "V00-Y99 External causes of morbidity" = "External causes of morbidity",
                                                            "Z00-Z99 Factors influencing health status" = "Factors influencing health status"), 
                                             selected = "Certain infectious and parasitic diseases"),
                                br(), 
    
                            ),
                            mainPanel(DT::dataTableOutput("table")
                            ))
                        )
               ), 
               tabPanel("Counts per month",
                        fluidRow(p(
                            "Here we compare the results across sites for the counts per month according different levels of aggregation"
                        ),
                        br(),
                        sidebarLayout(
                            sidebarPanel(
                                
                                radioButtons("type", label = h3("Count types:"),
                                             choices = list("Total" = "all", "Bacterial" = "bacterial"), 
                                             selected = "all"),
                                
                                br(),
                                
                                
                                radioButtons("aggregation", "Level of aggregation:",
                                             choices = list("age" = "age", "sex" = "sex",
                                                            "bacterial infection" = "bacterialInfection",
                                                            "icu" = "icu", "dead" = "dead",
                                                            "total" = "total"), 
                                             selected = "total"),
                                
                                br(),
                                
                                radioButtons("colorpalette", "Choose a color palette:",
                                            choices = list("magma" = "A","inferno" = "B","plasma" = "C","viridis" = "D" ),
                                            selected = "D")
                                
                            ),
                            mainPanel(
                                tags$head(tags$script('
                        var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
                                tabsetPanel(type = "tabs",
                                            tabPanel("Counts per month: heatmap", plotlyOutput("monthlyCounts_plot"),  width = "100%"), 
                                            tabPanel("Counts per month: barplots per site", plotlyOutput("totalCounts_plot"),  width = "100%"),
                                            tabPanel( "Counts per month: table",DT::dataTableOutput("monthlyCounts_table"))
                                )
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
            select( ICDcode, syndrome  )
        
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
                  #dplyr::filter( n_patients >= 10) %>%
                  dplyr::select( ICDcode, n_patients )
                colnames( int_site_codes)[2] <- paste0( site_id, "_nPatients") 
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
        
        
        ### add the 1 digit code for filtering
        code1d <- read.delim("icd10Codes.txt")
        compareTable <- compareTable %>%
            dplyr::mutate( CODE_1D = substr( ICDcode, 1, 1), 
                    Value = ifelse( CODE_1D %in% c("D","H"), substr( ICDcode, 1, 2 ), CODE_1D)) %>%
            dplyr::left_join( code1d, by="Value") %>%
            dplyr::filter( Description == input$icd1D ) %>%
            dplyr::select( -Value, -Description, -CODE_1D )
        
        compareTable <- compareTable[ order( compareTable$ICDcode, decreasing = FALSE), ]

    }, options = list("pageLength" = 50),  filter = "top", rownames = FALSE))
    
    #### monthly counts QC
    files <- list.files("./QC_counts/")
    
    for( i in 1:length( files )){
        site_id <- files[i]
        load(paste0("./QC_counts/", files[i], "/", "a2Tables.RData"))
        a2_total_all <-a2_total_all %>%
            dplyr::mutate( category_name = "total", 
                           type = "all" )
        a2_age_all <- a2_age_all %>%
            dplyr::mutate( category_name = "age", 
                           type = "all" )
        a2_sex_all <- a2_sex_all %>%
            dplyr::mutate( category_name = "sex", 
                           type = "all" )

        
        a2_total_bacterial <-a2_total_bacterial %>%
            dplyr::mutate( category_name = "total", 
                           type = "bacterial" )
        a2_age_bacterial <- a2_age_bacterial %>%
            dplyr::mutate( category_name = "age", 
                           type = "bacterial" )
        a2_sex_bacterial <- a2_sex_bacterial %>%
            dplyr::mutate( category_name = "sex", 
                           type = "bacterial" )
        a2_bact_infection <- a2_bact_infection %>%
            dplyr::mutate( category_name = "bacterialInfection", 
                           type = "bacterial" ) %>%
            dplyr::rename( category = disorder_group )
        
        ### add dead and ICU grouped by year
        a2_icu_all <- a2_icu_all %>%
            dplyr::mutate( category_name = "icu", 
                           type = "all" )
        a2_icu_bacterial <- a2_icu_bacterial %>%
            dplyr::mutate( category_name = "icu", 
                           type = "bacterial" )
        
        a2_mortality_all <- a2_mortality_all %>%
            dplyr::mutate( category_name = "dead", 
                           type = "all" )
        
        a2_mortality_bacterial <- a2_mortality_bacterial %>%
            dplyr::mutate( category_name = "dead", 
                           type = "bacterial" )
        
        a2_icu_mortality <- rbind( a2_icu_all, a2_icu_bacterial,
                                   a2_mortality_all, a2_mortality_bacterial)
        rm(a2_icu_all, a2_icu_bacterial, a2_mortality_all, a2_mortality_bacterial)
        
        a2_icu_mortality <- a2_icu_mortality %>%
            dplyr::mutate( year_val = year( timeToGroup), 
                           timeToGroup = lubridate::ymd( year_val, truncated = 2L )) %>%
            dplyr::group_by( timeToGroup, category, category_name, type ) %>%
            dplyr::summarise( count = sum(count )) %>%
            dplyr::mutate( category = as.character( category )) %>%
            dplyr::select( timeToGroup, category, count, category_name, type )
        ###

        a2_all <- rbind( a2_age_all, a2_sex_all, a2_total_all, 
                         a2_age_bacterial, a2_sex_bacterial, 
                         a2_total_bacterial, a2_bact_infection, a2_icu_mortality) %>%
            dplyr::mutate( site = site_id )
        
        rm( a2_age_all, a2_sex_all, a2_total_all, 
            a2_age_bacterial, a2_sex_bacterial, 
            a2_total_bacterial, a2_bact_infection, a2_icu_mortality)
        
        if( i ==1 ){
            a2_all_to_work <- a2_all
            rm(a2_all)
        }else{
            a2_all_to_work <- rbind( a2_all_to_work, a2_all)
            rm(a2_all)
        }
        
    }
    #### filter for table
    output$monthlyCounts_table <- DT::renderDataTable(DT::datatable({
        a2_all_to_work <- a2_all_to_work %>%
            dplyr::filter( type == input$type, 
                           category_name == input$aggregation) %>%
            #dplyr::filter( type == "bacterial", category_name == "sex") %>%
            tidyr::pivot_wider(names_from = "site", values_from = "count") %>%
            dplyr::select( - category_name, -type )
        
        if( input$aggregation == "total"){
            a2_all_to_work <- a2_all_to_work %>%
                dplyr::select( - category )
        }else{
            a2_all_to_work <- a2_all_to_work
        }

    }, options = list("pageLength" = 53),  filter = "top", rownames = FALSE))
    
    ### filter for plot
    
    output$monthlyCounts_plot <- renderPlotly({
        
        a2_all_to_work <- a2_all_to_work %>%
            dplyr::filter( type == input$type, 
                           category_name == input$aggregation) %>%
            #dplyr::filter( type == "bacterial", category_name == "icu") %>%
            tidyr::pivot_wider(names_from = "site", values_from = "count") %>%
            dplyr::select( - category_name, -type )
        
        a2_all_to_plot <- a2_all_to_work %>% 
            tidyr::pivot_longer( cols = 3:ncol(a2_all_to_work), 
                                 names_to = "site", values_to = "counts")
        
        
        if( input$aggregation %in% c("icu", "dead")){
            datebreaks <- seq(as.Date("2019-01-01"), as.Date("2023-03-01"), by = "year")
        }else{
            datebreaks <- seq(as.Date("2019-01-01"), as.Date("2023-03-01"), by = "3 month")
        }
        
        if( input$aggregation == "total" ){
            outputHeatmap <- a2_all_to_plot %>%  
                ggplot2::ggplot( ggplot2::aes(x = timeToGroup, y = site,fill= counts)) + 
                ggplot2::geom_tile() +
                ggplot2::scale_x_date(breaks = datebreaks) +
                ggplot2::geom_vline(xintercept = as.Date( "2020-02-01") , linetype = "dashed") +
                ggplot2::geom_vline(xintercept = as.Date( "2021-02-01"), linetype = "dashed") +
                ggplot2::theme(strip.text.x = ggplot2::element_text(size = 10),
                               axis.text.x = ggplot2::element_text(size = 10, angle = 45, hjust = 1), 
                               axis.text.y = ggplot2::element_text(size = 11), 
                               axis.title.x = ggplot2::element_blank(),
                               axis.title.y = ggplot2::element_blank(),
                               title = ggplot2::element_text(size = 12),
                               panel.background = ggplot2::element_blank(),
                               legend.position = "bottom") +
                viridis::scale_fill_viridis(option=input$colorpalette)
            
            plotly::ggplotly( outputHeatmap) %>% plotly::layout(width=1000)
        }else{
            outputHeatmap <- a2_all_to_plot %>%  
                ggplot2::ggplot( ggplot2::aes(x = timeToGroup, y = site,fill= counts)) + 
                ggplot2::facet_wrap( .~ category, ncol = 2 ) +
                ggplot2::geom_tile() +
                ggplot2::scale_x_date(breaks = datebreaks) +
                ggplot2::geom_vline(xintercept = as.Date( "2020-02-01") , linetype = "dashed") +
                ggplot2::geom_vline(xintercept = as.Date( "2021-02-01"), linetype = "dashed") +
                ggplot2::theme(strip.text.x = ggplot2::element_text(size = 10),
                               axis.text.x = ggplot2::element_text(size = 10, angle = 45, hjust = 1), 
                               axis.text.y = ggplot2::element_text(size = 11), 
                               axis.title.x = ggplot2::element_blank(),
                               axis.title.y = ggplot2::element_blank(),
                               title = ggplot2::element_text(size = 12),
                               panel.background = ggplot2::element_blank(),
                               legend.position = "bottom") +
                viridis::scale_fill_viridis(option=input$colorpalette)
            #panel.grid.major = ggplot2::element_blank(), 
                               #panel.grid.minor = ggplot2::element_blank()) 
            plotly::ggplotly( outputHeatmap,
                              width = (0.95*as.numeric(input$dimension[1])), 
                              height = as.numeric(input$dimension[2])) %>% plotly::layout(width=1000)
            
        }
        
        
    })
    
    
   
    ### barplot visualization
    output$totalCounts_plot <- renderPlotly({
        
        a2_all_to_work <- a2_all_to_work %>%
            dplyr::filter( type == input$type, 
                           category_name == input$aggregation) %>%
            #dplyr::filter( type == "bacterial", category_name == "total") %>%
            tidyr::pivot_wider(names_from = "site", values_from = "count") %>%
            dplyr::select( - category_name, -type )
        
        a2_all_to_plot <- a2_all_to_work %>% 
            tidyr::pivot_longer( cols = 3:ncol(a2_all_to_work), 
                                 names_to = "site", values_to = "counts")
        if( input$aggregation == "total" ){
            outputBarplot <- a2_all_to_plot %>%  
                ggplot2::ggplot(ggplot2::aes(x = as.Date(timeToGroup), y= counts )) +
                ggplot2::geom_bar(stat = "identity")+
                ggplot2::geom_vline(xintercept = as.Date( "2020-02-01") , linetype = "dashed") +
                ggplot2::geom_vline(xintercept = as.Date( "2021-02-01"), linetype = "dashed") +
                ggplot2::facet_wrap(. ~ site, scales = "free_y", ncol =  1) +
                ggplot2::theme(strip.text.x = ggplot2::element_text(size = 10),
                               axis.text.x = ggplot2::element_text(size = 10, angle = 45, hjust = 1), 
                               axis.text.y = ggplot2::element_text(size = 11), 
                               axis.title.x = ggplot2::element_blank(),
                               axis.title.y = ggplot2::element_blank(),
                               title = ggplot2::element_text(size = 12),
                               panel.background = ggplot2::element_blank(),
                               legend.position = "none")
            plotly::ggplotly( outputBarplot) %>% plotly::layout(width=1000)
        }else{
            outputBarplot <- a2_all_to_plot %>%  
                ggplot2::ggplot(ggplot2::aes(x = as.Date(timeToGroup), y= counts )) +
                ggplot2::geom_bar(stat = "identity")+
                ggplot2::geom_vline(xintercept = as.Date( "2020-02-01") , linetype = "dashed") +
                ggplot2::geom_vline(xintercept = as.Date( "2021-02-01"), linetype = "dashed") +
                ggplot2::facet_grid( site ~ category, scales = "free_y") +
                ggplot2::theme(strip.text.x = ggplot2::element_text(size = 10),
                               axis.text.x = ggplot2::element_text(size = 10, angle = 45, hjust = 1), 
                               axis.text.y = ggplot2::element_text(size = 11), 
                               axis.title.x = ggplot2::element_blank(),
                               axis.title.y = ggplot2::element_blank(),
                               title = ggplot2::element_text(size = 12),
                               panel.background = ggplot2::element_blank(),
                               legend.position = "none")
            plotly::ggplotly( outputBarplot,
                              width = (0.95*as.numeric(input$dimension[1])), 
                              height = as.numeric(input$dimension[2])) %>% plotly::layout(width=1000)
        }
            
       
        
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
