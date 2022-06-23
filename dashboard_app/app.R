#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# library(tidyverse)
# library(lubridate)
library(shinydashboard)
library(magrittr)



code_pays<-readr::read_delim("data/code_pays.csv")
owid_energy<-readr::read_delim("data/owid-energy-data__OWID.csv")
production_recolte<-readr::read_delim("data/Production_Crops_Livestock_E_All_Data.csv")
wpp2019<-readr::read_delim(
  "data/WPP2019_estimates.csv",
  locale=readr::locale(decimal_mark=","),
  na="...")



owid_energy_clean<-owid_energy %>% 
  dplyr::select(iso_code, country, year, ends_with("consumption")) %>% 
  dplyr::rename_with(~ gsub('_consumption', '', .x))  %>% 
  tidyr::pivot_longer(
    cols=4:16,
    names_to = "type",
    values_to="consommation")

wpp2019_clean<-wpp2019 %>% 
  dplyr::mutate(across("1950":"2020", ~ .x *1000)) %>% 
  tidyr::pivot_longer(
    cols="1950":"2020",
    names_to = "annee",
    values_to="population") %>% 
  dplyr::mutate(annee=as.integer(annee)) %>% 
  dplyr::rename(region="Region, subregion, country or area *")


production_recolte_clean<-production_recolte %>% 
  dplyr::select(-ends_with("F")) %>% 
  dplyr::rename_with(~ gsub('Y', '', .x)) %>% 
  tidyr::pivot_longer(
    cols=8:50,
    names_to = "annee",
    values_to="production"
  ) %>% 
  dplyr::select(-c(8:24)) %>% 
  dplyr::filter(Element=="Production") %>% 
  dplyr::filter(stringr::str_detect(Item, "Total")) 


production_recolte_pays_clean<-dplyr::inner_join(production_recolte_clean,code_pays,by=c("Area"="name"))
owid_energy_pays_clean<-dplyr::full_join(owid_energy_clean,code_pays,by=c("country"="name"))



pays<-wpp2019_clean %>%
  dplyr::filter(Type=="Country/Area") %>% 
  dplyr::distinct(region)

ui <- dashboardPage(
  skin="purple",
  dashboardHeader(title="Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      selectInput(
        inputId = "PAYS",
        label = "Choisir un pays",
        choices = pays,
        selected="France"
      )
    )
  ),
  dashboardBody(
    tabsetPanel(
      type = "tabs",
      
      tabPanel(
        "Visualisation",
        plotOutput("POPULATION"),
        plotOutput("RECOLTE"),
        plotOutput("ENERGY_SOURCE"),
        plotOutput("ENERGY_TYPE")
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$POPULATION<-renderPlot({
    population_mondiale<-wpp2019_clean %>%
      dplyr::filter(region== input$PAYS)
    
    ggplot2::ggplot(population_mondiale) +
      ggplot2::aes(x = annee, y=population)+
      ggplot2::geom_point() + 
      ggplot2::geom_line() +
      ggplot2::labs(title = paste("Evolution de la population: ", input$PAYS))
  })  
  
  output$RECOLTE<-renderPlot({
    production_region_annee<-production_recolte_pays_clean %>% 
      dplyr::filter(Area==input$PAYS) %>% 
      dplyr::group_by(annee) %>% 
      dplyr::summarise(
        somme_production=base::sum(production, na.rm = TRUE),  
        moyenne_production=base::mean(production, na.rm = TRUE),
        ecart_type_production=stats::sd(production, na.rm = TRUE),
        Area
      )
    
    ggplot2::ggplot(
      production_region_annee %>% 
        dplyr::arrange(desc(somme_production)) 
    ) +
      ggplot2::aes(x=annee, y=somme_production)+
      ggplot2::geom_bar(stat = "identity")+
      ggplot2::labs(title = base::paste("Evolution recolte: ", input$PAYS))+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    
  })  
  
  output$ENERGY_SOURCE<-renderPlot({
    owid_energy_source_region<-owid_energy_pays_clean %>% 
      dplyr::filter(country==input$PAYS) %>%
      dplyr::filter(
        type!="primary_energy",
        type!="fossil_fuel",
        type!="low_carbon",
        type!="renewables",
        type!="other_renewable"
      ) %>% 
      dplyr::group_by(year, type, region) %>% 
      dplyr::summarise(
        somme_consommation=base::sum(consommation, na.rm = TRUE),  
        moyenne_consommation=base::mean(consommation, na.rm = TRUE),
        ecart_type_consommation=stats::sd(consommation, na.rm = TRUE)
      ) %>% 
      dplyr::filter(year>=1960)
    
    
    ggplot2::ggplot(owid_energy_source_region)+
      ggplot2::aes(x = year, y= somme_consommation)+
      ggplot2::geom_area()+
      ggplot2::facet_grid(~type, scales = "free_y")+
      ggplot2::labs(title = base::paste("Evolution de la consommation par source d'énergie: ", input$PAYS))
    
  })
  
  
  output$ENERGY_TYPE<-renderPlot({
    
    owid_energy_type_region<-owid_energy_pays_clean %>% 
      dplyr::filter(country==input$PAYS) %>%
      dplyr::filter(
        type=="fossil_fuel"|
          type=="low_carbon"|
          type=="renewables"|
          type=="other_renewable"
      ) %>% 
      dplyr::group_by(year, type, region) %>% 
      dplyr::summarise(
        somme_consommation=base::sum(consommation, na.rm = TRUE),  
        moyenne_consommation=base::mean(consommation, na.rm = TRUE),
        ecart_type_consommation=stats::sd(consommation, na.rm = TRUE)
      ) %>% 
      dplyr::filter(year>=1960)
    
    ggplot2::ggplot(owid_energy_type_region)+
      ggplot2::aes(x = year, y= somme_consommation)+
      ggplot2::geom_area()+
      ggplot2::facet_grid(~type, scales = "free_y")+
      ggplot2::labs(title = base::paste("Evolution de la consommation par type d'énergie: ", input$PAYS))
    
  })
}


shinyApp(ui, server)


