#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(readr)
library(viridis)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)
library(viridis)
library(RColorBrewer) 
library(janitor)
cd <- read.csv("base_cd.csv", encoding="UTF-8")

cd_corrupcion <- read.csv("corr_.csv", encoding="UTF-8")
#cd_corrupcion$Entidad <- cd_corrupcion$X.U.FEFF.Entidad

names(cd_corrupcion)[names(cd_corrupcion) == "X.U.FEFF.Entidad"] <- "Entidad"

cd_corrupcion<-  cd_corrupcion %>%
  mutate(Cuatrimestre=factor(Cuatrimestre,
                             levels=c("Ene - Abr 2020","May - Ago 2020", "Sep - Dic 2020",
                                      "Ene - Abr 2021","May - Ago 2021",  "Sep - Dic 2021")))  

# cd <- read_excel("base_cd.xlsx")

cd$Fecha<-as.Date(cd$Fecha.de.Registro,format="%d/%m/%Y %H:%M:%S")
cd$Mes<-format(as.Date(cd$Fecha.de.Registro,format="%d/%m/%Y %H:%M:%S"), "%b/%Y")

cd <-cd %>% 
  filter(Validado..por.Nosotrxs.=="Sí")%>%
  mutate(Cuatrimestre =case_when(
    Fecha >= ymd('2019-01-01') & Fecha <= ymd('2019-04-30') ~ "Ene - Abr 2019",
    Fecha >= ymd('2019-05-01') & Fecha <= ymd('2019-08-31') ~ "May - Ago 2019",
    Fecha >= ymd('2019-09-01') & Fecha <= ymd('2019-12-31') ~ "Sep - Dic 2019",
    
    Fecha >= ymd('2020-01-01') & Fecha <= ymd('2020-04-30') ~ "Ene - Abr 2020",
    Fecha >= ymd('2020-05-01') & Fecha <= ymd('2020-08-31') ~ "May - Ago 2020",
    Fecha >= ymd('2020-09-01') & Fecha <= ymd('2020-12-31') ~ "Sep - Dic 2020",
    
    Fecha >= ymd('2021-01-01') & Fecha <= ymd('2021-04-30') ~ "Ene - Abr 2021",
    Fecha >= ymd('2021-05-01') & Fecha <= ymd('2021-08-31') ~ "May - Ago 2021",
    Fecha >= ymd('2021-09-01') & Fecha <= ymd('2021-12-31') ~ "Sep - Dic 2021",

    Fecha >= ymd('2022-01-01') & Fecha <= ymd('2022-04-30') ~ "Ene - Abr 2022",
  ))



slide2 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 2")


ui <- shinyUI(
  fluidPage(
    #-- Favicon ----
    tags$head(
      tags$link(rel = "shortcut icon", href = "logo.ico"),
      #-- biblio js ----
      tags$link(rel="stylesheet", type = "text/css",
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"),
      tags$link(rel="stylesheet", type = "text/css",
                href = "https://fonts.googleapis.com/css?family=Open+Sans|Source+Sans+Pro")
    ),
    ##-- Logo ----
    list(tags$head(HTML('<link rel="icon", href="logo.png",
                        type="image/png" />'))),
    div(style="padding: 1px 0px; width: '100%'",
        titlePanel(
          title= ""
        )
    ),
    #-- Header ----
    navbarPage("Cero Desabasto",
               
               tabPanel("Total de reportes",
                                     h4("Concentrado del total de reportes que se recaban desde cerodesabasto.org"),
                                     # tags$h2(tags$style("#Titulo{
               #                        color: #3b343a;
               #                        font-size: 24px;
               #                        font-style: italics;
               #                        font-family: Nutmeg;
               #                        }")),  #Con esto intentabamos cambiar la fuente :(
               tabsetPanel(
                 tabPanel(
                   "Mapa de entidades",
                   sidebarLayout(
                     sidebarPanel("Seleccione algunas características",
                                  selectInput(
                                    inputId = "Entidad1",
                                    label = "Entidad1",
                                    choices = unique(sort(cd$Entidad)),
                                    multiple = TRUE,
                                    selected = "Nacional"
                                  ),
                                  selectInput(
                                    inputId = "Cuatrimestre1",
                                    label = "Cuatrimestre1",
                                    choices = sort(unique(cd_corrupcion$Cuatrimestre)),
                                    multiple = TRUE
                                  ),
                                  selectInput("value1", "Seleccione alguna opción" , 
                                              choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
                                              selected = NULL ,  multiple = FALSE, selectize = TRUE)
                                  
                     ),
                     mainPanel(plotlyOutput("grafico1"))
               
    )),
    tabPanel("Gráfico por cuatrimestre")))
    
    ,
    
    ##########################################################################
    tabPanel("Instancias",
             h4("Concentrado del total de reportes que se recaban desde cerodesabasto.org"),
             # tags$h2(tags$style("#Titulo{
             #                        color: #3b343a;
             #                        font-size: 24px;
             #                        font-style: italics;
             #                        font-family: Nutmeg;
             #                        }")),  #Con esto intentabamos cambiar la fuente :(
             tabsetPanel(
               tabPanel(
                 "Grafico por instancia (burbuja)",
                 sidebarLayout(
                   sidebarPanel("Seleccione algunas características",
                                selectInput(
                                  inputId = "semana",
                                  label = "Cuatrimestre",
                                  choices = unique(slide2$Semana),
                                  multiple = TRUE
                                ),
                                selectInput(
                                  inputId = "zona",
                                  label = "Instancia",
                                  choices = sort(unique(slide2$Zona)),
                                  multiple = TRUE
                                ),
                                selectInput("value2", "Seleccione alguna opción" ,
                                            choices = c("Sin etiqueta de datos", "Con etiqueta de datos"),
                                            selected = NULL ,  multiple = FALSE, selectize = TRUE)

                   ),
                   mainPanel(plotOutput(outputId = "displot2", height = 400, width = 850))

                 )),
               tabPanel(
                 "Gráfico de área"))),
    ##########################################################################


    tabPanel("Patologías",
             h4("Concentrado del total de reportes que se recaban desde cerodesabasto.org"),
             # tags$h2(tags$style("#Titulo{
             #                        color: #3b343a;
             #                        font-size: 24px;
             #                        font-style: italics;
             #                        font-family: Nutmeg;
             #                        }")),  #Con esto intentabamos cambiar la fuente :(
             tabsetPanel(
               tabPanel(
                 "Gráfico slope",
                 sidebarLayout(
                   sidebarPanel("Seleccione algunas características",
                                selectInput(
                                  inputId = "semana",
                                  label = "Año",
                                  choices = unique(slide2$Semana),
                                  multiple = TRUE
                                ),
                                selectInput(
                                  inputId = "zona",
                                  label = "Vañor",
                                  choices = sort(unique(slide2$Zona)),
                                  multiple = TRUE
                                ),
                                selectInput("value2", "Seleccione alguna opción" ,
                                            choices = c("Sin etiqueta de datos", "Con etiqueta de datos"),
                                            selected = NULL ,  multiple = FALSE, selectize = TRUE)

                   ),
                   mainPanel(plotOutput(outputId = "displot3", height = 400, width = 850))

                 )),
               tabPanel(
                 "Gráfico área"))),

  ##########################################################################

  tabPanel("Total de reportes",
           h4("Concentrado del total de reportes que se recaban desde cerodesabasto.org"),
           
           tabsetPanel(
             tabPanel(
               "Gráfico",
               sidebarLayout(
                 sidebarPanel("Seleccione algunas características",
                              selectInput(
                                inputId = "Entidad",
                                label = "Entidad",
                                choices = unique(sort(cd_corrupcion$Entidad)),
                                multiple = TRUE,
                                selected = "Nacional"
                              ),
                              selectInput(
                                inputId = "Cuatrimestre",
                                label = "Cuatrimestre",
                                choices = sort(unique(cd_corrupcion$Cuatrimestre)),
                                multiple = TRUE
                              ),
                              selectInput("value3", "Seleccione alguna opción" , 
                                          choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
                                          selected = NULL ,  multiple = FALSE, selectize = TRUE)
                              
                 ),
                 mainPanel(plotlyOutput("grafico3",  height = 400, width = 850)))),
                 tabPanel("Mapa")))
             
                 
               ,
  





    
    ##-- Footer ----
    div(class = "footer",
        includeHTML("html/footer.html"),
        #div(includeHTML("html/google_analytics.html"))
    )
  )))



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
##################################################################################################333333  
  #Aquí inician los graficos para slide 3, con y sin etiqueta de datos.
  
  #Botones para slide 3    
  
  output$Entidad <- renderUI({
    selectInput("Entidad",
                label =  "Seleccione Entidad",
                choices = sort(unique(cd_corrupcion$Entidad)),
                multiple = T)
  })
  
  
  output$Cuatrimestre <- renderUI({
    selectInput("Cuatrimestre",
                label =  "Selecciona el cuatrimestre",
                choices = sort(unique(cd_corrupcion$Cuatrimestre)),
                multiple = T)
  })
  
  
  
  #base reactiva para slide 3
  data_slide3 <- reactive({
    
  
  cd_corrupcion %>%
      filter(
             if(!is.null(input$Cuatrimestre))      Cuatrimestre %in% input$Cuatrimestre       else Cuatrimestre != "",
             if(!is.null(input$Entidad))               Entidad %in% input$Entidad             else Entidad != "",)
  })
  
  
  output$grafico3 <- renderPlotly ({
    
    if (input$value3 == "Sin etiqueta de datos") {
      
      ggplot(data_slide3()) +
        aes(x =Cuatrimestre, y = Total, group=1, color=Entidad, fill=Entidad,
            text = paste("Cuatrimestre: ", Cuatrimestre, 
                         "\nEntidad: ", Entidad,
                         "\nTotal reportes que se creen sí están relacionados a corrupción: ", percent(Total), sep="")) +
        geom_point(size=3) +
        geom_line(size=1) +
        scale_colour_viridis_d(option = "plasma")+
        scale_y_continuous(labels = scales::percent) +
        
        scale_color_brewer(palette = "Dark2")+
        scale_fill_brewer(palette = "Dark2")+
        #scale_fill_viridis_d(option = "plasma", direction = 1)+
        #scale_colour_viridis_d(option = "plasma", direction = 1)+
#        scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) +
        labs(title = "Reportes donde se cree que sí hubo corrupción.", 
             caption = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org.", 
             x="Cuatrimestre", y="Porcentaje") +
        theme_minimal()+
        theme(text=element_text(size=12,  family="Nutmeg"))+
        theme(plot.title = element_text(size = 16L, hjust = 0.5), 
              plot.caption = element_text(size = 12L, hjust = 0))+
        theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=8))->grafico3
      
    }  
    
    if (input$value3 == "Con etiqueta de datos") {
      
        ggplot(data_slide3()) +        
        aes(x =Cuatrimestre, y = Total, group=1, color=Entidad, fill=Entidad,
            text = paste("Cuatrimestre: ", Cuatrimestre, 
                         "\nEntidad: ", Entidad,
                         "\n\nTotal reportes que se creen sí están relacionados a corrupción: ", percent(Total), sep="")) +
        geom_point(size=2) +
        geom_line(size=1) +
        scale_color_brewer(palette = "Dark2")+
        scale_fill_brewer(palette = "Dark2")+
        
#        scale_fill_viridis_d(option = "turbo", direction = -1)+
#       scale_colour_viridis_d(option = "turbo", direction = -1)+
        scale_y_continuous(labels = scales::percent) +
#        scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) +
        geom_text(aes(label=percent(Total)),
                  vjust=.5, hjust=2, size= 4)+
        labs(title = "Reportes donde se cree que sí hubo corrupción.", 
             caption = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org.", 
             x="Cuatrimestre", y="Porcentaje") +
        theme_minimal()+
        theme(text=element_text(size=12,  family="Nutmeg"))+
        theme(plot.title = element_text(size = 16L, hjust = 0.5), 
              plot.caption = element_text(size = 12L, hjust = 0))+
        theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=8))->grafico3
      
      
    }  
    
    ggplotly(grafico3, tooltip="text") %>% 
      layout(margin = list(b=130,t=100), annotations = 
               list(x = .6, y = -0.6, text = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=-10, yshift=-10,
                    font=list(size=10))
      )    
  })  
  
  
  
  
  
  
  ################################################3

  # input_delito <- reactive({
  #   input$entidades
  # })
  # 
  # output$distPlot <- renderPlotly({
  #   base<-cd %>% filter(cd$Entidad==entidades())
  #   
  #   graf<- base %>% 
  #     group_by(Cuatrimestre) %>% 
  #     summarise(Total=n()) %>% 
  #     as_tibble() %>% 
  #     ggplot(aes(x=Cuatrimestre,
  #                y=Total,
  #                group=1))+
  #     geom_line(color="violetred")+
  #     geom_point(color="violetred")
  #   
  #   ggplotly(graf) 
  #   
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
