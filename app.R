Skip to content
Search or jump to…
Pull requests
Issues
Marketplace
Explore
 
@nancymanzo 
nancymanzo
/
Codigo_Violeta_EANT
Public
Code
Issues
Pull requests
Actions
Projects
Wiki
Security
Insights
Settings
Codigo_Violeta_EANT/app.R
@nancymanzo
nancymanzo Código de comandos en R
Latest commit 146cf16 on 19 Jul 2021
 History
 1 contributor
2782 lines (2590 sloc)  123 KB
   
#Código 
options(encoding = 'UTF-8') #con esta línea nos deja agregar las "ñ" y acentos en la red con shiny.

#Paqueterías
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(leaflet)
library(tidyverse)
library(DT)
library(leaflet)
library(leaflet.extras)
library(readxl)
library(data.table)
library(shinythemes)
library(tidyr)
library(plotly)
library(sf) 
library(mxmaps)
library(extrafont)
library(extrafontdb)
library(showtext)
library(ggrepel)
library(forcats)
library(devtools)
library(shinybusy)
library(ggnewscale)
library(reshape)
library(rebus)
library(rgdal)
library(graphics)
library(utf8)


#Cargamos el excel que se usa para actualizar el codigo violeta en las ppt.

slide2 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 2")
slide3 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 3")
slide4 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 4")
slide5 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 5")
slide6 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 6")
slide7 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 7")
slide8 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 8")
slide9 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 9")
slide10 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 10")
slide11 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 11")
slide12 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 12")
slide13 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 13")
slide15 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 15")
slide18 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 18")
slide19 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 19")
slide20 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 20")
slide21 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 21")
slide22 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 22")

victimas_nacional <- read_excel("CodigoVioleta_bases.xlsx", sheet = "victimas_nacional")



##################
#Mapas hexagonales con mxmaps

data(df_mxstate_2020) #Base de una paquetería que contiene la información del Censo Nacional y que es necesaria para ponderar feminicidios por poblacion

slide18<-merge(df_mxstate_2020, slide18,
               by.y="Clave de entidad",
               by.x="region")


#Mapa hexagonal
mxmaps::mxhexbin_choropleth(slide18, num_colors = 1) +  
  labs(title="Muertes violentas de mujeres tipificados como feminicidios.",
       caption="Fuente: Elaboración propia con datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP). Se retoma la cifra de enero para respetar el ranking del SESNSP.",
       x="", y="",
       fill="Total") +
  scale_fill_gradient(
    low = "plum", 
    high = "magenta4",
    guide = "colourbar")+theme_minimal()+
  theme(text=element_text(family="Roboto",
                          face="plain",
                          size=23,
                          hjust = 0.5,
                          vjust = 0.5),
        legend.text = element_text(size=10, colour = "grey12"),
        legend.title = element_text(color = "grey7", size = 13),
        plot.title = element_text(hjust=0.5, size=15, face="plain", colour="grey6"),
        plot.caption = element_text(hjust=-1, size=12, face="plain"),
        plot.margin = margin(1, 1, 1, 1, "cm"))->grafico18



#Merge para juntar las bases de delitos con la de población
slide19<-merge(df_mxstate_2020, slide19,
               by.y="Clave de entidad",
               by.x="region")

#Mapa hexagonal
mxhexbin_choropleth(slide19, num_colors = 1) +  
  labs(title="Muertes violentas de mujeres tipificados como feminicidios.",
       caption="Fuente: Elaboración propia con datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP). Se retoma la cifra de enero para respetar el ranking del SESNSP.",
       x="", y="",
       fill="Tasa") +
  scale_fill_gradient(
    low = "plum", 
    high = "magenta4",
    guide = "colourbar")+theme_minimal()+
  theme(text=element_text(family="Roboto",
                          face="plain",
                          size=23,
                          hjust = 0.5,
                          vjust = 0.5),
        legend.text = element_text(size=10, colour = "grey12"),
        legend.title = element_text(color = "grey7", size = 13),
        plot.title = element_text(hjust=0.5, size=15, face="plain", colour="grey6"),
        plot.caption = element_text(hjust=-1, size=12, face="plain"),
        plot.margin = margin(1, 1, 1, 1, "cm"))->grafico19



#Aquí inicia lo que es la conceptualización de los delitos por regiones de Jalisco (esto no se visualiza en la shiny), es para un mañana.


# jalisco <- read.csv("jalisco.csv", encoding="UTF-8")
# 
# 
# #Regiones
# `Región Norte`<- c("Bolaños", "Chimaltitán", "Colotlán", "Huejúcar", "Huejuquilla el Alto",
#                    "Mezquitic", "San Martín de Bolaños", "Santa María de los Ángeles", "Totatiche", "Villa Guerrero")
# 
# `Región Altos Norte`<- c("Encarnación de Díaz", "Lagos de Moreno", "Ojuelos de Jalisco",
#                          "San Diego de Alejandría", "San Juan de los Lagos", "Teocaltiche",
#                          "Unión de San Antonio", "Villa Hidalgo")
# 
# `Región Altos Sur`<- c("Acatic", "Arandas", "Cañadas de Obregón", "Jalostotitlán", "Jesús María",
#                        "Mexticacán", "San Ignacio Cerro Gordo", "San Julián", "San Miguel el Alto",
#                        "Tepatitlán de Morelos", "Valle de Guadalupe", "Yahualica de González Gallo")
# 
# `Región Ciénega`<- c("Atotonilco el Alto", "Ayotlán", "Degollado", "Jamay", "La Barca", "Ocotlán",
#                      "Poncitlán", "Tototlán", "Zapotlán del Rey")
# 
# 
# `Región Sureste`<- c("Chapala", "Concepción de Buenos Aires", "Jocotepec", "La Manzanilla de la Paz",
#                      "Mazamitla", "Quitupan", "Santa María del Oro", "Tizapán el Alto",
#                      "Tuxcueca","Valle de Juárez")
# 
# 
# `Región Sur`<- c("Gómez Farías", "Jilotlán de los Dolores", "Pihuamo", "San Gabriel",
#                  "Tamazula de Gordiano", "Tecalitlán", "Tolimán", "Tonila", "Tuxpan",
#                  "Zapotiltic", "Zapotitlán de Vadillo", "Zapotlán el Grande")
# 
# `Región Sierra de Amula`<- c("Atengo", "Autlán de Navarro", "Ayutla", "Chiquilistlán",
#                              "Cuautla", "Ejutla", "El Grullo", "El Limón", "Juchitlán",
#                              "Tecolotlán", "Tenamaxtlán", "Tonaya", "Tuxcacuesco", "Unión de Tula")
# 
# 
# `Región Costa Sur`<- c("Casimiro Castillo", "Cihuatlán", "Cuautitlán de García Barragán", 
#                        "La Huerta", "Tomatlán", "Villa Purificación")
# 
# 
# 
# `Región Costa-Sierra Occidental`<- c("Atenguillo", "Cabo Corrientes", "Guachinango",
#                                      "Mascota", "Mixtlán", "Puerto Vallarta",
#                                      "San Sebastián del Oeste", "Talpa de Allende")
# 
# 
# 
# `Región Valles`<- c("Ahualulco de Mercado", "Amatitán", "Ameca", "El Arenal",
#                     "Etzatlán", "Hostotipaquillo", "Magdalena", 
#                     "San Juanito de Escobedo", "San Marcos", "Tala",
#                     "Tequila", "Teuchitlán")
# 
# 
# `Región Lagunas`<- c("Acatlán de Juárez", "Amacueca", "Atemajac de Brizuela",
#                      "Atoyac", "Cocula", "San Martín Hidalgo", "Sayula",
#                      "Tapalpa", "Techaluta de Montenegro", "Teocuitatlán de Corona",
#                      "Villa Corona", "Zacoalco de Torres")
# 
# 
# `Región Centro`<- c("Cuquío", "El Salto", "Guadalajara", "Ixtlahuacán de los Membrillos",
#                     "Ixtlahuacán del Río", "Juanacatlán", "San Cristóbal de la Barranca",
#                     "San Pedro Tlaquepaque", "Tlajomulco de Zúñiga", "Tonalá", "Zapopan", 
#                     "Zapotlanejo")
# 
# 
# Suma de los delitos por regiones:
# regiones<- jalisco %>% 
#   mutate(Region = case_when(
#     Municipio %in% `Región Norte` ~ "Región Norte",
#     Municipio %in% `Región Altos Norte` ~ "Región Altos Norte",
#     Municipio %in% `Región Altos Sur` ~ "Región Altos Sur",
#     Municipio %in% `Región Ciénega` ~ "Región Ciénega",
#     Municipio %in% `Región Sureste` ~ "Región Sureste",
#     Municipio %in% `Región Sur` ~ "Región Sur",
#     Municipio %in% `Región Sierra de Amula` ~ "Región Sierra de Amula",
#     Municipio %in% `Región Costa Sur` ~ "Región Costa Sur",
#     Municipio %in% `Región Costa-Sierra Occidental` ~ "Región Costa-Sierra Occidental",
#     Municipio %in% `Región Valles` ~ "Región Valles",
#     Municipio %in% `Región Lagunas` ~ "Región Lagunas",
#     Municipio %in% `Región Centro` ~ "Región Centro",
#     TRUE ~ "Sin especificar"
#   ))
# 
# 
# regiones<- regiones %>% 
#   group_by(Año, Region, Municipio, Tipo.de.delito) %>% 
#   summarise(ene=sum(Enero, na.rm = T),
#             feb=sum(Febrero, na.rm = T),
#             mar=sum(Marzo, na.rm = T),
#             abr=sum(Abril, na.rm = T),
#             may=sum(Mayo, na.rm = T),
#             jun=sum(Junio, na.rm = T),
#             jul=sum(Julio, na.rm = T),
#             ago=sum(Agosto, na.rm = T),
#             sep=sum(Septiembre, na.rm = T),
#             oct=sum(Octubre, na.rm = T),
#             nov=sum(Noviembre, na.rm = T),
#             dic=sum(Diciembre, na.rm = T),
#             tot_acu=sum(ene+ feb+ mar+ abr+ 
#                         may+ jun + jul+ ago+
#                         sep+ oct+ nov+ dic))
# 
# write.csv(regiones, "regiones.csv")
# 
#
# La visualización de los años 2015 a 2021 de los delitos por regiones.
# ggplot(regiones)+
#   aes(x = as.factor(Año) , y = tot_acu, fill = Region) +
#   geom_col() +
#   scale_fill_manual(values = c(
#     `Región Altos Norte`= "#562877",
#     `Región Altos Sur`= "#67224C",
#     `Región Centro`= "#C91682",
#     `Región Ciénega`= "#9540BF",
#     `Región Costa-Sierra Occidental`= "#8436A1",
#     `Región Costa Sur`= "#A136A1",
#     `Región Lagunas`= "#923CB4",
#     `Región Norte`= "#A136A1",
#     `Región Sierra de Amula`= "#C44FA5",
#     `Región Sur`= "#AD57C7",
#     `Región Sureste`= "#CD6AB1",
#     `Región Valles`= "#AC66CC",
#     `Sin especificar`= "#B56ECF"))+
#   theme_minimal()
# 
# library(viridis)
# 
#
# Calis de mapa con algunos formatos de colores
# ggplot(regiones)+
#   aes(x = as.factor(Año) , y = tot_acu, fill = Region) +
#   geom_col() +
#   scale_y_continuous(labels = comma)+
#   new_scale_fill() +
#   geom_col(aes(x = as.factor(Año) , y = tot_acu, fill = Region)) +
#   scale_fill_manual(values = c(
#     `Región Altos Norte`= "#92278F",
#     `Región Altos Sur`= "#9B57D3",
#     `Región Centro`= "#755DD9",
#     `Región Ciénega`= "#45A5ED",
#     `Región Costa-Sierra Occidental`= "#5982DB",
#     `Región Costa Sur`= "#9969d3",
#     `Región Lagunas`= "#581756",
#     `Región Norte`= "#A136A1",
#     `Región Sierra de Amula`= "#3A2397",
#     `Región Sur`= "#393374",
#     `Región Sureste`= "#1067A7",
#     `Región Valles`= "#214698",
#     `Sin especificar`= "#B56ECF"))+
#   new_scale_fill() +
#   geom_col(aes(x = as.factor(Año) , y = tot_acu, fill = Municipio)) +
#   scale_fill_viridis_d(option = "viridis") +  
#   theme_minimal()+
#   theme(legend.position = "none")
# 
# regiones_base<- regiones %>% 
#   group_by(Año, Region, Tipo.de.delito) %>% 
#   summarise(Total= sum(tot_acu))
# 
# fil_delitos<- c("Homicidio, Feminicidio")
# 
# regiones_base %>% 
#   filter(Tipo.de.delito %in% "fil_delitos") %>% 
# ggplot(aes(x=Año, y=Total, colour=Tipo.de.delito))+
#   geom_line(size=0.8)+
#   scale_color_viridis_d(option = "viridis", direction = 1) +
#   new_scale_fill() +
#   geom_line(aes(x=Año, y=Total, colour=Region)) +
#   theme_minimal()
# 
# 
# 
# 
# 
# regiones_base %>%
#   ggplot() +
#   aes(x = Año, y = Total, colour = Tipo.de.delito) +
#   geom_line() +
#   geom_point(aes(x=Año, y=Total, colour=Region))+
#   scale_color_viridis_d(option = "plasma", direction = 1) +
#   new_scale_colour() +
#   geom_line(aes(x=Año, y=Total, colour=Region)) +
#   #geom_point(aes(x=Año, y=Total, colour=Region))+
#   scale_color_viridis_d(option = "plasma", direction = 1) +
#   theme_minimal()
# 
# 
# 
# 
# 
# library(RColorBrewer)
# 
# 
# ggplot(regiones)+
#   aes(x = as.factor(Año) , y = tot_acu, fill = Region) +
#   geom_col() +
#   scale_fill_manual(values = colorRampPalette(brewer.pal(13, "Set2"))(colourCount)) +
#   scale_y_continuous(labels = scales::comma)+
#   theme_minimal()+labs(title = "Carpetas de investigación por tipo de delitos.",
#                        caption = "Fuente: elaboración propia con datos abiertos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP).",
#                        x="", y="", fill="Regiones") +
#   theme_minimal(base_size=15)+
#   theme(text=element_text(size=12,  family="Nutmeg"))+
#   theme(plot.title = element_text(size = 16L, hjust = 0.5), plot.caption = element_text(size = 12L, hjust = 0))
# 
# 
# 
# Aquí acaba lo de regiones.


################################################################################################################

# Inicio de UI

ui <- navbarPage("Datos abiertos",
                 theme = "mytheme.css",
                 navbarMenu("Código Violeta",
                            tabPanel("Violencia familiar",
                                     h4("La plataforma de Datos Abiertos de la Secretaría de Igualdad Sustantiva Entre 
                                        Mujeres y Hombres (SISEMH) te permite acceder, explorar, analizar, vizualizar y 
                                        descargar bases de datos de violencia de género de diferentes ámbitos."),
                                     # tags$h2(tags$style("#Titulo{
                                     #                        color: #3b343a;
                                     #                        font-size: 24px;
                                     #                        font-style: italics;
                                     #                        font-family: Nutmeg;
                                     #                        }")),  #Con esto intentabamos cambiar la fuente :(
                                     tabsetPanel(
                                       tabPanel(
                                         "Violencia familiar semanal por zonas del AMG",
                                         sidebarLayout(
                                           sidebarPanel("Seleccione algunas características",
                                                        selectInput(
                                                          inputId = "semana",
                                                          label = "Semana",
                                                          choices = unique(slide2$Semana),
                                                          multiple = TRUE
                                                        ),
                                                        selectInput(
                                                          inputId = "zona",
                                                          label = "Zona",
                                                          choices = sort(unique(slide2$Zona)),
                                                          multiple = TRUE
                                                        ),
                                                        selectInput("value2", "Seleccione alguna opción" , 
                                                                    choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
                                                                    selected = NULL ,  multiple = FALSE, selectize = TRUE)
                                                        
                                           ),
                                           mainPanel(plotlyOutput(outputId = "grafico2", height = 400, width = 850)))),
                                       # tags$head(
                                       #   tags$style(type='text/css',
                                       #              ".nav-tabs {font-size: 20px} ")),
                                       
                                       
                                       tabPanel("Denuncias de violencia familiar por sexo",
                                                sidebarLayout(
                                                  sidebarPanel("Seleccione algunas características",
                                                               selectInput(
                                                                 inputId = "semana_3",
                                                                 label = "Semana",
                                                                 choices = unique(slide3$Semana),
                                                                 multiple = TRUE
                                                               ),
                                                               selectInput(
                                                                 inputId = "sexo_3",
                                                                 label = "Sexo",
                                                                 choices = sort(unique(slide3$Sexo)),
                                                                 multiple = TRUE
                                                               ), 
                                                               selectInput("value3", "Seleccione alguna opción" , 
                                                                           choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
                                                                           selected = NULL ,  multiple = FALSE, selectize = TRUE)),
                                                  mainPanel(plotlyOutput(outputId = "grafico3", height = 400, width = 850)))),
                                       # tags$head(
                                       #   tags$style(type='text/css',
                                       #              ".nav-tabs {font-size: 20px} ")),
                                       
                                       
                                       tabPanel("Total anual",
                                                sidebarLayout(
                                                  sidebarPanel("Seleccione alguna característica",
                                                               selectInput(
                                                                 inputId = "anio_4",
                                                                 label = "Año",
                                                                 choices = sort(unique(slide4$Anio)),
                                                                 multiple = TRUE
                                                               ),
                                                               selectInput("value4", "Seleccione alguna opción" , 
                                                                           choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
                                                                           selected = NULL ,  multiple = FALSE, selectize = TRUE)),
                                                  mainPanel(plotlyOutput(outputId = "grafico4", height = 400, width = 850)))),
                                       # tags$head(
                                       #   tags$style(type='text/css',
                                       #              ".nav-tabs {font-size: 20px} ")),))),
                                       
                                       tabPanel("Total mensual",
                                                sidebarLayout(
                                                  sidebarPanel("Seleccione alguna característica",
                                                               selectInput(
                                                                 inputId = "anio_5",
                                                                 label = "Año",
                                                                 choices = sort(unique(slide5$Anio)),
                                                                 multiple = TRUE
                                                               ),
                                                               # selectInput(
                                                               #   inputId = "mes_5",
                                                               #   label = "Mes",
                                                               #   choices = unique(slide5$Mes),
                                                               #   multiple = TRUE
                                                               # ),
                                                               selectInput("value5", "Selecciona un tipo de gráfico" , 
                                                                           choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
                                                                           selected = NULL ,  multiple = FALSE, selectize = TRUE)
                                                               
                                                  ),
                                                  mainPanel(plotlyOutput(outputId = "grafico5", height = 400, width = 850))))
                                     )), 
                            
                            
                            
                            tabPanel("Delitos tipificados como femininicidios",
                                     h4("La plataforma de Datos Abiertos de la Secretaría de Igualdad Sustantiva Entre 
                                        Mujeres y Hombres (SISEMH) te permite acceder, explorar, analizar, vizualizar y 
                                        descargar bases de datos de violencia de género de diferentes ámbitos."),
                                     # tags$h2(tags$style("#Titulo{
                                     #                        color: #3b343a;
                                     #                        font-size: 24px;
                                     #                        font-style: italics;
                                     #                        font-family: Nutmeg;
                                     #                        }")),
                                     tabsetPanel(
                                       tabPanel("Total de feminicidios",
                                                sidebarLayout(
                                                  sidebarPanel("Seleccione algunas características",
                                                               
                                                               # tabPanel("Violencia contra la mujer",
                                                               # h4("Sitio en construcción - violencia vs la mujer", alig = "justify",
                                                               #    tabPanel("Visualización de los datos",
                                                               #      sidebarLayout(
                                                               #        sidebarPanel("Seleccione algunas características",
                                                               selectInput(
                                                                 inputId = "año_vic1", 
                                                                 label = "Año", 
                                                                 choices = sort(unique(victimas_nacional$año)),
                                                                 multiple = TRUE
                                                               ),
                                                               selectInput(
                                                                 inputId = "tipo.de.delito_vic1", 
                                                                 label = "Tipo de delito", 
                                                                 choices = sort(unique(victimas_nacional$tipo.de.delito)),
                                                                 multiple = TRUE
                                                               )),
                                                  
                                                  mainPanel(plotlyOutput(outputId = "grafico_vic1", height = 400, width = 850)))),
                                       # tags$head(
                                       #   tags$style(type='text/css', 
                                       #              ".nav-tabs {font-size: 20px} ")),
                                       
                                       
                                       
                                       tabPanel("Feminicidios por edad",
                                                plotlyOutput(outputId = "grafico_vic2", height = 600, width = 850)),
                                       
                                       tabPanel('Georreferenciación',
                                                navlistPanel(
                                                  tabPanel("México",
                                                           h3("México"),
                                                           plotlyOutput(outputId = "slide19", height = 500, width = 700))))
                                     ))
                 ),
                 
                 
                 
                 tabPanel("Documentación", icon = icon("far fa-file-alt"),
                          h3(align= "justify", "Documentación"),
                          h4(align= "justify", "La plataforma de datos abierto tiene la finalidad de mostrar el
                             comportamiento del fenómeno de la violencia de género contra las mujeres, adolescentes
                             y niñas que habitan y transitan el estado de Jalisco."),
                          h4(align= "justify", "Se busca brindar una herramienta que facilite la Participación, 
                             Transparencia y Colaboración que permita promover la generación de información de valor."),
                          h3(align= "justify", "Sobre las fuentes de información"),
                          h4(align= "justify", "La informacion presentada aquí se alimenta los datos de Fiscalía del estado de Jalisco."),
                          h4(align= "justify", "En el apartado de violencia de género integra la información de carpetas 
                             de investigación del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)."),
                          h4(includeHTML("reportes.html"))
                          ),
                      
                 
                 
                 
                 tabPanel("Descarga",icon = icon("fas fa-cloud-download-alt"),
                          downloadButton('downloadData', 'Download'),
                          dataTableOutput(outputId = "table1"))                 
)










################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################


# Aquí inicia el server
server <- function(input, output, session){
  
 #  Para visualizar base de datos en DT
  output$table1 <- DT::renderDataTable({
    slide2
  }, filter='top', 
  options = list(pageLength = 10, scrollX=TRUE, autoWidth = TRUE))
  
  #Botones de descargar 
  output$downloadData <- downloadHandler(
    filename = 'Download.csv',
    content = function(file) {
      write.csv(Data[input[["table1_rows_all"]],], file, row.names = FALSE)
    }
  )


  
 # Aquí inicia el cuadro de bienvenida
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "intro", label = "Iniciar", icon = icon("fas fa-home"))
      )
    ))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  
 ##Aquí inician los botones para los reactives de la shiny
  
  #Botones para slide 2    
  output$semana <- renderUI({
    selectInput("Semana",
                label =  "Seleccione semana",
                choices = sort(unique(slide2$Semana)),
                multiple = T)
  })
  
  
  output$zona <- renderUI({
    selectInput("Zona",
                label =  "Selecciona la zona",
                choices = sort(unique(slide2$Zona)),
                multiple = T)
  })
  
  
  #base reactiva para slide 2
  data_slide2 <- reactive({
    
    slide2 %>%
      filter(if(!is.null(input$semana))         Semana %in% input$semana     else Semana != "",
             if(!is.null(input$zona))             Zona %in% input$zona       else Zona != "",)
  })
  
  
  #Aquí inician los graficos para slide 2, con y sin etiqueta de datos.
  output$grafico2 <- renderPlotly ({
    
    if (input$value2 == "Sin etiqueta de datos") {
      
      
      ggplot(data_slide2()) +
        aes(x = fct_inorder(Periodo) , y = Total, 
            colour = Zona, group=Zona,
            text = paste("Semana: ", Semana, 
                         "\nÁrea: ", Zona,
                         "\nTotal de denuncias registradas: ", Total, sep="")) +
        geom_line(size = .8) +
        geom_point()+
        scale_y_continuous(labels = scales::comma) +
        scale_color_manual(values = c(
          AMG = "#7E3794",
          Interior = "#C91682",
          `Puerto Vallarta` = "#D98CBC")) +
        labs(title = "Denuncias por violencia familiar",
             caption = "Fuente: Elaboración propia con datos de la Fiscalía del estado de Jalisco.
Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración.",
             x="Semana", y="Total de denuncias", fill="Zona") +
        theme_minimal(base_size=15)+
        theme(text=element_text(size=12,  family="Nutmeg"))+
        theme(plot.title = element_text(size = 16L, hjust = 0.5), 
              plot.caption = element_text(size = 12L, hjust = 0))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))->grafico2
      
    }
    
    
    
    if (input$value2 == "Con etiqueta de datos") {
      
      ggplot(data_slide2()) +
        aes(x = fct_inorder(Periodo), y = Total, colour = Zona, group=Zona,
            text = paste("Semana: ", Periodo, 
                         "\nÁrea: ", Zona,
                         "\nTotal de denuncias registradas: ", Total, sep="")) +
        geom_line(size = .8) +
        geom_point()+ 
        scale_y_continuous(labels = scales::comma) +
        scale_color_manual(values = c(
          AMG = "#7E3794",
          Interior = "#C91682",
          `Puerto Vallarta` = "#D98CBC")) +
        geom_text(aes(label=Total, colour=Zona),
                  vjust=1, hjust=1, size= 4, angle=90)+
        #geom_text_repel()+
        labs(title = "Denuncias por violencia familiar",
             caption = "Fuente: Elaboración propia con datos de la Fiscalía del estado de Jalisco.
Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración.",
             x="Semana", y="Total de denuncias", fill="Zona") +
        theme_minimal(base_size=15)+
        theme(text=element_text(size=12,  family="Nutmeg"))+
        theme(plot.title = element_text(size = 16L, hjust = 0.5), 
              plot.caption = element_text(size = 12L, hjust = 0))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))->grafico2
      
      
    }
    
    ggplotly(grafico2, tooltip = "text")
    
  })
  
  
  
  #Botones para slide 3    
  output$semana_3 <- renderUI({
    selectInput("Semana",
                label =  "Seleccione semana",
                choices = sort(unique(slide3$Semana)),
                multiple = T)
  })
  
  
  output$sexo_3 <- renderUI({
    selectInput("Sexo",
                label =  "Selecciona el sexo",
                choices = sort(unique(slide3$Sexo)),
                multiple = T)
  })
  
  
  
  #base reactiva para slide 3
  data_slide3 <- reactive({
    
    slide3 %>%
      filter(if(!is.null(input$semana_3))         Semana %in% input$semana_3     else Semana != "",
             if(!is.null(input$sexo_3))             Sexo %in% input$sexo_3       else Sexo != "",)
  })

             
#Aquí inician los graficos para slide 3, con y sin etiqueta de datos.
            
  output$grafico3 <- renderPlotly ({
    
    if (input$value3 == "Sin etiqueta de datos") {
      ggplot(data_slide3()) +
        aes(x =fct_inorder(Periodo), y = Total, colour = Sexo, group=Sexo,
            text = paste("Semana: ", Periodo, 
                         "\nSexo: ", Sexo,
                         "\nTotal de denuncias registradas: ", Total, sep="")) +
        geom_point() +
        geom_line() +
        scale_y_continuous(labels = scales::comma) +
        scale_color_manual(
          values = c(
            Hombres = "#C91682",
            Mujeres = "#7E3794")) +
        labs(title = "Denuncias por violencia familiar desagregado.", 
             caption = "Fuente: Elaboración propia con datos de la Fiscalía del estado de Jalisco. 
Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración.", 
             fill = "Sexo:",
             x="Fecha", y="Total") +
        theme_minimal()+
        theme(text=element_text(size=12,  family="Nutmeg"))+
        theme(plot.title = element_text(size = 16L, hjust = 0.5), 
              plot.caption = element_text(size = 12L, hjust = 0))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))->grafico3
      
    }  
    
    if (input$value3 == "Con etiqueta de datos") {
      ggplot(data_slide3()) +
        aes(x =fct_inorder(Periodo), y = Total, colour = Sexo, group=Sexo,
            text = paste("Semana: ", Periodo, 
                         "\nSexo: ", Sexo,
                         "\nTotal de denuncias registradas: ", Total, sep="")) +
        geom_point() +
        geom_line() +
        scale_y_continuous(labels = scales::comma) +
        scale_color_manual(
          values = c(
            Hombres = "#C91682",
            Mujeres = "#7E3794")) +
        geom_text(aes(label=Total, colour=Sexo),
                  vjust=1, hjust=1, size= 4)+
        labs(title = "Denuncias por violencia familiar desagregado.", 
             caption = "Fuente: Elaboración propia con datos de la Fiscalía del estado de Jalisco. 
Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración.", 
             fill = "Sexo:",
             x="Fecha", y="Total") +
        theme_minimal()+
        theme(text=element_text(size=12,  family="Nutmeg"))+
        theme(plot.title = element_text(size = 16L, hjust = 0.5), 
              plot.caption = element_text(size = 12L, hjust = 0))+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))->grafico3
      
      
    }  
    
    ggplotly(grafico3, tooltip="text") 
    
  })
  
  
  
  #Botones para slide 4  
  
  output$anio_4 <- renderUI({
    selectInput("Anio",
                label =  "Seleccione los anios",
                choices = sort(unique(slide4$Anio)),
                multiple = T)
  })
  
  #base reactiva para slide 4
  data_slide4 <- reactive({
    
    slide4 %>%
      filter(if(!is.null(input$anio_4))         Anio %in% input$anio_4     else Anio != "")
  })
  
#Aquí inician los graficos para slide 4, con y sin etiqueta de datos.

  
  output$grafico4 <- renderPlotly ({
    
    if (input$value4 == "Sin etiqueta de datos") {
      
      ggplot(data_slide4()) +
        aes(x =as.factor(Anio), weight = Total,
            text = paste("Año: ", Anio, 
                         "\nTotal: ", scales::comma(Total,1), sep="")) +
        geom_bar(fill = "#AF1271") +
        scale_y_continuous(labels = scales::comma) +
        labs(title = "Denuncias por violencia familiar (anual)", 
             caption = "Datos al 31 diciembre de 2020.  
Fuente: Elaboración propia con datos de la Fiscalía del estado de Jalisco.", 
             fill = "Tipo de violencia:",
             x="Año", y="Total") +
        theme_minimal(base_size=25)+
        theme(plot.title = element_text(size = 16L, hjust = 0.5), plot.caption = element_text(size = 12L, hjust = 0))+
        theme(text=element_text(size=12,family="Nutmeg"))->grafico4
      
    }
    
    if (input$value4 == "Con etiqueta de datos") {
      
      ggplot(data_slide4()) +
        aes(x =as.factor(Anio), y = Total,
            text = paste("Año: ", Anio, 
                         "\nTotal: ", scales::comma(Total,1), sep="")) +
        geom_col(fill = "#AF1271", position = "dodge") +
        scale_y_continuous(labels = scales::comma) +
        # geom_text(aes(label=scales::comma(Total,1)),
        #           vjust=1, hjust=1, size= 4)+
        geom_text(aes(label=scales::comma(Total,1)), position = position_dodge(0.9),
                  vjust = -1, hjust=.5, size= 4, color="black")+
        labs(title = "Denuncias por violencia familiar (anual)", 
             caption = "Datos al 31 diciembre de 2020.  
Fuente: Elaboración propia con datos de la Fiscalía del estado de Jalisco.", 
             fill = "Tipo de violencia:",
             x="Año", y="Total") +
        theme_minimal(base_size=25)+
        theme(plot.title = element_text(size = 16L, hjust = 0.5), plot.caption = element_text(size = 12L, hjust = 0))+
        theme(text=element_text(size=12,family="Nutmeg"))->grafico4
      
      
    }
    
    ggplotly(grafico4, tooltip = "text")
    
  })
  
  ###########
  
  #Botones para slide 5  
  
  output$anio_5 <- renderUI({
    selectInput("Anio",
                label =  "Seleccione los anios",
                choices = sort(unique(slide5$Anio)),
                multiple = T)
  })
  
  output$mes_5 <- renderUI({
    selectInput("Mes",
                label =  "Seleccione mes",
                choices = unique(slide5$Mes),
                multiple = T)
  })
  
  
  #base reactiva para slide 5
  data_slide5 <- reactive({
    
    slide5 %>%
      filter(if(!is.null(input$anio_5))         Anio %in% input$anio_5     else Anio != "",
             if(!is.null(input$mes_5))         Anio %in% input$mes_5     else Mes != "",)
  })
  
  # slide5 <- slide5 %>%
  #   mutate(Mes=factor(Mes,
  #                     levels=c ("Enero", "Febrero", "Marzo", "Abril", "Mayo",
  #                               "Junio", "Julio", "Agosto", "Septiembre",
  #                               "Octubre", "Noviembre", "Diciembre")),
  #          Anio=factor(Anio,
  #                      levels=c("2021", "2020", "2019")))
  
    #Aquí inician los graficos para slide 5, con y sin etiqueta de datos.

  
  output$grafico5 <- renderPlotly ({
    
    if (input$value5 == "Sin etiqueta de datos") {
      #     
      #   ggplot(data_slide5())+
      #     # 
      #     # ggplot(slide5)+
      #        aes(x =as.POSIXct(Periodo), y = Total, colour =as.factor(Anio)) +
      #     geom_line(
      #       size = 0.8) +
      #     geom_point()+
      #     scale_y_continuous(labels = scales::comma) +
      #     scale_color_manual(values = c(
      #       `2019` = "#7E3794",
      #       `2020` = "#C91682",
      #       `2021` = "#D98CBC")) +
      #     labs(title = "Denuncias por violencia familiar (comparativo 2019-2020-2021)",
      #          caption = "Información con corte al 30 de abril 2021.
      # Fuente:Elaboración propia con datos de la Fiscalía del estado de Jalisco (2020-2021) y del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública 2019 (SESNSP).
      # Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración.
      # La unidad de medida son carpetas de investigación.",
      #          x= "Fecha",
      #          y= "Número de denuncias",
      #          fill = "Año") +
      #     theme_minimal()+  
      #     theme(plot.title = element_text(size = 12L, hjust = 0.5), 
      #           plot.caption = element_text(size = 12L, hjust = 0))+
      #     theme(text=element_text(size=12, family="Nutmeg"))->grafico5
      #   
      
      ggplot(data_slide5())+ 
        aes(x=fct_inorder(Mes),  y=Total, fill=as.factor(Anio),
            text = paste("Año: ", Anio, 
                         "\nMes: ", Mes,
                         "\nTotal: ", scales::comma(Total,1), sep="")) + 
        geom_bar(width=0.6, stat="identity") + 
        scale_y_continuous(labels = scales::comma) +
        scale_fill_manual(values = list(
          `2019`="#C91682", 
          `2020` = "#7E3794", 
          `2021`="#D98CBC"))+
        labs(title = "Denuncias por violencia familiar (comparativo 2019-2020-2021)", 
             caption = "Información con corte al 30 de abril 2021.
Fuente:Elaboración propia con datos de la Fiscalía del estado de Jalisco (2020-2021) y del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública 2019 (SESNSP). 
Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. 
La unidad de medida son carpetas de investigación.", 
             x= "Mes", 
             y= "Número de denuncias", 
             fill = "Año") +
        theme_minimal()+
        theme(plot.title = element_text(size = 16L, hjust = 0.5, angle = 45), 
              plot.caption = element_text(size = 12L, hjust = 0))+
        theme(text=element_text(size=12,family="Nutmeg"),
              axis.text.x = element_text(angle = 40))->grafico5
      
      
    }
    
    
    if (input$value5 == "Con etiqueta de datos") {
      
      ggplot(data_slide5())+ 
        aes(x=fct_inorder(Mes),  y=Total, fill=as.factor(Anio),
            text = paste("Año: ", Anio, 
                         "\nMes: ", Mes,
                         "\nTotal: ", scales::comma(Total,1), sep="")) + 
        geom_bar(width=0.6, stat="identity") + 
        scale_y_continuous(labels = scales::comma) +
        scale_fill_manual(values = list(
          `2019`="#C91682", 
          `2020` = "#7E3794", 
          `2021`="#D98CBC"))+
        geom_text(aes(label=scales::comma(Total,1)), position = position_stack(vjust = 0.5),
                  vjust=1, hjust=.5, size= 4, color="white")+
        labs(title = "Denuncias por violencia familiar (comparativo 2019-2020-2021)", 
             caption = "Información con corte al 30 de abril 2021.
Fuente:Elaboración propia con datos de la Fiscalía del estado de Jalisco (2020-2021) y del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública 2019 (SESNSP). 
Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. 
La unidad de medida son carpetas de investigación.", 
             x= "Mes", 
             y= "Número de denuncias", 
             fill = "Año") +
        theme_minimal()+
        theme(plot.title = element_text(size = 16L, hjust = 0.5, angle = 45), 
              plot.caption = element_text(size = 12L, hjust = 0))+
        theme(text=element_text(size=12,family="Nutmeg"),
              axis.text.x = element_text(angle = 40))->grafico5
      
      
      
    }
    
    ggplotly(grafico5, tooltip = "text")
    
  })
  
  
  
  
  
  
  #
  #
  #
  #
  #
  #
  #
  #
  
  
  #Botones para la plataforma
  output$año_vic1 <- renderUI({
    selectInput("año",
                label =  "Seleccione año",
                choices = sort(unique(victimas_nacional$año)),
                multiple = T)
  })
  
  
  output$tipo.de.delito_vic1 <- renderUI({
    selectInput("tipo.de.delito",
                label =  "Selecciona el delito",
                choices = sort(unique(victimas_nacional$delito)),
                multiple = T)
  })
  
  
  
  
  #base reactiva para que sea dinamica
  data_victimas <- reactive({
    
    victimas_nacional %>%
      filter(if(!is.null(input$año))                       año %in% input$año                  else año != "",
             if(!is.null(input$entidad))               entidad %in% input$entidad              else entidad != "",
             if(!is.null(input$tipo.de.delito)) tipo.de.delito %in% input$tipo.de.delito       else tipo.de.delito != "",)
  })
  
  
  
  #Gráfico con reactivo para que el mapa se mueva conforme al usuario
  output$grafico_vic1 <- renderPlotly ({
    # data_victimas <- data()
    
    
    ggplot(data_victimas()) +
      aes(x = as.factor(año), weight = tot_acu,
          fill = tipo.de.delito) +
      geom_bar(position = "dodge") +  scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = c("purple","#d10096")) +
      labs(title="Total de víctimas de delitos (absolutos).",
           x="Año",
           y="Total",
           fill="Delitos",
           caption = "Elaborado con los datos del SESPN.") +
      theme_minimal()+
      theme(text=element_text(size=12,  family="Nutmeg"))->grafico_vic1
    
    
    ggplotly(grafico_vic1)  #añadir ggplotly
  })
  
  
  #gráfico con reractivo 2
  
  output$grafico_vic2 <- renderPlotly ({
    
    ggplot(data_victimas()) +
      aes(x =as.factor(año), fill=rango.de.edad, weight = tot_acu) +
      geom_bar(position = "dodge") +  scale_y_continuous(labels = scales::comma) +
      scale_fill_manual(values = c("purple","#d10096", "plum")) +
      labs(title="Total de víctimas de delitos según edad y sexo (absolutos).",
           x="Año",
           y="Total",
           fill="Rango de edad",
           caption = "Elaborado con los datos del SESPN.") +
      theme_minimal()+
      theme(text=element_text(size=12,  family="Nutmeg"))
  })
  
  
  
  output$slide19 <- renderPlotly({
    plotly::ggplotly(grafico19, tooltip="value", dynamicTicks = TRUE)
    
  }) #añadir ggplotly
  
  
  
}


shinyApp(ui = ui, server = server)

#Aquí más códgio que aún no se añade a la shiny.

# #slide7
# 
# ggplot(slide7) +
#   aes(x = Fecha, y = Total, colour=`Tipo de violencia`, 
#       # text = paste( "Fecha de la semana: ", Fecha,
#       #              "\nTipo de violencia: ", `Tipo de violencia`,
#       #              "\nTotal de llamadas: ", comma(Total), sep="")
#       ) +
#   geom_line(size = 0.8) +
#   geom_point()+
#   scale_y_continuous(labels = scales::comma) +
#   scale_color_manual(
#     values = c(
#       `Violencia contra la mujer` = "#D98CBC",
#       `Violencia de Pareja` = "#C91682",
#       `Violencia Familiar` = "#7E3794")) +
#   labs(title = "Registro mensual de llamadas 911 relacionadas con violencia de género.",
#        caption = "Información con corte al 30 de abril 2021.
# Fuente: elaboración propia con datos de Escudo Urbano C5.",
#        x= "Fecha",
#        y= "Número de denuncias",
#        fill = "Año") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"))
# 
# #slide8
# ggplot(slide8) +
#   aes(x =as.factor(Anio), weight = Total, fill = `Tipo de violencia`,
#       text = paste( "Año: ", Anio,
#                     "\nTipo de violencia: ", `Tipo de violencia`,
#                     "\nTotal de llamadas: ", comma(Total), sep="")) +
#   geom_bar() +
#   scale_y_continuous(labels = scales::comma) +
#   scale_fill_manual(
#     values = list(
#       `Violencia contra mujer` = "#D98CBC",
#       `Violencia de pareja` = "#C91682",
#       `Violencia familiar` = "#7E3794")) +
#   labs(title = "Registro anual de llamadas al 911 relacionadas con violencia de género.",
#        caption = "Con datos al 28 de febrero del 2021.
# Fuente: elaboración propia con datos de Escudo Urbano C5.",
#        x= "Año",
#        y= "Número de denuncias",
#        fill = "Tipo de violencia") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"))
# 
# 
# #slide9
# ggplot(slide9) +
#   aes(x = Semana, y = Total, colour = Zona) +
#   geom_line(size = 0.8) +
#   geom_point()+
#   scale_color_manual(values = c(
#     AMG = "#7E3794",
#     Interior = "#C91682",
#     `Puerto Vallarta`= "#D98CBC")) +  
#   labs(title = "Medidas de protección emitidas.",
#        caption = "Datos al 05 de marzo, 2021.
# Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco.",
#        x= "Fecha",
#        y= "Total de medidas",
#        fill = "Zona") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"))
# 
# 
# #slide10
# slide10 <- slide10 %>% mutate(Fecha=as.Date(Fecha, "%d/%m/%Y"))
# slide10 <- slide10 %>% mutate(ym=as.yearmon(Fecha))
# 
# ggplot(slide10) +
#   aes(x = ym, y = Total, colour=as.character(Anio)) +
#   geom_line(size = 0.8) +
#   geom_point()+
#   scale_color_manual(values = c(
#     `2019` = "#7E3794",
#     `2020` = "#C91682",
#     `2021`= "#D98CBC")) +  
#   labs(title = "Abuso sexual infantil (ASI) 2019 - 2021.",
#        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco 2019- febrero 2021. 
# Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. La unidad de medida son
# carpetas de investigación",
#        x= "Fecha",
#        y= "Total de carpetas de investigación",
#        fill = "Año") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"))
# 
# 
# ggplot(slide10) +
#   aes(x =fct_inorder(Mes), fill =as.factor(Anio), weight = Total) +
#   geom_bar(position = "dodge") +
#   scale_fill_manual(values = c(
#     `2019` = "#7E3794",
#     `2020` = "#C91682",
#     `2021`= "#D98CBC")) +  
#   labs(title = "Abuso sexual infantil (ASI) 2019 - 2021.",
#        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco 2019- febrero 2021. 
# Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. La unidad de medida son
# carpetas de investigación",
#        x= "Fecha",
#        y= "Total de carpetas de investigación",
#        fill = "Año") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"))
# 
# 
# #slide11
# slide11 <- slide11 %>% mutate(Fecha=as.Date(Fecha, "%d/%m/%Y"))
# slide11 <- slide11 %>% mutate(ym=as.yearmon(Fecha))
# 
# 
# ggplot(slide11) +
#   aes(x = ym, y = Total, colour=as.character(Anio)) +
#   geom_line(size = 0.8) +
#   geom_point()+
#   scale_color_manual(values = c(
#     `2019` = "#7E3794",
#     `2020` = "#C91682",
#     `2021`= "#D98CBC")) +  
#   labs(title = "Violación 2019 - 2021.",
#        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco 2019 - febrero 2021. 
# Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. La unidad de medida son carpetas de investigación.",
#        x= "Fecha",
#        y= "Total de carpetas de investigación",
#        fill = "Año") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"))
# 
# 
# 
# ggplot(slide11) +
#   aes(x =fct_inorder(Mes), fill =as.factor(Anio), weight = Total) +
#   geom_bar(position = "dodge") +
#   scale_fill_manual(values = c(
#     `2019` = "#7E3794",
#     `2020` = "#C91682",
#     `2021`= "#D98CBC")) +  
#   labs(title = "Violación 2019 - 2021.",
#        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco 2019 - febrero 2021. 
# Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. La unidad de medida son carpetas de investigación.",
#        x= "Fecha",
#        y= "Total de carpetas de investigación",
#        fill = "Año") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"),
#         axis.text.x = element_text(angle = 40))
# 
# #slide12
# 
# slide12 <- slide12 %>% mutate(Fecha=as.Date(Fecha, "%d/%m/%Y"))
# slide12 <- slide12 %>% mutate(ym=as.yearmon(Fecha))
# 
# 
# ggplot(slide12) +
#   aes(x = ym, y = Total, colour =as.factor(Anio)) +
#   geom_line(size = 0.8) +
#   geom_point()+
#   scale_y_continuous(labels = scales::comma) +
#   scale_color_manual(values = c(
#     `2020` = "#7E3794",
#     `2021` = "#C91682")) +  
#   theme_minimal()+
#   labs(title = "Muertes violentas registradas como feminicidios en Jalisco (2015 - 2021).",
#        caption = "*Actualizado de acuerdo a los datos abiertos del SESNSP de carpetas de investigación iniciadas por feminicidios a febrero 2021.
# Fuente: elaboración propia con datos abiertos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP).
# La unidad de medida son carpetas de investigación.",
#        x= "Fecha",
#        y= "Total de carpetas de investigación",
#        fill = "Año") +
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"))
# 
# 
# 
# ggplot(slide12) +
#   aes(x =fct_inorder(Mes), fill =as.factor(Anio), weight = Total) +
#   geom_bar(position = "dodge") +
#   scale_y_continuous(labels = scales::comma) +
#   scale_fill_manual(values = c(
#     `2021` = "#7E3794",
#     `2020` = "#C91682")) +  
#   labs(title = "Muertes violentas registradas como feminicidios en Jalisco (2015 - 2021).",
#        caption = "*Actualizado de acuerdo a los datos abiertos del SESNSP de carpetas de investigación iniciadas por feminicidios a febrero 2021.
# Fuente: elaboración propia con datos abiertos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP).
# La unidad de medida son carpetas de investigación.",
#        x= "Fecha",
#        y= "Total de carpetas de investigación",
#        fill = "Año") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"),
#         axis.text.x = element_text(angle = 40))
# 
# 
# #slide13
# ggplot(slide13)+
#   aes(x=as.factor(Anio), y=Total)+
#   geom_col(fill="#7E3794")+
#   scale_y_continuous(labels = scales::comma) +
#   labs(title = "Muertes violentas registradas como feminicidios en Jalisco (2015 - 2021).",
#        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco para 2020 y 2021, para 2019 con datos abiertos del Secretariado Ejecutivo
# del Sistema de Seguridad Nacional. Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente
# siguen en proceso de integración. La unidad de medida son víctimas.",
#        x= "Fecha",
#        y= "Total de carpetas de investigación",
#        fill = "Año") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"))
# 
# 
# #slide14
# 
# library(ggplot2)
# 
# ggplot(slide14) +
#  aes(x = Fecha, y = Total, colour =Anio) +
#  geom_line(size = 0.8) +
#   geom_point()+
#   # scale_color_manual(values = c(
#   #   `2019` = "#7E3794",
#   #   `2020` = "#C91682",
#   #   `2021`= "#D98CBC")) +  
#   scale_color_gradient(low = "#7E3794", high = "#D98CBC") +
#   labs(title = "Violación 2019 - 2021.",
#        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco 2019 - febrero 2021. 
# Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. La unidad de medida son carpetas de investigación.",
#        x= "Fecha",
#        y= "Total de carpetas de investigación",
#        fill = "Año") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"))
# 
# 
# #slide16
# ggplot(slide16) +
#   aes(x = Fecha, y = Total, colour =Anio) +
#   geom_line(size = 0.8) +
#   geom_point()+
#   # scale_color_manual(values = c(
#   #   `2019` = "#7E3794",
#   #   `2020` = "#C91682",
#   #   `2021`= "#D98CBC")) +  
#   scale_color_gradient(low = "#7E3794", high = "#D98CBC") +
#   labs(title = "Violación 2019 - 2021.",
#        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco 2019 - febrero 2021. 
# Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. La unidad de medida son carpetas de investigación.",
#        x= "Fecha",
#        y= "Total de carpetas de investigación",
#        fill = "Año") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"))->g16
# 
# ggplotly(g16)
# 
# #19 y 20 mapas
# 
# #slide21
# ggplot(slide21) +
#   aes(x = Localizadas, fill = Localizadas, weight = Total) +
#   geom_bar(fill="#7E3794") +
#   coord_flip() +
#   scale_y_continuous(labels = comma)+
#   theme_minimal()+
#   labs(title = "Localización de mujeres reportadas como desaparecidas.",
#        caption = "Fuente: Sistema de Información sobre Víctimas de Desaparición (SISOVID). Información al 28 de febrero 2021.",
#        x= "",
#        y= "",
#        fill = "Localizadas") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0),
#         text=element_text(size=12, family="Nutmeg"))
# 
# 
# #slide22
# ggplot(slide22) +
#   aes(x = `Desaparicion y no localizacion`, weight = Total) +
#   geom_bar(fill="#7E3794") +
#   coord_flip() +
#   scale_y_continuous(labels = comma)+
#   theme_minimal()+
#   labs(title = "Desaparición y no localización de mujeres.",
#        caption = "De acuerdo con la Ley, se entiende por persona no localizada, aquello en donde se presume que no hay comisión de algún delito, y en el caso de las personas desaparecidas se presume de la comisión de algún delito.
# Fuente: Sistema de Información sobre Víctimas de Desaparición (SISOVID). Información al 28 de febrero 2021.",
#        x= "",
#        y= "",
#        fill = "") +
#   theme_minimal()+  
#   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#         plot.caption = element_text(size = 12L, hjust = 0))+
#   theme(text=element_text(size=12, family="Nutmeg"))  
# 
# #slide23
# ggplot(slide23) +
#   aes(x = Edad, weight = Total) +
#   geom_bar(fill="#7E3794") +
#   scale_y_continuous(labels = comma)+
#   labs(title = "Desaparición y no localización de mujeres (edad).",
#        caption = "Fuente: Sistema de Información sobre Víctimas de Desaparición (SISOVID). Información al 28 de febrero 2021.",
#        x= "",
#        y= "",
#        fill = "") +
#   theme_minimal(base_size=25)+
#   theme(text=element_text(size=12,  family="Nutmeg"))+
#   theme(plot.title = element_text(size = 16L, hjust = 0.5, family = "Nutmeg"), 
#         plot.caption = element_text(size = 12L, hjust = 0, family="Nutmeg"))+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg"))
# 
# 







# #Código 
# options(encoding = 'UTF-8')
# 
# library(shiny)
# library(shinydashboard)
# library(shinythemes)
# library(ggplot2)
# library(dplyr)
# library(leaflet)
# library(tidyverse)
# library(DT)
# library(leaflet)
# library(leaflet.extras)
# library(readxl)
# library(data.table)
# library(shinythemes)
# library(tidyr)
# library(plotly)
# library(sf) 
# library(mxmaps)
# library(extrafont)
# library(extrafontdb)
# library(showtext)
# library(ggrepel)
# library(forcats)
# library(devtools)
# library(shinybusy)
# library(ggnewscale)
# library(reshape)
# library(rebus)
# 
# library(rgdal)
# library(graphics)
# 
# # font_add("Nutmeg", "Nutmeg-Light.ttf")
# 
# 
# 
# slide2 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 2")
# slide3 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 3")
# slide4 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 4")
# slide5 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 5")
# slide6 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 6")
# slide7 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 7")
# slide8 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 8")
# slide9 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 9")
# slide10 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 10")
# slide11 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 11")
# slide12 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 12")
# slide13 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 13")
# slide15 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 15")
# slide18 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 18")
# slide19 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 19")
# slide20 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 20")
# slide21 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 21")
# slide22 <- read_excel("CodigoVioleta_bases.xlsx", sheet = "Diapositiva 22")
# 
# victimas_nacional <- read_excel("CodigoVioleta_bases.xlsx", sheet = "victimas_nacional")
# 
# 
# 
# ##################
# #Mapa hexagonales
# 
# data(df_mxstate_2020) #Base de una paquetería que contiene la información del Censo Nacional y que es necesaria para ponderar feminicidios por poblacion
# 
# slide18<-merge(df_mxstate_2020, slide18,
#                by.y="Clave de entidad",
#                by.x="region")
# 
# #merge_1$value<- merge_1$tot_acu
# 
# #Mapa hexagonal
# mxhexbin_choropleth(slide18, num_colors = 1) +  
#   labs(title="Muertes violentas de mujeres tipificados como feminicidios.",
#        caption="Fuente: Elaboración propia con datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP). Se retoma la cifra de enero para respetar el ranking del SESNSP.",
#        x="", y="",
#        fill="Total") +
#   scale_fill_gradient(
#     low = "plum", 
#     high = "magenta4",
#     guide = "colourbar")+theme_minimal()+
#   theme(text=element_text(family="Roboto",
#                           face="plain",
#                           size=23,
#                           hjust = 0.5,
#                           vjust = 0.5),
#         legend.text = element_text(size=10, colour = "grey12"),
#         legend.title = element_text(color = "grey7", size = 13),
#         plot.title = element_text(hjust=0.5, size=15, face="plain", colour="grey6"),
#         plot.caption = element_text(hjust=-1, size=12, face="plain"),
#         plot.margin = margin(1, 1, 1, 1, "cm"))->grafico18
# 
# 
# 
# 
# slide19<-merge(df_mxstate_2020, slide19,
#                by.y="Clave de entidad",
#                by.x="region")
# 
# #merge_1$value<- merge_1$tot_acu
# 
# #Mapa hexagonal
# mxhexbin_choropleth(slide19, num_colors = 1) +  
#   labs(title="Muertes violentas de mujeres tipificados como feminicidios.",
#        caption="Fuente: Elaboración propia con datos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP). Se retoma la cifra de enero para respetar el ranking del SESNSP.",
#        x="", y="",
#        fill="Tasa") +
#   scale_fill_gradient(
#     low = "plum", 
#     high = "magenta4",
#     guide = "colourbar")+theme_minimal()+
#   theme(text=element_text(family="Roboto",
#                           face="plain",
#                           size=23,
#                           hjust = 0.5,
#                           vjust = 0.5),
#         legend.text = element_text(size=10, colour = "grey12"),
#         legend.title = element_text(color = "grey7", size = 13),
#         plot.title = element_text(hjust=0.5, size=15, face="plain", colour="grey6"),
#         plot.caption = element_text(hjust=-1, size=12, face="plain"),
#         plot.margin = margin(1, 1, 1, 1, "cm"))->grafico19
# 
# 
# 
# 
# 
# 
# # jalisco <- read.csv("jalisco.csv", encoding="UTF-8")
# # 
# # 
# # #Regiones
# # `Región Norte`<- c("Bolaños", "Chimaltitán", "Colotlán", "Huejúcar", "Huejuquilla el Alto",
# #                    "Mezquitic", "San Martín de Bolaños", "Santa María de los Ángeles", "Totatiche", "Villa Guerrero")
# # 
# # `Región Altos Norte`<- c("Encarnación de Díaz", "Lagos de Moreno", "Ojuelos de Jalisco",
# #                          "San Diego de Alejandría", "San Juan de los Lagos", "Teocaltiche",
# #                          "Unión de San Antonio", "Villa Hidalgo")
# # 
# # `Región Altos Sur`<- c("Acatic", "Arandas", "Cañadas de Obregón", "Jalostotitlán", "Jesús María",
# #                        "Mexticacán", "San Ignacio Cerro Gordo", "San Julián", "San Miguel el Alto",
# #                        "Tepatitlán de Morelos", "Valle de Guadalupe", "Yahualica de González Gallo")
# # 
# # `Región Ciénega`<- c("Atotonilco el Alto", "Ayotlán", "Degollado", "Jamay", "La Barca", "Ocotlán",
# #                      "Poncitlán", "Tototlán", "Zapotlán del Rey")
# # 
# # 
# # `Región Sureste`<- c("Chapala", "Concepción de Buenos Aires", "Jocotepec", "La Manzanilla de la Paz",
# #                      "Mazamitla", "Quitupan", "Santa María del Oro", "Tizapán el Alto",
# #                      "Tuxcueca","Valle de Juárez")
# # 
# # 
# # `Región Sur`<- c("Gómez Farías", "Jilotlán de los Dolores", "Pihuamo", "San Gabriel",
# #                  "Tamazula de Gordiano", "Tecalitlán", "Tolimán", "Tonila", "Tuxpan",
# #                  "Zapotiltic", "Zapotitlán de Vadillo", "Zapotlán el Grande")
# # 
# # `Región Sierra de Amula`<- c("Atengo", "Autlán de Navarro", "Ayutla", "Chiquilistlán",
# #                              "Cuautla", "Ejutla", "El Grullo", "El Limón", "Juchitlán",
# #                              "Tecolotlán", "Tenamaxtlán", "Tonaya", "Tuxcacuesco", "Unión de Tula")
# # 
# # 
# # `Región Costa Sur`<- c("Casimiro Castillo", "Cihuatlán", "Cuautitlán de García Barragán", 
# #                        "La Huerta", "Tomatlán", "Villa Purificación")
# # 
# # 
# # 
# # `Región Costa-Sierra Occidental`<- c("Atenguillo", "Cabo Corrientes", "Guachinango",
# #                                      "Mascota", "Mixtlán", "Puerto Vallarta",
# #                                      "San Sebastián del Oeste", "Talpa de Allende")
# # 
# # 
# # 
# # `Región Valles`<- c("Ahualulco de Mercado", "Amatitán", "Ameca", "El Arenal",
# #                     "Etzatlán", "Hostotipaquillo", "Magdalena", 
# #                     "San Juanito de Escobedo", "San Marcos", "Tala",
# #                     "Tequila", "Teuchitlán")
# # 
# # 
# # `Región Lagunas`<- c("Acatlán de Juárez", "Amacueca", "Atemajac de Brizuela",
# #                      "Atoyac", "Cocula", "San Martín Hidalgo", "Sayula",
# #                      "Tapalpa", "Techaluta de Montenegro", "Teocuitatlán de Corona",
# #                      "Villa Corona", "Zacoalco de Torres")
# # 
# # 
# # `Región Centro`<- c("Cuquío", "El Salto", "Guadalajara", "Ixtlahuacán de los Membrillos",
# #                     "Ixtlahuacán del Río", "Juanacatlán", "San Cristóbal de la Barranca",
# #                     "San Pedro Tlaquepaque", "Tlajomulco de Zúñiga", "Tonalá", "Zapopan", 
# #                     "Zapotlanejo")
# # 
# # 
# # regiones<- jalisco %>% 
# #   mutate(Region = case_when(
# #     Municipio %in% `Región Norte` ~ "Región Norte",
# #     Municipio %in% `Región Altos Norte` ~ "Región Altos Norte",
# #     Municipio %in% `Región Altos Sur` ~ "Región Altos Sur",
# #     Municipio %in% `Región Ciénega` ~ "Región Ciénega",
# #     Municipio %in% `Región Sureste` ~ "Región Sureste",
# #     Municipio %in% `Región Sur` ~ "Región Sur",
# #     Municipio %in% `Región Sierra de Amula` ~ "Región Sierra de Amula",
# #     Municipio %in% `Región Costa Sur` ~ "Región Costa Sur",
# #     Municipio %in% `Región Costa-Sierra Occidental` ~ "Región Costa-Sierra Occidental",
# #     Municipio %in% `Región Valles` ~ "Región Valles",
# #     Municipio %in% `Región Lagunas` ~ "Región Lagunas",
# #     Municipio %in% `Región Centro` ~ "Región Centro",
# #     TRUE ~ "Sin especificar"
# #   ))
# # 
# # 
# # regiones<- regiones %>% 
# #   group_by(Año, Region, Municipio, Tipo.de.delito) %>% 
# #   summarise(ene=sum(Enero, na.rm = T),
# #             feb=sum(Febrero, na.rm = T),
# #             mar=sum(Marzo, na.rm = T),
# #             abr=sum(Abril, na.rm = T),
# #             may=sum(Mayo, na.rm = T),
# #             jun=sum(Junio, na.rm = T),
# #             jul=sum(Julio, na.rm = T),
# #             ago=sum(Agosto, na.rm = T),
# #             sep=sum(Septiembre, na.rm = T),
# #             oct=sum(Octubre, na.rm = T),
# #             nov=sum(Noviembre, na.rm = T),
# #             dic=sum(Diciembre, na.rm = T),
# #             tot_acu=sum(ene+ feb+ mar+ abr+ 
# #                         may+ jun + jul+ ago+
# #                         sep+ oct+ nov+ dic))
# # 
# # write.csv(regiones, "regiones.csv")
# # 
# # ggplot(regiones)+
# #   aes(x = as.factor(Año) , y = tot_acu, fill = Region) +
# #   geom_col() +
# #   scale_fill_manual(values = c(
# #     `Región Altos Norte`= "#562877",
# #     `Región Altos Sur`= "#67224C",
# #     `Región Centro`= "#C91682",
# #     `Región Ciénega`= "#9540BF",
# #     `Región Costa-Sierra Occidental`= "#8436A1",
# #     `Región Costa Sur`= "#A136A1",
# #     `Región Lagunas`= "#923CB4",
# #     `Región Norte`= "#A136A1",
# #     `Región Sierra de Amula`= "#C44FA5",
# #     `Región Sur`= "#AD57C7",
# #     `Región Sureste`= "#CD6AB1",
# #     `Región Valles`= "#AC66CC",
# #     `Sin especificar`= "#B56ECF"))+
# #   theme_minimal()
# # 
# # library(viridis)
# # 
# # ggplot(regiones)+
# #   aes(x = as.factor(Año) , y = tot_acu, fill = Region) +
# #   geom_col() +
# #   scale_y_continuous(labels = comma)+
# #   new_scale_fill() +
# #   geom_col(aes(x = as.factor(Año) , y = tot_acu, fill = Region)) +
# #   scale_fill_manual(values = c(
# #     `Región Altos Norte`= "#92278F",
# #     `Región Altos Sur`= "#9B57D3",
# #     `Región Centro`= "#755DD9",
# #     `Región Ciénega`= "#45A5ED",
# #     `Región Costa-Sierra Occidental`= "#5982DB",
# #     `Región Costa Sur`= "#9969d3",
# #     `Región Lagunas`= "#581756",
# #     `Región Norte`= "#A136A1",
# #     `Región Sierra de Amula`= "#3A2397",
# #     `Región Sur`= "#393374",
# #     `Región Sureste`= "#1067A7",
# #     `Región Valles`= "#214698",
# #     `Sin especificar`= "#B56ECF"))+
# #   new_scale_fill() +
# #   geom_col(aes(x = as.factor(Año) , y = tot_acu, fill = Municipio)) +
# #   scale_fill_viridis_d(option = "viridis") +  
# #   theme_minimal()+
# #   theme(legend.position = "none")
# # 
# # regiones_base<- regiones %>% 
# #   group_by(Año, Region, Tipo.de.delito) %>% 
# #   summarise(Total= sum(tot_acu))
# # 
# # fil_delitos<- c("Homicidio, Feminicidio")
# # 
# # regiones_base %>% 
# #   filter(Tipo.de.delito %in% "fil_delitos") %>% 
# # ggplot(aes(x=Año, y=Total, colour=Tipo.de.delito))+
# #   geom_line(size=0.8)+
# #   scale_color_viridis_d(option = "viridis", direction = 1) +
# #   new_scale_fill() +
# #   geom_line(aes(x=Año, y=Total, colour=Region)) +
# #   theme_minimal()
# # 
# # 
# # 
# # 
# # 
# # regiones_base %>%
# #   ggplot() +
# #   aes(x = Año, y = Total, colour = Tipo.de.delito) +
# #   geom_line() +
# #   geom_point(aes(x=Año, y=Total, colour=Region))+
# #   scale_color_viridis_d(option = "plasma", direction = 1) +
# #   new_scale_colour() +
# #   geom_line(aes(x=Año, y=Total, colour=Region)) +
# #   #geom_point(aes(x=Año, y=Total, colour=Region))+
# #   scale_color_viridis_d(option = "plasma", direction = 1) +
# #   theme_minimal()
# # 
# # 
# # 
# # 
# # 
# # library(RColorBrewer)
# # 
# # 
# # ggplot(regiones)+
# #   aes(x = as.factor(Año) , y = tot_acu, fill = Region) +
# #   geom_col() +
# #   scale_fill_manual(values = colorRampPalette(brewer.pal(13, "Set2"))(colourCount)) +
# #   scale_y_continuous(labels = scales::comma)+
# #   theme_minimal()+labs(title = "Carpetas de investigación por tipo de delitos.",
# #                        caption = "Fuente: elaboración propia con datos abiertos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP).",
# #                        x="", y="", fill="Regiones") +
# #   theme_minimal(base_size=15)+
# #   theme(text=element_text(size=12,  family="Nutmeg"))+
# #   theme(plot.title = element_text(size = 16L, hjust = 0.5), plot.caption = element_text(size = 12L, hjust = 0))
# # 
# # 
# # 
# # 
# 
# 
# ################################################################################################################
# 
# 
# 
# ui <- navbarPage("Datos abiertos",
#                  theme = "mytheme.css",
#                  navbarMenu("Código Violeta",
#                             tabPanel("Violencia familiar",
#                                      h4("Plataforma de datos abierto de la Secretaria de Igualdad Sustantiva entre Mujeres y Hombres del Estado de 
#                           Jalisco, conoce los datos de acceso público en materia de violencia contra las mujeres en Jalisco. Consulta el 
#                           reporte ejecutivo del Código violetal",
#                                         (a(target="_blank",href="https://igualdad.jalisco.gob.mx/pdf/reporte-sstadistico-mensual-codigo-violeta.pdf","aqui"))),
#                                      h4("Sitio en construcción", align = "justify"),
#   # tags$h2(tags$style("#Titulo{
#   #                        color: #3b343a;
#   #                        font-size: 24px;
#   #                        font-style: italics;
#   #                        font-family: Nutmeg;
#   #                        }")),
#                                      tabsetPanel(
#                                        tabPanel(
#                                          "Violencia familiar semanal por zonas del AMG",
#                                      sidebarLayout(
#                                        sidebarPanel("Seleccione algunas características",
#                                                     selectInput(
#                                                       inputId = "semana",
#                                                       label = "Semana",
#                                                       choices = unique(slide2$Semana),
#                                                       multiple = TRUE
#                                                     ),
#                                                     selectInput(
#                                                       inputId = "zona",
#                                                       label = "Zona",
#                                                       choices = sort(unique(slide2$Zona)),
#                                                       multiple = TRUE
#                                                     ),
#                                                     selectInput("value2", "Seleccione alguna opción" , 
#                                                                 choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
#                                                                 selected = NULL ,  multiple = FALSE, selectize = TRUE)
#                                                     
#                                        ),
#                                        mainPanel(plotlyOutput(outputId = "grafico2", height = 400, width = 850)))),
#                                          # tags$head(
#                                          #   tags$style(type='text/css',
#                                          #              ".nav-tabs {font-size: 20px} ")),
#                                          
#                                        
#                             tabPanel("Denuncias de violencia familiar por sexo",
#                                      sidebarLayout(
#                                        sidebarPanel("Seleccione algunas características",
#                                                     selectInput(
#                                                       inputId = "semana_3",
#                                                       label = "Semana",
#                                                       choices = unique(slide3$Semana),
#                                                       multiple = TRUE
#                                                     ),
#                                                     selectInput(
#                                                       inputId = "sexo_3",
#                                                       label = "Sexo",
#                                                       choices = sort(unique(slide3$Sexo)),
#                                                       multiple = TRUE
#                                                     ),
#                                                     selectInput("value3", "Seleccione alguna opción" , 
#                                                                 choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
#                                                                 selected = NULL ,  multiple = FALSE, selectize = TRUE)),
#                                        mainPanel(plotlyOutput(outputId = "grafico3", height = 400, width = 850)))),
#                                          # tags$head(
#                                          #   tags$style(type='text/css',
#                                          #              ".nav-tabs {font-size: 20px} ")),
#                                          
#                                        
#                             tabPanel("Total anual",
#                                      sidebarLayout(
#                                        sidebarPanel("Seleccione alguna característica",
#                                                     selectInput(
#                                                       inputId = "anio_4",
#                                                       label = "Año",
#                                                       choices = sort(unique(slide4$Anio)),
#                                                       multiple = TRUE
#                                                     ),
#                                                     selectInput("value4", "Seleccione alguna opción" , 
#                                                                 choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
#                                                                 selected = NULL ,  multiple = FALSE, selectize = TRUE)),
#                                        mainPanel(plotlyOutput(outputId = "grafico4", height = 400, width = 850)))),
#                             # tags$head(
#                             #   tags$style(type='text/css',
#                             #              ".nav-tabs {font-size: 20px} ")),))),
#                             
#                             tabPanel("Total mensual",
#                                      sidebarLayout(
#                                        sidebarPanel("Seleccione alguna característica",
#                                                     selectInput(
#                                                       inputId = "anio_5",
#                                                       label = "Año",
#                                                       choices = sort(unique(slide5$Anio)),
#                                                       multiple = TRUE
#                                                     ),
#                                                     # selectInput(
#                                                     #   inputId = "mes_5",
#                                                     #   label = "Mes",
#                                                     #   choices = unique(slide5$Mes),
#                                                     #   multiple = TRUE
#                                                     # ),
#                                                     selectInput("value5", "Selecciona un tipo de gráfico" , 
#                                                                 choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
#                                                                 selected = NULL ,  multiple = FALSE, selectize = TRUE)
#                                                     
#                                        ),
#                                        mainPanel(plotlyOutput(outputId = "grafico5", height = 400, width = 850)))))), 
#   
#   
#   
#                         tabPanel("Delitos tipificados como femininicidios",
#                                  h4("Plataforma de datos abierto de la Secretaria de Igualdad Sustantiva entre Mujeres y Hombres del Estado de 
#                                                 Jalisco, conoce los datos de acceso público en materia de violencia contra las mujeres en Jalisco. Consulta el 
#                                                 reporte ejecutivo del Código violetal",
#                                     (a(target="_blank",href="https://igualdad.jalisco.gob.mx/pdf/reporte-sstadistico-mensual-codigo-violeta.pdf","aqui"))),
#                                  h4("Sitio en construcción", align = "justify"),
#            # tags$h2(tags$style("#Titulo{
#            #                        color: #3b343a;
#            #                        font-size: 24px;
#            #                        font-style: italics;
#            #                        font-family: Nutmeg;
#            #                        }")),
#                                  tabsetPanel(
#                                    tabPanel("Total de feminicidios",
#                                      sidebarLayout(
#                                        sidebarPanel("Seleccione algunas características",
#                         
#                                        # tabPanel("Violencia contra la mujer",
#                                                 # h4("Sitio en construcción - violencia vs la mujer", alig = "justify",
#                                                 #    tabPanel("Visualización de los datos",
#                                                 #      sidebarLayout(
#                                                 #        sidebarPanel("Seleccione algunas características",
#                                               selectInput(
#                                                 inputId = "año_vic1", 
#                                                 label = "Año", 
#                                                 choices = sort(unique(victimas_nacional$año)),
#                                                 multiple = TRUE
#                                               ),
#                                               selectInput(
#                                                 inputId = "tipo.de.delito_vic1", 
#                                                 label = "Tipo de delito", 
#                                                 choices = sort(unique(victimas_nacional$tipo.de.delito)),
#                                                 multiple = TRUE
#                                               )),
#                                               
#                                  mainPanel(plotlyOutput(outputId = "grafico_vic1", height = 400, width = 850)))),
#                                    # tags$head(
#                                    #   tags$style(type='text/css', 
#                                    #              ".nav-tabs {font-size: 20px} ")),
#                                  
#                              
#                              
#                              tabPanel("Feminicidios por edad",
#                                       plotlyOutput(outputId = "grafico_vic2", height = 600, width = 850)),
#   
#                             tabPanel('Georreferenciación',
#                                      navlistPanel(
#                                        tabPanel("México",
#                                                 h3("México"),
#                                                 plotlyOutput(outputId = "slide19", height = 500, width = 700)))))),
#   
#   
# 
#                  
#                  tabPanel("Llamadas al 911",
#                           h4("Sitio en construcción - 911", align = "justify"))
# ),
# 
# 
# 
# tabPanel("Documentación", icon = icon("far fa-file-alt"),
#          h2(align= "center", "Sitio en construcción - Documentación ")),
# 
# 
# 
# tabPanel("Descarga",icon = icon("fas fa-cloud-download-alt"),
#          h2(align= "center", "Sitio en construcción"),
#          downloadButton('downloadData', 'Download'))
# 
# )
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ################################################################################################################
# ################################################################################################################
# ################################################################################################################
# ################################################################################################################
# ################################################################################################################
# 
# 
# ############################################
# 
# server <- function(input, output, session){
#   
#   
#   #Botones para slide 2    
#   output$semana <- renderUI({
#     selectInput("Semana",
#                 label =  "Seleccione semana",
#                 choices = sort(unique(slide2$Semana)),
#                 multiple = T)
#   })
#   
#   
#   output$zona <- renderUI({
#     selectInput("Zona",
#                 label =  "Selecciona la zona",
#                 choices = sort(unique(slide2$Zona)),
#                 multiple = T)
#   })
#   
#   
#   
#   #base reactiva para slide 2
#   data_slide2 <- reactive({
#     
#     slide2 %>%
#       filter(if(!is.null(input$semana))         Semana %in% input$semana     else Semana != "",
#              if(!is.null(input$zona))             Zona %in% input$zona       else Zona != "",)
#   })
#   
#   
#   output$grafico2 <- renderPlotly ({
#     
#     if (input$value2 == "Sin etiqueta de datos") {
#       
#       
#       ggplot(data_slide2()) +
#         aes(x = fct_inorder(Periodo) , y = Total, 
#             colour = Zona, group=Zona,
#             text = paste("Semana: ", Semana, 
#                          "\nÁrea: ", Zona,
#                          "\nTotal de denuncias registradas: ", Total, sep="")) +
#         geom_line(size = .8) +
#         geom_point()+
#         scale_y_continuous(labels = scales::comma) +
#         scale_color_manual(values = c(
#           AMG = "#7E3794",
#           Interior = "#C91682",
#           `Puerto Vallarta` = "#D98CBC")) +
#         labs(title = "Denuncias por violencia familiar",
#              caption = "Fuente: Elaboración propia con datos de la Fiscalía del estado de Jalisco.
# Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración.",
#              x="Semana", y="Total de denuncias", fill="Zona") +
#         theme_minimal(base_size=15)+
#         theme(text=element_text(size=12,  family="Nutmeg"))+
#         theme(plot.title = element_text(size = 16L, hjust = 0.5), 
#               plot.caption = element_text(size = 12L, hjust = 0))+
#         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))->grafico2
#       
#     }
#     
#     
#     
#     if (input$value2 == "Con etiqueta de datos") {
#       
#       ggplot(data_slide2()) +
#         aes(x = fct_inorder(Periodo), y = Total, colour = Zona, group=Zona,
#             text = paste("Semana: ", Periodo, 
#                          "\nÁrea: ", Zona,
#                          "\nTotal de denuncias registradas: ", Total, sep="")) +
#         geom_line(size = .8) +
#         geom_point()+ 
#         scale_y_continuous(labels = scales::comma) +
#         scale_color_manual(values = c(
#           AMG = "#7E3794",
#           Interior = "#C91682",
#           `Puerto Vallarta` = "#D98CBC")) +
#         geom_text(aes(label=Total, colour=Zona),
#                   vjust=1, hjust=1, size= 4, angle=90)+
#         #geom_text_repel()+
#         labs(title = "Denuncias por violencia familiar",
#              caption = "Fuente: Elaboración propia con datos de la Fiscalía del estado de Jalisco.
# Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración.",
#              x="Semana", y="Total de denuncias", fill="Zona") +
#         theme_minimal(base_size=15)+
#         theme(text=element_text(size=12,  family="Nutmeg"))+
#         theme(plot.title = element_text(size = 16L, hjust = 0.5), 
#               plot.caption = element_text(size = 12L, hjust = 0))+
#         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))->grafico2
#       
#       
#     }
#     
#     ggplotly(grafico2, tooltip = "text")
#     
#   })
#   
#   
#   
#   #Botones para slide 3    
#   output$semana_3 <- renderUI({
#     selectInput("Semana",
#                 label =  "Seleccione semana",
#                 choices = sort(unique(slide3$Semana)),
#                 multiple = T)
#   })
#   
#   
#   output$sexo_3 <- renderUI({
#     selectInput("Sexo",
#                 label =  "Selecciona el sexo",
#                 choices = sort(unique(slide3$Sexo)),
#                 multiple = T)
#   })
#   
#   
#   
#   #base reactiva para slide 3
#   data_slide3 <- reactive({
#     
#     slide3 %>%
#       filter(if(!is.null(input$semana_3))         Semana %in% input$semana_3     else Semana != "",
#              if(!is.null(input$sexo_3))             Sexo %in% input$sexo_3       else Sexo != "",)
#   })
#   
#   
#   output$grafico3 <- renderPlotly ({
#     
#     if (input$value3 == "Sin etiqueta de datos") {
#       ggplot(data_slide3()) +
#         aes(x =fct_inorder(Periodo), y = Total, colour = Sexo, group=Sexo,
#             text = paste("Semana: ", Periodo, 
#                          "\nSexo: ", Sexo,
#                          "\nTotal de denuncias registradas: ", Total, sep="")) +
#         geom_point() +
#         geom_line() +
#         scale_y_continuous(labels = scales::comma) +
#         scale_color_manual(
#           values = c(
#             Hombres = "#C91682",
#             Mujeres = "#7E3794")) +
#         labs(title = "Denuncias por violencia familiar desagregado.", 
#              caption = "Fuente: Elaboración propia con datos de la Fiscalía del estado de Jalisco. 
# Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración.", 
#              fill = "Sexo:",
#              x="Fecha", y="Total") +
#         theme_minimal()+
#         theme(text=element_text(size=12,  family="Nutmeg"))+
#         theme(plot.title = element_text(size = 16L, hjust = 0.5), 
#               plot.caption = element_text(size = 12L, hjust = 0))+
#         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))->grafico3
#       
#     }  
#     
#     if (input$value3 == "Con etiqueta de datos") {
#       ggplot(data_slide3()) +
#         aes(x =fct_inorder(Periodo), y = Total, colour = Sexo, group=Sexo,
#             text = paste("Semana: ", Periodo, 
#                          "\nSexo: ", Sexo,
#                          "\nTotal de denuncias registradas: ", Total, sep="")) +
#         geom_point() +
#         geom_line() +
#         scale_y_continuous(labels = scales::comma) +
#         scale_color_manual(
#           values = c(
#             Hombres = "#C91682",
#             Mujeres = "#7E3794")) +
#         geom_text(aes(label=Total, colour=Sexo),
#                   vjust=1, hjust=1, size= 4)+
#         labs(title = "Denuncias por violencia familiar desagregado.", 
#              caption = "Fuente: Elaboración propia con datos de la Fiscalía del estado de Jalisco. 
# Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración.", 
#              fill = "Sexo:",
#              x="Fecha", y="Total") +
#         theme_minimal()+
#         theme(text=element_text(size=12,  family="Nutmeg"))+
#         theme(plot.title = element_text(size = 16L, hjust = 0.5), 
#               plot.caption = element_text(size = 12L, hjust = 0))+
#         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))->grafico3
#       
#       
#     }  
#     
#     ggplotly(grafico3, tooltip="text") 
#     
#   })
#   
#   
#   
#   #Botones para slide 4  
#   
#   output$anio_4 <- renderUI({
#     selectInput("Anio",
#                 label =  "Seleccione los anios",
#                 choices = sort(unique(slide4$Anio)),
#                 multiple = T)
#   })
#   
#   #base reactiva para slide 4
#   data_slide4 <- reactive({
#     
#     slide4 %>%
#       filter(if(!is.null(input$anio_4))         Anio %in% input$anio_4     else Anio != "")
#   })
#   
#   
#   output$grafico4 <- renderPlotly ({
#     
#     if (input$value4 == "Sin etiqueta de datos") {
#       
#       ggplot(data_slide4()) +
#         aes(x =as.factor(Anio), weight = Total,
#             text = paste("Año: ", Anio, 
#                          "\nTotal: ", scales::comma(Total,1), sep="")) +
#         geom_bar(fill = "#AF1271") +
#         scale_y_continuous(labels = scales::comma) +
#         labs(title = "Denuncias por violencia familiar (anual)", 
#              caption = "Datos al 31 diciembre de 2020.  
# Fuente: Elaboración propia con datos de la Fiscalía del estado de Jalisco.", 
#              fill = "Tipo de violencia:",
#              x="Año", y="Total") +
#         theme_minimal(base_size=25)+
#         theme(plot.title = element_text(size = 16L, hjust = 0.5), plot.caption = element_text(size = 12L, hjust = 0))+
#         theme(text=element_text(size=12,family="Nutmeg"))->grafico4
#       
#     }
#     
#     if (input$value4 == "Con etiqueta de datos") {
#       
#       ggplot(data_slide4()) +
#         aes(x =as.factor(Anio), y = Total,
#             text = paste("Año: ", Anio, 
#                          "\nTotal: ", scales::comma(Total,1), sep="")) +
#         geom_col(fill = "#AF1271", position = "dodge") +
#         scale_y_continuous(labels = scales::comma) +
#         # geom_text(aes(label=scales::comma(Total,1)),
#         #           vjust=1, hjust=1, size= 4)+
#         geom_text(aes(label=scales::comma(Total,1)), position = position_dodge(0.9),
#                   vjust = -1, hjust=.5, size= 4, color="black")+
#         labs(title = "Denuncias por violencia familiar (anual)", 
#              caption = "Datos al 31 diciembre de 2020.  
# Fuente: Elaboración propia con datos de la Fiscalía del estado de Jalisco.", 
#              fill = "Tipo de violencia:",
#              x="Año", y="Total") +
#         theme_minimal(base_size=25)+
#         theme(plot.title = element_text(size = 16L, hjust = 0.5), plot.caption = element_text(size = 12L, hjust = 0))+
#         theme(text=element_text(size=12,family="Nutmeg"))->grafico4
#       
#       
#     }
#     
#     ggplotly(grafico4, tooltip = "text")
#     
#   })
#   
#   ###########
#   
#   #Botones para slide 5  
#   
#   output$anio_5 <- renderUI({
#     selectInput("Anio",
#                 label =  "Seleccione los anios",
#                 choices = sort(unique(slide5$Anio)),
#                 multiple = T)
#   })
#   
#   output$mes_5 <- renderUI({
#     selectInput("Mes",
#                 label =  "Seleccione mes",
#                 choices = unique(slide5$Mes),
#                 multiple = T)
#   })
#   
#   
#   #base reactiva para slide 5
#   data_slide5 <- reactive({
#     
#     slide5 %>%
#       filter(if(!is.null(input$anio_5))         Anio %in% input$anio_5     else Anio != "",
#              if(!is.null(input$mes_5))         Anio %in% input$mes_5     else Mes != "",)
#   })
#   
#   # slide5 <- slide5 %>%
#   #   mutate(Mes=factor(Mes,
#   #                     levels=c ("Enero", "Febrero", "Marzo", "Abril", "Mayo",
#   #                               "Junio", "Julio", "Agosto", "Septiembre",
#   #                               "Octubre", "Noviembre", "Diciembre")),
#   #          Anio=factor(Anio,
#   #                      levels=c("2021", "2020", "2019")))
#   
#   
#   
#   output$grafico5 <- renderPlotly ({
#     
#     if (input$value5 == "Sin etiqueta de datos") {
#       #     
#       #   ggplot(data_slide5())+
#       #     # 
#       #     # ggplot(slide5)+
#       #        aes(x =as.POSIXct(Periodo), y = Total, colour =as.factor(Anio)) +
#       #     geom_line(
#       #       size = 0.8) +
#       #     geom_point()+
#       #     scale_y_continuous(labels = scales::comma) +
#       #     scale_color_manual(values = c(
#       #       `2019` = "#7E3794",
#       #       `2020` = "#C91682",
#       #       `2021` = "#D98CBC")) +
#       #     labs(title = "Denuncias por violencia familiar (comparativo 2019-2020-2021)",
#       #          caption = "Información con corte al 30 de abril 2021.
#       # Fuente:Elaboración propia con datos de la Fiscalía del estado de Jalisco (2020-2021) y del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública 2019 (SESNSP).
#       # Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración.
#       # La unidad de medida son carpetas de investigación.",
#       #          x= "Fecha",
#       #          y= "Número de denuncias",
#       #          fill = "Año") +
#       #     theme_minimal()+  
#       #     theme(plot.title = element_text(size = 12L, hjust = 0.5), 
#       #           plot.caption = element_text(size = 12L, hjust = 0))+
#       #     theme(text=element_text(size=12, family="Nutmeg"))->grafico5
#       #   
#       
#       ggplot(data_slide5())+ 
#         aes(x=fct_inorder(Mes),  y=Total, fill=as.factor(Anio),
#             text = paste("Año: ", Anio, 
#                          "\nMes: ", Mes,
#                          "\nTotal: ", scales::comma(Total,1), sep="")) + 
#         geom_bar(width=0.6, stat="identity") + 
#         scale_y_continuous(labels = scales::comma) +
#         scale_fill_manual(values = list(
#           `2019`="#C91682", 
#           `2020` = "#7E3794", 
#           `2021`="#D98CBC"))+
#         labs(title = "Denuncias por violencia familiar (comparativo 2019-2020-2021)", 
#              caption = "Información con corte al 30 de abril 2021.
# Fuente:Elaboración propia con datos de la Fiscalía del estado de Jalisco (2020-2021) y del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública 2019 (SESNSP). 
# Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. 
# La unidad de medida son carpetas de investigación.", 
#              x= "Mes", 
#              y= "Número de denuncias", 
#              fill = "Año") +
#         theme_minimal()+
#         theme(plot.title = element_text(size = 16L, hjust = 0.5, angle = 45), 
#               plot.caption = element_text(size = 12L, hjust = 0))+
#         theme(text=element_text(size=12,family="Nutmeg"),
#               axis.text.x = element_text(angle = 40))->grafico5
#       
#       
#     }
#     
#     
#     if (input$value5 == "Con etiqueta de datos") {
#       
#       ggplot(data_slide5())+ 
#         aes(x=fct_inorder(Mes),  y=Total, fill=as.factor(Anio),
#             text = paste("Año: ", Anio, 
#                          "\nMes: ", Mes,
#                          "\nTotal: ", scales::comma(Total,1), sep="")) + 
#         geom_bar(width=0.6, stat="identity") + 
#         scale_y_continuous(labels = scales::comma) +
#         scale_fill_manual(values = list(
#           `2019`="#C91682", 
#           `2020` = "#7E3794", 
#           `2021`="#D98CBC"))+
#         geom_text(aes(label=scales::comma(Total,1)), position = position_stack(vjust = 0.5),
#                   vjust=1, hjust=.5, size= 4, color="white")+
#         labs(title = "Denuncias por violencia familiar (comparativo 2019-2020-2021)", 
#              caption = "Información con corte al 30 de abril 2021.
# Fuente:Elaboración propia con datos de la Fiscalía del estado de Jalisco (2020-2021) y del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública 2019 (SESNSP). 
# Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. 
# La unidad de medida son carpetas de investigación.", 
#              x= "Mes", 
#              y= "Número de denuncias", 
#              fill = "Año") +
#         theme_minimal()+
#         theme(plot.title = element_text(size = 16L, hjust = 0.5, angle = 45), 
#               plot.caption = element_text(size = 12L, hjust = 0))+
#         theme(text=element_text(size=12,family="Nutmeg"),
#               axis.text.x = element_text(angle = 40))->grafico5
#       
#       
#       
#     }
#     
#     ggplotly(grafico5, tooltip = "text")
#     
#   })
#   
#   
#   
#   
#   
#   #Grafico slide 6
#   ggplot(slide6) +
#     aes(x = Semana, y = Total, colour = Zona, 
#         text = paste("Número de semana: ", Semana, 
#                      "\nZona: ", Zona,
#                      "\nTotal de reportes: ", Total, sep="")) +
#     geom_line(size = 0.5) +
#     geom_point()+
#     scale_y_continuous(labels = scales::comma) +
#     scale_color_manual(values = c(
#       AMG = "#7E3794",
#       Estatal = "#C91682",
#       Interior = "#D98CBC")) +
#     labs(title="Total de reportes 911 durante COVID - 19.",
#          x="Semana",
#          y="Total",
#          fill="Rango de edad",
#          caption = "Fuente: elaboración propia con datos de Escudo Urbano C5.
# Los reportes son la sumatoria de tres categorías señaladas en el Catálogo Nacional: Violencia contra la mujer, violencia de pareja y violencia familiar. ") +
#     theme_minimal()+
#     theme(plot.title = element_text(size = 16L, hjust = 0.5), 
#           plot.caption = element_text(size = 12L, hjust = 0))+
#     theme(text=element_text(size=12,family="Nutmeg"),
#           axis.text.x = element_text(angle = 40))
#   
#   
#   #
#   #
#   #
#   #
#   #
#   #
#   #
#   #
#   
# 
#   #Botones para la plataforma
#   output$año_vic1 <- renderUI({
#     selectInput("año",
#                 label =  "Seleccione año",
#                 choices = sort(unique(victimas_nacional$año)),
#                 multiple = T)
#   })
# 
# 
#   output$tipo.de.delito_vic1 <- renderUI({
#     selectInput("tipo.de.delito",
#                 label =  "Selecciona el delito",
#                 choices = sort(unique(victimas_nacional$delito)),
#                 multiple = T)
#   })
# 
# 
# 
# 
#   #base reactiva para que sea dinamica
#   data_victimas <- reactive({
# 
#     victimas_nacional %>%
#       filter(if(!is.null(input$año))                       año %in% input$año                  else año != "",
#              if(!is.null(input$entidad))               entidad %in% input$entidad              else entidad != "",
#              if(!is.null(input$tipo.de.delito)) tipo.de.delito %in% input$tipo.de.delito       else tipo.de.delito != "",)
#   })
# 
# 
# 
#   #Gráfico con reactivo para que el mapa se mueva conforme al usuario
#   output$grafico_vic1 <- renderPlotly ({
#     # data_victimas <- data()
# 
# 
#     ggplot(data_victimas()) +
#       aes(x = as.factor(año), weight = tot_acu,
#           fill = tipo.de.delito) +
#       geom_bar(position = "dodge") +  scale_y_continuous(labels = scales::comma) +
#       scale_fill_manual(values = c("purple","#d10096")) +
#       labs(title="Total de víctimas de delitos (absolutos).",
#            x="Año",
#            y="Total",
#            fill="Delitos",
#            caption = "Elaborado con los datos del SESPN.") +
#       theme_minimal()+
#       theme(text=element_text(size=12,  family="Nutmeg"))->grafico_vic1
# 
# 
#     ggplotly(grafico_vic1)  #añadir ggplotly
#   })
# 
# 
#   #gráfico con reractivo 2
# 
#   output$grafico_vic2 <- renderPlotly ({
# 
#     ggplot(data_victimas()) +
#       aes(x =as.factor(año), fill=rango.de.edad, weight = tot_acu) +
#       geom_bar(position = "dodge") +  scale_y_continuous(labels = scales::comma) +
#       scale_fill_manual(values = c("purple","#d10096", "plum")) +
#       labs(title="Total de víctimas de delitos según edad y sexo (absolutos).",
#            x="Año",
#            y="Total",
#            fill="Rango de edad",
#            caption = "Elaborado con los datos del SESPN.") +
#       theme_minimal()+
#       theme(text=element_text(size=12,  family="Nutmeg"))
#   })
# 
# 
# 
#   output$slide19 <- renderPlotly({
#     plotly::ggplotly(grafico19, tooltip="value", dynamicTicks = TRUE)
# 
#   }) #añadir ggplotly
# 
# 
# 
# }
# 
# 
# shinyApp(ui = ui, server = server)
# 
# 
# 
# # #slide7
# # 
# # ggplot(slide7) +
# #   aes(x = Fecha, y = Total, colour=`Tipo de violencia`, 
# #       # text = paste( "Fecha de la semana: ", Fecha,
# #       #              "\nTipo de violencia: ", `Tipo de violencia`,
# #       #              "\nTotal de llamadas: ", comma(Total), sep="")
# #       ) +
# #   geom_line(size = 0.8) +
# #   geom_point()+
# #   scale_y_continuous(labels = scales::comma) +
# #   scale_color_manual(
# #     values = c(
# #       `Violencia contra la mujer` = "#D98CBC",
# #       `Violencia de Pareja` = "#C91682",
# #       `Violencia Familiar` = "#7E3794")) +
# #   labs(title = "Registro mensual de llamadas 911 relacionadas con violencia de género.",
# #        caption = "Información con corte al 30 de abril 2021.
# # Fuente: elaboración propia con datos de Escudo Urbano C5.",
# #        x= "Fecha",
# #        y= "Número de denuncias",
# #        fill = "Año") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"))
# # 
# # #slide8
# # ggplot(slide8) +
# #   aes(x =as.factor(Anio), weight = Total, fill = `Tipo de violencia`,
# #       text = paste( "Año: ", Anio,
# #                     "\nTipo de violencia: ", `Tipo de violencia`,
# #                     "\nTotal de llamadas: ", comma(Total), sep="")) +
# #   geom_bar() +
# #   scale_y_continuous(labels = scales::comma) +
# #   scale_fill_manual(
# #     values = list(
# #       `Violencia contra mujer` = "#D98CBC",
# #       `Violencia de pareja` = "#C91682",
# #       `Violencia familiar` = "#7E3794")) +
# #   labs(title = "Registro anual de llamadas al 911 relacionadas con violencia de género.",
# #        caption = "Con datos al 28 de febrero del 2021.
# # Fuente: elaboración propia con datos de Escudo Urbano C5.",
# #        x= "Año",
# #        y= "Número de denuncias",
# #        fill = "Tipo de violencia") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"))
# # 
# # 
# # #slide9
# # ggplot(slide9) +
# #   aes(x = Semana, y = Total, colour = Zona) +
# #   geom_line(size = 0.8) +
# #   geom_point()+
# #   scale_color_manual(values = c(
# #     AMG = "#7E3794",
# #     Interior = "#C91682",
# #     `Puerto Vallarta`= "#D98CBC")) +  
# #   labs(title = "Medidas de protección emitidas.",
# #        caption = "Datos al 05 de marzo, 2021.
# # Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco.",
# #        x= "Fecha",
# #        y= "Total de medidas",
# #        fill = "Zona") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"))
# # 
# # 
# # #slide10
# # slide10 <- slide10 %>% mutate(Fecha=as.Date(Fecha, "%d/%m/%Y"))
# # slide10 <- slide10 %>% mutate(ym=as.yearmon(Fecha))
# # 
# # ggplot(slide10) +
# #   aes(x = ym, y = Total, colour=as.character(Anio)) +
# #   geom_line(size = 0.8) +
# #   geom_point()+
# #   scale_color_manual(values = c(
# #     `2019` = "#7E3794",
# #     `2020` = "#C91682",
# #     `2021`= "#D98CBC")) +  
# #   labs(title = "Abuso sexual infantil (ASI) 2019 - 2021.",
# #        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco 2019- febrero 2021. 
# # Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. La unidad de medida son
# # carpetas de investigación",
# #        x= "Fecha",
# #        y= "Total de carpetas de investigación",
# #        fill = "Año") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"))
# # 
# # 
# # ggplot(slide10) +
# #   aes(x =fct_inorder(Mes), fill =as.factor(Anio), weight = Total) +
# #   geom_bar(position = "dodge") +
# #   scale_fill_manual(values = c(
# #     `2019` = "#7E3794",
# #     `2020` = "#C91682",
# #     `2021`= "#D98CBC")) +  
# #   labs(title = "Abuso sexual infantil (ASI) 2019 - 2021.",
# #        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco 2019- febrero 2021. 
# # Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. La unidad de medida son
# # carpetas de investigación",
# #        x= "Fecha",
# #        y= "Total de carpetas de investigación",
# #        fill = "Año") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"))
# # 
# # 
# # #slide11
# # slide11 <- slide11 %>% mutate(Fecha=as.Date(Fecha, "%d/%m/%Y"))
# # slide11 <- slide11 %>% mutate(ym=as.yearmon(Fecha))
# # 
# # 
# # ggplot(slide11) +
# #   aes(x = ym, y = Total, colour=as.character(Anio)) +
# #   geom_line(size = 0.8) +
# #   geom_point()+
# #   scale_color_manual(values = c(
# #     `2019` = "#7E3794",
# #     `2020` = "#C91682",
# #     `2021`= "#D98CBC")) +  
# #   labs(title = "Violación 2019 - 2021.",
# #        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco 2019 - febrero 2021. 
# # Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. La unidad de medida son carpetas de investigación.",
# #        x= "Fecha",
# #        y= "Total de carpetas de investigación",
# #        fill = "Año") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"))
# # 
# # 
# # 
# # ggplot(slide11) +
# #   aes(x =fct_inorder(Mes), fill =as.factor(Anio), weight = Total) +
# #   geom_bar(position = "dodge") +
# #   scale_fill_manual(values = c(
# #     `2019` = "#7E3794",
# #     `2020` = "#C91682",
# #     `2021`= "#D98CBC")) +  
# #   labs(title = "Violación 2019 - 2021.",
# #        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco 2019 - febrero 2021. 
# # Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. La unidad de medida son carpetas de investigación.",
# #        x= "Fecha",
# #        y= "Total de carpetas de investigación",
# #        fill = "Año") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"),
# #         axis.text.x = element_text(angle = 40))
# # 
# # #slide12
# # 
# # slide12 <- slide12 %>% mutate(Fecha=as.Date(Fecha, "%d/%m/%Y"))
# # slide12 <- slide12 %>% mutate(ym=as.yearmon(Fecha))
# # 
# # 
# # ggplot(slide12) +
# #   aes(x = ym, y = Total, colour =as.factor(Anio)) +
# #   geom_line(size = 0.8) +
# #   geom_point()+
# #   scale_y_continuous(labels = scales::comma) +
# #   scale_color_manual(values = c(
# #     `2020` = "#7E3794",
# #     `2021` = "#C91682")) +  
# #   theme_minimal()+
# #   labs(title = "Muertes violentas registradas como feminicidios en Jalisco (2015 - 2021).",
# #        caption = "*Actualizado de acuerdo a los datos abiertos del SESNSP de carpetas de investigación iniciadas por feminicidios a febrero 2021.
# # Fuente: elaboración propia con datos abiertos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP).
# # La unidad de medida son carpetas de investigación.",
# #        x= "Fecha",
# #        y= "Total de carpetas de investigación",
# #        fill = "Año") +
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"))
# # 
# # 
# # 
# # ggplot(slide12) +
# #   aes(x =fct_inorder(Mes), fill =as.factor(Anio), weight = Total) +
# #   geom_bar(position = "dodge") +
# #   scale_y_continuous(labels = scales::comma) +
# #   scale_fill_manual(values = c(
# #     `2021` = "#7E3794",
# #     `2020` = "#C91682")) +  
# #   labs(title = "Muertes violentas registradas como feminicidios en Jalisco (2015 - 2021).",
# #        caption = "*Actualizado de acuerdo a los datos abiertos del SESNSP de carpetas de investigación iniciadas por feminicidios a febrero 2021.
# # Fuente: elaboración propia con datos abiertos del Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP).
# # La unidad de medida son carpetas de investigación.",
# #        x= "Fecha",
# #        y= "Total de carpetas de investigación",
# #        fill = "Año") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"),
# #         axis.text.x = element_text(angle = 40))
# # 
# # 
# # #slide13
# # ggplot(slide13)+
# #   aes(x=as.factor(Anio), y=Total)+
# #   geom_col(fill="#7E3794")+
# #   scale_y_continuous(labels = scales::comma) +
# #   labs(title = "Muertes violentas registradas como feminicidios en Jalisco (2015 - 2021).",
# #        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco para 2020 y 2021, para 2019 con datos abiertos del Secretariado Ejecutivo
# # del Sistema de Seguridad Nacional. Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente
# # siguen en proceso de integración. La unidad de medida son víctimas.",
# #        x= "Fecha",
# #        y= "Total de carpetas de investigación",
# #        fill = "Año") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"))
# # 
# # 
# # #slide14
# # 
# # library(ggplot2)
# # 
# # ggplot(slide14) +
# #  aes(x = Fecha, y = Total, colour =Anio) +
# #  geom_line(size = 0.8) +
# #   geom_point()+
# #   # scale_color_manual(values = c(
# #   #   `2019` = "#7E3794",
# #   #   `2020` = "#C91682",
# #   #   `2021`= "#D98CBC")) +  
# #   scale_color_gradient(low = "#7E3794", high = "#D98CBC") +
# #   labs(title = "Violación 2019 - 2021.",
# #        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco 2019 - febrero 2021. 
# # Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. La unidad de medida son carpetas de investigación.",
# #        x= "Fecha",
# #        y= "Total de carpetas de investigación",
# #        fill = "Año") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"))
# # 
# # 
# # #slide16
# # ggplot(slide16) +
# #   aes(x = Fecha, y = Total, colour =Anio) +
# #   geom_line(size = 0.8) +
# #   geom_point()+
# #   # scale_color_manual(values = c(
# #   #   `2019` = "#7E3794",
# #   #   `2020` = "#C91682",
# #   #   `2021`= "#D98CBC")) +  
# #   scale_color_gradient(low = "#7E3794", high = "#D98CBC") +
# #   labs(title = "Violación 2019 - 2021.",
# #        caption = "Fuente: elaboración propia con datos de la Fiscalía del estado de Jalisco 2019 - febrero 2021. 
# # Las cifras pueden variar con el tiempo dado que son registros de carpetas de investigación que actualmente siguen en proceso de integración. La unidad de medida son carpetas de investigación.",
# #        x= "Fecha",
# #        y= "Total de carpetas de investigación",
# #        fill = "Año") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"))->g16
# # 
# # ggplotly(g16)
# # 
# # #19 y 20 mapas
# # 
# # #slide21
# # ggplot(slide21) +
# #   aes(x = Localizadas, fill = Localizadas, weight = Total) +
# #   geom_bar(fill="#7E3794") +
# #   coord_flip() +
# #   scale_y_continuous(labels = comma)+
# #   theme_minimal()+
# #   labs(title = "Localización de mujeres reportadas como desaparecidas.",
# #        caption = "Fuente: Sistema de Información sobre Víctimas de Desaparición (SISOVID). Información al 28 de febrero 2021.",
# #        x= "",
# #        y= "",
# #        fill = "Localizadas") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0),
# #         text=element_text(size=12, family="Nutmeg"))
# # 
# # 
# # #slide22
# # ggplot(slide22) +
# #   aes(x = `Desaparicion y no localizacion`, weight = Total) +
# #   geom_bar(fill="#7E3794") +
# #   coord_flip() +
# #   scale_y_continuous(labels = comma)+
# #   theme_minimal()+
# #   labs(title = "Desaparición y no localización de mujeres.",
# #        caption = "De acuerdo con la Ley, se entiende por persona no localizada, aquello en donde se presume que no hay comisión de algún delito, y en el caso de las personas desaparecidas se presume de la comisión de algún delito.
# # Fuente: Sistema de Información sobre Víctimas de Desaparición (SISOVID). Información al 28 de febrero 2021.",
# #        x= "",
# #        y= "",
# #        fill = "") +
# #   theme_minimal()+  
# #   theme(plot.title = element_text(size = 12L, hjust = 0.5), 
# #         plot.caption = element_text(size = 12L, hjust = 0))+
# #   theme(text=element_text(size=12, family="Nutmeg"))  
# # 
# # #slide23
# # ggplot(slide23) +
# #   aes(x = Edad, weight = Total) +
# #   geom_bar(fill="#7E3794") +
# #   scale_y_continuous(labels = comma)+
# #   labs(title = "Desaparición y no localización de mujeres (edad).",
# #        caption = "Fuente: Sistema de Información sobre Víctimas de Desaparición (SISOVID). Información al 28 de febrero 2021.",
# #        x= "",
# #        y= "",
# #        fill = "") +
# #   theme_minimal(base_size=25)+
# #   theme(text=element_text(size=12,  family="Nutmeg"))+
# #   theme(plot.title = element_text(size = 16L, hjust = 0.5, family = "Nutmeg"), 
# #         plot.caption = element_text(size = 12L, hjust = 0, family="Nutmeg"))+
# #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, family="Nutmeg"))
# # 
# # 
# 
# 
# 
