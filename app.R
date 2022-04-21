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
library(mxmaps)
library(stringr)
library(tm)
library(wordcloud2)
library(RColorBrewer)
library(shinydashboard)
library(wordcloud)
library(shinydashboard)
library(shiny)
library(plotly)
library(dashboardthemes)
library(shinythemes)
library(shinybusy)
#library(grDevices)

cd <- read.csv("base_cd.csv", encoding="UTF-8")


nb.cols <- 27
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)



cd$Fecha<-as.Date(cd$Fecha.de.Registro,format="%d/%m/%Y %H:%M:%S")
cd$Mes<-format(as.Date(cd$Fecha.de.Registro,format="%d/%m/%Y %H:%M:%S"), "%Y-%m")
cd$Año<-format(as.Date(cd$Fecha.de.Registro,format="%d/%m/%Y %H:%M:%S"), "%Y")

cd <-cd %>% 
  filter(Validado..por.Nosotrxs.=="Sí")%>%
  mutate(Cuatrimestre =case_when(
    Fecha >= ymd('2019-01-01') & Fecha <= ymd('2019-04-30') ~ "Ene-Abr 19",
    Fecha >= ymd('2019-05-01') & Fecha <= ymd('2019-08-31') ~ "May-Ago 19",
    Fecha >= ymd('2019-09-01') & Fecha <= ymd('2019-12-31') ~ "Sep-Dic 19",
    Fecha >= ymd('2020-01-01') & Fecha <= ymd('2020-04-30') ~ "Ene-Abr 20",
    Fecha >= ymd('2020-05-01') & Fecha <= ymd('2020-08-31') ~ "May-Ago 20",
    Fecha >= ymd('2020-09-01') & Fecha <= ymd('2020-12-31') ~ "Sep-Dic 20",
    Fecha >= ymd('2021-01-01') & Fecha <= ymd('2021-04-30') ~ "Ene-Abr 21",
    Fecha >= ymd('2021-05-01') & Fecha <= ymd('2021-08-31') ~ "May-Ago 21",
    Fecha >= ymd('2021-09-01') & Fecha <= ymd('2021-12-31') ~ "Sep-Dic 21",

    Fecha >= ymd('2022-01-01') & Fecha <= ymd('2022-04-30') ~ "Ene - Abr 2022",)) %>% 
  mutate(Cuatrimestre=factor(Cuatrimestre,
                             levels=c("Ene-Abr 19","May-Ago 19", "Sep-Dic 19",
                                      "Ene-Abr 20","May-Ago 20", "Sep-Dic 20",
                                      "Ene-Abr 21","May-Ago 21", "Sep-Dic 21")))

cd$Entidad[cd$Entidad=="Coahuila"] <- "Coahuila de Zaragoza"
cd$Entidad[cd$Entidad=="Michoacán"] <- "Michoacán de Ocampo"
cd$Entidad[cd$Entidad=="Estado de México"] <- "México"
cd$Entidad[cd$Entidad=="Veracruz"] <- "Veracruz de Ignacio de la Llave"



############################################################

#---------------------------- Total ------------------------

cd %>% 
  group_by(Entidad, Año, Cuatrimestre) %>% 
  filter(!(Entidad %in% c(NA, "NA",""))) %>% 
  summarise(Total=n())->cd_total


cd %>% 
  filter(Validado..por.Nosotrxs.=="Sí") %>% 
  group_by(Año, Cuatrimestre) %>% 
  summarise(Total=n()) -> Nacional_total

Entidad<- c("Acumulado nacional")
cbind(Entidad, Nacional_total)->Nacional_total
names(Nacional_total)[names(Nacional_total) == "...1"] <- "Entidad"

rbind(cd_total, Nacional_total)->cd_total




#---------------------------- Informante ------------------------

names(cd)[names(cd) == "Tipo.de.informante"] <- "Tipo de informante"
cd$`Tipo de informante`[cd$`Tipo de informante`=="paciente"] <- "Paciente"
cd$`Tipo de informante`[cd$`Tipo de informante`=="profesional"] <- "Profesional"



#--------------------------MAPA---------------------------------

cd_1 <- cd %>% group_by(Entidad) %>% summarise(value=n())
data("df_mxstate_2020")
cd_1<-merge(cd_1, df_mxstate_2020, by.x="Entidad", by.y="state_name_official")

cd_1<-cd_1 %>% mutate(text = paste("Entidad: ", state_name,
                                   "n/ Total de reportes: ", value  
                                   , sep=""))





cd_1_19 <- cd %>% filter(Año==2019) %>%  group_by(Entidad) %>% summarise(value=n())
data("df_mxstate_2020")
cd_1_19<-merge(cd_1_19, df_mxstate_2020, by.x="Entidad", by.y="state_name_official")

cd_1_19<-cd_1_19 %>% mutate(text = paste("Entidad: ", state_name,
                                   "n/ Total de reportes: ", value  
                                   , sep=""))

#-----------------------------------------------------------

cd_1_20 <- cd %>% filter(Año==2020) %>%  group_by(Entidad) %>% summarise(value=n())
data("df_mxstate_2020")
cd_1_20<-merge(cd_1_20, df_mxstate_2020, by.x="Entidad", by.y="state_name_official")

cd_1_20<-cd_1_20 %>% mutate(text = paste("Entidad: ", state_name,
                                         "n/ Total de reportes: ", value  
                                         , sep=""))

#-----------------------------------------------------------

cd_1_21 <- cd %>% filter(Año==2021) %>%  group_by(Entidad) %>% summarise(value=n())
data("df_mxstate_2020")
cd_1_21<-merge(cd_1_21, df_mxstate_2020, by.x="Entidad", by.y="state_name_official")

cd_1_21<-cd_1_21 %>% mutate(text = paste("Entidad: ", state_name,
                                         "n/ Total de reportes: ", value  
                                         , sep=""))

#-----------------------------------------------------------

cd_1_22 <- cd %>% filter(Año==2022) %>%  group_by(Entidad) %>% summarise(value=n())
data("df_mxstate_2020")
cd_1_22<-merge(cd_1_22, df_mxstate_2020, by.x="Entidad", by.y="state_name_official")

cd_1_22<-cd_1_22 %>% mutate(text = paste("Entidad: ", state_name,
                                         "n/ Total de reportes: ", value, sep=""))
###########################################################3

cd %>% 
  filter(Validado..por.Nosotrxs.=="Sí") %>% 
  group_by(Año, Cuatrimestre, `Tipo de informante`) %>% 
  summarise(Total=n()) -> Nacional

Entidad<- c("Acumulado nacional")
cbind(Entidad, Nacional)->Nacional
names(Nacional)[names(Nacional) == "...1"] <- "Entidad"


cd %>% 
  group_by(Año, Cuatrimestre, Entidad, `Tipo de informante`) %>% 
  summarise(Total=n())->cd_reportes



#----------------------

cd %>% 
  filter(Validado..por.Nosotrxs.=="Sí") %>% 
  group_by(Año, Cuatrimestre, `Tipo de informante`) %>% 
  summarise(Total=n()) -> Informante


Entidad<- c("Acumulado nacional")
`Tipo de informante`<- c("Total")


cbind(Informante,Entidad,`Tipo de informante`)->Informante
names(Informante)[names(Informante) == "...4"] <- "Entidad"
names(Informante)[names(Informante) == "...5"] <- "Entidad"

Informante %>% select(Año, Cuatrimestre, Entidad, `Tipo de informante`, Total)->Informante



rbind(Informante,cd_reportes)->cd_2

cd_2 %>% filter(Año %in% c(2019, 2020, 2021))->cd_2






# --------------------------Insumos-----------------------------------
names(cd)[names(cd) == "Tipo.de.Medicina"] <- "Tipo de insumo"
cd$`Tipo de insumo`[cd$`Tipo de insumo`=="material"] <- "Material de curación"
cd$`Tipo de insumo`[cd$`Tipo de insumo`=="Material de Curación"] <- "Material de curación"
cd$`Tipo de insumo`[cd$`Tipo de insumo`=="Insumo"] <- "Material de curación"
cd$`Tipo de insumo`[cd$`Tipo de insumo`=="medicamento"] <- "Medicamento"
cd$`Tipo de insumo`[cd$`Tipo de insumo`=="vacuna"] <- "Vacuna"
cd$`Tipo de insumo`[cd$`Tipo de insumo`=="otro"] <- "Otro"


cd %>% 
  filter(Validado..por.Nosotrxs.=="Sí") %>% 
  group_by(Año, Cuatrimestre, `Tipo de insumo`) %>% 
  summarise(Total=n()) -> Nacional_insumo

Entidad_insumo<- c("Acumulado nacional")
cbind(Entidad_insumo, Nacional_insumo)->Nacional_insumo
names(Nacional_insumo)[names(Nacional_insumo) == "...1"] <- "Entidad"


cd %>% 
  filter(!Entidad %in% c("", "NA", NA)) %>% 
  group_by(Entidad, Año, Cuatrimestre,`Tipo de insumo`) %>% 
  summarise(Total=n())->cd_insumo

rbind(Nacional_insumo, cd_insumo)->cd_insumo




#################################################################
#                         Instituciones                         #
#################################################################

cd %>% 
  filter(Año %in% c(2020, 2021),
         !(Institución..s.CLUES. %in% c("NA", NA, NA_character_, "")),
         Validado..por.Nosotrxs.=="Sí") %>% 
  mutate(Institución=case_when(  
    Institución..s.CLUES.=="ISSSTE" ~ "ISSSTE",
    Institución..s.CLUES.=="IMSS" ~ "IMSS",
    Institución..s.CLUES.=="INSABI (Secretaría de Salud)" ~ "INSABI (Secretaría de Salud)",
    Institución..s.CLUES.=="PEMEX" ~ "Otro",
    Institución..s.CLUES.=="Secretaría de Marina" ~ "Otro",
    Institución..s.CLUES.=="Servicios Médicos Estatales" ~ "Otro",
    Institución..s.CLUES.=="PRIVADOS" ~ "Privado",
    Institución..s.CLUES.=="FISCALIA GENERAL DEL ESTADO" ~ "Otro",
    Institución..s.CLUES.=="EN TODOS LADOS" ~ "Otro",
    Institución..s.CLUES.=="IMSS-BIENESTAR" ~ "IMSS-BIENESTAR",
    Institución..s.CLUES.=="DIF" ~ "Otro",
    Institución..s.CLUES.=="SEDENA" ~ "Otro",
    Institución..s.CLUES.=="Servicios Médicos Universitarios" ~ "Otro",
    Institución..s.CLUES.=="Servicios Médicos Municipales" ~ "Otro",
    Institución..s.CLUES.=="Cruz Roja" ~ "Otro",
    Institución..s.CLUES.=="Secretaría de Comunicaciones y Transportes" ~ "Otro"
    )) %>%
  group_by(Año, Cuatrimestre, Institución) %>% 
  summarise(Reportes=n())-> Nacional_2

Entidad_2<- c("Acumulado nacional")

cbind(Entidad_2, Nacional_2)->Nacional_2
names(Nacional_2)[names(Nacional_2) == "...1"] <- "Entidad"


cd %>% 
  filter(Año %in% c(2020, 2021),
         !(Institución..s.CLUES. %in% c("NA", NA, NA_character_, "")),
         Validado..por.Nosotrxs.=="Sí") %>% 
  mutate(Institución=case_when(  
    Institución..s.CLUES.=="ISSSTE" ~ "ISSSTE",
    Institución..s.CLUES.=="IMSS" ~ "IMSS",
    Institución..s.CLUES.=="INSABI (Secretaría de Salud)" ~ "INSABI (Secretaría de Salud)",
    Institución..s.CLUES.=="PEMEX" ~ "Otro",
    Institución..s.CLUES.=="Secretaría de Marina" ~ "Otro",
    Institución..s.CLUES.=="Servicios Médicos Estatales" ~ "Otro",
    Institución..s.CLUES.=="PRIVADOS" ~ "Privado",
    Institución..s.CLUES.=="FISCALIA GENERAL DEL ESTADO" ~ "Otro",
    Institución..s.CLUES.=="EN TODOS LADOS" ~ "Otro",
    Institución..s.CLUES.=="IMSS-BIENESTAR" ~ "IMSS-BIENESTAR",
    Institución..s.CLUES.=="NA" ~ "NA",
    Institución..s.CLUES.=="DIF" ~ "Otro",
    Institución..s.CLUES.=="SEDENA" ~ "Otro",
    Institución..s.CLUES.=="Servicios Médicos Universitarios" ~ "Otro",
    Institución..s.CLUES.=="Servicios Médicos Municipales" ~ "Otro",
    Institución..s.CLUES.=="Cruz Roja" ~ "Otro",
    Institución..s.CLUES.=="Secretaría de Comunicaciones y Transportes" ~ "Otro")) %>% 
  group_by(Entidad, Año, Cuatrimestre, Institución) %>% 
  summarise(Reportes = n()) %>%
  mutate(Cuatrimestre=paste0(Cuatrimestre, " "), 
  Institución=factor(Institución, 
                     levels = c("IMSS", "IMSS-BIENESTAR", "INSABI (Secretaría de Salud)", 
                                "ISSSTE", "Otro", "Privado"))) ->cd_instituciones
  


rbind(Nacional_2, cd_instituciones)->cd_instituciones



#-----------------------------mensual-------------------------------------------



cd %>% 
  filter(!(Institución..s.CLUES. %in% c("NA", NA, NA_character_, "")),
         Validado..por.Nosotrxs.=="Sí") %>% 
  mutate(Institución=case_when(  
    Institución..s.CLUES.=="ISSSTE" ~ "ISSSTE",
    Institución..s.CLUES.=="IMSS" ~ "IMSS",
    Institución..s.CLUES.=="INSABI (Secretaría de Salud)" ~ "INSABI (Secretaría de Salud)",
    Institución..s.CLUES.=="PEMEX" ~ "Otro",
    Institución..s.CLUES.=="Secretaría de Marina" ~ "Otro",
    Institución..s.CLUES.=="Servicios Médicos Estatales" ~ "Otro",
    Institución..s.CLUES.=="PRIVADOS" ~ "Privado",
    Institución..s.CLUES.=="FISCALIA GENERAL DEL ESTADO" ~ "Otro",
    Institución..s.CLUES.=="EN TODOS LADOS" ~ "Otro",
    Institución..s.CLUES.=="IMSS-BIENESTAR" ~ "IMSS-BIENESTAR",
    Institución..s.CLUES.=="DIF" ~ "Otro",
    Institución..s.CLUES.=="SEDENA" ~ "Otro",
    Institución..s.CLUES.=="Servicios Médicos Universitarios" ~ "Otro",
    Institución..s.CLUES.=="Servicios Médicos Municipales" ~ "Otro",
    Institución..s.CLUES.=="Cruz Roja" ~ "Otro",
    Institución..s.CLUES.=="Secretaría de Comunicaciones y Transportes" ~ "Otro")) %>%
  # mutate(Mes=factor(Mes,
  #                            levels=c("enero 2019", "febrero 2019", "marzo 2019",
  #                                     "abril 2019", "mayo 2019", "junio 2019", "julio 2019",
  #                                     "agosto 2019","septiembre 2019","octubre 2019", "noviembre 2019",
  #                                     "diciembre 2019", "enero 2020", "febrero 2020","marzo 2020",
  #                                     "abril 2020", "mayo 2020", "junio 2020", "julio 2020","agosto 2020",
  #                                     "septiembre 2020", "octubre 2020", "noviembre 2020","diciembre 2020",
  #                                     "enero 2021", "febrero 2021", "marzo 2021", "abril 2021","mayo 2021",
  #                                     "junio 2021", "julio 2021", "agosto 2021", "septiembre 2021",
  #                                     "octubre 2021", "noviembre 2021", "diciembre 2021", "enero 2022",
  #                                     "febrero 2022","marzo 2022"))) %>% 
  group_by(Año, Mes, Institución) %>% 
  summarise(Reportes=n())-> Nacional_2_mensual

Entidad_2_mensual<- c("Acumulado nacional")

cbind(Entidad_2_mensual, Nacional_2_mensual)->Nacional_2_mensual
names(Nacional_2_mensual)[names(Nacional_2_mensual) == "...1"] <- "Entidad"


cd %>% 
  filter(#Año %in% c(2020, 2021),
         !(Institución..s.CLUES. %in% c("NA", NA, NA_character_, "")),
         Validado..por.Nosotrxs.=="Sí") %>% 
  mutate(Institución=case_when(  
    Institución..s.CLUES.=="ISSSTE" ~ "ISSSTE",
    Institución..s.CLUES.=="IMSS" ~ "IMSS",
    Institución..s.CLUES.=="INSABI (Secretaría de Salud)" ~ "INSABI (Secretaría de Salud)",
    Institución..s.CLUES.=="PEMEX" ~ "Otro",
    Institución..s.CLUES.=="Secretaría de Marina" ~ "Otro",
    Institución..s.CLUES.=="Servicios Médicos Estatales" ~ "Otro",
    Institución..s.CLUES.=="PRIVADOS" ~ "Privado",
    Institución..s.CLUES.=="FISCALIA GENERAL DEL ESTADO" ~ "Otro",
    Institución..s.CLUES.=="EN TODOS LADOS" ~ "Otro",
    Institución..s.CLUES.=="IMSS-BIENESTAR" ~ "IMSS-BIENESTAR",
    Institución..s.CLUES.=="NA" ~ "NA",
    Institución..s.CLUES.=="DIF" ~ "Otro",
    Institución..s.CLUES.=="SEDENA" ~ "Otro",
    Institución..s.CLUES.=="Servicios Médicos Universitarios" ~ "Otro",
    Institución..s.CLUES.=="Servicios Médicos Municipales" ~ "Otro",
    Institución..s.CLUES.=="Cruz Roja" ~ "Otro",
    Institución..s.CLUES.=="Secretaría de Comunicaciones y Transportes" ~ "Otro")) %>% 
  group_by(Entidad, Año, Mes, Institución) %>% 
  # mutate(Mes=factor(Mes,
  #                   levels=c("enero 2019", "febrero 2019", "marzo 2019",
  #                            "abril 2019", "mayo 2019", "junio 2019", "julio 2019",
  #                            "agosto 2019","septiembre 2019","octubre 2019", "noviembre 2019",
  #                            "diciembre 2019", "enero 2020", "febrero 2020","marzo 2020",
  #                            "abril 2020", "mayo 2020", "junio 2020", "julio 2020","agosto 2020",
  #                            "septiembre 2020", "octubre 2020", "noviembre 2020","diciembre 2020",
  #                            "enero 2021", "febrero 2021", "marzo 2021", "abril 2021","mayo 2021",
  #                            "junio 2021", "julio 2021", "agosto 2021", "septiembre 2021",
  #                            "octubre 2021", "noviembre 2021", "diciembre 2021", "enero 2022",
  #                            "febrero 2022","marzo 2022" ))) %>% 
  summarise(Reportes = n()) %>%
  mutate(Mes=paste0(Mes, " "), 
         Institución=factor(Institución, 
                            levels = c("IMSS", "IMSS-BIENESTAR", "INSABI (Secretaría de Salud)", 
                                       "ISSSTE", "Otro", "Privado"))) ->cd_instituciones_mensual



rbind(Nacional_2_mensual, cd_instituciones_mensual)->cd_mensual_inst



#######################################################################
#                               Patología                             #
#######################################################################


cd %>% 
  filter(Validado..por.Nosotrxs.=="Sí",
         Año %in% c(2019,2020, 2021)) %>% 
  mutate(Año=case_when(
    Año==2019~"Año 2019",
    Año==2020~"Año 2020",
    Año==2021~"Año 2021"),
    Año=ordered(Año,
                levels=c("Año 2019","Año 2020", "Año 2021"))) %>% 
  group_by(Año, Padecimiento) %>% 
  summarise(Total=n())-> Nacional_3

Entidad_3<- c("Acumulado nacional")


cbind(Entidad_3, Nacional_3)->Nacional_3
names(Nacional_3)[names(Nacional_3) == "...1"] <- "Entidad"


cd_patologia<- cd %>% 
  filter(Año %in% c(2019, 2020, 2021),
         Validado..por.Nosotrxs.=="Sí",
         !(Entidad %in% c("NA", NA, NA_character_, ""))) %>% 
  group_by(Entidad, Año, Padecimiento) %>% 
  summarise(Total = n()) %>% 
  mutate(Año=case_when(
    Año==2019~"Año 2019",
    Año==2020~"Año 2020",
    Año==2021~"Año 2021"),
    Año=ordered(Año,
                levels=c("Año 2019","Año 2020", "Año 2021")))



rbind(Nacional_3, cd_patologia)->cd_patologia





#-----------------------------mensual-------------------------------------------



cd %>% 
  filter(!(Entidad %in% c("NA", NA, NA_character_, "")),
         Validado..por.Nosotrxs.=="Sí") %>% 
  # mutate(Mes=factor(Mes,
  #                   levels=c("enero 2019", "febrero 2019", "marzo 2019",
  #                            "abril 2019", "mayo 2019", "junio 2019", "julio 2019",
  #                            "agosto 2019","septiembre 2019","octubre 2019", "noviembre 2019",
  #                            "diciembre 2019", "enero 2020", "febrero 2020","marzo 2020",
  #                            "abril 2020", "mayo 2020", "junio 2020", "julio 2020","agosto 2020",
  #                            "septiembre 2020", "octubre 2020", "noviembre 2020","diciembre 2020",
  #                            "enero 2021", "febrero 2021", "marzo 2021", "abril 2021","mayo 2021",
  #                            "junio 2021", "julio 2021", "agosto 2021", "septiembre 2021",
  #                            "octubre 2021", "noviembre 2021", "diciembre 2021", "enero 2022",
  #                            "febrero 2022","marzo 2022" ))) %>% 
  group_by(Año, Mes, Padecimiento) %>% 
  summarise(Reportes=n())-> Nacional_2_mensual_pat

Entidad_2_mensual_pat<- c("Acumulado nacional")

cbind(Entidad_2_mensual_pat, Nacional_2_mensual_pat)->Nacional_2_mensual_pat
names(Nacional_2_mensual_pat)[names(Nacional_2_mensual_pat) == "...1"] <- "Entidad"


cd %>% 
  filter(#Año %in% c(2020, 2021),
    !(Entidad %in% c("NA", NA, NA_character_, "")),
    Validado..por.Nosotrxs.=="Sí") %>% 
  group_by(Entidad, Año, Mes, Padecimiento) %>% 
  # mutate(Mes=factor(Mes,
  #                   levels=c("enero 2019", "febrero 2019", "marzo 2019",
  #                            "abril 2019", "mayo 2019", "junio 2019", "julio 2019",
  #                            "agosto 2019","septiembre 2019","octubre 2019", "noviembre 2019",
  #                            "diciembre 2019", "enero 2020", "febrero 2020","marzo 2020",
  #                            "abril 2020", "mayo 2020", "junio 2020", "julio 2020","agosto 2020",
  #                            "septiembre 2020", "octubre 2020", "noviembre 2020","diciembre 2020",
  #                            "enero 2021", "febrero 2021", "marzo 2021", "abril 2021","mayo 2021",
  #                            "junio 2021", "julio 2021", "agosto 2021", "septiembre 2021",
  #                            "octubre 2021", "noviembre 2021", "diciembre 2021", "enero 2022",
  #                            "febrero 2022","marzo 2022" ))) %>% 
  summarise(Reportes = n())  ->cd_patología_mensual



rbind(Nacional_2_mensual_pat, cd_patología_mensual)->cd_mensual_pat



###############################################################################
#                                 corrupción                                  #
###############################################################################

cd_corrupcion <- read.csv("corr_.csv", encoding="UTF-8")
names(cd_corrupcion)[names(cd_corrupcion) == "X.U.FEFF.Entidad"] <- "Entidad"

cd_corrupcion<-  cd_corrupcion %>%
  mutate(Cuatrimestre=factor(Cuatrimestre,
                             levels=c("Ene - Abr 2020","May - Ago 2020", "Sep - Dic 2020",
                                      "Ene - Abr 2021","May - Ago 2021",  "Sep - Dic 2021")))  




################################################################

#Create a vector containing only the text
text <- cd$Relato.de.la.corrupción
# Create a corpus  
docs <- Corpus(VectorSource(text))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)


docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("spanish"))


dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix),decreasing=TRUE) 

df <- data.frame(word = names(words),freq=words)
subset(df, !df[,-2] == "medicamentos")->df
subset(df, !df[,-2] == "medicamento")->df































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
    navbarPage("Reportes en cerodesabasto.org",
               navbarMenu("Reportes",
               
              tabPanel("Total de reportes",
                      
               tabsetPanel(
                 tabPanel("Total de reportes",
                          sidebarLayout(
                            sidebarPanel("\nSeleccione algunas características",
                                         selectInput(
                                           inputId = "Entidad_total",
                                           label = "\nEntidad",
                                           choices = unique(sort(cd_total$Entidad)),
                                           multiple = F,
                                           selected = "Acumulado nacional"
                                         ),
                                         selectInput(
                                           inputId = "Año_total",
                                           label = "\nAño",
                                           choices = unique(sort(cd_total$Año)),
                                           multiple = TRUE,
                                           #selected = "Nacional"
                                         ),
                                         selectInput("value_total", 
                                                     "\nSeleccione alguna opción" , 
                                                     choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
                                                     selected = NULL ,  multiple = FALSE, selectize = TRUE),
                                         
                                         
                                         downloadButton("downloadData_total", "\nDescarga (.csv)")
                                         
                            ),
                            mainPanel(plotlyOutput("grafico_total",  height = 440, width = 850))
                            
                          )),
                 tabPanel(
                   "Mapa del total de reportes",
                   column(12, align="center",
                   #sidebarLayout(
                     #sidebarPanel("Seleccione algunas características",
                                   # selectInput(
                                   #   inputId = "Año_mapa",
                                   #   label = "Selecciona año de los reportes",
                                   #   choices = unique(sort(cd$Año)),
                                   #   multiple = TRUE,
                                   #   #selected = "Nacional"
                                   # ),
                   selectInput("mapa_mx", "Seleccione el o los años de reporte" ,
                               choices = c("Año 2019", "Año 2020", 
                                           "Año 2021", #"Año 2022",
                                           "Histórico 2019 - 2022"),
                               selected = "Histórico 2019 - 2022",  multiple = FALSE, selectize = TRUE),

                                  # selectInput(
                                  #   inputId = "Cuatrimestre",
                                  #   label = "Período",
                                  #   choices = sort(unique(cd$Cuatrimestre)),
                                  #   multiple = TRUE
                                  # ),
                                  
                     #),
                 
                     #mainPanel(
                       plotlyOutput("mapa_1",  height = 400, width = 850))),
               
    )),
    
    tabPanel("Reportes por tipo de informante",
    tabsetPanel(
      tabPanel("Reportes por tipo de informante",
               sidebarLayout(
                 sidebarPanel("Seleccione algunas características",
                              selectInput(
                                inputId = "Entidad2",
                                label = "Entidad",
                                choices = unique(sort(cd_2$Entidad)),
                                multiple = F,
                                selected = "Acumulado nacional"
                              ),
                              selectInput(
                                inputId = "Informante2",
                                label = "Tipo de informante",
                                choices = unique(sort(cd_2$`Tipo de informante`)),
                                multiple = TRUE,
                                #selected = "Nacional"
                              ),
                              selectInput(
                                inputId = "Año2",
                                label = "Año",
                                choices = unique(sort(cd_2$Año)),
                                multiple = TRUE,
                                #selected = "Nacional"
                              ),
                              selectInput("value12", "Seleccione alguna opción" , 
                                          choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
                                          selected = NULL ,  multiple = FALSE, selectize = TRUE),
                              downloadButton("downloadData", "Descarga (.csv)")),
                 mainPanel(plotlyOutput("grafico_1",  height = 440, width = 850))
                 
                 ))))
                              ,
    
    tabPanel("Reportes por tipo de insumos",
             tabsetPanel(
               tabPanel("Reportes por tipo de insumos",
                        sidebarLayout(
                          sidebarPanel("Seleccione algunas características",
                                       selectInput(
                                         inputId = "Entidad_insumo",
                                         label = "Entidad",
                                         choices = unique(sort(cd_insumo$Entidad)),
                                         multiple = F,
                                         selected = "Acumulado nacional"
                                       ),
                                       selectInput(
                                         inputId = "Cuatrimestre_insumo",
                                         label = "Cuatrimestre",
                                         choices = unique(sort(cd_insumo$Cuatrimestre)),
                                         multiple = TRUE,
                                         #selected = "Nacional"
                                       ),
                                       selectInput(
                                         inputId = "Año_insumo",
                                         label = "Año",
                                         choices = unique(sort(cd_insumo$Año)),
                                         multiple = TRUE,
                                         #selected = "Nacional"
                                       ),
                                       downloadButton("downloadData_insumo", "Descarga (.csv)")),
                          mainPanel(plotlyOutput("grafico_insumo",  height = 440, width = 850)))))),
               ),
    
    ##########################################################################
    tabPanel("Instituciones de salud",
             tabsetPanel(
               tabPanel("Grafico por cuatrimestre",
                 sidebarLayout(
                   sidebarPanel("Seleccione algunas características",
                                selectInput(
                                  inputId = "Entidad_inst",
                                  label = "Entidad",
                                  choices = unique(sort(cd_instituciones$Entidad)),
                                  multiple = F,
                                  selected = "Acumulado nacional"
                                ),
                                selectInput(
                                  inputId = "Cuatrimestre_inst",
                                  label = "Cuatrimestre",
                                  choices = unique(cd_instituciones$Cuatrimestre),
                                  multiple = TRUE
                                ),
                                selectInput(
                                  inputId = "Año_inst",
                                  label = "Año",
                                  choices = sort(unique(cd_instituciones$Año)),
                                  multiple = TRUE
                                ),
                                downloadButton("downloadData1", "Descarga (.csv)"),
                                
                                # selectInput("value_ins", "Seleccione alguna opción" ,
                                #             choices = c("Sin etiqueta de datos", "Con etiqueta de datos"),
                                #             selected = NULL ,  multiple = FALSE, selectize = TRUE)

                   ),
                   mainPanel(plotlyOutput(outputId = "grafico_ins", height = 440, width = 850)))),

                # tabsetPanel(
                   tabPanel("Reportes mensuales",
                     sidebarLayout(
                       sidebarPanel("Seleccione algunas características",
                                    selectInput(
                                      inputId = "Entidad_mensual_inst",
                                      label = "Entidad",
                                      choices = unique(sort(cd_mensual_inst$Entidad)),
                                      multiple = F,
                                      selected = "Acumulado nacional"
                                    ),
                                    selectInput(
                                      inputId = "Año_mensual_inst",
                                      label = "Año",
                                      choices = sort(unique(cd_mensual_inst$Año)),
                                      multiple = TRUE
                                    ),
                                    # selectInput(
                                    #   inputId = "Mes_mensual_inst",
                                    #   label = "Mes",
                                    #   choices = sort(unique(cd_mensual_inst$Mes)),
                                    #   multiple = TRUE
                                    # ),
                                    selectInput(
                                      inputId = "Institucion_mensual_inst",
                                      label = "Institución de salud",
                                      choices = sort(unique(cd_mensual_inst$Institución)),
                                      multiple = TRUE
                                    ),
                                    # selectInput("value_mensual_inst", "Seleccione alguna opción" , 
                                    #             choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
                                    #             selected = NULL ,  multiple = FALSE, selectize = TRUE),
                                    downloadButton("downloadData_mensual_inst", "Descarga (.csv)")),
                       
                       mainPanel(plotlyOutput("grafico_mensual_1",  height = 400, width = 850))))
             )),
    ##########################################################################


    tabPanel("Patologías y/o enfermedades",
             tabsetPanel(
               tabPanel(
                 "Reportes por año",
                 sidebarLayout(
                   sidebarPanel("Seleccione algunas características",
                              selectInput(
                                inputId = "Entidad_patologia",
                                label = "Entidad",
                                choices = unique(sort(cd_patologia$Entidad)),
                                multiple = F,
                                selected = "Acumulado nacional"
                   ),
                                selectInput(
                                  inputId = "Padecimiento",
                                  label = "Padecimiento y/o enfermedad",
                                  choices = sort(unique(cd_patologia$Padecimiento)),
                                  multiple = TRUE,
                                  selected = c("Cáncer", "Diabetes Mellitus", "Post trasplante",
                                    "Hipertensión Arterial")
                                ),
                                selectInput(
                                  inputId = "Año",
                                  label = "Año",
                                  choices = sort(unique(cd_patologia$Año)),
                                  multiple = TRUE
                                ),                                         
                   downloadButton("downloadData2", "Descarga (.csv)"),

                                

                   ),
                   mainPanel(plotlyOutput(outputId = "grafico_3", height = 400, width = 850))

                 )),

               
                 tabPanel("Reportes mensuales",
                          sidebarLayout(
                            sidebarPanel("Seleccione algunas características",
                                         selectInput(
                                           inputId = "Entidad_mensual_pat",
                                           label = "Entidad",
                                           choices = unique(sort(cd_mensual_pat$Entidad)),
                                           multiple = F,
                                           selected = "Acumulado nacional"
                                         ),
                                         selectInput(
                                           inputId = "Año_mensual_pat",
                                           label = "Año",
                                           choices = sort(unique(cd_mensual_pat$Año)),
                                           multiple = TRUE
                                         ),
                                         # selectInput(
                                         #   inputId = "Mes_mensual_pat",
                                         #   label = "Mes",
                                         #   choices = sort(unique(cd_mensual_pat$Mes)),
                                         #   multiple = TRUE
                                         # ),
                                         selectInput(
                                           inputId = "Padecimiento_mensual_pat",
                                           label = "Tipo de enfermedad y/o patología",
                                           choices = sort(unique(cd_mensual_pat$Padecimiento)),
                                           multiple = TRUE,
                                           selected = c(
                                             "Cáncer", "Diabetes Mellitus", "Post trasplante",
                                             "Hipertensión Arterial")
                                         ),
                                         # selectInput("value_mensual_pat", "Seleccione alguna opción" , 
                                         #             choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
                                         #             selected = NULL ,  multiple = FALSE, selectize = TRUE),
                                         downloadButton("downloadData_mensual_pat", "Descarga (.csv)"),
                                         
                                         
                            ),
                            mainPanel(plotlyOutput(outputId = "grafico_pat_mensual", height = 400, width = 850))
                          )),
               )), 

  ##########################################################################

  tabPanel("Relatos de corrupción",
           # h4("En los últimos dos años, el número de reportes que perciben a la corrupción 
           # como una causa potencial de desabasto de sus medicamentos ha variado en 
           # promedio 38%. El segundo cuatrimestre alcanzó el nivel más alto (45.1%), 
           # seguido de los registrados en los últimos dos períodos del año 2021 con valores alrededor del 37%.",
           #    align = "justify", style = "font-size:15px;"),
           
           tabsetPanel(
             tabPanel(
               "Porcentaje de corrupción",
               sidebarLayout(
                 sidebarPanel("Seleccione algunas características",
                              selectInput(
                                inputId = "Entidad",
                                label = "Entidad",
                                choices = unique(sort(cd_corrupcion$Entidad)),
                                multiple = F,
                                selected = "Acumulado nacional"
                              ),
                              selectInput(
                                inputId = "Cuatrimestre",
                                label = "Cuatrimestre",
                                choices = sort(unique(cd_corrupcion$Cuatrimestre)),
                                multiple = TRUE
                              ),
                              selectInput("value3", "Seleccione alguna opción" , 
                                          choices = c("Sin etiqueta de datos", "Con etiqueta de datos"), 
                                          selected = NULL ,  multiple = FALSE, selectize = TRUE),
                              downloadButton("downloadData3", "Descarga (.csv)")
                              
                              
                 ),
                 
                 mainPanel(plotlyOutput("grafico3",  height = 400, width = 850)))),
                 tabPanel("Relato de la corrupción",
                          column(12, align="center",
                                wordcloud2Output("word_1",  height = 450, width = 900)))))
             
                 
               ,
  
          tabPanel("Testimonios del desabasto",
                  h4(includeHTML("carrusel.html")),
            br())),



    
    ##-- Footer ----
    div(class = "footer",
        includeHTML("html/footer.html"),
        #div(includeHTML("html/google_analytics.html"))
    )
  
))

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

# Define server logic required to draw a histogram
server <- function(input, output) {

###############################################################################
#                              Reportes     
###############################################################################
  output$downloadData_total <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_total(), file, row.names = FALSE)
    }
  )
  
  
  output$Entidad_total <- renderUI({
    selectInput("Entidad_total",
                label =  "Seleccione la entidad",
                choices = sort(unique(cd_total$Entidad)),
                multiple = T)
  })
  
  output$Año_total <- renderUI({
    selectInput("Año_total",
                label =  "Seleccione el año",
                choices = sort(unique(cd_total$Año)),
                multiple = T)
  })
  
  
  output$Cuatrimestre_total <- renderUI({
    selectInput("Cuatrimestre_total",
                label =  "Selecciona el cuatrimestre",
                choices = sort(unique(cd_total$Cuatrimestre)),
                multiple = T)
  })
  
  
  
  #base reactiva para slide 3
  data_total <- reactive({
    
    cd_total %>%
      filter(
        if(!is.null(input$Entidad_total))                     Entidad %in% input$Entidad_total              else Entidad != "",
        if(!is.null(input$Año_total))                             Año %in% input$Año_total                  else Año != "",
        if(!is.null(input$Cuatrimestre_total))           Cuatrimestre %in% input$Cuatrimestre_total         else Cuatrimestre != "",
      )
    
  })
  
  
  output$grafico_total <- renderPlotly ({
    
  if (input$value_total == "Con etiqueta de datos") {
      
  ggplot(data_total())+
      aes(x =Cuatrimestre, y = Total, group=1, #colour=Entidad, 
                        text = paste("Cuatrimestre: ", Cuatrimestre, 
                                     "\nEntidad: ", Entidad,
                                     "\nTotal reportes: ", comma(Total, accuracy = 1), sep="")) +
      geom_point(size=3, fill="#9443FF", colour="#9443FF")+
      geom_line(size=1, color="#9443FF", fill="#9443FF")+
      geom_text(aes(x=Cuatrimestre, y=Total, label=comma(Total, accuracy = 1)),
                colour="black", size=4,
                hjust=0.5, vjust=1, angle=0)+
      
      #scale_fill_brewer(palette = "Dark2")+
      #facet_grid( ~ Año, space = 'free_x', scales = 'free_x', switch = 'x')+        
      
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      labs(title = paste("Total de reportes registrados \n", data_total()$Entidad),
           caption = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org.", 
           x="Cuatrimestre", y="Total de reportes") +
      theme_minimal()+
      theme(text=element_text(size=12,  family="Century Gothic"),
            legend.position='none',
            strip.text.x = element_text(size = 12, face = "bold", angle=0),
            plot.title = element_text(size = 12L, hjust = 0.5), 
            plot.caption = element_text(size = 12L, hjust = 0),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9.5))->grafico_total_
    
  }  
  
  if (input$value_total == "Sin etiqueta de datos") {
    
    ggplot(data_total()) +
      aes(x =Cuatrimestre, y = Total, group=1, #colour=Entidad, 
          text = paste("Cuatrimestre: ", Cuatrimestre, 
                       "\nEntidad: ", Entidad,
                       "\nTotal reportes: ", comma(Total, accuracy = 1), 
                       "\nTipo de informante: ", `Tipo de informante` , sep="")) +
      geom_point(size=3, fill="#9443FF", colour="#9443FF")+
      geom_line(size=1, color="#9443FF", fill="#9443FF")+
      # facet_grid( ~ Año, space = 'free_x', scales = 'free_x', switch = 'x')+        
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      labs(title = paste("Total de reportes registrados \n", data_total()$Entidad),
           caption = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org.", 
           x="Cuatrimestre", y="Total de reportes") +
      theme_minimal()+
      theme(text=element_text(size=11,  family="Century Gothic"),
            legend.position='none',
            strip.text.x = element_text(size = 12, face = "bold", angle=0),
            plot.title = element_text(size = 12L, hjust = 0.5), 
            plot.caption = element_text(size = 12L, hjust = 0),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=9.5))->grafico_total_   
    
  }  
  
  ggplotly(grafico_total_, tooltip="text") %>% 
    layout(margin = list(b=-5,t=40), 
           annotations =
             list(
               x = .50, y = -.25, 
               text = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org",
               showarrow = F, xref='paper', yref='paper',
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10, color="#9443FF")),
           #title = "Indicador 12",
           # legend = list(orientation = 'h', 
           #               x =0, y = -1), 
           xaxis = list(side = "bottom"),legend = list(side="bottom"))
  
  
  
  
})  
  
  
# ---------------------------- Informante ----------------------------------  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_slide2(), file, row.names = FALSE)
    }
  )
  

  output$Entidad2 <- renderUI({
    selectInput("Entidad2",
                label =  "Seleccione la entidad",
                choices = sort(unique(cd_2$Entidad)),
                multiple = T)
  })
  
  output$Informante2 <- renderUI({
    selectInput("Informante2",
                label =  "Seleccione el tipo de informante",
                choices = sort(unique(cd_2$`Tipo de informante`)),
                multiple = T)
  })
  
  output$Año2 <- renderUI({
    selectInput("Año2",
                label =  "Seleccione el año",
                choices = sort(unique(cd_2$Año)),
                multiple = T)
  })
  
  
  output$Cuatrimestre2 <- renderUI({
    selectInput("Cuatrimestre2",
                label =  "Selecciona el cuatrimestre",
                choices = sort(unique(cd_2$Cuatrimestre)),
                multiple = T)
  })
  
  
  
  #base reactiva para slide 3
  data_slide2 <- reactive({
    
    cd_2 %>%
      filter(
        if(!is.null(input$Informante2))               `Tipo de informante` %in% input$Informante2 else `Tipo de informante` != "",
        if(!is.null(input$Entidad2))                     Entidad %in% input$Entidad2              else Entidad != "",
        if(!is.null(input$Año2))                             Año %in% input$Año2                  else Año != "",
        if(!is.null(input$Cuatrimestre2))           Cuatrimestre %in% input$Cuatrimestre2         else Cuatrimestre != "",
        )

  })

  
  output$grafico_1 <- renderPlotly ({
    
  
  if (input$value12 == "Con etiqueta de datos") {
    
    ggplot(data_slide2()) +
      aes(x =Cuatrimestre, y = Total,  fill=`Tipo de informante`,
          text = paste("Cuatrimestre: ", Cuatrimestre, 
                       "\nEntidad: ", Entidad,
                       "\nTotal reportes: ", comma(Total), 
                       "\nTipo de informante: ", `Tipo de informante` , sep="")) +
      geom_col(#fill="#7570B3"
        )+
      
      geom_text(aes(x=Cuatrimestre, y=Total, label=comma(Total, accuracy = 1)),
                colour="black", size=4,
                hjust=0.5, vjust=1, angle=0)+
      
      scale_fill_manual(
        values = c(
          `Paciente` = "#6F32BF",
          `Profesional` = "#FC0B40",
          Total="purple"))+
      #scale_fill_brewer(palette = "Dark2")+
      #facet_grid( ~ Año, space = 'free_x', scales = 'free_x', switch = 'x')+        
      
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      labs(title = paste("Total de reportes registrados \n", data_slide2()$Entidad),
           caption = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org.", 
           x="Cuatrimestre", y="Total de reportes") +
      theme_minimal()+
      theme(text=element_text(size=12,  family="Century Gothic"))+
      theme(plot.title = element_text(size = 14L, hjust = 0.5), 
            plot.caption = element_text(size = 12L, hjust = 0))+
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))->grafico1
    
  }  
  
  if (input$value12 == "Sin etiqueta de datos") {
    
    ggplot(data_slide2()) +
      aes(x =Cuatrimestre, y = Total,  fill=`Tipo de informante`,
          text = paste("Cuatrimestre: ", Cuatrimestre, 
                       "\nEntidad: ", Entidad,
                       "\nTotal reportes: ", comma(Total, accuracy = 1), 
                       "\nTipo de informante: ", `Tipo de informante` , sep="")) +
      geom_col(#fill="#7570B3"
               )+
      scale_fill_manual(
        values = c(
          `Paciente` = "#6F32BF",
          `Profesional` = "#FC0B40",
          Total="purple"))+
     # facet_grid( ~ Año, space = 'free_x', scales = 'free_x', switch = 'x')+        
      scale_y_continuous(labels = scales::comma) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      labs(title = paste("Total de reportes registrados \n", data_slide2()$Entidad),
           caption = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org.", 
           x="Cuatrimestre", y="Total de reportes") +
      theme_minimal()+
      theme(text=element_text(size=12,  family="Century Gothic"))+
      theme(plot.title = element_text(size = 14L, hjust = 0.5), 
            plot.caption = element_text(size = 12L, hjust = 0))+
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=10))->grafico1    
    
  }  
  
  ggplotly(grafico1, tooltip="text") %>% 
    layout(margin = list(b=5,t=40), 
           annotations =
             list(
               x = .5, y = -0.09, 
               text = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org",
                  showarrow = F, xref='paper', yref='paper',
                  xanchor='right', yanchor='auto', xshift=0, yshift=-88,
                  font=list(size=10, color="#9443FF")),
           #title = "Indicador 12",
           legend = list(orientation = 'h', 
                         x =0.25, y = -0.25), 
           xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
          
              
})  

  
  
  
  
  
  
  
  
  
# ----------------------------- Mapa -----------------------------------------
  
  output$mapa_1 <- renderPlotly ({
      #mxhexbin_choropleth

    if (input$mapa_mx == "Año 2019") {
      
    mxstate_choropleth(cd_1_19, num_colors = 1,
                       # popup = sprintf("Entidad: %s<br/>Valor: s%%",
                       #                 cd_1$state_name,
                       #                 cd_1$value)
    ) +  
      labs(title="Reportes por entidad, 2019", 
           fill=" Total", x="", y="") +
      scale_fill_gradient(
        low = "#9443FF",
        high = "#FC0B40",
        guide = "colourbar",
        labels = comma)+
      #        scale_color_brewer(palette = "BuPu", direction = 1, labels = comma) + 
      theme_minimal()+
      theme(legend.position = "bottom",
            legend.key.height= unit(1, 'cm'),
            legend.key.width= unit(1, 'cm'),
            legend.title = element_text(size=14),
            legend.text = element_text(size=12),
            text=element_text(size=12,  family="Century Gothic"),
            plot.title = element_text(size = 14L, hjust = 0.5), 
            plot.caption = element_text(size = 12L, hjust = 0),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=8))  -> mapa_1
    }
      
      if (input$mapa_mx == "Año 2020") {
        
    
    mxstate_choropleth(cd_1_20, num_colors = 1,
                       # popup = sprintf("Entidad: %s<br/>Valor: s%%",
                       #                 cd_1$state_name,
                       #                 cd_1$value)
    ) +  
      labs(title="Reportes por entidad, 2020", 
           fill=" Total", x="", y="") +
      scale_fill_gradient(
        low = "#9443FF",
        high = "#FC0B40",
        guide = "colourbar",
        labels = comma)+
      #        scale_color_brewer(palette = "BuPu", direction = 1, labels = comma) + 
      theme_minimal()+
      theme(legend.position = "bottom",
            legend.key.height= unit(1, 'cm'),
            legend.key.width= unit(1, 'cm'),
            legend.title = element_text(size=14),
            legend.text = element_text(size=12),
            text=element_text(size=12,  family="Century Gothic"),
            plot.title = element_text(size = 14L, hjust = 0.5), 
            plot.caption = element_text(size = 12L, hjust = 0),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=8))  -> mapa_1
      }
    
        if (input$mapa_mx == "Año 2021") {
          
    
    mxstate_choropleth(cd_1_21, num_colors = 1,
                       # popup = sprintf("Entidad: %s<br/>Valor: s%%",
                       #                 cd_1$state_name,
                       #                 cd_1$value)
    ) +  
      labs(title="Reportes por entidad, 2021", 
           fill=" Total", x="", y="") +
      scale_fill_gradient(
        low = "#9443FF",
        high = "#FC0B40",
        guide = "colourbar",
        labels = comma)+
      #        scale_color_brewer(palette = "BuPu", direction = 1, labels = comma) + 
      theme_minimal()+
      theme(legend.position = "bottom",
            legend.key.height= unit(1, 'cm'),
            legend.key.width= unit(1, 'cm'),
            legend.title = element_text(size=14),
            legend.text = element_text(size=12),
            text=element_text(size=12,  family="Century Gothic"),
            plot.title = element_text(size = 14L, hjust = 0.5), 
            plot.caption = element_text(size = 12L, hjust = 0),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=8))  -> mapa_1
        }
          
    if (input$mapa_mx == "Año 2022") {
            
    
    
    mxstate_choropleth(cd_1_22, num_colors = 1,
                         # popup = sprintf("Entidad: %s<br/>Valor: s%%",
                         #                 cd_1$state_name,
                         #                 cd_1$value)
                         ) +  
        labs(title="Reportes por entidad, 2022", 
             fill=" Total", x="", y="") +
        scale_fill_gradient(
          low = "#9443FF",
          high = "#FC0B40",
          guide = "colourbar",
          labels = comma)+
#        scale_color_brewer(palette = "BuPu", direction = 1, labels = comma) + 
        theme_minimal()+
        theme(legend.position = "bottom",
              legend.key.height= unit(1, 'cm'),
              legend.key.width= unit(1, 'cm'),
              legend.title = element_text(size=14),
              legend.text = element_text(size=12),
              text=element_text(size=12,  family="Century Gothic"),
              plot.title = element_text(size = 14L, hjust = 0.5), 
              plot.caption = element_text(size = 12L, hjust = 0),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=8))  -> mapa_1
    }  
    
      if (input$mapa_mx == "Histórico 2019 - 2022") {
        
        mxstate_choropleth(cd_1, num_colors = 1,
                           # popup = sprintf("Entidad: %s<br/>Valor: s%%",
                           #                 cd_1$state_name,
                           #                 cd_1$value)
        ) +  
          labs(title="Reportes por entidad, 2019-2021", 
               fill=" Total", x="", y="") +
          scale_fill_gradient(
            low = "#9443FF",
            high = "#FC0B40",
            guide = "colourbar",
            labels = comma)+
          #        scale_color_brewer(palette = "BuPu", direction = 1, labels = comma) + 
          theme_minimal()+
          theme(legend.position = "bottom",
                legend.key.height= unit(.5, 'cm'),
                legend.key.width= unit(.5, 'cm'),
                legend.title = element_text(size=14),
                legend.text = element_text(size=12),
                text=element_text(size=12,  family="Century Gothic"),
                plot.title = element_text(size = 14L, hjust = 0.5), 
                plot.caption = element_text(size = 12L, hjust = 0),
                axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=8))  -> mapa_1
        
      }
      
     ggplotly(mapa_1, tooltip=c("value")) %>%
       layout(margin = list(b=-5,t=40), 
              annotations =
                list(
                  x = .7, y = -0.29,  
                  text = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org",
                  showarrow = F, xref='paper', yref='paper',
                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                  font=list(size=10, color="#9443FF")),
              #title = "Indicador 12",
              legend = list(orientation = 'h', 
                            x =0.25, y = -0.25), 
              xaxis = list(side = "bottom"),legend = list(side="bottom"))
     
  
  })  
  

  
  
  
# -------------------- Insumos ----------------------------------------------
  output$downloadData_insumo <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_slide_insumo(), file, row.names = FALSE)
    }
  )
  
  
  output$Entidad_insumo <- renderUI({
    selectInput("Entidad_insumo",
                label =  "Seleccione la entidad",
                choices = sort(unique(cd_insumo$Entidad)),
                multiple = F)
  })
  
  
  output$Año_insumo <- renderUI({
    selectInput("Año_insumo",
                label =  "Seleccione el año",
                choices = sort(unique(cd_insumo$Año)),
                multiple = T)
  })
  
  
  output$Cuatrimestre_insumo <- renderUI({
    selectInput("Cuatrimestre_insumo",
                label =  "Selecciona el cuatrimestre",
                choices = sort(unique(cd_insumo$Cuatrimestre)),
                multiple = T)
  })
  
  
  
  data_slide_insumo <- reactive({
    
    
    cd_insumo %>%
      filter(
        if(!is.null(input$Entidad_insumo))                     Entidad %in% input$Entidad_insumo             else Entidad != "",
        if(!is.null(input$Año_insumo))                             Año %in% input$Año_insumo                 else Año != "",
        if(!is.null(input$Cuatrimestre_insumo))           Cuatrimestre %in% input$Cuatrimestre_insumo        else Cuatrimestre != "",
      )
  })
  
  
  output$grafico_insumo <- renderPlotly ({
    
    
    
    ggplot(data_slide_insumo())+
      aes(x=Cuatrimestre, y=`Tipo de insumo`, size=Total,
          text = paste("Cuatrimestre: ", Cuatrimestre, 
                       "\nEntidad: ", Entidad,
                       "\nTotal reportes: ", comma(Total, accuracy = 1), 
                       "\nInsumo: ", `Tipo de insumo` , sep=""))+
      geom_point(mapping=aes(colour=`Tipo de insumo`))+
      theme(panel.grid.major = element_line(colour = "grey"))+
      #scale_y_continuous(labels = scales::comma, limits = c(0, 1500)) +  
      geom_text(aes(label=comma(Total, accuracy = 1)),#hjust=.5, vjust=-.8,
                size=4, color="ghostwhite")+
      scale_y_discrete(limits = rev , 
                       labels = function(x) str_wrap(x, width = 15)) + 
      scale_size_continuous(range = c(5,10)) +
      scale_color_brewer(palette = "Dark2")+
      scale_fill_brewer(palette = "Dark2")+
      # scale_colour_manual(
      #   values = c(
      #     `IMSS` = "#FC0B40",
      #     `IMSS-BIENESTAR` = "#05b396",
      #     `INSABI (Secretaría de Salud)` = "#9443FF",
      #     `ISSSTE` = "#eb1c9b",
      #     `Otro` = "#f77d31",
      #     Privado ="#e6e20b"))+
      
      labs(title=paste("Total de reportes por tipo de insumo \n", data_slide_insumo()$Entidad), 
           x="", y="")+
      facet_grid( ~ Año, space = 'free_x', scales = 'free_x', switch = 'x')+        
      #scale_y_discrete(labels = function(x) str_wrap(x, width = 15)) +
      
      theme_minimal()+
      theme(legend.position = "none",
            legend.key.height= unit(1.3, 'cm'),
            legend.key.width= unit(1.3, 'cm'),
            legend.title = element_text(size=14),
            legend.text = element_text(size=12),
            text=element_text(size=12,  family="Century Gothic"),
            plot.title = element_text(size = 14L, hjust = 0.5), 
            plot.caption = element_text(size = 12L, hjust = 0),
            strip.text.x = element_text(size = 11, color = "black", face = "bold.italic"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=8)) ->grafico_insumo
    
    
    ggplotly(grafico_insumo, tooltip="text") %>%
      layout(margin = list(b=25,t=90), annotations =
               list(x = .6, y = -.15,
                    text = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=10,  color="#9443FF"))
      )
    
  })
  
  
  
  
  
  
  
###############################################################################
#                              Instituciones     
###############################################################################
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_slide_inst(), file, row.names = FALSE)
    }
  )
  
  
  output$Entidad_inst <- renderUI({
    selectInput("Entidad_inst",
                label =  "Seleccione la entidad",
                choices = sort(unique(cd_instituciones$Entidad)),
                multiple = T)
  })
  
  
  output$Año_inst <- renderUI({
    selectInput("Año_inst",
                label =  "Seleccione el año",
                choices = sort(unique(cd_instituciones$Año)),
                multiple = T)
  })
  
  
  output$Cuatrimestre_inst <- renderUI({
    selectInput("Cuatrimestre_inst",
                label =  "Selecciona el cuatrimestre",
                choices = sort(unique(cd_instituciones$Cuatrimestre)),
                multiple = T)
  })
  
  
  
  #base reactiva para slide 3
  data_slide_inst <- reactive({
    
    
    cd_instituciones %>%
      filter(
        if(!is.null(input$Entidad_inst))                     Entidad %in% input$Entidad_inst             else Entidad != "",
        if(!is.null(input$Año_inst))                             Año %in% input$Año_inst                 else Año != "",
        if(!is.null(input$Cuatrimestre_inst))           Cuatrimestre %in% input$Cuatrimestre_inst        else Cuatrimestre != "",
      )
  })
  
  
  output$grafico_ins <- renderPlotly ({
    
    
  
  ggplot(data_slide_inst())+
    aes(x=Cuatrimestre, y=Institución, size=Reportes,
        text = paste("Cuatrimestre: ", Cuatrimestre, 
                     "\nEntidad: ", Entidad,
                     "\nTotal reportes: ", comma(Reportes, accuracy = 1), 
                     "\nInstitución: ", Institución , sep=""))+
    geom_point(mapping=aes(colour=Institución))+
    theme(panel.grid.major = element_line(colour = "grey"))+
    #scale_y_continuous(labels = scales::comma, limits = c(0, 1500)) +  
    geom_text(aes(label=Reportes, accuracy = 1),#hjust=.5, vjust=-.8,
              size=4, color="ghostwhite")+
    scale_y_discrete(limits = rev , 
                     labels = function(x) str_wrap(x, width = 15)) + 
    scale_size_continuous(range = c(5,10)) +
    scale_color_brewer(palette = "Dark2")+
    scale_fill_brewer(palette = "Dark2")+
      # scale_colour_manual(
      #   values = c(
      #     `IMSS` = "#FC0B40",
      #     `IMSS-BIENESTAR` = "#05b396",
      #     `INSABI (Secretaría de Salud)` = "#9443FF",
      #     `ISSSTE` = "#eb1c9b",
      #     `Otro` = "#f77d31",
      #     Privado ="#e6e20b"))+
      
    labs(title=paste("Total de reportes por instituciones de salud 2020 - 2021 \n", data_slide_inst()$Entidad), 
         x="", y="")+
    facet_grid( ~ Año, space = 'free_x', scales = 'free_x', switch = 'x')+        
    #scale_y_discrete(labels = function(x) str_wrap(x, width = 15)) +
    
      theme_minimal()+
      theme(legend.position = "none",
            legend.key.height= unit(1.3, 'cm'),
            legend.key.width= unit(1.3, 'cm'),
            legend.title = element_text(size=14),
            legend.text = element_text(size=12),
            text=element_text(size=12,  family="Century Gothic"),
            plot.title = element_text(size = 14L, hjust = 0.5), 
            plot.caption = element_text(size = 12L, hjust = 0),
            strip.text.x = element_text(size = 11, color = "black", face = "bold.italic"),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=8)) ->grafico_ins
  
  
    ggplotly(grafico_ins, tooltip="text") %>%
    layout(margin = list(b=25,t=90), annotations =
             list(x = .53, y = .1,
                  text = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org",
                  showarrow = F, xref='paper', yref='paper',
                  xanchor='right', yanchor='auto', xshift=-1, yshift=-70,
                  font=list(size=9,  color="#9443FF"))
    )

  })
  
  
  
  
# --------------------------------mensual---------------------------------------#  
  
  output$downloadData_mensual_inst <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_slide_inst_mensual(), file, row.names = FALSE)
    }
  )
  
  
  output$Entidad_mensual_inst <- renderUI({
    selectInput("Entidad_inst_mensual",
                label =  "Seleccione la entidad",
                choices = sort(unique(cd_mensual_inst$Entidad)),
                multiple = T)
  })
  
  
  output$Año_mensual_inst <- renderUI({
    selectInput("Año_ins_mensual",
                label =  "Seleccione el año",
                choices = sort(unique(cd_mensual_inst$Año)),
                multiple = T)
  })
  
  output$Mes_mensual_ins <- renderUI({
    selectInput("Mes_ins_mensual",
                label =  "Seleccione el año",
                choices = sort(unique(cd_mensual_inst$Mes)),
                multiple = T)
  })
  
  
  output$Institucion_mensual_inst <- renderUI({
    selectInput("Institución_ins",
                label =  "Selecciona la institución de salud",
                choices = sort(unique(cd_mensual_inst$Institución)),
                multiple = T)
  })
  
  
  
  #base reactiva para slide 3
  data_slide_inst_mensual <- reactive({
    
    
    cd_mensual_inst %>%
      filter(
        if(!is.null(input$Entidad_mensual_inst))                     Entidad %in% input$Entidad_mensual_inst             else Entidad != "",
        if(!is.null(input$Mes_mensual_inst))                             Mes %in% input$Mes_mensual_inst                 else Mes != "",
        if(!is.null(input$Año_mensual_inst))                             Año %in% input$Año_mensual_inst                 else Año != "",
        if(!is.null(input$Institucion_mensual_inst))           Institución %in% input$Institucion_mensual_inst           else Institución != "",
      )
  })
  
  
  output$grafico_mensual_1 <- renderPlotly ({
    
    ggplot(data_slide_inst_mensual())+
      aes(x=Mes, y=Reportes,
          color=Institución, fill=Institución, group=1,
          #fill=Institución, colour=Institución,
          text = paste("Mes: ", Mes, 
                       "\nEntidad: ", Entidad,
                       "\nTotal reportes: ", comma(Reportes, accuracy = 1), 
                       "\nInstitución: ", Institución , sep=""))+
      geom_line(size=.7)+
      geom_point(size=2)+
#      scale_x_date(date_breaks = "1 month",date_labels = "%B %Y")+
      scale_y_continuous(labels = scales::comma) +  
      # geom_text(aes(label=Reportes, accuracy = 1),#hjust=.5, vjust=-.8,
      #           size=4, color="ghostwhite")+
      #scale_y_discrete(labels = function(x) str_wrap(x, width = 15)) + 
      scale_color_brewer(palette = "Dark2")+
      scale_fill_brewer(palette = "Dark2")+
      # scale_colour_manual(
      #   values = c(
      #     `IMSS` = "#FC0B40",
      #     `IMSS-BIENESTAR` = "#05b396",
      #     `INSABI (Secretaría de Salud)` = "#9443FF",
      #     `ISSSTE` = "#eb1c9b",
      #     `Otro` = "#f77d31",
      #     Privado ="#e6e20b"))+
      # scale_fill_manual(
      #   values = c(
      #     `IMSS` = "#FC0B40",
      #     `IMSS-BIENESTAR` = "#05b396",
      #     `INSABI (Secretaría de Salud)` = "#9443FF",
      #     `ISSSTE` = "#eb1c9b",
      #     `Otro` = "#f77d31",
      #     Privado ="#e6e20b"))+
      
      labs(title=paste("Total de reportes por institución de salud 2019 - 2022 \n", 
                       data_slide_inst_mensual()$Entidad), x="", y="Total de reportes")+
      theme_minimal()+
      theme(legend.position = "bottom",
            legend.key.height= unit(1.3, 'cm'),
            legend.key.width= unit(1.3, 'cm'),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            text=element_text(size=12,  family="Century Gothic"),
            plot.title = element_text(size = 14L, hjust = 0.5), 
            plot.caption = element_text(size = 12L, hjust = 0),
            strip.text.x = element_text(size = 11, color = "black", face = "bold.italic"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8)) ->grafico_inst_mensual
    
    
    ggplotly(grafico_inst_mensual, tooltip="text") %>%
      layout(margin = list(b=30,t=90), annotations =
               list(x =.67, y = -.27,
                    text = "   Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='auto', xshift=0, yshift=4,
                    font=list(size=10,  color="#9443FF"))
      )
      
      # layout(margin = list(b=25,t=90), annotations =
      #          list(x = .5, y = .1,
      #               text = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org",
      #               showarrow = F, xref='paper', yref='paper',
      #               xanchor='right', yanchor='auto', xshift=-1, yshift=-70,
      #               font=list(size=9,  color="#9443FF"),
      #               xaxis = list(side = "bottom"),
      #               legend = list(side="bottom"))
      #)
    
    # ggplotly(gr6) %>% 
    #   layout(title = "Indicador 6",
    #          legend = list(orientation = 'v', 
    #                        x = 0, y = -1), 
    #          xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  
  
    
###############################################################################
#                              Patología     
###############################################################################
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_slide_3(), file, row.names = FALSE)
    }
  )
  
  output$Entidad_patologia <- renderUI({
    selectInput("Entidad_patologia",
                label =  "Seleccione el padecimiento",
                choices = sort(unique(cd_patologia$Entidad)),
                multiple = T)
  })
  
  
  
  output$Padecimiento <- renderUI({
    selectInput("Padecimiento",
                label =  "Seleccione el padecimiento",
                choices = sort(unique(cd_patologia$Padecimiento)),
                multiple = T)
  })
  
  
  output$Año <- renderUI({
    selectInput("Año",
                label =  "Selecciona el año",
                choices = sort(unique(cd_patologia$Año)),
                multiple = T)
  })
  
  
  
  #base reactiva para slide 3
  data_slide_3 <- reactive({
    
   cd_patologia %>%
      filter(
        if(!is.null(input$Entidad))      Entidad %in% input$Entidad_patologia       else Entidad != "",
        if(!is.null(input$Padecimiento))      Padecimiento %in% input$Padecimiento       else Padecimiento != "",
        if(!is.null(input$Año))                        Año %in% input$Año                else Año != "",)
  })
  
  
  output$grafico_3 <- renderPlotly ({
     
      ggplot(data_slide_3()) +
      aes(x = Año, y = Total,           
          color=Padecimiento, 
          fill=Padecimiento, group=1,
          text = paste("Padecimiento: ", Padecimiento, 
                       "\nEntidad", Entidad,
                       "\nAño: ", Año)) +
      geom_line(aes(color = Padecimiento), size = 1) +
      geom_point(aes(color = Padecimiento), size = 2)+
      scale_fill_manual(values = mycolors) +
      scale_color_manual(values=mycolors)+
      # geom_text(aes(label = Total, accuracy = 1) , 
      #           hjust = 1.35, 
      #           fontface = "bold", 
      #           size = 4,
      #           color="#5d5e5e") +
      labs(title=paste("Principales patologías y/o enfermedades reportadas, 2019 - 2021 \n", 
                       data_slide_3()$Entidad), x="", y="Total de reportes")+
      scale_x_discrete(position = "top")+
      theme_minimal()+
      theme(text=element_text(size=12,  family="Century Gothic"),
            plot.title = element_text(size = 14L, hjust = 0.5), 
            plot.caption = element_text(size = 12L, hjust = 0),
            axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=8),
            legend.text = element_text(size = 8),
            legend.title = element_text(size = 10),
            legend.key.size = unit(1.5, 'cm'))->grafico_3
      
    
    ggplotly(grafico_3, tooltip="text") %>% 
      layout(margin = list(b=0,t=60), 
             annotations = 
               list(x = 0.6, y = -0.068, 
                    text = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=-10, yshift=-10,
                    font=list(size=10, color="#9443FF"))
      )    
  })  
  
  
  
  
# ---------------------------------mensual -------------------------------------
  output$downloadData_mensual_pat <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_slide_pat_mensual(), file, row.names = FALSE)
    }
  )
  
  
  output$Entidad_mensual_pat <- renderUI({
    selectInput("Entidad_pat",
                label =  "Seleccione la entidad",
                choices = sort(unique(cd_mensual_pat$Entidad)),
                multiple = T)
  })
  
  
  output$Año_mensual_pat <- renderUI({
    selectInput("Año_pat",
                label =  "Seleccione el año",
                choices = sort(unique(cd_mensual_pat$Año)),
                multiple = T)
  })
  
  output$Mes_mensual_pat <- renderUI({
    selectInput("Mes_pat",
                label =  "Seleccione el año",
                choices = sort(unique(cd_mensual_pat$Mes)),
                multiple = T)
  })
  
  
  output$Institucion_mensual_pat <- renderUI({
    selectInput("Padecimiento_pat",
                label =  "Selecciona la institución de salud",
                choices = sort(unique(cd_mensual_pat$Padecimiento)),
                multiple = T)
  })
  
  
  
  #base reactiva para slide 3
  data_slide_pat_mensual <- reactive({
    
    
    cd_mensual_pat %>%
      filter(
        if(!is.null(input$Entidad_mensual_pat))                     Entidad %in% input$Entidad_mensual_pat            else Entidad != "",
        if(!is.null(input$Mes_mensual_pat))                             Mes %in% input$Mes_mensual_pat                else Mes != "",
        if(!is.null(input$Año_mensual_pat))                             Año %in% input$Año_mensual_pat                else Año != "",
        if(!is.null(input$Padecimiento_mensual_pat))           Padecimiento %in% input$Padecimiento_mensual_pat       else Padecimiento != "",
      )
  })
  
  
  output$grafico_pat_mensual <- renderPlotly ({
    
    #ggplot(data_slide_pat_mensual())+
    ggplot(data_slide_pat_mensual(),
      aes(x=Mes, y=Reportes,
          color=Padecimiento, 
          fill=Padecimiento, group=1,
          #fill=Institución, colour=Institución,
          text = paste("Mes: ", Mes, 
                       "\nEntidad: ", Entidad,
                       "\nTotal reportes: ", comma(Reportes, accuracy = 1), 
                       "\nPadecimiento o enfermedad: ", Padecimiento , sep="")))+
      geom_line(size=0.7)+
      geom_point(size=2)+
      scale_fill_manual(values = mycolors) +
      scale_color_manual(values=mycolors)+
      #scale_x_date(date_breaks = "1 month",date_labels = "%B %Y")+
      scale_y_continuous(labels = scales::comma) +  
      labs(title=paste("Total de reportes por padecimiento o enfermedad de salud 2019 - 2022 \n", 
                     data_slide_pat_mensual()$Entidad), 
         x="", y="Total de reportes")+
      theme_minimal()+
      theme(legend.position = "bottom",
            legend.key.height= unit(1.3, 'cm'),
            legend.key.width= unit(1.3, 'cm'),
            legend.title = element_text(size=10),
            legend.text = element_text(size=8),
            text=element_text(size=12,  family="Century Gothic"),
            plot.title = element_text(size = 14L, hjust = 0.5), 
            plot.caption = element_text(size = 12L, hjust = 0),
            strip.text.x = element_text(size = 11, color = "black", face = "bold.italic"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8)) ->grafico_pat_mensual
    
    
    ggplotly(grafico_pat_mensual, tooltip="text") %>%
      layout(margin = list(b=30,t=90), annotations =
               list(x =.62, y = -.27,
                    text = "   Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org",
                    showarrow = F, xref='paper', yref='paper',
                    xanchor='right', yanchor='auto', xshift=0, yshift=4,
                    font=list(size=10,  color="#9443FF"))
      )
    
    # layout(margin = list(b=25,t=90), annotations =
    #          list(x = .5, y = .1,
    #               text = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org",
    #               showarrow = F, xref='paper', yref='paper',
    #               xanchor='right', yanchor='auto', xshift=-1, yshift=-70,
    #               font=list(size=9,  color="#9443FF"),
    #               xaxis = list(side = "bottom"),
    #               legend = list(side="bottom"))
    #)
    
    # ggplotly(gr6) %>% 
    #   layout(title = "Indicador 6",
    #          legend = list(orientation = 'v', 
    #                        x = 0, y = -1), 
    #          xaxis = list(side = "bottom"),legend = list(side="bottom"))
    
  })
  
  
  
  
  
  

##################################################################################################
#                             corrupcion  
##################################################################################################333333  
  #Aquí inician los graficos para slide 3, con y sin etiqueta de datos.
  
  #Botones para corrupción
  
  output$downloadData3 <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_slide3(), file, row.names = FALSE)
    }
  )
  
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
        aes(x =Cuatrimestre, y = Total, group=1, 
            color=Entidad, fill=Entidad,
            text = paste("Cuatrimestre: ", Cuatrimestre, 
                         "\nEntidad: ", Entidad,
                         "\nTotal reportes que se creen sí están relacionados a corrupción: ", 
                         percent(Total), sep="")) +
        geom_point(size=3, fill="#9443FF", color="#9443FF") +
        geom_line(size=1, fill="#9443FF", color="#9443FF") +
        scale_y_continuous(labels = scales::percent) +
        labs(title = paste("Reportes donde se cree que sí hubo corrupción \n", data_slide3()$Entidad), 
             caption = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org.", 
             x="", 
             y="Porcentaje") +
        theme_minimal()+
        theme(text=element_text(size=12,  family="Century Gothic"))+
        theme(plot.title = element_text(size = 14L, hjust = 0.5), 
              plot.caption = element_text(size = 12L, hjust = 0))+
        theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=8))->grafico3
      
    }  
    
    if (input$value3 == "Con etiqueta de datos") {
      
        ggplot(data_slide3()) +        
        aes(x =Cuatrimestre, y = Total, group=1, 
            color=Entidad, fill=Entidad,
            text = paste("Cuatrimestre: ", Cuatrimestre, 
                         "\nEntidad: ", Entidad,
                         "\nTotal reportes que se creen sí están relacionados a corrupción: ", percent(Total), sep="")) +
        geom_point(size=3, fill="#9443FF", color="#9443FF") +
        geom_line(size=1, fill="#9443FF", color="#9443FF") +
        # scale_fill_manual(values = mycolors) +
        # scale_color_manual(values=mycolors)+
        scale_y_continuous(labels = scales::percent) +
        geom_text(aes(label=percent(Total)),
                  vjust=.5, hjust=2, size= 4, color="black")+
        labs(title = paste("Reportes donde se cree que sí hubo corrupción \n", data_slide3()$Entidad), 
             caption = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org.", 
             x="",
             y="Porcentaje") +
        theme_minimal()+
        theme(text=element_text(size=12,  family="Century Gothic"))+
        theme(plot.title = element_text(size = 14L, hjust = 0.5), 
              plot.caption = element_text(size = 12L, hjust = 0))+
        theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1, size=8))->grafico3
      
      
    }  
    
    ggplotly(grafico3, tooltip="text") %>% 
      layout(margin = list(b=55,t=55), annotations = 
               list(x = .7, y = -0.2, 
                    #text = "",
                    text = "Fuente: Elaboración propia con base a los datos abiertos de cerodesabasto.org", 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=-10, yshift=-.1,
                    font=list(size=10, color="#9443FF"))
      )    
  })  
  
  
  
  
output$word_1 <- renderWordcloud2({
    
# 
#   wordcloud(words = df$word, freq = df$freq, min.freq = 10,           
#             max.words=200, random.order=FALSE, rot.per=0.15,           
#             colors=brewer.pal(8, "Dark2"), size=14,
#             font = 14)
  
  
  wordcloud2(data = df, fontWeight=600,  #minSize = 1,
             size=.6, shape="rectangle", fontFamily = "Century Gothic",
             backgroundColor = "#ebeae4",
             color=rep_len( c("#9443FF","#FC0B40", "#45BFA7", "#17628F"),
                            nrow(demoFreq)))
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
