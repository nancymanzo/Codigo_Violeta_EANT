# SHINY: Código Violeta



A partir del confinamiento por motivos del COVID-19 en Jalisco México se inicio a recabar y publicar lo que es el [reporte ejecutivo](https://igualdad.jalisco.gob.mx/pdf/Reporte-Ejecutivo-Codigo-Violeta_pub_28feb2021_vf.pdf) del Codigo Violeta que se actualiza cada semana y al mes se publica este reporte, parte del nombre recae a la medida para la atención prioritaria en casos de violencia familiar. En caso de que una mujer viva una emergencia por violencia o sienta que su vida o, en su caso, la de sus hijas e hijos corren riesgo, debe llamar al 911 y decir: [“Código Violeta”](https://igualdad.jalisco.gob.mx/quedate-segura/).


Los datos referidos tienen el objetivo de mostrar el comportamiento del fenómeno de la violencia de género contra las mujeres, adolescentes y niñas que habitan y transitan el estado de Jalisco, incluye lo que es delitos que pueden entenderse desde la perspectiva de género o que son más comunes de sufrir las mujeres, tales como:

- Feminicidios
- Violencia familiar
- Homicidios
- Desaparición y no localización



> Además incluye lo que son las carpetas de investigación y/o denuncias que se levantan ante violencias del ambito familiar, de pareja y por motivo de género. El objetivo principal de este proyecto es crear y aportar a la visualización y acceso a estos datos de interés nacional, a través de una herramienta dinamica que permita observar, descargar y compartir el contenido de esta plataforma.



```
install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "mxmpas",
                 "plotly", "scales", "viridis", "leaflet"))
                 
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(dplyr)
 
#Más paqueterias que se encuentran en el script.

```

## Documentación
La plataforma de datos abierto tiene la finalidad de mostrar el comportamiento del fenómeno de la violencia de género contra las mujeres, adolescentes y niñas que habitan y transitan el estado de Jalisco.
Se busca brindar una herramienta que facilite la Participación, Transparencia y Colaboración que permita promover la generación de información de valor.

## Sobre las fuentes de información
Las bases de datos son de acceso publico y pueden solicitarlas a la SISEMH de sus reportes ejecutivos del [Codigo Violeta](https://igualdad.jalisco.gob.mx/pdf/Reporte-Ejecutivo-Codigo-Violeta_pub_28feb2021_vf.pdf) que se alimenta de los datos de Fiscalía del estado de Jalisco.
En el apartado de violencia de género integra la información de carpetas de investigación del [Secretariado Ejecutivo del Sistema Nacional de Seguridad Pública (SESNSP)](https://www.gob.mx/sesnsp/acciones-y-programas/datos-abiertos-de-incidencia-delictiva).

#
#
#
#
#
#### Proyecto colaborativo entre Macarena Zappe y Nancy Manzo.
https://rladiesgdl.shinyapps.io/plataforma_subse/

