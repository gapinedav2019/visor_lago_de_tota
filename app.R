#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# VisorShinyApp ####
# 1. cambiar la data
# 2. poner iconos diferentes, awesome icons
# 3. Incluir contenido 
# 4. Compartir con mas gente 
library(shiny)
library(leaflet)
library(readxl)
library(writexl)
library(leaflet.extras)

# con shinylib creo un html de mi app
# importante este link 
# https://medium.com/@rami.krispin/deploy-shiny-app-on-github-pages-b4cbd433bdc
library(shinylive) 
library(httpuv)

# data ####

ruta="E:/HOSPEDAJE PA VIVIR SABROSO/BASES DE DATOS/Codigos/data_puntos.xlsx"
# Df<-as.data.frame(df)
# write_xlsx(Df, historico_actualizado,col_names=TRUE)

# data con los puntos los links y las fotos y ñas coordendas etc
data1 <- read_excel(ruta, sheet = "Sheet1" )

# la data no debe ser SpatialPointsDataFrame
# pero los factores si deben estar bien definodos OJO
# SINO HAY PROBLEMAS CON LOS MARCADORES



# df: controla los iconos ####
df <- sp::SpatialPointsDataFrame(
  cbind( 
    lng = data1$lng,
    lat = data1$lat
  ),
  data.frame(type = factor(data1$type, c("Cruce vial importante", "Hospedaje","Paramo", "Caminata o Bici","Experiencia")),
             type1 = data1$type1)
)

df




# IconSet - dento del Mapa ####
# iconos dentro del mapa 

IconSet <- awesomeIconList(
  "Cruce vial importante"   = makeAwesomeIcon(icon= 'road', markerColor = 'blue', iconColor = 'white', library = "glyphicon"),
  "Hospedaje" = makeAwesomeIcon(icon= 'fa-hotel', markerColor = 'red', iconColor = 'white', library = "fa"),
  "Paramo" = makeAwesomeIcon(icon= 'fa-tree', markerColor = 'green', iconColor = 'white', library = "fa"),
  "Paramo" = makeAwesomeIcon(icon= 'fa-tree', markerColor = 'green', iconColor = 'white', library = "fa"),
  "Hospedaje" = makeAwesomeIcon(icon= 'fa-hotel', markerColor = 'red', iconColor = 'white', library = "fa"),
  "Hospedaje" = makeAwesomeIcon(icon= 'fa-hotel', markerColor = 'red', iconColor = 'white', library = "fa"),
  "Cruce vial importante"   = makeAwesomeIcon(icon= 'road', markerColor = 'blue', iconColor = 'white', library = "glyphicon"),
  "Cruce vial importante"   = makeAwesomeIcon(icon= 'road', markerColor = 'blue', iconColor = 'white', library = "glyphicon"),
  "Cruce vial importante"   = makeAwesomeIcon(icon= 'road', markerColor = 'blue', iconColor = 'white', library = "glyphicon"),
  "Cruce vial importante"   = makeAwesomeIcon(icon= 'road', markerColor = 'blue', iconColor = 'white', library = "glyphicon"),
  "Cruce vial importante"   = makeAwesomeIcon(icon= 'road', markerColor = 'blue', iconColor = 'white', library = "glyphicon"),
  "Caminata o Bici" = makeAwesomeIcon(icon= 'fa-bicycle', markerColor = 'orange', iconColor = 'white', library = "fa"),
  "Caminata o Bici" = makeAwesomeIcon(icon= 'fa-bicycle', markerColor = 'orange', iconColor = 'white', library = "fa"),
  "Experiencia"   = makeAwesomeIcon(icon= 'fa-ship', markerColor = 'orange', iconColor = 'white', library = "fa")
)


# Icon Box - Gropus ####
# iconos en el icon box 

groups <- c( "Cruce vial importante" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-blue awesome-marker'><i class='glyphicon glyphicon-road icon-white '></i></div>Cruce vial importante",
             "Hospedaje"    <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-red awesome-marker'><i class='fa fa-hotel icon-white '></i></div>Hospedaje",
             "Paramo" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-green awesome-marker'><i class='fa fa-tree icon-white '></i></div>Paramo",
             "Caminata o Bici" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-orange awesome-marker'><i class='fa fa-bicycle icon-white '></i></div>Caminata o Bici",
             "Experiencia" <- "<div style='position: relative; display: inline-block' class='awesome-marker-icon-white awesome-marker'><i class='fa fa-ship icon-black '></i></div>Experiencia")
str(groups)


# contenido ####
Contenido<-list()
for (i in 1:length(df[,1])) {
  Contenido[[i]] <- paste(sep = "<br/>",
                          #paste0("<img src='https://www.r-project.org/logo/Rlogo.png", "' />"),
                          paste0("<img src='", data1$image[i], "'  width = 300 />"  ),
                          #paste0("<b>Nombre: </b>", df$type1[i]),
                          paste0("<b>", data1$type[i] , "</b>" ), 
                          paste0("<b>", data1$type1[i] , "</b>" ),
                          paste0("<b>Departamento: </b>", data1$type2[i]),
                          paste0("<b>Municipio: </b>", data1$type3[i]),
                          paste0("<a href='", data1$link[i] , "'>Link</a>" ))
}


# UI ####
ui <- fluidPage(
  tags$h1("Programa tu viaje!"),
  # selectInput(inputId = "inputDepto", label = "Selecciona el Departamento:", multiple = TRUE, choices = sort(df$Departamento), selected = "Cundinamarca"),
  tags$h2("Éste es tu mapa personalizado"),
  leafletOutput(outputId = "map1")
)


# server ####
server <- function(input, output, session) {
  output$map1 <- renderLeaflet({
    leaflet(df)%>%
      #addTiles() %>%
      addTiles(group = "OpenStreetMap") %>%
      addProviderTiles("CartoDB.Voyager", group = "Carto Voyager") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagen satelital") %>% 
      
      # minimapa
      addMiniMap(width = 150, height = 150,position = "bottomleft") %>%
      
      # incluyo posibilidad de medir distancias 
      addMeasure(position = "bottomleft",
                 primaryLengthUnit = "meters",
                 primaryAreaUnit = "sqmeters",
                 activeColor = "#3D535D",
                 completedColor = "#7D4479") %>%
      
      # incluyo barra de dibujo
      addDrawToolbar()%>%
      
      #addMarkers(~long, ~lat, group = ~cat) %>%
      addAwesomeMarkers( icon = ~IconSet[type], group = ~groups[type]  , popup = ~Contenido) %>% # group=~groups[cat] ó mas basica group=~cat
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Carto Voyager","Imagen satelital"),
        overlayGroups = groups , # si se usa arriba addAwesomeMarkers group=~cat entonces aca  overlayGroups = unique(df$cat)
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

shinyApp(ui, server)


