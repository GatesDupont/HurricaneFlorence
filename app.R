# Gates Dupont   #
# September 2018 #
# # # # # # # # #

#----Global Space----

# Loading libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(RColorBrewer)
library(rgdal)
library(viridis)

# Fetching custom map tiles and adding citation
custom_map = "https://api.mapbox.com/styles/v1/heliornis/cjboo3dac64w02srud7535eec/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiaGVsaW9ybmlzIiwiYSI6ImNqNGtjZjE1cjBoYmcycXAzbmlmNmJieGEifQ.ERz0DjQKEE1PBd7myLKwZA"
mb_attribution <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a> © <a href='http://ebird.org/content/ebird/about/'>eBird / Cornell Lab of Ornithology</a> © <a href='https://www.gatesdupont.com/'>Gates Dupont</a>"

# Pull notables from second API
api2 = function(regionCode, back){
  url = paste('https://ebird.org/ws2.0/data/obs/', regionCode, 
              '/recent/notable?detail=full&key=phvesltiq609&back=',back, sep = "")
  data = fromJSON(readLines(url, warn=FALSE))
}

# Hurricane Shapefile
cone = readOGR("./hurricaneforecastcone.kml")
track = readOGR("./hurricaneforecasttrack.kml")

# Concatenate checklist urls for popups
subIDurl = function(subID){
  paste0("<a href = 'http://www.ebird.org/ebird/view/checklist/", 
         subID,"' target='_blank'","> ", "View in eBird"," </a>",collapse = "")}

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  
  # Setting THEME
  theme = shinytheme("superhero"),
  
  # Setting map to FULL-SCREEN
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  
  # Initializing LEAFLET output
  leafletOutput("myMap", width="100%", height="100%"),

  # Adding TITLE overlayed on leaflet map
  absolutePanel(top = 1, left = 50, draggable = F, 
                titlePanel("Hurricane Rarities"),
                helpText("by Gates Dupont")),
  
  # Adding SLIDER input overlayed on leaflet map
  absolutePanel(bottom = 1, left = 45, draggable = F, 
                sliderInput("slider_in", "Days Back", 
                            min = 1, max = 30, value = 3, round = T))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  APIdata <- reactive({
    usNotables = api2("US",as.numeric(input$slider_in))
    
    
    statesOI = c("Maine", "New Hampshire", "Vermont", "Massachusetts","Rhode Island",
                 "Connecticut", "New York", "Pennsylvania", "Ohio", "New Jersey", "Deleware",
                 "Maryland", "West Virginia", "Virginia", "North Carolina", "South Carolina",
                 "Indiana", "Kentucky", "Tennessee", "Georgia", "Florida", "Alabama", 
                 "Mississippi", "Arkansas", "Louisiana", "Texas")
    
    speciesOI = c("Brown Noddy", "Royal Tern","Sooty Tern", "Bridled Tern", 
                  "Magnificent Frigatebird", "Parasitic Jaeger", "Sandwich Tern",
                  "Black-capped Petrel", "White-tailed Tropicbird", "Brown Booby",
                  "Long-tailed Jaeger", "Cory's Shearwater", "Sabine's Gull", 
                  "Great Shearwater", "Pomarine Jaeger", "Band-rumped Storm-Petrel")
    
    df = usNotables[usNotables$subnational1Name %in% statesOI,]
    df = df[df$comName %in% speciesOI,]
    
    df["date"] = format(strptime(df$obsDt, format = "%Y-%m-%d"), "%b %d")
    
    df["url"] = sapply(df$subId, subIDurl)
    
    df$lat = jitter(df$lat, factor = 3) 
    
    return(df)
  })
  
  # Leaflet map
  output$myMap = renderLeaflet({
    if(is.null(APIdata()))
    {
      # Rendering leaflet map
      return(leaflet() %>% addTiles(urlTemplate = custom_map, attribution = mb_attribution)) %>%
        addSearchOSM(options = searchOSMOptions(zoom = 8)) %>%
        setView(-19.451108, 30.479968, 2)
    }
    else
    {
      #pal = colorFactor(palette = brewer.pal(length(unique(APIdata()$comName)), "YlGnBu"), domain = length(unique(APIdata())))
      
      len = length(unique(APIdata()$comName))
      pal = colorFactor(colorRampPalette(brewer.pal(len,"Spectral"))(len),
                        domain = unique(APIdata()$comName))
      
      pal = colorFactor(colorRampPalette(viridis_pal()(len))(len),
                        domain = unique(APIdata()$comName))
      
      # Rendering leaflet map
      leaflet() %>% addTiles(urlTemplate = custom_map, attribution = mb_attribution) %>%
        addCircleMarkers(data = APIdata(), stroke=F, popup = APIdata()$url,
                         color = ~pal(comName), fillOpacity = 1,
                         label = paste(APIdata()$comName,", ",APIdata()$date, ", ",
                                       APIdata()$locName,sep = "")) %>%
        addPolygons(data=cone, color = brewer.pal(3,"YlOrRd")[1], opacity=0, fillOpacity = 0.1, smoothFactor = 0.5) %>%
        addPolylines(data = track, color = brewer.pal(3,"YlOrRd")[2], opacity=0.5)

    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
