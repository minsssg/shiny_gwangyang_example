getwd()

library(shiny)
#library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(sf)
library(tidyverse)
library(RColorBrewer)

gwangyang = st_read('./qgis/18.광양시_법정경계(시군구).geojson')
gwangyang_dong = st_read('./qgis/19.광양시_법정경계(읍면동).geojson')

# gwangyang_dong

gwangyang_dong_df = gwangyang_dong %>%
  st_set_geometry(NULL)

gwangyang_dong_df

print(getwd())
ui = bootstrapPage(
  
  navbarPage(theme=shinytheme("flatly"), collapsible=TRUE,
             title="광양시 대시보드", id="nav",
             
             tabPanel("광양시 지도",
                      div(class="outer",
                          tags$head(includeCSS("./css/style.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          absolutePanel(id="controls", class="panel panel-default",
                                        top=75, left=55, width=350, fixed=TRUE,
                                        draggable=TRUE, height="auto",
                                        
                                        selectInput("colors", "Color scheme",
                                                    rownames(subset(brewer.pal.info, category %in% c("seq", div))),
                                                    selected="YlOrRd"
                                        )
                                        
                          )
                      )
             )
             
  )
)

server = function(input, output) {

  colorpal = reactive({
    colorNumeric(input$colors, gwnagyang)
  })
  
  output$mymap = renderLeaflet({
    
    pop_labels = sprintf(
      "<strong>%s</strong>", gwangyang_dong$EMD_KOR_NM
    ) %>% lapply(htmltools::HTML)
    
    leaflet(gwangyang_dong) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        opacity=1.0, fillOpacity=0.5,
        weight=1,
        highlightOptions=highlightOptions(color="red", weight=3, bringToFront=TRUE),
        label=pop_labels,
        labelOptions=labelOptions(
          style=list('font-weight'='normal', padding='3px 8px'),
          textsize='15px',
          direction='auto'
        )
      )
  })
}

shinyApp(ui=ui, server=server)