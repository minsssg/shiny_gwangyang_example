getwd()

library(shiny)
#library(shinyWidgets)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(highcharter)
library(formattable)

# shape file
gwangyang_si = st_read('./qgis/18.gwangyang_sigungu_sigungu_bubjeongdong.geojson')
gwangyang_hangjeongdong = st_read('./qgis/20.gwangyang_hangjeongdong.geojson')
gwangyang_house = st_read("./qgis/광양시_주택건물정보.shp")
gwangyang_house = st_join(gwangyang_house, gwangyang_hangjeongdong)
gwangyang_house$long = st_coordinates(st_centroid(gwangyang_house))[,1]
gwangyang_house$lat = st_coordinates(st_centroid(gwangyang_house))[,2]
gwangyang_house_number = gwangyang_house %>% st_set_geometry(NULL)
# gwangyang_house_number = gwangyang_house_number %>% group_by(ADM_DR_NM) %>% summarise(count=n())
gwangyang_house_number$category = "주택"

gwangyang_apartment = st_read("./qgis/광양시_아파트오피스텔건물정보.shp")
gwangyang_apartment = st_join(gwangyang_apartment, gwangyang_hangjeongdong)
gwangyang_apartment$long = st_coordinates(st_centroid(gwangyang_apartment))[,1]
gwangyang_apartment$lat = st_coordinates(st_centroid(gwangyang_apartment))[,2]
gwangyang_apartment_number = gwangyang_apartment %>% st_set_geometry(NULL)
# gwangyang_apartment_number = gwangyang_apartment_number %>% group_by(ADM_DR_NM) %>% summarise(count=n())
gwangyang_apartment_number$category = "아파트"

gwangyang_accommodation = st_read("./qgis/광양시_숙박시설정보.shp")
gwangyang_accommodation = st_join(gwangyang_accommodation, gwangyang_hangjeongdong)
gwangyang_accommodation$long = st_coordinates(st_centroid(gwangyang_accommodation))[,1]
gwangyang_accommodation$lat = st_coordinates(st_centroid(gwangyang_accommodation))[,2]
gwangyang_accommodation_number = gwangyang_accommodation %>% st_set_geometry(NULL)
# gwangyang_accommodation_number = gwangyang_accommodation_number %>% group_by(ADM_DR_NM) %>% summarise(count=n())
gwangyang_accommodation_number$category = '숙박'

names(gwangyang_hangjeongdong)[1:2] = c("code", "dong")

gwangyang_people = read.csv("./data/csv/gwangyang_people.csv", fileEncoding="cp949")
names(gwangyang_people)[4:5] = c("dong","population")
gwangyang_hangjeongdong_people = gwangyang_hangjeongdong %>% left_join(gwangyang_people, by="dong")
gwangyang_hangjeongdong_people
gwangyang_hangjeongdong_people = gwangyang_hangjeongdong_people %>%
  select(code, dong, population, geometry)

###############################################################################
#gwangyang_building_db = list("house"=gwangyang_house_number, "apartment"=gwangyang_apartment_number, "accommodation"=gwangyang_accommodation_number)
gwangyang_building_db = rbind(gwangyang_house_number, gwangyang_apartment_number, gwangyang_accommodation_number)

gwangyang_electronic_car = st_read('./qgis/20.광양시_행정경계(읍면동)/20.광양시_행정경계(읍면동).geojson')
head(gwangyang_electronic_car)

gwangyang_ecar_potential_buyer = st_read('./qgis/eccar_potential_buyer/eccar_potential_buyer.shp')
gwangyang_ecar_potential_buyer = st_transform(gwangyang_ecar_potential_buyer, "+proj=longlat +datum=WGS84")
gwangyang_ecar_potential_buyer = na.omit(gwangyang_ecar_potential_buyer)
# gwangyang_ecar_potential_buyer$잠재적_구[is.na(gwangyang_ecar_potential_buyer$잠재적_구)] = 0
gwangyang_ecar_potential_buyer = gwangyang_ecar_potential_buyer[gwangyang_ecar_potential_buyer$잠재적_구!=0, ]
gwangyang_ecar_potential_buyer = st_join(gwangyang_ecar_potential_buyer, gwangyang_hangjeongdong)
gwangyang_ecar_potential_buyer_number = gwangyang_ecar_potential_buyer %>% group_by(dong) %>% summarise(number=sum(`잠재적_구`))

df = gwangyang_ecar_potential_buyer_number %>% st_set_geometry(NULL)
gwangyang_ecar_potential_buyer_number = left_join(gwangyang_hangjeongdong, df, by=c('dong'))
gwangyang_ecar_potential_buyer_number$number[is.na(gwangyang_ecar_potential_buyer_number$number)] = 0

# cols = colorNumeric('YlOrRd', gwangyang_ecar_potential_buyer_number$number)
# leaflet(data=gwangyang_ecar_potential_buyer_number) %>%
#   addTiles() %>%
#   addPolygons(
#     opacity=1.0, fillOpacity=0.5,
#     weight=1,
#     color="#000000",
#     fillColor=~cols(number),
#     group=~dong,
#     highlightOptions=highlightOptions(color='red', weight=3, bringToFront=TRUE),
#     label=sprintf("<strong>%s</strong><br/><strong>잠재구매고객수: %s</strong>",
#                   gwangyang_ecar_potential_buyer_number$dong, gwangyang_ecar_potential_buyer_number$number) %>% lapply(htmltools::HTML),
#     labelOptions=labelOptions(
#       style=list('font-weight'='normal', 'box-shadow'="3px 3Px rgba(0,0,0,25)", padding="3px 8px"),
#       textsize='15px',
#       direction='auto'
#     )
# )

gwangyang_ecar_potential_buyer$long = st_coordinates(st_centroid(gwangyang_ecar_potential_buyer))[,1]
gwangyang_ecar_potential_buyer$lat = st_coordinates(st_centroid(gwangyang_ecar_potential_buyer))[,2]
df = gwangyang_ecar_potential_buyer %>% st_set_geometry(NULL)
# leafletMap = leaflet() %>% addTiles()

# 
# leafletMap %>%
#   addMarkers(data=df,
#              lng=~long, lat=~lat,
#              label=~as.character(잠재적_구),
#              popup=~as.character(잠재적_구),
#              clusterOptions=markerClusterOptions(removeOutsideVisibleBounds=F),
#              labelOptions=labelOptions(noHide=F,
#                                        direction='auto'))



bbox = st_bbox(gwangyang_hangjeongdong)
xmin = as.numeric(bbox$xmin)
ymin = as.numeric(bbox$ymin)
xmax = as.numeric(bbox$xmax)
ymax = as.numeric(bbox$ymax)

db = gwangyang_hangjeongdong_people
db$population = as.integer(gsub(',','', db$population))
db = db[order(db$population), ]

################################################################################
# 충전소 위치 데이터 찍기
# gwangyang_electronic_charging_station = read.csv('./data/csv/electronic_charging_station_in_gwangyang.csv', fileEncoding="utf-8")
gwangyang_electronic_charging_station = read.csv('./data/csv/광양충전소현황.csv')
gwangyang_electronic_charging_station = gwangyang_electronic_charging_station %>% select(`충전소`, `주소`, x, y)
names(gwangyang_electronic_charging_station)[3:4] = c('lon', 'lat')
gwangyang_electronic_charging_station$id = as.character(c(1:nrow(gwangyang_electronic_charging_station)))
gwangyang_electronic_charging_station$급속_완속='완속'
################################################################################
categories = c("인구", "전기차 충전소", "전기차보급현황", "건물", "생활인구(TODO)")

ui = bootstrapPage(
  
  navbarPage(theme=shinytheme("flatly"), collapsible=TRUE,
             title="광양시 대시보드", id="nav",
             
             tabPanel("광양시 지도",
                      div(class="outer",
                          tags$head(includeCSS("./css/style.css")),
                          leafletOutput("map", width="100%", height="100%"),
                          absolutePanel(id="controls", class="panel panel-default",
                                        top=75, left=55, width=350, fixed=TRUE,
                                        draggable=TRUE, height="auto",
                                        
                                        selectInput("category", "분류",
                                                    choices=categories,
                                                    selected="인구",
                                                    multiple=TRUE
                                        ),
                                        
                                        selectInput("colors", "Color scheme",
                                                    rownames(subset(brewer.pal.info, category %in% c("seq", div))),
                                                    selected="Blues"
                                        ),
                                        
                                        sliderInput("pixel", "image size",
                                                    min=20, max=100, value=50),
                                        highchartOutput("population_barchart")
                                        
                          )
                      )
             )
             
  )
)


server = function(input, output) {

  leafletDB = reactive({
    gwangyang_hangjeongdong_people
  })
  
  colorpal = reactive({
    colorNumeric("YlOrRd", gwangyang_electronic_car$`전기차보급`)
  })
  
  colorpal_for_population = reactive({
    colorNumeric(input$colors, db$population)
  })
  
  output$map = renderLeaflet({
    
    gwangyang_map = leaflet(db) %>%
      addTiles() %>%
      fitBounds(~xmin, ~ymin, ~xmax, ~ymax) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
   observe({
    
    proxy = leafletProxy("map", data=db)
    if ("인구" %in% input$category) {
      pop_labels = sprintf(
        '<strong>행정동: %s</strong><br/><span><strong>인구수: %s명</strong></span>',
        db$dong,
        comma(db$population, format="d")
      ) %>% lapply(htmltools::HTML)
      
      pal = colorpal_for_population()
      
      proxy %>%
        addLegend("bottomright", pal=pal, values=~population,
                  title='인구수', layerId="population_legend",
                  labFormat=labelFormat(suffix='명'), opacity=1) %>%
        addMapPane("population_polygons", zIndex=300) %>%
        addPolygons(
          opacity=1.0, fillOpacity=0.5,
          weight=1,
          fillColor=~pal(population),
          color='#000000',
          group=~dong,
          layerId=~code,
          highlightOptions=highlightOptions(color="red", weight=3, bringToFront=TRUE),
          label=pop_labels,
          labelOptions=labelOptions(
            style=list('font-weight'='normal', padding='3px 8px'),
            textsize='15px',
            direction='auto'
          ),
          options=pathOptions(pane="population_polygons")
        )
    } else {
      proxy %>%
        removeControl(layerId="population_legend") %>%
        removeShape(layerId=~code)
        
    }
     
    proxy = leafletProxy("map", data=gwangyang_electronic_charging_station)
    print("전기차 충전소" %in% input$category)
    
    if ("전기차 충전소" %in% input$category) {
      
      icons = iconList(
        급속 = makeIcon(iconUrl="./images/fast_charge.png", iconWidth=input$pixel, iconHeight=input$pixel),
        완속 = makeIcon(iconUrl="./images/slow_charge.png", iconWidth=input$pixel, iconHeight=input$pixel)
      )
      
      # popups = sprintf('<strong>충전소위치: %s</strong><br/><span><strong>충전소: %s</strong></span><br/>
      #                  <strong><span>유형: %s</span><br/><span>이용대수: %d</span></strong>',
      #                  gwangyang_electronic_charging_station$충전소위치,
      #                  gwangyang_electronic_charging_station$충전소명,
      #                  gwangyang_electronic_charging_station$급속_완속,
      #                  gwangyang_electronic_charging_station$이용대수
      # ) %>% lapply(htmltools::HTML)
      popups = sprintf('<strong>충전소 위치: %s</strong><br/><span><strong>주소: %s</strong></span><br/>', 
                      gwangyang_electronic_charging_station$충전소,
                      gwangyang_electronic_charging_station$주소)
      print(head(gwangyang_electronic_charging_station))
      proxy %>%
        # addPolygons(data=gwangyang_si, 
        #             opacity=1.0, fillOpacity=0.2,
        #             weight=3, color="#FFFFFF", fillColor="orange",
        #             layerId="sido",
        #             highlightOptions=highlightOptions(color="red", weight=3, bringToFront=TRUE),
        #             label=sprintf('<strong>%s</strong>', gwangyang_si$SIG_KOR_NM) %>% lapply(htmltools::HTML),
        #             labelOptions=labelOptions(
        #               style=list('font-weight'='normal', padding='3px 8px'),
        #               textsize='15px',
        #               direction='auto'
        #             )) %>%
        addMarkers(lng=~lon, lat=~lat, layerId=~id, popup=popups, icon=~icons[`급속_완속`])
    } else {
      proxy %>% 
        removeShape(layerId="sido") %>%
        removeMarker(layerId=~id)
    }
    
    
    proxy = leafletProxy("map", data=gwangyang_electronic_car)
    
    if ("전기차보급현황" %in% input$category) {
      pop_labels = sprintf(
        '<strong>행정동: %s</strong><br/><span><strong>전기차보급대수: %s대</strong></span>',
        gwangyang_electronic_car$ADM_DR_NM,
        comma(gwangyang_electronic_car$전기차보급, format="d")
      ) %>% lapply(htmltools::HTML)
      
      pal = colorpal()
      
      proxy %>%
        addLegend("bottomright", pal=pal, values=~`전기차보급`,
                  title='전기차보급대수', layerId="have_electronic_car",
                  labFormat=labelFormat(suffix='명'), opacity=1) %>%
        addPolygons(
          opacity=1.0, fillOpacity=0.5,
          weight=1,
          fillColor=~pal(`전기차보급`),
          color='#000000',
          group=~ADM_DR_NM,
          layerId=~paste0(ADM_DR_CD, "_car"),
          highlightOptions=highlightOptions(color="red", weight=3, bringToFront=TRUE),
          label=pop_labels,
          labelOptions=labelOptions(
            style=list('font-weight'='normal', padding='3px 8px'),
            textsize='15px',
            direction='auto'
          )
        )
    } else {
      proxy %>%
        removeControl(layerId="have_electronic_car") %>%
        removeShape(layerId=~paste0(ADM_DR_CD, "_car"))
    }
    
    proxy = leafletProxy("map", data=gwangyang_house)
    
    if ("주거건물" %in% input$category) {
      pop_labels = sprintf(
        '<strong>용도: %s</strong>', gwangyang_house$세부용도명
      ) %>% lapply(htmltools::HTML)
      print(paste0("주거건물: ", "주거건물" %in% input$category))
      
      proxy %>%
        # addPolygons(
        #   opacity=1.0, fillOpacity=0.5,
        #   weight=1,
        #   color='#000000',
        #   group=~`세부용도명`,
        #   layerId=~`고유번호`,
        #   highlightOptions=highlightOptions(color="red", weight=3, bringToFront=TRUE),
        #   label=pop_labels,
        #   labelOptions=labelOptions(
        #     style=list('font-weight'='normal', padding='3px 8px'),
        #     textsize='15px',
        #     direction='auto'
        #   )
        # ) %>%
        addCircles(lng=~long, lat=~lat, color="green", opacity=0.5, fillOpacity=0.5, layerId=~`고유번호`,label=~`세부용도명`) %>%
        addHeatmap(lng=~long, lat=~lat, intensity=~1, blur=20, max=0.05, radius=15, layerId="house_heatmap")
        
    } else {
      proxy %>%
        removeShape(layerId=~`고유번호`) %>%
        removeHeatmap(layerId="house_heatmap")
    }
        
    proxy = leafletProxy("map", data=gwangyang_apartment)
    
    if ("아파트" %in% input$category) {
      pop_labels = sprintf(
        '<strong>용도: %s</strong>', gwangyang_apartment$세부용도명
      ) %>% lapply(htmltools::HTML)
      print(paste0("아파트: ", "아파트" %in% input$category))
      print(gwangyang_apartment)
      # print(sum(gwangyang_apartment_number$count))
      proxy %>%
        # addPolygons(
        #   opacity=1.0, fillOpacity=0.5,
        #   weight=1,
        #   color='#000000',
        #   group=~`세부용도명`,
        #   layerId=~`고유번호`,
        #   highlightOptions=highlightOptions(color="red", weight=3, bringToFront=TRUE),
        #   label=pop_labels,
        #   labelOptions=labelOptions(
        #     style=list('font-weight'='normal', padding='3px 8px'),
        #     textsize='15px',
        #     direction='auto'
        #   )
        # ) %>%
        
        # addCircles(lng=~long, lat=~lat, color="red", layerId=c(1:nrow(gwangyang_apartment)), opacity=0.5, fillOpacity=0.5, label=~`세부용도명`) %>%
        # addHeatmap(lng=~long, lat=~lat, intensity=~1, blur=20, max=0.05, radius=15, layerId="apart_heatmap")
        addCircles(lng=~long, lat=~lat, color="red", layerId=paste0("apartment", c(1:nrow(gwangyang_apartment))), opacity=0.5, fillOpacity=0.5, label=~`세부용도명`) %>%
        addMarkers(lng=~long, lat=~lat, label=pop_labels, icon=makeIcon("./images/icon.png", iconWidth="10", iconHeight="10"), layerId=paste0("apartment", c(1:nrow(gwangyang_apartment))),
                   clusterOptions=markerClusterOptions(removeOutsideVisibleBounds=F),
                   labelOptions=labelOptions(noHide=F, direction='auto')) # %>%
        # addAwesomeMarkers(lng=~long, lat=~lat)
    } else {
      proxy %>%
        removeMarkerCluster(layerId=paste0("apartment", c(1:nrow(gwangyang_apartment)))) %>%
        removeShape(layerId=paste0("apartment", c(1:nrow(gwangyang_apartment)))) 
        # %>% removeHeatmap(layerId="apart_heatmap")
    }
    
    
    proxy = leafletProxy("map", data=gwangyang_accommodation)
    
    if ("숙박" %in% input$category) {
      pop_labels = sprintf(
        '<strong>용도: %s</strong>', gwangyang_accommodation$세부용도명
      ) %>% lapply(htmltools::HTML)
      print(paste0("숙박: ", "숙박" %in% input$category))
      print(gwangyang_accommodation)
      
      proxy %>%
        addCircles(lng=~long, lat=~lat, color="blue", opacity=0.5, fillOpacity=0.5, layerId=paste0("accommodation", c(1:nrow(gwangyang_accommodation))), label=~`세부용도명`) %>%
        addMarkers(lng=~long, lat=~lat, label=~`세부용도명`, icon=makeIcon("./images/icon.png", iconWidth="30", iconHeight="30"), layerId=paste0("accommodation", c(1:nrow(gwangyang_accommodation))),
                   clusterOptions=markerClusterOptions(removeOutsideVisibleBounds=F),
                   labelOptions=labelOptions(noHide=F, direction='auto'))
        #addHeatmap(lng=~long, lat=~lat, intensity=1, blur=20, max=0.05, radius=15, layerId="accommodation_heatmap")
    } else {
      proxy %>%
        #  %>%
        removeShape(layerId=paste0("accommodation", c(1:nrow(gwangyang_accommodation)))) %>%
        removeMarkerCluster(layerId=paste0("accommodation", c(1:nrow(gwangyang_accommodation))))
        #removeHeatmap(layerId="accommodation_heatmap")
    }    
  })
  
  output$population_barchart = renderHighchart({
    
    chart=highchart() %>%
      hc_exporting(enabled=TRUE, formAttributes=list(target="_blank")) %>%
      hc_xAxis(title=list("읍면동"), categories=db$dong, linewidth=3) %>%
      hc_yAxis_multiples(
        list(title=list("인구수"), linewidth=3, labels=list(format="{value}명")),
        list(title=list("전기차보급"), linewidth=3, labels=list(format="{value}대"), opposite=TRUE)
      ) %>%
      hc_add_series(name="인구수", data=db$population, type="column", yAxis=0) %>%
      hc_add_series(name="전기차보급", data=gwangyang_electronic_car$전기차보급, type="column", yAxis=1) %>%
      hc_colors(c("#FFC300", "blue")) %>%
      hc_plotOptions(
        column=list(
          dataLabels=list(enabled=FALSE),
          enableMouseTracking=TRUE
        )
      ) %>%
      hc_tooltip(crosshairs=TRUE,
                 table=TRUE,
                 sort=TRUE,
                 backgroundColor="black",
                 pointFormat=paste0('<br><span style="color: {point.color}; font-size:15px":</span>',
                                    "{series.name}: {point.y}"),
                 headerFormat='<span style="color: {point.color}; font-size:15px">행정동: {point.key}</span>'
                 )
    
    
  })
}

shinyApp(ui=ui, server=server)