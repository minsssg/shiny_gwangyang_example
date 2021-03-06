library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)
library(sf)
library(tidyverse)
library(RColorBrewer)
library(highcharter)
library(formattable)
library(fontawesome)

gwangyang_hangjeongdong = st_read('./qgis/20.gwangyang_hangjeongdong.geojson', options="ENCODING=CP949")
gwangyang_hangjeongdong = gwangyang_hangjeongdong %>% rename("code"="ADM_DR_CD", "dong"="ADM_DR_NM")
gwangyang_house = st_read("./qgis/gwangyang_house.shp")
gwangyang_house = st_join(gwangyang_house, gwangyang_hangjeongdong)
gwangyang_house$long = st_coordinates(st_centroid(gwangyang_house))[,1]
gwangyang_house$lat = st_coordinates(st_centroid(gwangyang_house))[,2]
gwangyang_house$code = paste0("주택", c(1:nrow(gwangyang_house)))
gwangyang_house = gwangyang_house %>% rename("name"="세부용도명")
gwangyang_house_df = gwangyang_house %>% select(code, name, dong, long, lat) %>% st_set_geometry(NULL)
gwangyang_house_number = gwangyang_house %>% st_set_geometry(NULL)
gwangyang_house_number_by_dong = gwangyang_house_number %>% group_by(dong) %>% summarise("주택수"=n())

gwangyang_apartment = st_read("./qgis/gwangyang_apartment.shp", options="ENCODING=CP949")
gwangyang_apartment = st_join(gwangyang_apartment, gwangyang_hangjeongdong)
gwangyang_apartment$long = st_coordinates(st_centroid(gwangyang_apartment))[,1]
gwangyang_apartment$lat = st_coordinates(st_centroid(gwangyang_apartment))[,2]
gwangyang_apartment$code = paste0("아파트", c(1:nrow(gwangyang_apartment)))
gwangyang_apartment = gwangyang_apartment %>% rename("name"="세부용도명")
gwangyang_apartment_df = gwangyang_apartment %>% select(code, name, dong, long, lat) %>% st_set_geometry(NULL)
gwangyang_apartment_number = gwangyang_apartment %>% st_set_geometry(NULL)
gwangyang_apartment_number_by_dong = gwangyang_apartment_number %>% group_by(dong) %>% summarise("아파트수"=n())

gwangyang_accommodation = st_read("./qgis/gwangyang_accommodation.shp", options="ENCODING=CP949")
gwangyang_accommodation = st_join(gwangyang_accommodation, gwangyang_hangjeongdong)
gwangyang_accommodation$long = st_coordinates(st_centroid(gwangyang_accommodation))[,1]
gwangyang_accommodation$lat = st_coordinates(st_centroid(gwangyang_accommodation))[,2]
gwangyang_accommodation$code = paste0("숙박", c(1:nrow(gwangyang_accommodation)))
gwangyang_accommodation = gwangyang_accommodation %>% rename("name"="세부용도명")
gwangyang_accommodation_df = gwangyang_accommodation %>% select(code, name, dong, long, lat) %>% st_set_geometry(NULL)
gwangyang_accommodation_number = gwangyang_accommodation %>% st_set_geometry(NULL)
gwangyang_accommodation_number_by_dong = gwangyang_accommodation_number %>% group_by(dong) %>% summarise("숙박수"=n())

gwangyang_offices = st_read("./qgis/gwangyang_offices.shp", options="ENCODING=CP949")
gwangyang_offices = st_join(gwangyang_offices, gwangyang_hangjeongdong)
gwangyang_offices$long = st_coordinates(st_centroid(gwangyang_offices))[,1]
gwangyang_offices$lat = st_coordinates(st_centroid(gwangyang_offices))[,2]
gwangyang_offices$code = paste0("관공서", c(1:nrow(gwangyang_offices)))
gwangyang_offices = gwangyang_offices %>% rename("name"="세부용도명")
gwangyang_offices_df = gwangyang_offices %>% select(code, name, dong, long, lat) %>% st_set_geometry(NULL)
gwangyang_offices_number = gwangyang_offices %>% st_set_geometry(NULL)
gwangyang_offices_number_by_dong = gwangyang_offices_number %>% group_by(dong) %>% summarise("관공서수"=n())

gwangyang_public = st_read("./qgis/gwangyang_public.shp", options="ENCODING=CP949")
gwangyang_public = st_join(gwangyang_public, gwangyang_hangjeongdong)
gwangyang_public$long = st_coordinates(st_centroid(gwangyang_public))[,1]
gwangyang_public$lat = st_coordinates(st_centroid(gwangyang_public))[,2]
gwangyang_public$code = paste0("다중이용시설", c(1:nrow(gwangyang_public)))
gwangyang_public = gwangyang_public %>% rename("name"="DGM_NM")
gwangyang_public_df = gwangyang_public %>% select(code, name, dong, long, lat) %>% st_set_geometry(NULL)
gwangyang_public_number = gwangyang_public %>% st_set_geometry(NULL)
gwangyang_public_number_by_dong = gwangyang_public_number %>% group_by(dong) %>% summarise("다중이용시설수"=n())

gwangyang_dong = gwangyang_hangjeongdong %>% st_set_geometry(NULL)

gwangyang_food = read.csv("./data/csv/gwangyang_food.csv", fileEncoding="cp949")
gwangyang_food = gwangyang_food %>% select(개방서비스명, 업태구분명, 시군구코드, dong, X좌표, Y좌표)
gwangyang_food = gwangyang_food %>% rename("name"="업태구분명", "long"="X좌표", "lat"="Y좌표")
gwangyang_food$code = paste0("음식점", c(1:nrow(gwangyang_food)))
gwangyang_food_df = gwangyang_food %>% select(code,name, dong, long, lat)
gwangyang_food_by_dong = gwangyang_food %>% group_by(dong) %>% summarise("음식점수"=n())

gwangyang_mart = read.csv("./data/csv/gwangyang_mart.csv", fileEncoding="cp949")
gwangyang_mart$고유번호 = c(1:nrow(gwangyang_mart))
gwangyang_mart = gwangyang_mart %>% rename("name"="이름", "long"="경도", "lat"="위도")
gwangyang_mart$code = paste0("마트", c(1:nrow(gwangyang_mart)))
gwangyang_mart = gwangyang_mart %>% select(code, name, dong, long, lat)
gwangyang_mart_df = gwangyang_mart %>% select(code, name, dong, long, lat)
gwangyang_mart_by_dong = gwangyang_mart %>% group_by(dong) %>% summarise("마트수"=n())

gwangyang_hangman = st_read("./qgis/gwangyang_hangman.shp", options="ENCODING=CP949")
gwangyang_hangman = st_transform(gwangyang_hangman, "+proj=longlat +datum=WGS84")

gwangyang_parking = st_read("./qgis/gwangyang_parking.shp", options="ENCODING=CP949")
gwangyang_parking = st_transform(gwangyang_parking, "+proj=longlat +datum=WGS84")
gwangyang_parking = st_join(gwangyang_parking, gwangyang_hangjeongdong)
gwangyang_parking = gwangyang_parking %>% rename("name"="DGM_NM")
gwangyang_parking$long = st_coordinates(st_centroid(gwangyang_parking))[,1]
gwangyang_parking$lat = st_coordinates(st_centroid(gwangyang_parking))[,2]
gwangyang_parking$code = paste0("주차장", c(1:nrow(gwangyang_parking)))
gwangyang_parking_df = gwangyang_parking %>% select(code, name, dong, long, lat) %>% st_set_geometry(NULL)
gwangyang_parking_number = gwangyang_parking %>% st_set_geometry(NULL)
gwangyang_parking_number_by_dong = gwangyang_public_number %>% group_by(dong) %>% summarise("주차장수"=n())

###############################################################################
gwangyang_building_db = list("주택"=gwangyang_house_number, 
                             "아파트"=gwangyang_apartment_number, 
                             "숙박"=gwangyang_accommodation_number,
                             "음식점"=gwangyang_food,
                             "관공서"=gwangyang_offices_number,
                             "다중이용시설"=gwangyang_public_number,
                             "마트"=gwangyang_mart,
                             "주차장"=gwangyang_parking_number)

gwangyang_electronic_car = st_read('./qgis/gwangyang_electronic_car.geojson', options="ENCODING=CP949")
gwangyang_electronic_car = gwangyang_electronic_car %>% rename("code"="ADM_DR_CD", "dong"="ADM_DR_NM")

gwangyang_ecar_potential_buyer = st_read('./qgis/eccar_potential_buyer.shp', options="ENCODING=CP949")
gwangyang_ecar_potential_buyer = st_transform(gwangyang_ecar_potential_buyer, "+proj=longlat +datum=WGS84")
gwangyang_ecar_potential_buyer = na.omit(gwangyang_ecar_potential_buyer)
gwangyang_ecar_potential_buyer = st_join(gwangyang_hangjeongdong,gwangyang_ecar_potential_buyer)
gwangyang_ecar_potential_buyer_number = gwangyang_ecar_potential_buyer %>% group_by(dong) %>% summarise("잠재적구매자수"=sum(`잠재적_구`))
# gwangyang_ecar_potential_buyer_number = gwangyang_ecar_potential_buyer_number %>% st_set_geometry(NULL)
gwangyang_ecar_potential_buyer_number$code = paste0("잠재적구매자",c(1:nrow(gwangyang_ecar_potential_buyer_number)))
gwangyang_ecar_potential_buyer$long = st_coordinates(st_centroid(gwangyang_ecar_potential_buyer))[,1]
gwangyang_ecar_potential_buyer$lat = st_coordinates(st_centroid(gwangyang_ecar_potential_buyer))[,2]

################################################################################
# 충전소 위치 데이터 찍기
gwangyang_electronic_charging_station = read.csv('./data/csv/gwangyang_electronic_charging_station_update.csv', fileEncoding="cp949")
gwangyang_electronic_charging_station = gwangyang_electronic_charging_station %>% select(`충전소`, dong, `주소`, long, lat)
gwangyang_electronic_charging_station$code = paste0("충전소", c(1:nrow(gwangyang_electronic_charging_station)))
gwangyang_electronic_charging_station$급속_완속='완속'
gwangyang_electronic_charging_station_number = gwangyang_electronic_charging_station %>% group_by(dong) %>% summarise(count=n())
gwangyang_electronic_charging_station_number = gwangyang_dong %>% left_join(gwangyang_electronic_charging_station_number, by="dong") %>% select(dong, count)
gwangyang_electronic_charging_station_number$count[is.na(gwangyang_electronic_charging_station_number$count)] = 0

bbox = st_bbox(gwangyang_hangjeongdong)
xmin = as.numeric(bbox$xmin)
ymin = as.numeric(bbox$ymin)
xmax = as.numeric(bbox$xmax)
ymax = as.numeric(bbox$ymax)

df = rbind(gwangyang_house_df, gwangyang_apartment_df, gwangyang_accommodation_df,
           gwangyang_offices_df, gwangyang_public_df, gwangyang_food_df, gwangyang_mart_df, gwangyang_parking_df)
#db = gwangyang_hangjeongdong_people
db = gwangyang_ecar_potential_buyer_number

db = left_join(db, gwangyang_electronic_car%>% st_set_geometry(NULL) %>% select(dong, `전기차보급`), by="dong")
db = left_join(db, gwangyang_electronic_charging_station_number, by="dong")
db = left_join(db, gwangyang_house_number_by_dong, by="dong")
db = left_join(db, gwangyang_apartment_number_by_dong, by="dong")
db = left_join(db, gwangyang_accommodation_number_by_dong, by="dong")
db = left_join(db, gwangyang_offices_number_by_dong, by="dong")
db = left_join(db, gwangyang_public_number_by_dong, by="dong")
db = left_join(db, gwangyang_food_by_dong, by="dong")
db = left_join(db, gwangyang_mart_by_dong, by="dong")
db = left_join(db, gwangyang_parking_number_by_dong, by="dong")
db[is.na(db)] = 0
#db = db[order(db$population), ]

categories = c("전기차 충전소", "전기차보급현황", "건물", "항만")

ui = bootstrapPage(
  
  navbarPage(theme=shinytheme("flatly"), collapsible=TRUE,
             title="광양시 대시보드", id="nav",
             
             tabPanel("광양시 지도",
                      div(class="outer",
                          tags$head(includeCSS("./css/style.css"),
                                    includeScript("./js/gomap.js")),
                          leafletOutput("map", width="100%", height="100%"),
                          absolutePanel(id="controls", class="panel panel-default",
                                        top=75, left=55, width=350, fixed=TRUE,
                                        draggable=TRUE, height="auto",
                                        
                                        selectInput("category", "분류",
                                                    choices=categories,
                                                    multiple=TRUE
                                        ),
                                        
                                        selectInput("colors", "Color scheme",
                                                    rownames(subset(brewer.pal.info, category %in% c("seq", div))),
                                                    selected="Blues"
                                        ),
                                        
                                        sliderInput("pixel", "image size",
                                                    min=20, max=100, value=50)
                        
                          )
                      )
             ),
             
             tabPanel("동별 통계 차트",
                      sidebarLayout(
                        sidebarPanel(
                          span(tags$i(h3("광양시 동별 통계"))),
                          
                          pickerInput("dongs",
                                      "동 선택:",
                                      options=list(`actions-box`=TRUE, `none-selected-text`="선택해주세요!"),
                                      choices=gwangyang_hangjeongdong$dong,
                                      selected=gwangyang_hangjeongdong$dong,
                                      multiple=TRUE),
                          
                          pickerInput("category_names",
                                      "카테고리:",
                                      options=list(`actions-box`=TRUE, `none-selected-text`="선택해주세요!"),
                                      choices=c("인구", "건물"),
                                      multiple=FALSE)
                        ),
                        
                        mainPanel(
                          tabsetPanel(
                            tabPanel("동별 비교", highchartOutput("population_barchart"))
                          )
                        )
                      )
             ),
             
             tabPanel("데이터 탐색",
                      id="dataset",
                      tabsetPanel(
                        tabPanel("건물데이터", DT::dataTableOutput("building_db")),
                        tabPanel("충전소 현황", DT::dataTableOutput("charging_station"))
                      )
                      
             ),
             conditionalPanel("false", icon("crosshair"))
             
  )
)

server = function(input, output, session) {

  check = list(#"잠재적구매자"=TRUE,
               "전기차 충전소"=TRUE,
               "전기차보급현황"=TRUE,
               "건물"=TRUE,
               "항만"=TRUE)
  leafletDB = reactive({
    gwangyang_hangjeongdong_people
  })
  
  colorpal = reactive({
    colorNumeric(input$colors, gwangyang_electronic_car$`전기차보급`)
  })
  
  output$map = renderLeaflet({
    
    gwangyang_map = leaflet(db) %>%
      addTiles() %>%
      fitBounds(~xmin, ~ymin, ~xmax, ~ymax) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })
  
  observe({
    
    # proxy = leafletProxy("map", data=db)
    
    # if ("잠재적구매자" %in% input$category & as.logical(check["잠재적구매자"])) {
    #   
    #   check["잠재적구매자"] = FALSE
    #   pop_labels = sprintf(
    #     '<strong>행정동: %s</strong><br/><span><strong>인구수: %s명</strong></span>',
    #     db$dong,
    #     comma(db$`잠재적구매자수`, format="d")
    #   ) %>% lapply(htmltools::HTML)
    #   
    #   pal = colorpal_for_population()
    #   
    #   proxy %>%
    #     addLegend("bottomright", pal=pal, values=~`잠재적구매자수`,
    #               title='잠재적구매자수', layerId="population_legend",
    #               labFormat=labelFormat(suffix='명'), opacity=1) %>%
    #     addMapPane("population_polygons", zIndex=300) %>%
    #     addPolygons(
    #       opacity=1.0, fillOpacity=0.5,
    #       weight=1,
    #       fillColor=~pal(`잠재적구매자수`),
    #       color='#000000',
    #       group=~dong,
    #       layerId=~code,
    #       highlightOptions=highlightOptions(color="red", weight=3, bringToFront=TRUE),
    #       label=pop_labels,
    #       labelOptions=labelOptions(
    #         style=list('font-weight'='normal', padding='3px 8px'),
    #         textsize='15px',
    #         direction='auto'
    #       ),
    #       options=pathOptions(pane="population_polygons")
    #     )
    # } else if (!("잠재적구매자" %in% input$category)) {
    #   #check["잠재적구매자"] = TRUE
    #   proxy %>%
    #     removeControl(layerId="population_legend") %>%
    #     removeShape(layerId=~code)
    # }
    
     
    proxy = leafletProxy("map", data=gwangyang_electronic_charging_station)
    
    if ("전기차 충전소" %in% input$category & as.logical(check["전기차 충전소"])) {
      check["전기차 충전소"] = FALSE
      icons = iconList(
        급속 = makeIcon(iconUrl="./images/fast_charge.png", iconWidth=input$pixel, iconHeight=input$pixel),
        완속 = makeIcon(iconUrl="./images/slow_charge.png", iconWidth=input$pixel, iconHeight=input$pixel)
      )
      groups <- c("주택"<-"<div style='position: relative; display: inline-block' class='awesome-marker-icon-cadetblue awesome-marker'><i class='glyphicon glyphicon-glass icon-white'></i></div>급속충전소",
                  "아파트"<-"<div style='position: relative; display: inline-block' class='awesome-marker-icon-blue awesome-marker'><i class='glyphicon glyphicon-home icon-white'></i></div>완속충전소")
      
      popups = sprintf('<strong>충전소 위치: %s</strong><br/><span><strong>주소: %s</strong></span><br/>', 
                      gwangyang_electronic_charging_station$충전소,
                      gwangyang_electronic_charging_station$주소)
      proxy %>%
        addMarkers(lng=~long, lat=~lat, layerId=~code, popup=popups, icon=~icons[`급속_완속`])
    } else if (!("전기차 충전소" %in% input$category)){
      #check["전기차 충전소"] = TRUE
      proxy %>% 
        removeShape(layerId="sido") %>%
        removeMarker(layerId=~code)
    }
    
    
    proxy = leafletProxy("map", data=gwangyang_electronic_car)
    
    if ("전기차보급현황" %in% input$category & as.logical(check["전기차보급현황"])) {
      check["전기차보급현황"] = FALSE
      pop_labels = sprintf(
        '<strong>행정동: %s</strong><br/><span><strong>전기차보급대수: %s대</strong></span>',
        gwangyang_electronic_car$dong,
        comma(gwangyang_electronic_car$전기차보급, format="d")
      ) %>% lapply(htmltools::HTML)
      
      pal = colorpal()
      
      proxy %>%
        addLegend("bottomright", pal=pal, values=~`전기차보급`,
                  title='전기차보급대수', layerId="have_electronic_car",
                  labFormat=labelFormat(suffix='대'), opacity=1) %>%
        addPolygons(
          opacity=1.0, fillOpacity=0.5,
          weight=1,
          fillColor=~pal(`전기차보급`),
          color='#000000',
          group=~dong,
          layerId=~paste0(code, "_car"),
          highlightOptions=highlightOptions(color="red", weight=3, bringToFront=TRUE),
          label=pop_labels,
          labelOptions=labelOptions(
            style=list('font-weight'='normal', padding='3px 8px'),
            textsize='15px',
            direction='auto'
          )
        )
    } else if (!("전기차보급현황" %in% input$category)){
      #check["전기차보급현황"] = TRUE
      proxy %>%
        removeControl(layerId="have_electronic_car") %>%
        removeShape(layerId=~paste0(code, "_car"))
    }
    
    if ("건물" %in% input$category & as.logical(check["건물"])) {
      check["건물"] = FALSE
      IconSet = awesomeIconList(
        "주택" = makeAwesomeIcon(icon="home", markerColor="cadetblue", iconColor="#ffffff", library="glyphicon"),
        "아파트" = makeAwesomeIcon(icon="building", markerColor="cadetblue", iconColor="#ffffff", library="fa"),
        "숙박" = makeAwesomeIcon(icon="hotel", markerColor="cadetblue", iconColor="#ffffff", library="fa"),
        "관공서" = makeAwesomeIcon(icon="bank", markerColor="cadetblue", iconColor="#ffffff", library="fa"),
        "다중이용시설"= makeAwesomeIcon(icon="child", markerColor="cadetblue", iconColor="#ffffff", library="fa"),
        "음식점" = makeAwesomeIcon(icon="apple", markerColor="cadetblue", iconColor="#ffffff", library="fa"),
        "마트" = makeAwesomeIcon(icon="shopping-cart", markerColor="cadetblue", iconColor="#ffffff", library="fa"),
        "주차장" = makeAwesomeIcon(icon="car", markerColor="cadetblue", iconColor="#ffffff", library="fa")
      )
      # group names
      groups = list("주택"="<div style='position: relative; display: inline-block' class='awesome-marker-icon-cadetblue awesome-marker'><i class='glyphicon glyphicon-home icon-white'></i></div>주택",
                 "아파트"="<div style='position: relative; display: inline-block' class='awesome-marker-icon-cadetblue awesome-marker'><i class='fa fa-building icon-white'></i></div>아파트",
                 "숙박"="<div style='position: relative; display: inline-block' class='awesome-marker-icon-cadetblue awesome-marker'><i class='fa fa-hotel icon-white'></i></div>숙박",
                 "관공서"="<div style='position: relative; display: inline-block' class='awesome-marker-icon-cadetblue awesome-marker'><i class='fa fa-bank icon-white'></i></div>관공서",
                 "다중이용시설"="<div style='position: relative; display: inline-block' class='awesome-marker-icon-cadetblue awesome-marker'><i class='fa fa-child icon-white'></i></div>다중이용시설",
                 "음식점"="<div style='position: relative; display: inline-block' class='awesome-marker-icon-cadetblue awesome-marker'><i class='fa fa-apple icon-white'></i></div>음식점",
                 "마트"="<div style='position: relative; display: inline-block' class='awesome-marker-icon-cadetblue awesome-marker'><i class='fa fa-shopping-cart icon-white'></i></div>마트",
                 "주차장"="<div style='position: relative; display: inline-block' class='awesome-marker-icon-cadetblue awesome-marker'><i class='fa fa-car icon-white'></i></div>주차장")
      
      proxy = leafletProxy("map", data=gwangyang_building_db)
      
      # layer_name = ""
      names(gwangyang_building_db) %>%
        purrr::walk(function(alias) {
          proxy <<- proxy %>%
            addAwesomeMarkers(data=gwangyang_building_db[[alias]],
                       lng=~long, lat=~lat,
                       label=~sprintf(
                         '<strong>용도명: %s<br>행정동: %s<br/>경도: %s<br>위도: %s<br></strong>',
                         name, dong, long, lat
                       ) %>% lapply(htmltools::HTML),
                       popup=~sprintf(
                         '<strong>용도명: %s<br>행정동: %s<br/>경도: %s<br>위도: %s<br></strong>',
                         name, dong, long, lat
                       ) %>% lapply(htmltools::HTML),
                       layerId=~code,
                       group=as.character(groups[alias]),
                       icon=~IconSet[alias],
                       clusterOptions=markerClusterOptions(
                         removeOutsideVisibleBounds=FALSE
                       ),
                       labelOptions=labelOptions(noHide=FALSE, direction='auto', textsize="15px"))
        })
      
      proxy %>%
        addLayersControl(
          overlayGroups=as.character(groups),
          options=layersControlOptions(collapsed=FALSE)
        )
      
    } else if (!("건물" %in% input$category)) {
      #check["건물"] = TRUE
      proxy = leafletProxy("map", data=gwangyang_building_db)
      proxy %>%
        clearMarkerClusters() %>%
        removeLayersControl()
    }
    
    proxy = leafletProxy("map", data=gwangyang_hangman)
    
    if ("항만" %in% input$category & as.logical(check["항만"])) {
      
      pop_labels = sprintf(
        '<strong>용도명: %s</strong>',
        gwangyang_hangman$DGM_NM
      ) %>% lapply(htmltools::HTML)
      
      proxy %>%
        addPolygons(
          opacity=1.0, fillOpacity=0.5,
          weight=1,
          fillColor="cadetblue",
          color='#000000',
          group=~DGM_NM,
          layerId=~DGM_NM,
          highlightOptions=highlightOptions(color="red", weight=3, bringToFront=TRUE),
          label=pop_labels,
          labelOptions=labelOptions(
            style=list('font-weight'='normal', padding='3px 8px'),
            textsize='15px',
            direction='auto'
          )
        )
    } else if (!("항만" %in% input$category)) {
      #check["항만"] = TRUE
      proxy %>%
        removeShape(layerId=~DGM_NM)
    }
    
  })

  output$population_barchart = renderHighchart({
    data = db[db$dong %in% input$dongs, ]
    data = data[order(data$전기차보급),]
    # default 차트 전기관련 차!
    chart=highchart() %>%
      hc_exporting(enabled=TRUE, formAttributes=list(target="_blank")) %>%
      hc_xAxis(title=list("읍면동"), categories=data$dong, linewidth=3) %>%
      hc_yAxis(title=list("대수"), linewidth=3, labels=list(format="{value}개")) %>%
      hc_series(list(name="전기차보급", data=data$전기차보급, type="column", yAxis=0),
                list(name="전기차대수", data=data$count, type="column", yAxis=0)) %>%
      hc_plotOptions(
        column=list(
          dataLabels=list(enabled=FALSE),
          enableMouseTracking=TRUE
        )
      ) %>%
      hc_tooltip(crosshairs=TRUE,
        table=TRUE,
        sort=TRUE,
        backgroundColor="white",
        pointFormat=paste0('<br><span style="color: {point.color}; font-size:15px":</span>',
                           "{series.name}: {point.y}"),
        headerFormat='<span style="color: {point.color}; font-size:15px">행정동: {point.key}</span>'
      )
    
    if ("인구" == input$category_names) {
      chart=highchart() %>%
        hc_exporting(enabled=TRUE, formAttributes=list(target="_blank")) %>%
        hc_xAxis(title=list("읍면동"), categories=data$dong, linewidth=3) %>%
        hc_yAxis_multiples(
          list(title=list("대수"), linewidth=3, labels=list(format="{value}개"), sort=TRUE),
          list(title=list("인구수"), linewidth=3, labels=list(format="{value}명"), opposite=TRUE)
        ) %>%
        hc_series(list(name="전기차보급", data=data$전기차보급, type="column", yAxis=0),
                  list(name="전기차대수", data=data$count, type="column", yAxis=0),
                  list(name="잠재적구매자수", data=data$잠재적구매자수, type="column", yAxis=1)) %>%
        hc_plotOptions(
          column=list(
            dataLabels=list(enabled=FALSE),
            enableMouseTracking=TRUE
          )
        ) %>%
        hc_tooltip(crosshairs=TRUE,
                   table=TRUE,
                   sort=TRUE,
                   backgroundColor="white",
                   pointFormat=paste0('<br><span style="color: {point.color}; font-size:15px":</span>',
                                      "{series.name}: {point.y}"),
                   headerFormat='<span style="color: {point.color}; font-size:15px">행정동: {point.key}</span>'
        )
    } else if ("건물" == input$category_names) {
      chart=highchart() %>%
        hc_exporting(enabled=TRUE, formAttributes=list(target="_blank")) %>%
        hc_xAxis(title=list("읍면동"), categories=data$dong, linewidth=3) %>%
        hc_yAxis_multiples(
          list(title=list("대수"), linewidth=3, labels=list(format="{value}개")),
          list(title=list("건물개수"), linewidth=3, labels=list(format="{value}개"), opposite=TRUE)
        ) %>%
        hc_series(list(name="전기차보급", data=data$전기차보급, type="column", yAxis=0),
                  list(name="전기차댓수", data=data$count, type="column", yAxis=0),
                  list(name="주택", data=data$주택수, type="column", yAxis=1),
                  list(name="아파트", data=data$아파트수, type="column", yAxis=1),
                  list(name="숙박", data=data$숙박수, type="column", yAxis=1),
                  list(name="관공서", data=data$관공서수, type="column", yAxis=1),
                  list(name="다중이용시설", data=data$다중이용시설수, type="column", yAxis=1),
                  list(name="마트", data=data$마트수, type="column", yAxis=1),
                  list(name="주차장", data=data$주차장수, type="column", yAxis=1)) %>%
        hc_plotOptions(
          column=list(
            dataLabels=list(enabled=FALSE),
            enableMouseTracking=TRUE
          )
        ) %>%
        hc_tooltip(crosshairs=TRUE,
                   table=TRUE,
                   sort=TRUE,
                   backgroundColor="white",
                   pointFormat=paste0('<br><span style="color: {point.color}; font-size:15px":</span>',
                                      "{series.name}: {point.y}"),
                   headerFormat='<span style="color: {point.color}; font-size:15px">행정동: {point.key}</span>'
        )
    }
    
    return(chart)
  })
  
  # Show a popup at the given location
  showPopup = function(code, lat, lng) {
    info = df[df$code==code,]
    label = sprintf(
      '<strong>용도명: %s<br>행정동: %s<br/>경도: %s<br>위도: %s<br></strong>',
      info$`name`, info$dong, info$long, info$lat
    ) %>% htmltools::HTML()
    leafletProxy("map") %>% addPopups(lng=lng,lat=lat, popup=label, layerId=code)
  }
  
  showChargingPopup = function(code, lat, lng) {
    info = gwangyang_electronic_charging_station[gwangyang_electronic_charging_station$code==code, ]
    label = sprintf(
      '<strong>충전소: %s<br>행정동: %s<br/>경도: %s<br>위도: %s<br></strong>',
      info$`충전소`, info$dong, info$long, info$lat
    ) %>% htmltools::HTML()
    leafletProxy("map") %>% addPopups(lng=lng,lat=lat, popup=label, layerId=code)
  }
  
  observe({
    if (is.null(input$goto)) return()
    isolate({
      map = leafletProxy("map")
      dist=0.0005
      code=input$goto$code
      lat=input$goto$lat
      lng=input$goto$lng
      if (startsWith(code, "충전소")) {
        showChargingPopup(code, lat, lng)
      } else {
        showPopup(code, lat, lng)
      }
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$building_db = DT::renderDataTable({
    df = df %>%
      mutate(action=paste('<a class="go-map" href="" data-lat="', lat, '"data-long="', long, '"data-code="', code, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action = DT::dataTableAjax(session, df, outputId="building_db")
    DT::datatable(df, options=list(ajax=list(url=action)), escape=FALSE)
  })
  
  output$charging_station = DT::renderDataTable({
    data = gwangyang_electronic_charging_station
    data = data %>%
      mutate(action=paste('<a class="go-map" href="" data-lat="', lat, '"data-long="', long, '"data-code="', code, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action = DT::dataTableAjax(session, data, outputId="charging_station")
    DT::datatable(data, options=list(ajax=list(url=action)), escape=FALSE)
  })
}

shinyApp(ui=ui, server=server)