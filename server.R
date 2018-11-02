# server.R

library(shiny)
library(highcharter)
library(tidyverse)

shinyServer(function(input, output) {
  
  output$HHt <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284120", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("current_village"), contains("current_rectangle_id"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,12,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village", "current_rec_id",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")
      
      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      
    })
    dataLIVE<- reactive(filter(CVA3_HH,submission_date==input$date1))
    datatable(dataLIVE(), filter = "top", extensions = 'Buttons', 
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  output$HHgt <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284120", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("current_village"), contains("current_rectangle_id"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,12,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village", "current_rec_id",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")
      
      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      HHcount <- CVA3_HH %>% count(current_village, submission_date, sort = TRUE)
      HHcount<- as.data.frame(spread(HHcount, submission_date, n))
      
    })
    datatable(HHcount, filter = "top", extensions = 'Buttons', 
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  dataLIVE <- reactive({
    input$loadDataset
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284120", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("current_village"), contains("current_rectangle_id"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,12,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village", "current_rec_id",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")
      
      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      newtry<- merge.data.frame(CVA3_HH, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
      newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
      newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
      
      CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7, 8, 11, 12, 13, 14, 19, 27,28)
      CVA3_HH<-filter(CVA3_HH,submission_date==input$date1)
      #coordinates(CVA3_HH)<- c("lng","lat")
      coordinates(CVA3_HH)<-~lng+lat #make dataframe into points using x y coordinates in sheet
      proj4string(CVA3_HH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      CVA3_HH_p <- spTransform(x = CVA3_HH, CRSobj = proj4string(oblasts_p))
      
      m = gDistance(CVA3_HH_p, settm_p, byid = TRUE)
      row = apply(m, 2, function(x) which(x == min(x)))
      labels = unlist(settm_p@data[row, ]$adm4Pcode)
      CVA3_HH_p@data
      CVA3_HH_p$jNAMEid <- labels
      labels2 = unlist(settm_p@data[row, ]$adm4NameLa)
      CVA3_HH_p$jNAME <- labels2
      
      #attach distance to closest settlment
      dist<-apply(gDistance(CVA3_HH_p, settm_p,byid=TRUE),2,min)
      CVA3_HH_p$dist<- dist
      CVA3_HH<-spTransform(x= CVA3_HH_p, CRSobj = proj4string(raions))
      return(CVA3_HH)
  })
  output$HHmap <- renderLeaflet({
    #input$loadDataset
      CVA3_HH<-dataLIVE()
      #check for far distances from settlement
      LocationDiff<- subset(CVA3_HH, dist>0)
      #check discrepancy in settlement names
      NameDiff<-subset(CVA3_HH,(!CVA3_HH$jNAMEid==CVA3_HH$current_village))
      
    map<- leaflet(options=leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(37.9762, 48.5793, zoom = 7)%>%
      addPolygons(data = oblasts,    # NON-GOVERNMENT CONTROLED AREA
                  color = "black",
                  fill= TRUE,
                  fillColor= "grey",
                  fillOpacity = 0.1,
                  weight= 1,
                  opacity = 1) %>%
      addPolygons(data= raions,    # NUMBER OF IDPS
                  color = "orange",
                  weight = 1,
                  label= raions$NAME_LAT,
                  opacity = 1.0,
                  smoothFactor = 0.5,
                  fill = FALSE) %>%
      
      addPolygons(data = settm,     #NO VALUES RAYON
                  color = "grey",
                  fillColor= "grey",
                  weight=1,
                  fill=TRUE,
                  fillOpacity = 0.2,
                  label = paste0( "Settlement:  ",settm$adm4NameLa, ", ", settm$adm4Pcode),
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE),
                  opacity = 1
                  )%>%
      addCircles(data=CVA3_HH,
                 color="black",
                 weight=5,
                 opacity=1,
                 group="All surveys")%>%
      addCircles(data=NameDiff,
                 weight=9,
                 opacity=1,
                 fillColor = "magenta",
                 color="magenta",
                 group="Settlement Name Differences",
                 popup = paste0('<h7 style="color:black;">',  "Settlement Entered:  ", "<b>", NameDiff$current_village, "</b>", '</h7>', "<br>",
                                NameDiff$uuid,  "<br>", "Coordinate Settlement:", NameDiff$jNAMEid, NameDiff$jNAME))%>%
      addCircles(data=LocationDiff,
                 color="cyan",
                 fillColor = "cyan",
                 weight=9,
                 opacity=1,
                 group="Settlement Location Differences",
                 popup = paste0('<h7 style="color:black;">',  "Settlement Entered:  ", "<b>", LocationDiff$current_village,  "</b>", '</h7>', "<br>",
                                LocationDiff$uuid, "<br>", "Coordinate Settlement:", LocationDiff$jNAMEid))%>%
      addLayersControl(overlayGroups=c("All surveys","Settlement Name Differences", "Settlement Location Differences"))
  })
  output$Commt <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284121", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("current_village"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")

      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)

    })
    dataLIVE<- reactive(filter(CVA3_HH,submission_date==input$date2))
    datatable(dataLIVE(), filter = "top", extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  output$Commgt <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284121", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("current_village"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")

      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      HHcount <- CVA3_HH %>% count(current_village, submission_date, sort = TRUE)
      HHcount<- as.data.frame(spread(HHcount, submission_date, n))

    })
    datatable(HHcount, filter = "top", extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  dataLIVEc <- reactive({
    input$loadDataset
    user_name <- if (input$username == "") NULL else input$username
    password <- if (input$password == "") NULL else input$password
    CVA3_HH <- kobo_data_downloader(
      "284121", c(user_name, password), input$api)
    CVA3_HH<-as.data.frame(CVA3_HH)
    CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                            contains("first_name"),contains("current_village"),contains("gps_work"),
                            contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
    CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
    colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village",
                          "gps_work", "y", "x", "uuid","submisDate", "submisTime")

    CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                       y = as.numeric(y))
    CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
    newtry<- merge.data.frame(CVA3_HH, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
    newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
    newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)

    CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7, 10, 13, 14,17, 19, 26,27)
    CVA3_HH<-filter(CVA3_HH,submission_date==input$date2)
    #coordinates(CVA3_HH)<- c("lng","lat")
    coordinates(CVA3_HH)<-~lng+lat #make dataframe into points using x y coordinates in sheet
    proj4string(CVA3_HH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    CVA3_HH_p <- spTransform(x = CVA3_HH, CRSobj = proj4string(oblasts_p))

    m = gDistance(CVA3_HH_p, settm_p, byid = TRUE)
    row = apply(m, 2, function(x) which(x == min(x)))
    labels = unlist(settm_p@data[row, ]$adm4Pcode)
    CVA3_HH_p$jNAMEid <- labels
    labels2 = unlist(settm_p@data[row, ]$adm4NameLa)
    CVA3_HH_p$jNAME <- labels2

    #attach distance to closest settlment
    dist<-apply(gDistance(CVA3_HH_p, settm_p,byid=TRUE),2,min)
    CVA3_HH_p$dist<- dist
    CVA3_HH<-spTransform(x= CVA3_HH_p, CRSobj = proj4string(raions))
    return(CVA3_HH)
  })
  output$Commmap <- renderLeaflet({
    #input$loadDataset
    CVA3_HH<-dataLIVEc()
    #check for far distances from settlement
    LocationDiff<- subset(CVA3_HH, dist>0)
    #check discrepancy in settlement names
    NameDiff<-subset(CVA3_HH,(!CVA3_HH$jNAMEid==CVA3_HH$current_village))
    
    map<- leaflet(options=leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(37.9762, 48.5793, zoom = 7)%>%
      addPolygons(data = oblasts,    # NON-GOVERNMENT CONTROLED AREA
                  color = "black",
                  fill= TRUE,
                  fillColor= "grey",
                  fillOpacity = 0.1,
                  weight= 1,
                  opacity = 1) %>%
      addPolygons(data= raions,    # NUMBER OF IDPS
                  color = "orange",
                  weight = 1,
                  label= raions$NAME_LAT,
                  opacity = 1.0,
                  smoothFactor = 0.5,
                  fill = FALSE) %>%
      
      addPolygons(data = settm,     #NO VALUES RAYON
                  color = "grey",
                  fillColor= "grey",
                  weight=1,
                  fill=TRUE,
                  fillOpacity = 0.2,
                  label = paste0( "Settlement:  ",settm$adm4NameLa, ", ", settm$adm4Pcode),
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE),
                  opacity = 1)%>%
      addCircles(data=CVA3_HH,
                 color="black",
                 weight=5,
                 opacity=1,
                 group="All surveys")%>%
      addCircles(data=NameDiff,
                 weight=9,
                 opacity=1,
                 fillColor = "magenta",
                 color="magenta",
                 group="Settlement Name Differences",
                 popup = paste0('<h7 style="color:black;">',  "Settlement Entered:  ", "<b>", NameDiff$current_village, "</b>", '</h7>', "<br>",
                                NameDiff$uuid,  "<br>", "Coordinate Settlement:", NameDiff$jNAMEid, NameDiff$jNAME))%>%
      addCircles(data=LocationDiff,
                 color="cyan",
                 fillColor = "cyan",
                 weight=9,
                 opacity=1,
                 group="Settlement Location Differences",
                 popup = paste0('<h7 style="color:black;">',  "Settlement Entered:  ", "<b>", LocationDiff$current_village,  "</b>", '</h7>', "<br>",
                                LocationDiff$uuid, "<br>", "Coordinate Settlement:", LocationDiff$jNAMEid))%>%
      addLayersControl(overlayGroups=c("All surveys","Settlement Name Differences", "Settlement Location Differences"))
   })
  output$Healtht <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284125", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_settlement",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")
      
      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      
    })
    dataLIVE<- reactive(filter(CVA3_HH,submission_date==input$date3))
    datatable(dataLIVE(), filter = "top", extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  output$Healthgt <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284125", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")
      
      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      newtry<- merge.data.frame(CVA3_HH, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
      newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
      newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
      
      CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7,10, 12, 13, 14, 26,27)
      coordinates(CVA3_HH)<- c("lng","lat")
      #coordinates(CVA3_HH)<-c("x","y") #make dataframe into points using x y coordinates in sheet
      proj4string(CVA3_HH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      CVA3_HH_p <- spTransform(x = CVA3_HH, CRSobj = proj4string(oblasts_p))
      
      m = gDistance(CVA3_HH_p, settm_p, byid = TRUE)
      row = apply(m, 2, function(x) which(x == min(x)))
      labels = unlist(settm_p@data[row, ]$adm4Pcode)
      CVA3_HH_p$jNAMEid <- labels
      labels2 = unlist(settm_p@data[row, ]$adm4NameLa)
      CVA3_HH_p$jNAME <- labels2
      
      #attach distance to closest settlment
      dist<-apply(gDistance(CVA3_HH_p, settm_p,byid=TRUE),2,min)
      CVA3_HH_p$dist<- dist
      CVA3_HH<-spTransform(x= CVA3_HH_p, CRSobj = proj4string(raions))
      
      
      #group
      CVA3_HH<-as.data.frame(CVA3_HH@data)
      HHcount <- CVA3_HH %>% count(current_village, submission_date, sort = TRUE)
      HHcount<- as.data.frame(spread(HHcount, submission_date, n))
      
    })
    datatable(HHcount, filter = "top", extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  dataLIVEh <- reactive({
    input$loadDataset
    user_name <- if (input$username == "") NULL else input$username
    password <- if (input$password == "") NULL else input$password
    CVA3_HH <- kobo_data_downloader(
      "284125", c(user_name, password), input$api)
    CVA3_HH<-as.data.frame(CVA3_HH)
    CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                            contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                            contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
    CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
    colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village",
                          "gps_work", "y", "x", "uuid","submisDate", "submisTime")
    
    CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                       y = as.numeric(y))
    CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
    CVA3_HH<-filter(CVA3_HH,submission_date==input$date3)
    newtry<- merge.data.frame(CVA3_HH, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
    newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
    newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
    
    CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7,10, 12, 13, 14, 26,27)
    coordinates(CVA3_HH)<- c("lng","lat")
    #coordinates(CVA3_HH)<-c("x","y") #make dataframe into points using x y coordinates in sheet
    proj4string(CVA3_HH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    CVA3_HH_p <- spTransform(x = CVA3_HH, CRSobj = proj4string(oblasts_p))
    
    m = gDistance(CVA3_HH_p, settm_p, byid = TRUE)
    row = apply(m, 2, function(x) which(x == min(x)))
    labels = unlist(settm_p@data[row, ]$adm4Pcode)
    CVA3_HH_p$jNAMEid <- labels
    labels2 = unlist(settm_p@data[row, ]$adm4NameLa)
    CVA3_HH_p$jNAME <- labels2
    
    #attach distance to closest settlment
    dist<-apply(gDistance(CVA3_HH_p, settm_p,byid=TRUE),2,min)
    CVA3_HH_p$dist<- dist
    CVA3_HH<-spTransform(x= CVA3_HH_p, CRSobj = proj4string(raions))
    return(CVA3_HH)
  })
  output$Healthmap <- renderLeaflet({
    #input$loadDataset
    CVA3_HH<-dataLIVEh()
    #check for far distances from settlement
    LocationDiff<- subset(CVA3_HH, dist>0)

    #check discrepancy in settlement names
    NameDiff<-subset(CVA3_HH,(!CVA3_HH$jNAMEid==CVA3_HH$current_village))
    
    map<- leaflet(options=leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(37.9762, 48.5793, zoom = 7)%>%
      addPolygons(data = oblasts,    # NON-GOVERNMENT CONTROLED AREA
                  color = "black",
                  fill= TRUE,
                  fillColor= "grey",
                  fillOpacity = 0.1,
                  weight= 1,
                  opacity = 1) %>%
      addPolygons(data= raions,    # NUMBER OF IDPS
                  color = "orange",
                  weight = 1,
                  label= raions$NAME_LAT,
                  opacity = 1.0,
                  smoothFactor = 0.5,
                  fill = FALSE) %>%
      
      addPolygons(data = settm,     #NO VALUES RAYON
                  color = "grey",
                  fillColor= "grey",
                  weight=1,
                  fill=TRUE,
                  fillOpacity = 0.2,
                  label = paste0( "Settlement:  ",settm$adm4NameLa, ", ", settm$adm4Pcode),
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE),
                 
                  opacity = 1)%>%
      addCircles(data=CVA3_HH,
                 color="black",
                 weight=5,
                 opacity=1,
                 group="All surveys")%>%
      addCircles(data=NameDiff,
                 weight=9,
                 opacity=1,
                 fillColor = "magenta",
                 color="magenta",
                 group="Settlement Name Differences",
                 popup = paste0('<h7 style="color:black;">',  "Settlement Entered:  ", "<b>", NameDiff$current_village, "</b>", '</h7>', "<br>",
                                NameDiff$uuid,  "<br>", "Coordinate Settlement:", NameDiff$jNAMEid, NameDiff$jNAME))%>%
     
      addCircles(data=LocationDiff,
                 color="cyan",
                 fillColor = "cyan",
                 weight=9,
                 opacity=1,
                 group="Settlement Location Differences",
                 popup = paste0(LocationDiff$uuid,"<br>", "Coordinate Settlement:", LocationDiff$jNAMEid, LocationDiff$jNAME))%>%
      addLayersControl(overlayGroups=c("All surveys","Settlement Location Differences", "Settlement Name Differences"))
  })
  output$Admint <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284115", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_settlement",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")
      
      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      
    })
    dataLIVE<- reactive(filter(CVA3_HH,submission_date==input$date4))
    datatable(dataLIVE(), filter = "top", extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  output$Admingt <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284115", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")
      
      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      newtry<- merge.data.frame(CVA3_HH, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
      newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
      newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
      
      CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7,10, 12, 13, 14, 26,27)
      coordinates(CVA3_HH)<- c("lng","lat")
      #coordinates(CVA3_HH)<-c("x","y") #make dataframe into points using x y coordinates in sheet
      proj4string(CVA3_HH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      CVA3_HH_p <- spTransform(x = CVA3_HH, CRSobj = proj4string(oblasts_p))
      
      m = gDistance(CVA3_HH_p, settm_p, byid = TRUE)
      row = apply(m, 2, function(x) which(x == min(x)))
      labels = unlist(settm_p@data[row, ]$adm4Pcode)
      CVA3_HH_p$jNAMEid <- labels
      labels2 = unlist(settm_p@data[row, ]$adm4NameLa)
      CVA3_HH_p$jNAME <- labels2
      
      #attach distance to closest settlment
      dist<-apply(gDistance(CVA3_HH_p, settm_p,byid=TRUE),2,min)
      CVA3_HH_p$dist<- dist
      CVA3_HH<-spTransform(x= CVA3_HH_p, CRSobj = proj4string(raions))
      
      
      #group
      CVA3_HH<-as.data.frame(CVA3_HH@data)
      HHcount <- CVA3_HH %>% count(current_village, submission_date, sort = TRUE)
      HHcount<- as.data.frame(spread(HHcount, submission_date, n))
      
    })
    datatable(HHcount, filter = "top", extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  dataLIVEa <- reactive({
    input$loadDataset
    user_name <- if (input$username == "") NULL else input$username
    password <- if (input$password == "") NULL else input$password
    CVA3_HH <- kobo_data_downloader(
      "284115", c(user_name, password), input$api)
    CVA3_HH<-as.data.frame(CVA3_HH)
    CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                            contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                            contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
    CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
    colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village",
                          "gps_work", "y", "x", "uuid","submisDate", "submisTime")
    
    CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                       y = as.numeric(y))
    CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
    CVA3_HH<-filter(CVA3_HH,submission_date==input$date4)
    newtry<- merge.data.frame(CVA3_HH, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
    newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
    newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
    
    CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7,10, 12, 13, 14, 26,27)
    coordinates(CVA3_HH)<- c("lng","lat")
    #coordinates(CVA3_HH)<-c("x","y") #make dataframe into points using x y coordinates in sheet
    proj4string(CVA3_HH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    CVA3_HH_p <- spTransform(x = CVA3_HH, CRSobj = proj4string(oblasts_p))
    
    m = gDistance(CVA3_HH_p, settm_p, byid = TRUE)
    row = apply(m, 2, function(x) which(x == min(x)))
    labels = unlist(settm_p@data[row, ]$adm4Pcode)
    CVA3_HH_p$jNAMEid <- labels
    labels2 = unlist(settm_p@data[row, ]$adm4NameLa)
    CVA3_HH_p$jNAME <- labels2
    
    #attach distance to closest settlment
    dist<-apply(gDistance(CVA3_HH_p, settm_p,byid=TRUE),2,min)
    CVA3_HH_p$dist<- dist
    CVA3_HH<-spTransform(x= CVA3_HH_p, CRSobj = proj4string(raions))
    return(CVA3_HH)
  })
  output$Adminmap <- renderLeaflet({
    #input$loadDataset
    CVA3_HH<-dataLIVEa()
    #check for far distances from settlement
    LocationDiff<- subset(CVA3_HH, dist>0)
    
    #check discrepancy in settlement names
    NameDiff<-subset(CVA3_HH,(!CVA3_HH$jNAMEid==CVA3_HH$current_village))
    
    map<- leaflet(options=leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(37.9762, 48.5793, zoom = 7)%>%
      addPolygons(data = oblasts,    # NON-GOVERNMENT CONTROLED AREA
                  color = "black",
                  fill= TRUE,
                  fillColor= "grey",
                  fillOpacity = 0.1,
                  weight= 1,
                  opacity = 1) %>%
      addPolygons(data= raions,    # NUMBER OF IDPS
                  color = "orange",
                  weight = 1,
                  label= raions$NAME_LAT,
                  opacity = 1.0,
                  smoothFactor = 0.5,
                  fill = FALSE) %>%
      
      addPolygons(data = settm,     #NO VALUES RAYON
                  color = "grey",
                  fillColor= "grey",
                  weight=1,
                  fill=TRUE,
                  fillOpacity = 0.2,
                  label = paste0( "Settlement:  ",settm$adm4NameLa, ", ", settm$adm4Pcode),
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE),
                  
                  opacity = 1)%>%
      addCircles(data=CVA3_HH,
                 color="black",
                 weight=5,
                 opacity=1,
                 group="All surveys")%>%
      addCircles(data=NameDiff,
                 weight=9,
                 opacity=1,
                 fillColor = "magenta",
                 color="magenta",
                 group="Settlement Name Differences",
                 popup = paste0('<h7 style="color:black;">',  "Settlement Entered:  ", "<b>", NameDiff$current_village, "</b>", '</h7>', "<br>",
                                NameDiff$uuid,  "<br>", "Coordinate Settlement:", NameDiff$jNAMEid, NameDiff$jNAME))%>%
      
      addCircles(data=LocationDiff,
                 color="cyan",
                 fillColor = "cyan",
                 weight=9,
                 opacity=1,
                 group="Settlement Location Differences",
                 popup = paste0(LocationDiff$uuid,"<br>", "Coordinate Settlement:", LocationDiff$jNAMEid, LocationDiff$jNAME))%>%
      addLayersControl(overlayGroups=c("All surveys","Settlement Location Differences", "Settlement Name Differences"))
  })
  output$SSt <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284143", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_settlement",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")
      
      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      
    })
    dataLIVE<- reactive(filter(CVA3_HH,submission_date==input$date5))
    datatable(dataLIVE(), filter = "top", extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  output$SSgt <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284143", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")
      
      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      newtry<- merge.data.frame(CVA3_HH, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
      newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
      newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
      
      CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7,10, 12, 13, 14, 26,27)
      coordinates(CVA3_HH)<- c("lng","lat")
      #coordinates(CVA3_HH)<-c("x","y") #make dataframe into points using x y coordinates in sheet
      proj4string(CVA3_HH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      CVA3_HH_p <- spTransform(x = CVA3_HH, CRSobj = proj4string(oblasts_p))
      
      m = gDistance(CVA3_HH_p, settm_p, byid = TRUE)
      row = apply(m, 2, function(x) which(x == min(x)))
      labels = unlist(settm_p@data[row, ]$adm4Pcode)
      CVA3_HH_p$jNAMEid <- labels
      labels2 = unlist(settm_p@data[row, ]$adm4NameLa)
      CVA3_HH_p$jNAME <- labels2
      
      #attach distance to closest settlment
      dist<-apply(gDistance(CVA3_HH_p, settm_p,byid=TRUE),2,min)
      CVA3_HH_p$dist<- dist
      CVA3_HH<-spTransform(x= CVA3_HH_p, CRSobj = proj4string(raions))
      
      
      #group
      CVA3_HH<-as.data.frame(CVA3_HH@data)
      HHcount <- CVA3_HH %>% count(current_village, submission_date, sort = TRUE)
      HHcount<- as.data.frame(spread(HHcount, submission_date, n))
      
    })
    datatable(HHcount, filter = "top", extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  dataLIVEs <- reactive({
    input$loadDataset
    user_name <- if (input$username == "") NULL else input$username
    password <- if (input$password == "") NULL else input$password
    CVA3_HH <- kobo_data_downloader(
      "284143", c(user_name, password), input$api)
    CVA3_HH<-as.data.frame(CVA3_HH)
    CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                            contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                            contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
    CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
    colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village",
                          "gps_work", "y", "x", "uuid","submisDate", "submisTime")
    
    CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                       y = as.numeric(y))
    CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
    CVA3_HH<-filter(CVA3_HH,submission_date==input$date5)
    newtry<- merge.data.frame(CVA3_HH, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
    newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
    newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
    
    CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7,10, 12, 13, 14, 26,27)
    coordinates(CVA3_HH)<- c("lng","lat")
    #coordinates(CVA3_HH)<-c("x","y") #make dataframe into points using x y coordinates in sheet
    proj4string(CVA3_HH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    CVA3_HH_p <- spTransform(x = CVA3_HH, CRSobj = proj4string(oblasts_p))
    
    m = gDistance(CVA3_HH_p, settm_p, byid = TRUE)
    row = apply(m, 2, function(x) which(x == min(x)))
    labels = unlist(settm_p@data[row, ]$adm4Pcode)
    CVA3_HH_p$jNAMEid <- labels
    labels2 = unlist(settm_p@data[row, ]$adm4NameLa)
    CVA3_HH_p$jNAME <- labels2
    
    #attach distance to closest settlment
    dist<-apply(gDistance(CVA3_HH_p, settm_p,byid=TRUE),2,min)
    CVA3_HH_p$dist<- dist
    CVA3_HH<-spTransform(x= CVA3_HH_p, CRSobj = proj4string(raions))
    return(CVA3_HH)
  })
  output$SSmap <- renderLeaflet({
    #input$loadDataset
    CVA3_HH<-dataLIVEs()
    #check for far distances from settlement
    LocationDiff<- subset(CVA3_HH, dist>0)
    
    #check discrepancy in settlement names
    NameDiff<-subset(CVA3_HH,(!CVA3_HH$jNAMEid==CVA3_HH$current_village))
    
    map<- leaflet(options=leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(37.9762, 48.5793, zoom = 7)%>%
      addPolygons(data = oblasts,    # NON-GOVERNMENT CONTROLED AREA
                  color = "black",
                  fill= TRUE,
                  fillColor= "grey",
                  fillOpacity = 0.1,
                  weight= 1,
                  opacity = 1) %>%
      addPolygons(data= raions,    # NUMBER OF IDPS
                  color = "orange",
                  weight = 1,
                  label= raions$NAME_LAT,
                  opacity = 1.0,
                  smoothFactor = 0.5,
                  fill = FALSE) %>%
      
      addPolygons(data = settm,     #NO VALUES RAYON
                  color = "grey",
                  fillColor= "grey",
                  weight=1,
                  fill=TRUE,
                  fillOpacity = 0.2,
                  label = paste0( "Settlement:  ",settm$adm4NameLa, ", ", settm$adm4Pcode),
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE),
                  
                  opacity = 1)%>%
      addCircles(data=CVA3_HH,
                 color="black",
                 weight=5,
                 opacity=1,
                 group="All surveys")%>%
      addCircles(data=NameDiff,
                 weight=9,
                 opacity=1,
                 fillColor = "magenta",
                 color="magenta",
                 group="Settlement Name Differences",
                 popup = paste0('<h7 style="color:black;">',  "Settlement Entered:  ", "<b>", NameDiff$current_village, "</b>", '</h7>', "<br>",
                                NameDiff$uuid,  "<br>", "Coordinate Settlement:", NameDiff$jNAMEid, NameDiff$jNAME))%>%
      
      addCircles(data=LocationDiff,
                 color="cyan",
                 fillColor = "cyan",
                 weight=9,
                 opacity=1,
                 group="Settlement Location Differences",
                 popup = paste0(LocationDiff$uuid,"<br>", "Coordinate Settlement:", LocationDiff$jNAMEid, LocationDiff$jNAME))%>%
      addLayersControl(overlayGroups=c("All surveys","Settlement Location Differences", "Settlement Name Differences"))
  })
  output$Edut <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284123", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_settlement",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")
      
      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      
    })
    dataLIVE<- reactive(filter(CVA3_HH,submission_date==input$date6))
    datatable(dataLIVE(), filter = "top", extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  output$Edugt <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284123", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village",
                            "gps_work", "y", "x", "uuid","submisDate", "submisTime")
      
      CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                         y = as.numeric(y))
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      newtry<- merge.data.frame(CVA3_HH, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
      newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
      newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
      
      CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7,10, 12, 13, 14, 26,27)
      coordinates(CVA3_HH)<- c("lng","lat")
      #coordinates(CVA3_HH)<-c("x","y") #make dataframe into points using x y coordinates in sheet
      proj4string(CVA3_HH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      CVA3_HH_p <- spTransform(x = CVA3_HH, CRSobj = proj4string(oblasts_p))
      
      m = gDistance(CVA3_HH_p, settm_p, byid = TRUE)
      row = apply(m, 2, function(x) which(x == min(x)))
      labels = unlist(settm_p@data[row, ]$adm4Pcode)
      CVA3_HH_p$jNAMEid <- labels
      labels2 = unlist(settm_p@data[row, ]$adm4NameLa)
      CVA3_HH_p$jNAME <- labels2
      
      #attach distance to closest settlment
      dist<-apply(gDistance(CVA3_HH_p, settm_p,byid=TRUE),2,min)
      CVA3_HH_p$dist<- dist
      CVA3_HH<-spTransform(x= CVA3_HH_p, CRSobj = proj4string(raions))
      
      
      #group
      CVA3_HH<-as.data.frame(CVA3_HH@data)
      HHcount <- CVA3_HH %>% count(current_village, submission_date, sort = TRUE)
      HHcount<- as.data.frame(spread(HHcount, submission_date, n))
      
    })
    datatable(HHcount, filter = "top", extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
  dataLIVEe <- reactive({
    input$loadDataset
    user_name <- if (input$username == "") NULL else input$username
    password <- if (input$password == "") NULL else input$password
    CVA3_HH <- kobo_data_downloader(
      "284123", c(user_name, password), input$api)
    CVA3_HH<-as.data.frame(CVA3_HH)
    CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                            contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                            contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
    CVA3_HH <- separate(CVA3_HH,11,into=c("submission_date", "submission_time"), sep=" ")
    colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_village",
                          "gps_work", "y", "x", "uuid","submisDate", "submisTime")
    
    CVA3_HH<-transform(CVA3_HH,x = as.numeric(x), #change x and y to numeric
                       y = as.numeric(y))
    CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
    CVA3_HH<-filter(CVA3_HH,submission_date==input$date6)
    newtry<- merge.data.frame(CVA3_HH, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
    newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
    newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
    
    CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7,10, 12, 13, 14, 26,27)
    coordinates(CVA3_HH)<- c("lng","lat")
    #coordinates(CVA3_HH)<-c("x","y") #make dataframe into points using x y coordinates in sheet
    proj4string(CVA3_HH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    CVA3_HH_p <- spTransform(x = CVA3_HH, CRSobj = proj4string(oblasts_p))
    
    m = gDistance(CVA3_HH_p, settm_p, byid = TRUE)
    row = apply(m, 2, function(x) which(x == min(x)))
    labels = unlist(settm_p@data[row, ]$adm4Pcode)
    CVA3_HH_p$jNAMEid <- labels
    labels2 = unlist(settm_p@data[row, ]$adm4NameLa)
    CVA3_HH_p$jNAME <- labels2
    
    #attach distance to closest settlment
    dist<-apply(gDistance(CVA3_HH_p, settm_p,byid=TRUE),2,min)
    CVA3_HH_p$dist<- dist
    CVA3_HH<-spTransform(x= CVA3_HH_p, CRSobj = proj4string(raions))
    return(CVA3_HH)
  })
  output$Edumap <- renderLeaflet({
    #input$loadDataset
    CVA3_HH<-dataLIVEe()
    #check for far distances from settlement
    LocationDiff<- subset(CVA3_HH, dist>0)
    
    #check discrepancy in settlement names
    NameDiff<-subset(CVA3_HH,(!CVA3_HH$jNAMEid==CVA3_HH$current_village))
    
    map<- leaflet(options=leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      setView(37.9762, 48.5793, zoom = 7)%>%
      addPolygons(data = oblasts,    # NON-GOVERNMENT CONTROLED AREA
                  color = "black",
                  fill= TRUE,
                  fillColor= "grey",
                  fillOpacity = 0.1,
                  weight= 1,
                  opacity = 1) %>%
      addPolygons(data= raions,    # NUMBER OF IDPS
                  color = "orange",
                  weight = 1,
                  label= raions$NAME_LAT,
                  opacity = 1.0,
                  smoothFactor = 0.5,
                  fill = FALSE) %>%
      
      addPolygons(data = settm,     #NO VALUES RAYON
                  color = "grey",
                  fillColor= "grey",
                  weight=1,
                  fill=TRUE,
                  fillOpacity = 0.2,
                  label = paste0( "Settlement:  ",settm$adm4NameLa, ", ", settm$adm4Pcode),
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE),
                  
                  opacity = 1)%>%
      addCircles(data=CVA3_HH,
                 color="black",
                 weight=5,
                 opacity=1,
                 group="All surveys")%>%
      addCircles(data=NameDiff,
                 weight=9,
                 opacity=1,
                 fillColor = "magenta",
                 color="magenta",
                 group="Settlement Name Differences",
                 popup = paste0('<h7 style="color:black;">',  "Settlement Entered:  ", "<b>", NameDiff$current_village, "</b>", '</h7>', "<br>",
                                NameDiff$uuid,  "<br>", "Coordinate Settlement:", NameDiff$jNAMEid, NameDiff$jNAME))%>%
      
      addCircles(data=LocationDiff,
                 color="cyan",
                 fillColor = "cyan",
                 weight=9,
                 opacity=1,
                 group="Settlement Location Differences",
                 popup = paste0(LocationDiff$uuid,"<br>", "Coordinate Settlement:", LocationDiff$jNAMEid, LocationDiff$jNAME))%>%
      addLayersControl(overlayGroups=c("All surveys","Settlement Location Differences", "Settlement Name Differences"))
  })
  output$AdminSt <- renderDataTable({
    input$loadDataset
    isolate({
      user_name <- if (input$username == "") NULL else input$username
      password <- if (input$password == "") NULL else input$password
      CVA3_HH <- kobo_data_downloader(
        "284116", c(user_name, password), input$api)
      CVA3_HH<-as.data.frame(CVA3_HH)
      CVA3_HH<- dplyr::select(CVA3_HH, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                              contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                              contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
      CVA3_HH <- separate(CVA3_HH,8,into=c("submission_date", "submission_time"), sep=" ")
      colnames(CVA3_HH)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", "current_settlement",
                             "uuid","submisDate", "submisTime")
      
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      CVA3_HH$submission_date<-as.Date(CVA3_HH$submisDate)
      HHcount <- CVA3_HH %>% count(current_settlement, submission_date, sort = TRUE)
      HHcount<- as.data.frame(spread(HHcount, submission_date, n))
      
    })
    datatable(HHcount, filter = "top", extensions = 'Buttons',
              options = list(dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             paging=FALSE))
  })
})