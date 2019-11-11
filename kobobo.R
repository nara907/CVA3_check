library(koboloadeR)
library(shiny)
library(DT)
library(koboloadeR)
library(highcharter)
library(devtools)
library(shinythemes)
library("rgdal")
library("raster")
library(gdata)
library(tidyverse)
library(sp)
library(rgeos)
library(dplyr)
library(maptools)
library(sf)
library("geosphere") #centroids
library(leaflet)
library(readxl)
library(openxlsx)

install_github("mrdwab/koboloadeR")


KOBO_datasets <- kobo_datasets(user = c("reach_initiative_ukraine","ukraine_reach"), api="kobohr")
KOBO_datasets

CVA3_HH<-kobo_data_downloader(formid = 284120,user= c("reach_initiative_ukraine",""), api="kobohr", check = FALSE)
CVA3_comm<-kobo_data_downloader(formid = 284121,user= c("reach_initiative_ukraine",""), api="kobohr", check = FALSE)
CVA3_admin<-kobo_data_downloader(formid = 284115,user= c("reach_initiative_ukraine",""), api="kobohr", check = FALSE)
CVA3_health<-kobo_data_downloader(formid = 284125,user= c("reach_initiative_ukraine",""), api="kobohr", check = FALSE)
CVA3_SS<-kobo_data_downloader(formid = 284143,user= c("reach_initiative_ukraine",""), api="kobohr", check = FALSE)
CVA3_edu<-kobo_data_downloader(formid = 284123,user= c("reach_initiative_ukraine",""), api="kobohr", check = FALSE)
CVA3_adminStat<-kobo_data_downloader(formid = 284116,user= c("reach_initiative_ukraine",""), api="kobohr", check = FALSE)


CVA3_health$`Questionnaire/facility_address/facility_settlement`
CVA3_HH<- dplyr::select(CVA3_health, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
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
colnames(newtry)
CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7,10, 12, 13, 14, 26,27)
coordinates(CVA3_HH)<- c("lng","lat")

#filter cols
CVA3_HH<-as.data.frame(CVA3_HH)
colnames(CVA3_HH)
CVA3_HH<-dplyr::select(CVA3_HH, c("date_assessment", "Questionnaire/enum_slov_id", "Questionnaire/enum_siev_id" , "Questionnaire/enum_mari_id",
                                              "Questionnaire/hh_profile/hh_representative_name/first_name","Questionnaire/Location_information/current_village", 
                                              "Questionnaire/Location_information/current_rectangle_id", "Questionnaire/Location_information/gps_work",
                                              "Questionnaire/Location_information/_gpslocation_latitude","Questionnaire/Location_information/_gpslocation_longitude",
                                              "_uuid",  "_submission_time"))

CVA3_comm<- dplyr::select(CVA3_adminStat, contains("date_assessment"), contains("enum_slov_id"), contains("enum_siev_id"), contains("enum_mari_id"),
                       contains("first_name"),contains("facility_settlement"),contains("gps_work"),
                       contains("latitude"),contains("longitude"), contains("uuid"),  contains("submission_time"))
colnames(CVA3_comm)
CVA3_comm <- separate(CVA3_comm,10,into=c("submission_date", "submission_time"), sep=" ")
colnames(CVA3_comm)<- c("assessment_date", "Slov_enumID", "Siev_enumID","Mari_enumID", "name", 
                   "gps_work", "y", "x", "uuid","submisDate", "submisTime")






CVA3_comm<-transform(CVA3_comm,x = as.numeric(x), #change x and y to numeric
               y = as.numeric(y))
CVA3_comm$submission_date<-as.Date(CVA3_comm$submisDate)
#grouping
HHcount <- CVA3_HH %>% count(current_village, submission_date, sort = TRUE)
HHcount<- as.data.frame(spread(HHcount, submission_date, n))


setwd("C:/Users/Nara/Documents/R/CVA3_check/")
raions<-shapefile("Rayons.shp")
oblasts_p<- shapefile("Oblasts.shp")
oblasts<-spTransform(x= oblasts, CRSobj = proj4string(raions))
settm_p<-shapefile("settlementsDirtyy.shp")
settm<-spTransform(x= settm, CRSobj = proj4string(raions))

#Add coordinates to surveys without GPS 
newtry<- merge.data.frame(CVA3_comm, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
colnames(newtry)
CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7, 10, 13, 14,17, 19, 26,27)

coordinates(CVA3_comm)<- c("x","y")
coordinates(CVA3_HH)<-~lng+lat #make dataframe into points using x y coordinates in sheet
proj4string(CVA3_comm) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
CVA3_HH_p <- spTransform(x = CVA3_comm, CRSobj = proj4string(oblasts_p))



#Attach settlement code of nearest settlement

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

#check for far distances from settlement

LocationDiff<- subset(CVA3_HH, dist>20)
count(LocationDiff@data)

#check discrepancy in settlement names
NameDiff<-subset(CVA3_HH,(!CVA3_HH$jNAMEid==CVA3_HH$current_village))
count(NameDiff@data)


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
                                                  bringToFront = TRUE),
              popup = paste0('<h7 style="color:black;">',  "Settlement:  ", "<b>", settm$adm4NameLa, "</b>", '</h7>', "<br>",
                             " UUID: ", settm$adm4Pcode),
              opacity = 1)%>%
  addCircles(data=CVA3_HH)%>%
  addCircles(data=NameDiff)%>%
  addCircles(data=LocationDiff)
map


#isolatefrom Leaflet HH

isolate({
  #Add coordinates to surveys without GPS 
  newtry<- merge.data.frame(dataLIVE(), settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
  newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
  newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
  
  CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7, 8, 11, 12, 13, 14, 19, 26,27)
  
  coordinates(CVA3_HH)<- c("lng","lat") #make dataframe into points using x y coordinates in sheet
  proj4string(CVA3_HH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  CVA3_HH_p <- spTransform(x = CVA3_HH, CRSobj = proj4string(oblasts_p))
  
  #Attach settlement code of nearest settlement
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
  
  #check for far distances from settlement
  LocationDiff<- subset(CVA3_HH, dist>20)
  
  #check discrepancy in settlement names
  NameDiff<-subset(CVA3_HH,(!CVA3_HH$jNAMEid==CVA3_HH$current_village))
  
})

  
  
  #working data table of coordinate results, refreshed when date is changed
  HHdataLIVE<- reactive({
    CVA3_HH1<-filter(CVA3_HH,submission_date==input$date1)
    newtry<- merge.data.frame(CVA3_HH1, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
    newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
    newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
    
    CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7, 8, 11, 12, 13, 14, 19, 26,27)
    
    coordinates(CVA3_HH)<- c("lng","lat") #make dataframe into points using x y coordinates in sheet
    proj4string(CVA3_HH) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    CVA3_HH_p <- spTransform(x = CVA3_HH, CRSobj = proj4string(oblasts_p))
    
    #Attach settlement code of nearest settlement
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
    
    #check for far distances from settlement
    LocationDiff<- subset(CVA3_HH, dist>20)
    
    #check discrepancy in settlement names
    NameDiff<-subset(CVA3_HH,(!CVA3_HH$jNAMEid==CVA3_HH$current_village))
    
    return(CVA3_HH)
  })
  
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
                                                    bringToFront = TRUE),
                popup = paste0('<h7 style="color:black;">',  "Settlement:  ", "<b>", settm$adm4NameLa, "</b>", '</h7>', "<br>",
                               " UUID: ", settm$adm4Pcode),
                opacity = 1)
})
output$HHmap <- renderDataTable({
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
    newtry<- merge.data.frame(CVA3_HH, settm, all.x= TRUE, all.y= FALSE, by.x="current_village", by.y= "adm4Pcode")
    newtry$lat <- ifelse(test = newtry$gps_work != "yes", yes = newtry$yC, no = newtry$y) #add lat.lng of settlement centroid for NA values
    newtry$lng<- ifelse(test = newtry$gps_work != "yes", yes = newtry$xC, no = newtry$x)
    
    CVA3_HH<- dplyr::select(newtry, 1, 2, 3, 4, 5,6, 7, 8, 11, 12, 13, 14, 19, 27,28)
  })
  dataLIVE<- reactive({
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
    return(as.data.frame(CVA3_HH))
  })
  
  datatable(dataLIVE(), filter = "top", extensions = 'Buttons', 
            options = list(dom = 'Bfrtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           paging=TRUE))
})
 
ukraine_reach<-1
