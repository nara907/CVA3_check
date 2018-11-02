library(ggplot2)
library(highcharter)
library(vegalite)
library(reshape2)
install.packages("vegalite")
psych<-as.data.frame(cbind(c("rural","urban"), c(39, 57)))

ggplot(fscore1, aes(x = variable, y = value, fill = Score)) + 
  geom_bar(stat = "identity", position="stack")

fscore<-as.data.frame(cbind( c("poor","borderline","acceptable"),c(2, 7,91), c(1, 3,96), c(0, 3,97), c(1, 4,96)))
colnames(fscore)<-c("Score", "5km","5-20km", "Over 20km", "Overall")
fscore1<-melt(fscore, id.var="Score")
fscore1
DF <- read.table(text="Rank F1     F2     F3
1    500    250    50
2    400    100    30
3    300    155    100
4    200    90     10", header=TRUE)
DF
library(reshape2)
DF1 <- melt(DF, id.var="Rank")
DF1

vegalite() %>%
  add_data(fscore1) %>%
  encode_x("value", "quantitative", aggregate="sum") %>%
  encode_y("variable", "nominal") %>%
  encode_color("Score", "nominal") %>%
  mark_bar()

vegalite() %>%
  add_data(fscore1) %>%
  encode_x("variable", "nominal") %>%
  encode_y("value", "quantitative", aggregate="sum") %>%
  encode_color("Score", "nominal") %>%
  scale_x_ordinal(band_size=17) %>%
  scale_color_nominal(range=c("#EA98D2", "#659CCA", "#58595B")) %>%
  mark_bar(stack="normalize")



CS1<-as.data.frame(cbind( c("A","B","C","D"),c(49, 44,11,4), c(37,12,2,2), c(39,35,3,3), c(39,28,4,3)))
colnames(CS1)<-c("Score", "5km","5-20km", "Over 20km", "Overall")
CS1<-melt(CS1, id.var="Score")
CS1
ggplot(CS1) +
  # add bar for each discipline colored by gender
  geom_bar(aes(x = Score, y = value, fill = variable),
           stat = "identity", position = "dodge") +
  # name axes and remove gap between bars and y-axis
  # remove grey theme
  scale_fill_manual(values = c("#95A0A9", "#1A76A9", "#58595B", "#F3A1A4")) +
  theme_classic(base_size = 18) +
  # rotate x-axis and remove superfluous axis elements
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1, vjust = 0),
        axis.line = element_blank(),
        axis.ticks.x = element_blank()) 

library(extrafont)
install.packages("hrbrthemes")
library(hrbrthemes)
font_import()
fonts()
y

CS1<-as.data.frame(cbind( c("Rural","Urban"),c(234, 296)))
colnames(CS1)<-c("Settlement", "Income (USD)")
CS1<-melt(CS1, id.var="Settlement")
CS1
ggplot(CS1, aes( Settlement, value)) +
  geom_label(aes(label = value))+
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("#58595b")) +
  theme_minimal() +
  theme(legend.position = "none")+
  labs(title="", 
       x = '',
       y = '')+  
  theme_ipsum()+  
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank())+ 
  scale_y_discrete(labels=NULL)+ 
  scale_x_discrete(labels=NULL)



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
LocationDiff<- subset(CVA3_HH, dist>0)
#check discrepancy in settlement names
NameDiff<-subset(CVA3_HH,(!CVA3_HH$jNAMEid==CVA3_HH$current_village))
LocationDiff
CVA3_HH
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
                            "Coordinate Settlement:", NameDiff$jNAMEid))%>%
  addCircles(data=LocationDiff,
             color="cyan",
             fillColor = "cyan",
             weight=9,
             opacity=1,
             group="Settlement Location Differences",
             popup = paste0("Settlement Entered:  ", LocationDiff$current_village,
                            "Coordinate Settlement:", LocationDiff$jNAMEid))%>%
  addLayersControl(overlayGroups=c("All surveys","Settlement Name Differences", "Settlement Location Differences"))
map
