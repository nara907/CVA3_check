library(koboloadeR)
library(shiny)
library(DT)
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

#setwd("C:/Users/Nara/Documents/R/CVA3_check/")
raions<-shapefile("Rayons.shp")
oblasts_p<- shapefile("Oblasts.shp")
oblasts<-spTransform(x= oblasts_p, CRSobj = proj4string(raions))
settm_p<-shapefile("settlementsDirtyy.shp")
settm<-spTransform(x= settm_p, CRSobj = proj4string(raions))


cent<-as.data.frame(centroid(settm))
names(cent)<-c("xC","yC")

settm@data$xC<-cent$xC
settm@data$yC<-cent$y

#Replace NA values with settlement centroids

