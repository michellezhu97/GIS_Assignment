#import planning areas and bus stop location shapefiles
library(RCurl)
library(sf)

library(RCurl)
Planning_areas <- st_read(text=getURL("https://raw.github.com/michellezhu97/GIS_Assignment/blob/master/Planning_Area_Census2010.shp"))
Bus_stop_location <- st_read(text=getURL("https://raw.github.com/michellezhu97/GIS_Assignment/blob/master/BusStop.shp"))
st_crs(Planning_areas)
st_crs(Bus_stop_location)

#check that the shp works
library(tmap)
qtm(Planning_areas)
class(Planning_areas)
qtm(Bus_stop_location)
class(Bus_stop_location)

#import CSV data
library(tidyverse)
Income_data <- read_csv(url("https://raw.githubusercontent.com/michellezhu97/GIS_Assignment/master/Percentage%20of%20working%20population%20in%20income%20bracket%20by%20area.csv"))
Commute_data <- read_csv(url("https://raw.githubusercontent.com/michellezhu97/GIS_Assignment/master/Commute%20data.csv"))

#join the csv data with the SG map
Income_data_map <- merge(Planning_areas,
                         Income_data,
                         by.x="PLN_AREA_N",
                         by.y="Planning_Area",
                         all.x=TRUE)
#check that it worked
summary(Income_data_map)
#repeat the process for the commute data
Commute_data_map <- merge(Planning_areas,
                          Commute_data,
                          by.x="PLN_AREA_N",
                          by.y="Planning_Area",
                          all.x=TRUE)
summary(Commute_data_map)
qtm(Income_data_map)

#join the bus stop locations with planning areas?


Bus_stop_location_MAP <- rbind(Planning_areas, Bus_stop_location, sf_column_name = "geometry")




cbind(Planning_areas, Bus_stop_location)
do.call(rbind, list(sfc1, sfc2)).






#plot the maps for spatial income inequality 
library(tmap)
library(tmaptools)

#rename columns
names(Income_data_map)[names(Income_data_map) == "1_Quartile"] <- "FirstQuartile"
names(Income_data_map)[names(Income_data_map) == "2_Quartile"] <- "SecondQuartile"
names(Income_data_map)[names(Income_data_map) == "3_Quartile"] <- "ThirdQuartile"
names(Income_data_map)[names(Income_data_map) == "4_Quartile"] <- "FourthQuartile"

#add column for labels, excluding non-residential areas
Income_data_map$PLN <- Income_data_map$PLN_AREA_N
Income_data_map$PLN[is.na(Income_data_map$FirstQuartile)] <- NA
#add column for legend
Income_data_map$PercentagePeopleWithinIncomeBracket <- Income_data_map$FourthQuartile
names(Income_data_map)[names(Income_data_map) == "PercentagePeopleWithinIncomeBracket"] <- "Percentage of people within income bracket"

map1 <- tm_shape(Income_data_map) +
  tm_polygons("FirstQuartile") +
  tm_legend(show=FALSE) +
  tm_text("PLN", scale=1, 
          size.lim=c(0.4,0.5), 
          remove.overlap=FALSE, 
          showNA=FALSE,
          col="black",
          bg.color="white",
          bg.alpha=0.25) +
  tm_fill(colorNA="grey90",
          textNA="Non-residential area") +
  tm_layout(frame=FALSE) +
  tm_credits ("Income distribution bottom 25%", position=c(0,0.05), size=1.2) 

map2 <- tm_shape(Income_data_map) +
  tm_polygons("FourthQuartile") +
  tm_legend(show=FALSE) +
  tm_text("PLN", scale=1, 
          size.lim=c(0.4,0.5), 
          remove.overlap=FALSE, 
          showNA=FALSE,
          col="black",
          bg.color="white",
          bg.alpha=0.25) +
  tm_fill(colorNA="grey90",
          textNA="Non-residential area") +
  tm_layout(frame=FALSE) +
  tm_credits("Income distribution top 25%", position=c(0,0.05), size=1.2)

map3 <- tm_shape(Income_data_map) +
  tm_polygons("Below_1000",) +
  tm_legend(show=FALSE) +
  tm_text("PLN", scale=1, 
          size.lim=c(0.4,0.5), 
          remove.overlap=FALSE, 
          showNA=FALSE,
          col="black",
          bg.color="white",
          bg.alpha=0.25) +
  tm_fill(colorNA="grey90",
          textNA="Non-residential area") +
  tm_layout(frame=FALSE) +
  tm_credits("Income distribution bottom 10%", position=c(0,0.05), size=1.2)

map4 <- tm_shape(Income_data_map) +
  tm_polygons("12000_Above") +
  tm_legend(show=FALSE) +
  tm_text("PLN", scale=1, 
          size.lim=c(0.4,0.5), 
          remove.overlap=FALSE, 
          showNA=FALSE,
          col="black",
          bg.color="white",
          bg.alpha=0.25) +
  tm_fill(colorNA="grey90",
          textNA="Non-residential area") +
  tm_layout(frame=FALSE) +
  tm_credits("Income distribution top 10%", position=c(0,0.05), size=1.2)

legend1 <- tm_shape(Income_data_map) +
  tm_polygons("Percentage of people within income bracket",
              textNA = "Non-residential area") +
  tm_scale_bar(position=c(0.2, 0.15), text.size=0.8) +
  tm_compass(north=0, position=c(0.65, 0.15)) +
  tm_layout(legend.only=TRUE,
            legend.position=c(0.2,0.3), 
            asp=0.1) +
  tm_credits("(c) Department of Statistics, Singapore", position=c(0.0,0.0)) +
  tm_fill()

incomemaps <- tmap_arrange(map1, legend1, map2)
incomemaps

incomemaps2 <- tmap_arrange(map3, legend1, map4)
incomemaps2



#count the number of bus stops per planning area

library(rgdal)
library(maptools)

Bus_stop_location_SP <- as(Bus_stop_location, "Spatial")
Bus_stop_location_SP <- spTransform(Bus_stop_location_SP, EPSG)
Bus_stop_location_SP <- remove.duplicates(Bus_stop_location_SP)
summary(Bus_stop_location_SP)

Planning_areas_SP <- as(Planning_areas, "Spatial")
Planning_areas_SP <- spTransform(Planning_areas_SP, EPSG)
summary(Planning_areas_SP)

#COUNTING? VIA ST_INTERSECT
Intersects <- lengths(st_intersects(Planning_areas, Bus_stop_location))
Intersects
#merge the output with the Planning_areas shp
Planning_areas_with_bus <- merge(Planning_areas, Intersects)

#merge the output with income map
Planningnames <- subset(Planning_areas, select=c("PLN_AREA_N"))
Planningnames$intersects <- Intersects
Income_data_with_bus <- merge(Income_data, 
                              Planningnames,
                              by.x="Planning_Area",
                              by.y="PLN_AREA_N")
names(Income_data_with_bus)[names(Income_data_with_bus) == "1_Quartile"] <- "FirstQuartile"
names(Income_data_with_bus)[names(Income_data_with_bus) == "2_Quartile"] <- "SecondQuartile"
names(Income_data_with_bus)[names(Income_data_with_bus) == "3_Quartile"] <- "ThirdQuartile"
names(Income_data_with_bus)[names(Income_data_with_bus) == "4_Quartile"] <- "FourthQuartile"


#Scatterplot bus stops against quartile income

library(ggplot2)
library(ggrepel)

Scatterplot1 <- ggplot(Income_data_with_bus, aes(x= FirstQuartile, y=intersects)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE)  +
  labs(title="Percentage of residents in first quartile against number of bus stops",
       x="Percentage of residents in first quartile income bracket",
       y="Number of bus stops in planning area") 
Scatterplot1 + geom_label_repel(aes(label=Planning_Area),
                                box.padding = 0.35,
                                point.padding = 0.5,
                                segment.color = 'grey50') +
  theme_classic()


Scatterplot2 <- ggplot(Income_data_with_bus, aes(x= FourthQuartile, y=intersects)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE)  +
  labs(title="Percentage of residents in fourth quartile against number of bus stops",
       x="Percentage of residents in fourth quartile income bracket",
       y="Number of bus stops in planning area") 
Scatterplot2 + geom_label_repel(aes(label=Planning_Area),
                                box.padding = 0.35,
                                point.padding = 0.5,
                                segment.color = 'grey50') +
  theme_classic()





#merge income and commute data
Commute_data_income <- merge(Income_data,
                          Commute_data,
                          by.x="Planning_Area",
                          by.y="Planning_Area",
                          all.x=TRUE)
summary(Commute_data_income)

names(Commute_data_income)[names(Commute_data_income) == "1_Quartile"] <- "FirstQuartile"
names(Commute_data_income)[names(Commute_data_income) == "2_Quartile"] <- "SecondQuartile"
names(Commute_data_income)[names(Commute_data_income) == "3_Quartile"] <- "ThirdQuartile"
names(Commute_data_income)[names(Commute_data_income) == "4_Quartile"] <- "FourthQuartile"


#plot income quartiles against percentage of people who take bus
Scatterplot3 <- ggplot(Commute_data_income, aes(x= FirstQuartile, y=All_bus)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE)  +
  labs(title="Percentage of residents in first quartile against percentage of people who commute via bus",
       x="Percentage of residents in first quartile income bracket",
       y="Percentage of residents who commute via bus") 
Scatterplot3 + geom_label_repel(aes(label=Planning_Area),
                                box.padding = 0.35,
                                point.padding = 0.5,
                                segment.color = 'grey50') +
  theme_classic()


Scatterplot4 <- ggplot(Commute_data_income, aes(x= FourthQuartile, y=All_bus)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE)  +
  labs(title="Percentage of residents in fourth quartile against percentage of people who commute via bus",
       x="Percentage of residents in fourth quartile income bracket",
       y="Percentage of residents who commute via bus") 
Scatterplot4 + geom_label_repel(aes(label=Planning_Area),
                                box.padding = 0.35,
                                point.padding = 0.5,
                                segment.color = 'grey50') +
  theme_classic()