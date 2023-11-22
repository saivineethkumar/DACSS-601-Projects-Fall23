#Plotting Time-series
##Example Data: FedFundRate.csv
library(tidyverse)
library(readr)
library(lubridate)
library(ggplot2)

FedFundsRate <- read_csv("./challege2_data/FedFundsRate.csv")

#creating a date column
FedFundsRate_date <- FedFundsRate |>
  mutate(date = str_c(Year, Month, Day, sep = "-"))

FedFundsRate_date <- FedFundsRate_date|>
  relocate(date, .before = Year)

head(FedFundsRate_date) #for sanity check

#Create a date column
#FedFundsRate_date <- FedFundsRate_date|> 
#  mutate(ymd(date))

##Plotting the federal funds target rate (Y) over year (X)
FedFund_Year_point<-ggplot(FedFundsRate_date, aes(x=Year, y=`Federal Funds Target Rate`))+
  lims(x=c(1980, 2010), y = c(0, 12))+
  geom_point()+
  stat_smooth()

FedFund_Year_point

FedFund_Year_line<-ggplot(FedFundsRate_date, aes(x=Year, y=`Federal Funds Target Rate`))+
  lims(x=c(1980, 2010), y = c(0, 12))+
  geom_line()

FedFund_Year_line

#plotting over date(X)
##Make sure we force the date column to be recognized as a Date/Time class object: use as.Date()
#option 1
FedFundsRate_date$date <- as.Date(FedFundsRate_date$date) 

#plotting
FedFund_date_point<-ggplot(FedFundsRate_date, aes(x= date, y=`Federal Funds Target Rate`))+
  geom_point()+
  stat_smooth()

FedFund_date_point

FedFund_date_line<-ggplot(FedFundsRate_date, aes(x=date, y=`Federal Funds Target Rate`))+
  geom_line()

FedFund_date_line

#option 2
FedFund_date_point<-ggplot(FedFundsRate_date, aes(x = as.Date(date,"%Y-%m-%d"), y =`Federal Funds Target Rate`))+
  geom_point()+
  stat_smooth()
FedFund_date_point


FedFund_date_line<-ggplot(FedFundsRate_date, aes(x = as.Date(date,"%Y-%m-%d"), y =`Federal Funds Target Rate`))+
  geom_line()
FedFund_date_line


##we can even change the scale of date/time:= use scale_x_date(limits, date_breaks)


FedFund_date_line<-ggplot(FedFundsRate_date, aes(x = date, y =`Federal Funds Target Rate`))+
  geom_line()+
  scale_x_date(limits = as.Date(c("1983-06-01","1985-06-01")), date_breaks = "1 month",
               date_labels = "%B") 

FedFund_date_line

FedFund_date_line<-ggplot(FedFundsRate_date, aes(x = date, y =`Federal Funds Target Rate`))+
  geom_line()+
  scale_x_date(limits = as.Date(c("1982-06-01","1983-06-01")), date_breaks = "1 week",
               date_labels = "%U") 

FedFund_date_line

FedFund_date_line<-ggplot(FedFundsRate_date, aes(x = date, y =`Federal Funds Target Rate`))+
  geom_line()+
  scale_x_date(limits = as.Date(c("1983-01-01","1990-01-01")), date_breaks = "1 year",
               date_labels = "%Y") 

FedFund_date_line

#%Y: year; %B month; "%U": week number; %D: day of the month; 

#changing the axis text layout: theme() and the angle option --> will be covered more in Week#9 

FedFund_date_line<-ggplot(FedFundsRate_date, aes(x = date, y =`Federal Funds Target Rate`))+
  geom_line()+
  scale_x_date(limits = as.Date(c("1983-06-01","1985-06-01")), date_breaks = "1 month",
               date_labels = "%B") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

FedFund_date_line

##Plotting multiple variables of economic data;
##most straight-foward method: two geom_line() functions
FedFund_date_line<-ggplot(FedFundsRate_date, aes(x = date))+
  geom_line(aes(y = `Federal Funds Target Rate`, color ="green"))+
  geom_line(aes(y = `Inflation Rate`, color="orange"))+
  scale_x_date(limits = as.Date(c("1983-06-01","1985-06-01")), date_breaks = "1 month",
               date_labels = "%B") +
  theme(axis.text.x=element_text(angle=60, hjust=1))

FedFund_date_line



##Plotly Example:
library(plotly)

library(hrbrthemes)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)

# Usual area chart
p <- data |>
  ggplot( aes(x=date, y=value)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("bitcoin price ($)") +
  theme_ipsum()

# Turn it interactive with ggplotly
p <- ggplotly(p)
p


##Animated line
library(gganimate)
library(gifski)
#
FedFundsRate_animated<-ggplot(FedFundsRate_date, aes(x=date, y=`Federal Funds Target Rate`)) +
  geom_line() +
  geom_point() +
  scale_x_date(limits = as.Date(c("1983-01-01","1990-12-01")), date_breaks = "1 month",
               date_labels = "%B") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  transition_reveal(date)


animate(FedFundsRate_animated, duration = 20, fps = 10, width = 500, height = 500, renderer = gifski_renderer())

anim_save("FedFundsRate.gif")




#Spatial Visualization
##Spatial Points
#let's take a look at what spatial data looks like first:
library(sp)

x_coords <- runif(n = 100, min = -100, max = -80)
y_coords <- runif(n = 100, min = 25, max = 45)

# Have a look at the first coordinates
head(spatial_points<-cbind(x_coords,y_coords))


#create a coordinate column
firstPoints <- SpatialPoints(coords = cbind(x_coords,y_coords))
head(firstPoints)

plot(firstPoints, pch = 19)

##take another example:
#ColeraDeath Case Distribution of the 1854 Broad Street cholera outbreak. (by John Snow)

library(mdsr)
library(sf)
head(CholeraDeaths)
CholeraDeaths<-CholeraDeaths

##drawing the points
ggplot(CholeraDeaths) +
  geom_sf()
#error 



#Plotting Map: Using geom_polygon (the example code in the reading)
library(maps)
us_states <- map_data("state")
head(us_states)
dim(us_states)

##the has more than 15,000 rows because you need a lot of lines to draw a good-looking map
p <- ggplot(data = us_states,
            aes(x = long, y = lat,
                          group = group))

usa_states<- p + geom_polygon(fill = "white", color = "black")

usa_states


#notice: what's wrong with this US Map? What is/are missing?



##Plotting Map: Using geom_sf()

#directly downloading and reading the shapefiles:
# Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_state_500k.zip", destfile= "/cloud/project/geodata/state.zip")
# You now have it in your current working directory, have a look!

# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
system("unzip /cloud/project/geodata/state.zip")
#  -- > You now have 4 files. One of these files is a .shp file! (TM_WORLD_BORDERS_SIMPL-0.3.shp)

states_sf<-st_read(file.path("/cloud/project/cb_2018_us_state_500k.shp"))


states<-ggplot()+
  geom_sf(data = states_sf)

states

##Hmm, the figure shows where the US in the world. Let's restricting the dimension and focus on the US.
##to do so, we can use coord_sf(xlim, ylim) to specify a specific range of the map to present. 

states<-ggplot()+
  geom_sf(data = states_sf)+
  coord_sf(xlim = c(-180, -65),
          ylim = c(20, 70))+
  theme_light() #making the background color as white/light.

states

##: Plotting Map: existing R packages for world map.
library(rnaturalearth)
library(rnaturalearthdata)

#ne_countries: Get natural earth world country polygons
world <- ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf')
world

#specify the US shape data
US <- world %>%
  filter(name == "United States")#

ggplot()+
  geom_sf(data = US)+
  theme_light()+
  coord_sf(xlim = c(-180, -65),
           ylim = c(20, 70))


##Plotting Data on the Map: 1. Filling the States 
head(us_states)

#let's use the USArrests data (Violent Crime Rates by US State)
data()
USArrests<-USArrests
head(USArrests)
USArrests$region <- row.names(USArrests) 


USArrests$region<-tolower(USArrests$region)#lower case in region so that we can join the two data

head(USArrests)

#joining the us_states and USArrests data
us_joined<-right_join(us_states, USArrests, by = "region")
head(us_joined)

##plotting the states by Murder rate
crime_states <- ggplot(data = us_joined,
             aes(x = long, y = lat,
                           group = group, fill = Murder))
crime_states + geom_polygon(color = "gray90", size = 0.1)

#if we want to have a map of Albers projection (reflecting the curve and shape of the earth)
crime_states + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 



##plotting points on Map. 
us.cities<-us.cities
head(us.cities)

#Notice the alternative way I write the ggplot() and geom_polygon() functions
cities_map <- ggplot() +
  geom_polygon(data = us_joined,
               aes(x = long, y = lat,
                   group = group), fill = "white", color = "grey")+   #I don't fill with a variable here
  geom_point(data = us.cities, aes(x = long, y = lat), pch = 19, size=0.00005) 
#noted that the data of us.cities for spatial coordinates of the cities is used in geom_point()

##Question: what if we use group_by() before geom_polygon()

cities_map

#Alternatively, we can also use the shapefile of US above for plotting
cities_map<-ggplot()+
  geom_sf(data = states_sf)+
  theme_light() +
  geom_point(data = us.cities, aes(x = long, y = lat), pch = 19, size=0.00005)+
  coord_sf(xlim = c(-180, -65),
           ylim = c(20, 70))

cities_map


##In class practice of mapping: plot the states with wrongful convictions
#source: https://www.nealdavislaw.com/criminal-defense-guides/exonerations-by-state-2019/
##download and read the "wrong_convictions.csv" data from Canvas
##Please use the code between line#260-289 to plot a map of US states varied by numbers 
#of wrongful cases


