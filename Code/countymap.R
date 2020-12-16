# load required libraries
library(ggplot2)
library(rgdal)
library(raster)
library(wesanderson)
library(dplyr)

countyplotlist <- list()

# read the shape files
setwd("~/GitHub/TCD_FinalYearProject/Data/")
countyshp <- readOGR("counties/counties.shp")
worldshp  <- readOGR("world/world.shp")

# read the county case data
#countycases <- read.csv("https://opendata-geohive.hub.arcgis.com/datasets/4779c505c43c40da9101ce53f34bb923_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D")
countycases <- read.csv("https://opendata.arcgis.com/datasets/d9be85b30d7748b5b7c09450b8aede63_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D")
countycases$TimeStamp <- as.Date(countycases$TimeStamp)

# just latest date
latest_date <- countycases$TimeStamp[nrow(countycases)]
latest_dat  <- countycases[countycases$TimeStamp == latest_date,]
fortnightbefore_dat <- countycases[countycases$TimeStamp == latest_date-13,]
latest_dat$ConfirmedCovidCases <- latest_dat$ConfirmedCovidCases - fortnightbefore_dat$ConfirmedCovidCases
# make shape data ggplot-friendly
countyshp@data$id <- rownames(countyshp@data)
countyshp.points  <- fortify(countyshp, region="id")
counties <- inner_join(countyshp.points, countyshp@data, by="id")
counties$CountyName <- gsub("County ", "", counties$NAME_EN)

# join case numbers for latest date to county data, in order to colour nicely
countycase_map <- left_join(counties, latest_dat, by=c("CountyName" = "CountyName"))

# color gradient
col_grad <- wes_palette("Zissou1", 20, type = "continuous")

# county plots
countyplotlist[["rep"]] <- ggplot(countycase_map) + 
  aes(long, lat, group=group, fill=PopulationProportionCovidCases) +
  geom_polygon(colour="grey40") + labs(fill = "Cases per 100k") +
  scale_fill_gradientn(colours = col_grad) +
  geom_text(data = latest_dat, aes(x = Long, y = Lat, label = floor(PopulationProportionCovidCases)), inherit.aes = FALSE) +
  ggtitle("Cases in Ireland per 100,000 population by county", 
          subtitle = paste("Cumulative, up to", format.Date(latest_date, "%B %d, %Y"))) +
  theme(axis.title        = element_blank(),
        axis.text         = element_blank(),
        axis.ticks        = element_blank(),
        panel.background  = element_blank(),
        legend.title      = element_blank(),
        legend.background = element_blank(),
        legend.position   = c(0.25,0.87))

countyplotlist[["fourteendaycases"]] <- ggplot(countycase_map) + 
  aes(long, lat, group=group, fill=ConfirmedCovidCases) +
  geom_polygon(colour="grey40") + labs(fill = "Cases") +
  scale_fill_gradientn(colours = col_grad) +
  geom_text(data = latest_dat, aes(x = Long, y = Lat, label = floor(ConfirmedCovidCases)), inherit.aes = FALSE) +
  ggtitle("Cases in Ireland by county", 
          subtitle = paste("From", format.Date(latest_date-13, "%B %d, %Y"),
                           "to", format.Date(latest_date, "%B %d, %Y"))) +
  theme(axis.title        = element_blank(),
        axis.text         = element_blank(),
        axis.ticks        = element_blank(),
        panel.background  = element_blank(),
        legend.title      = element_blank(),
        legend.background = element_blank(),
        legend.position   = c(0.25,0.87))

countyplotlist[["names"]] <- ggplot(countycase_map) + 
  aes(long, lat, group=group, fill=PopulationProportionCovidCases) +
  geom_polygon(colour="grey40") + labs(fill = "Cases per 100k") +
  scale_fill_gradientn(colours = col_grad) +
  geom_text(data = latest_dat, aes(x = Long, y = Lat, label = CountyName), size=3,inherit.aes = FALSE) +
  ggtitle("Cases in Ireland per 100,000 population by county", 
          subtitle = paste("Cumulative, up to", format.Date(latest_date, "%B %d, %Y"))) +
  theme(axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        panel.background  = element_blank(),
        legend.title      = element_blank(),
        legend.background = element_blank(),
        legend.position   = c(0.25,0.87))

countyplotlist[["blank"]] <- ggplot(countycase_map) + 
  aes(long, lat, group=group, fill=PopulationProportionCovidCases) +
  geom_polygon(colour="grey40") + labs(fill = "Cases per 100k") +
  scale_fill_gradientn(colours = col_grad) +
  ggtitle("Cases in Ireland per 100,000 population by county", 
          subtitle = paste("Cumulative, up to", format.Date(latest_date, "%B %d, %Y"))) +
  theme(axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        panel.background  = element_blank(),
        legend.title      = element_blank(),
        legend.background = element_blank(),
        legend.position   = c(0.25,0.87))



webdat <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# just latest date
latest_date <- as.Date(webdat$dateRep[1], tryFormats = c("%d/%m/%Y"))
latest_dat  <- webdat[webdat$dateRep == "26/11/2020",]


# make shape data ggplot-friendly
worldshp@data$id <- rownames(worldshp@data)
worldshp.points  <- fortify(worldshp, region="id")
countries <- inner_join(worldshp.points, worldshp@data, by="id")
countries$CNTRY_NAME[countries$CNTRY_NAME == "United Kingdom"] <- "United_Kingdom"
countries$CNTRY_NAME[countries$CNTRY_NAME == "United States"]  <- "United_States_of_America"
countries$CNTRY_NAME <- gsub(" ", "_", countries$CNTRY_NAME)

# join case numbers for latest date to country data, in order to colour nicely
world_map <- left_join(countries, latest_dat, by=c("CNTRY_NAME" = "countriesAndTerritories"))

worldplot <- list()

worldplot[["blank"]] <- ggplot(world_map) + 
  aes(long, lat, group=group, fill=Cumulative_number_for_14_days_of_COVID.19_cases_per_100000) +
  geom_polygon(colour="grey40") + labs(fill = "Cases per 100k") +
  scale_fill_gradientn(colours = col_grad) +
  ggtitle("Cumulative cases per 100,000 population by county", 
          subtitle = paste("From", format.Date(latest_date-13, "%B %d, %Y"), "to", format.Date(latest_date, "%B %d, %Y"))) +
  theme(axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        panel.background = element_blank(),
        plot.margin      = margin(0, 0, 0, 0, "cm"),
        legend.title      = element_blank(),
        legend.background = element_blank(),
        legend.position   = c(0.1,0.4))

europe_map <- world_map[world_map$long >= -20 & world_map$long <= 40,]
europe_map <- europe_map[europe_map$lat >= 35 & europe_map$lat <= 75,]


worldplot[["europe"]] <- ggplot(europe_map) + 
  aes(long, lat, group=group, fill=Cumulative_number_for_14_days_of_COVID.19_cases_per_100000) +
  geom_polygon(colour="grey40") + labs(fill = "Cases per 100k") +
  scale_fill_gradientn(colours = col_grad) +
  ggtitle("Cumulative cases per 100,000 population by county", 
          subtitle = paste("From", format.Date(latest_date-13, "%B %d, %Y"), "to", format.Date(latest_date, "%B %d, %Y"))) +
  theme(axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        panel.background = element_blank(),
        plot.margin      = margin(0, 0, 0, 0, "cm"),
        legend.title      = element_blank(),
        legend.background = element_blank(),
        legend.position   = c(0.1,0.3))
