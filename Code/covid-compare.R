source("Code/covid-plotutils.R")
source("Code/covid-modelutils.R")

owiddat      <- read.csv("Data/owid-covid-data.csv")
owiddat$date <- as.Date(owiddat$date, tryFormats = c("%Y-%m-%d"))

comparelist  <- list()
comparepairs <- list("IreUK"  = c("Ireland", "United Kingdom"),
                     "IreUS"  = c("Ireland", "United States"),
                     "IreIta" = c("Ireland", "Italy"),
                     "IreNl"  = c("Ireland", "Netherlands"),
                     "IreIce" = c("Ireland", "Iceland"))

compDates <- c("2020-12-15", "2021-02-15")

compare_theme <- function(){
  p <- theme(axis.text.x        = element_text(vjust=0.5),
             axis.line          = element_line(),
             panel.background   = element_rect(fill  = "grey"),
             panel.grid         = element_line(colour = "darkgrey"),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             legend.key         = element_blank(),
             legend.key.size    = unit(0.8,"line"),
             legend.text        = element_text(size = 8))
  return(p)
}

compareplots <- function(dat, countries, dates){
  getcountrydat <- function(dat, country, dates){
    cind <- grep(country, dat$location)
    countrydat <- data.frame(date = dat$date[cind], casepm = dat$new_cases_per_million[cind])
    #Specific dates
    countrydat <- countrydat[countrydat$date >= dates[1] & countrydat$date <= dates[2],]
    return(countrydat)
  }
  
  countryA    <- countries[1]
  countryB    <- countries[2]
  countryAdat <- getcountrydat(owiddat, countryA, compDates)
  countryBdat <- getcountrydat(owiddat, countryB, compDates)
  
  compcols <- wes_palettes$Darjeeling1[1:2]
  p <- ggplot(countryAdat) + 
    geom_point(data = countryAdat, aes(x = date, y = casepm, colour = countryA)) + 
    geom_line(data = countryAdat, aes(x = date, y = casepm, colour = countryA)) +
    geom_point(data = countryBdat, aes(x = date, y = casepm, colour = countryB)) + 
    geom_line(data = countryBdat, aes(x = date, y = casepm, colour = countryB)) +
    gg_scale_xy +  ylab(" ") + xlab(" ") + labs(colour = "Country")+
    scale_colour_manual(values = compcols) + compare_theme()
  return(p)
}

for(pair in names(comparepairs)){
  comparelist[[pair]] <- compareplots(owiddat, comparepairs[[pair]], compDates)
}

compdaterange <- owiddat$date >= compDates[1] & owiddat$date <= compDates[2]
countriescmp <- c("Ireland", "United Kingdom", "United States", "Italy", "Germany")
comparelist[["all"]] <- ggplot(owiddat[owiddat$location %in% countriescmp & compdaterange,]) + 
  geom_point(aes(x = date, y = new_cases_per_million, colour = location)) + 
  geom_line(aes(x = date, y = new_cases_per_million, colour = location)) +
  gg_scale_xy +  ylab(" ") + xlab(" ") + labs(colour = "Country")+
  scale_colour_manual(values = wes_palette("Darjeeling1", length(countriescmp))) +
  compare_theme()
