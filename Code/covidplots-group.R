require(ggplot2)
require(forecast)
require(dplyr)
require(wesanderson)
require(gridExtra)
#webdat <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
owiddat <- read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

owiddat$date <- as.Date(owiddat$date, tryFormats = c("%Y-%m-%d"))

plotslist <- list()

plot_xn <- function(countrydat, cols, labs){
  p <- ggplot(countrydat, binwidth = 0) + 
    geom_bar(aes(x = date, y = xn, fill = cols$xn), stat = "identity") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0))+
    scale_y_continuous(expand = c(0,0)) +
    scale_fill_manual(values = cols$xn, name = "", labels = labs$xn) +
    xntheme()
  return(p)
}

plot_yn <- function(countrydat, cols, labs){
  p <- ggplot(countrydat) + 
    geom_line(aes(x = date, y = yn, colour = "blue")) + 
    geom_point(aes(x = date, y = yn, colour = "blue"))+
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0))+
    scale_y_continuous(expand = c(0,0)) +
    scale_colour_manual(values = cols$yn, name = "", labels = labs$yn) +
    yntheme()
  return(p)
}

plot_basexn <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth = 0) + 
    geom_bar(aes(x = date, y = xn, fill = cols$xn), stat = "identity") + 
    geom_point(data = modeldat, aes(x = date, y = basexn, colour = cols$basexn)) + 
    geom_line(data = modeldat, aes(x = date, y = basexn, colour = cols$basexn)) +
    gg_scale_xy + 
    scale_fill_manual(  name = "leg", values = cols$xn,     labels = labs$xn) +
    scale_colour_manual(name = "leg", values = cols$basexn, labels = labs$basexn) +
    xntheme()
  return(p)
}

plot_baseyn <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat) + 
    geom_point(aes(x = date, y = yn, colour = "blue")) + geom_line(aes(x = date, y = yn, colour = "blue")) +
    geom_point(data = modeldat, aes(x = date, y = baseyn, colour = "base"), shape = 1) + 
    geom_line(data = modeldat, aes(x = date, y = baseyn, colour = "base")) +
    gg_scale_xy + 
    scale_colour_manual(name = "leg",values = c("blue" = cols$yn, "base" = cols$baseyn), 
                        labels = c("blue" = labs$yn, "base" = labs$baseyn),
                        guide = guide_legend(override.aes = list(
                          shape = c("blue"=1, "base" = 16)))) +
    yntheme()
  return(p)
}

plot_crn <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth = 0) +
    geom_bar(aes(x = date, y = xn, fill = cols$xn), stat = "identity") + 
    geom_point(data = modeldat, aes(x = date, y = basexn, colour = "basexn")) + 
    geom_line(data = modeldat, aes(x = date, y = basexn, colour = "basexn")) +
    geom_line(data = modeldat, aes(x = date, y = Crn, colour = "Crn")) +
    gg_scale_xy + 
    scale_fill_manual(  name = "leg",values = cols$xn, labels = labs$xn) +
    scale_colour_manual(name = "leg",
                        values = c("basexn" = cols$basexn, "Crn" = cols$Crn), 
                        labels = c("basexn" = labs$basexn, "Crn" = labs$Crn),
                        guide = guide_legend(override.aes = list(
                          shape = c("basexn" = 16, "Crn" = NA)))) +
    xntheme()
  return(p)
}

plot_mavgx3 <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth = 0) + 
    geom_bar(aes(x = date, y = xn, fill = cols$xn), stat = "identity") + 
    geom_point(data = modeldat, aes(x = date, y = basexn, colour = "basexn")) + 
    geom_line(data = modeldat, aes(x = date, y = basexn, colour = "basexn")) +
    geom_line(data = countrydat, aes(x = date, y = mavgx3, colour = "x3")) +
    scale_fill_manual(values = cols$xn, labels = labs$xn) +
    scale_colour_manual(values = c("basexn" = cols$basexn, "x3" = cols$x3), 
                        labels = c("basexn" = labs$basexn, "x3" = labs$x3),
                        guide = guide_legend(override.aes = list(
                          shape = c("basexn" = 16, "x3" = NA)))) +
    gg_scale_xy + xntheme()
  return(p)
}

plot_periodic <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth = 0) + 
    geom_bar(aes(x = date, y = xn, fill = cols$xn), stat = "identity") + 
    geom_point(data = modeldat, aes(x = date, y = basexn, colour = "base")) + 
    geom_line(data = modeldat, aes(x = date, y = basexn, colour = "base")) +
    geom_line(data = countrydat, aes(x = date, y = mavgx3, colour = "x3")) +
    geom_point(data = modeldat,aes(x = date, y = modxPeriodic, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = modxPeriodic, colour = "periodic")) +
    gg_scale_xy + 
    guides(colour=guide_legend(ncol=1,nrow=3,byrow=TRUE),
           fill=guide_legend(ncol=1,nrow=1,byrow=TRUE)) +
    scale_fill_manual(values = cols$xn, labels = labs$xn) +
    scale_colour_manual(values = c("base" = cols$basexn, "periodic" = cols$periodic, "x3" = cols$x3), 
                        labels = c("base" = labs$basexn, "periodic" = labs$periodic, "x3" = labs$x3)) +
    guides(colour = guide_legend(override.aes = list(shape = c("base" = 16, "periodic" = 16, "x3" = NA)))) +
    xntheme()
  return(p)
}

plot_hw <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth = 0) + 
    geom_bar(aes(x = date, y = xn, fill = "actual"), stat = "identity") + 
    geom_ribbon(data = modeldat, aes(x = date, ymin = hwlo, ymax = hwhi, fill = "hw"), alpha = 0.5) +
    geom_point(data = modeldat, aes(x = date, y = hwxn, colour = "hw")) + 
    geom_line(data = modeldat, aes(x = date, y = hwxn, colour = "hw")) +
    geom_point(data = modeldat, aes(x = date, y = basexn, colour = "base")) + 
    geom_line(data = modeldat, aes(x = date, y = basexn, colour = "base")) +
    geom_point(data = modeldat,aes(x = date, y = modxPeriodic, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = modxPeriodic, colour = "periodic")) +
    gg_scale_xy + 
    guides(colour=guide_legend(ncol=1,nrow=3,byrow=TRUE),
           fill=guide_legend(ncol=1,nrow=2,byrow=TRUE)) +
    scale_fill_manual(labels = c("actual" = labs$xn, "hw" = labs$hwpi),
                      values = c("actual" = cols$xn, "hw" = cols$hwpi))+ 
    scale_colour_manual(labels = c("base" = labs$basexn, "hw" = labs$hw, "periodic" = labs$periodic),
                        values = c("base" = cols$basexn, "hw" = cols$hw, "periodic" = cols$periodic)
                        ) +
    xntheme()
  return(p)
}

plot_hwy <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat) + 
    geom_point(aes(x = date, y = yn, colour = "blue")) + 
    geom_line(aes(x = date, y = yn, colour = "blue")) +
    geom_ribbon(data = modeldat, aes(x = date, ymin = hwylo, ymax = hwyhi, fill = "pi"), alpha = 0.5) +
    geom_point(data = modeldat, aes(x = date, y = hwyn, colour = "hw"), shape = 5) + 
    geom_line(data = modeldat, aes(x = date, y = hwyn, colour = "hw")) +
    geom_point(data = modeldat, aes(x = date, y = baseyn, colour = "base"), shape = 1) + 
    geom_line(data = modeldat, aes(x = date, y = baseyn, colour = "base")) +
    gg_scale_xy + 
    guides(colour=guide_legend(ncol=1,nrow=3,byrow=TRUE),
           fill=guide_legend(ncol=1,nrow=1,byrow=TRUE)) +
    scale_fill_manual(values = c("pi" = cols$hwpi), 
                      labels = c("pi" = labs$hwpi)) +
    scale_colour_manual(values = c("actual" = cols$xn, "base" = cols$baseyn, "blue" = cols$yn, "hw" = cols$hw), 
                        labels = c("actual" = labs$xn, "base" = labs$baseyn, "blue" = labs$yn, "hw" = labs$hwy),
                        guide = guide_legend(override.aes = list(
                          shape = c("actual" = 16, "base" = 1, "blue"=16, "hw" = 5)))) +
    yntheme()
  return(p)
}

plot_arima <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth = 0) + 
    geom_bar(aes(x = date, y = xn, fill = "actual"), stat = "identity") + 
    geom_ribbon(data = modeldat, aes(x = date, ymin = arimalo, ymax = arimahi, fill = "pi"), alpha = 0.5) +
    geom_point(data = modeldat, aes(x = date, y = arimaxn, colour = "arima")) + 
    geom_line(data = modeldat, aes(x = date, y = arimaxn, colour = "arima")) +
    geom_point(data = modeldat, aes(x = date, y = basexn, colour = "base")) + 
    geom_line(data = modeldat, aes(x = date, y = basexn, colour = "base")) +
    geom_point(data = modeldat,aes(x = date, y = modxPeriodic, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = modxPeriodic, colour = "periodic")) +
    gg_scale_xy + 
    guides(colour=guide_legend(ncol=1,nrow=3,byrow=TRUE),
           fill=guide_legend(ncol=1,nrow=2,byrow=TRUE)) +
    scale_fill_manual(values = c("actual" = cols$xn, "pi" = cols$arimapi), 
                      labels = c("actual" = labs$xn, "pi" = labs$arimapi)) +
    scale_colour_manual(values = c("arima" = cols$arima, "base" = cols$basexn, "periodic" = cols$periodic), 
                        labels = c("arima" = labs$arima, "base" = labs$basexn, "periodic" = labs$periodic)) +
    xntheme()
  return(p)
}

plot_arimay <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat) + 
    geom_point(aes(x = date, y = yn, colour = "blue")) + 
    geom_line(aes(x = date, y = yn, colour = "blue")) +
    geom_ribbon(data = modeldat, aes(x = date, ymin = arimaylo, ymax = arimayhi, fill = "pi"), alpha = 0.5) +
    geom_point(data = modeldat, aes(x = date, y = arimayn, colour = "arima"), shape = 2) + 
    geom_line(data = modeldat, aes(x = date, y = arimayn, colour = "arima")) +
    geom_point(data = modeldat, aes(x = date, y = baseyn, colour = "base"), shape = 1) + 
    geom_line(data = modeldat, aes(x = date, y = baseyn, colour = "base")) +
    gg_scale_xy + 
    scale_fill_manual(values = c("pi" = cols$arimapi), 
                      labels = c("pi" = labs$arimapi)) +
    scale_colour_manual(values = c("arima" = cols$arima, "base" = cols$baseyn, "blue" = cols$yn), 
                        labels = c("arima" = labs$arimay, "base" = labs$baseyn, "blue" = labs$yn),
                        guide = guide_legend(override.aes = list(
                          shape = c("arima" = 2, "base" = 1, "blue"=16)))) +
    yntheme()
  return(p)
}

plot_hwarima <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth = 0) + 
    geom_bar(aes(x = date, y = xn, fill = "actual"), stat = "identity") + 
    geom_point(data = modeldat, aes(x = date, y = arimaxn, colour = "arima")) + 
    geom_line(data = modeldat, aes(x = date, y = arimaxn, colour = "arima")) +
    geom_point(data = modeldat, aes(x = date, y = hwxn, colour = "hw")) + 
    geom_line(data = modeldat, aes(x = date, y = hwxn, colour = "hw")) +
    gg_scale_xy + 
    guides(colour=guide_legend(ncol=1,nrow=2,byrow=TRUE),
           fill=guide_legend(ncol=1,nrow=1,byrow=TRUE)) +
    scale_fill_manual(values = c("actual" = cols$xn), 
                      labels = c("actual" = labs$xn)) +
    scale_colour_manual(values = c("arima" = cols$arima, "hw" = cols$hw), 
                        labels = c("arima" = labs$arima, "hw" = labs$hw)) +
    xntheme()
  return(p)
}

plot_nn <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth = 0) + 
    geom_bar(aes(x = date, y = xn, fill = "actual"), stat = "identity") + 
    geom_point(data = modeldat, aes(x = date, y = basexn, colour = "base")) + 
    geom_line(data = modeldat, aes(x = date, y = basexn, colour = "base")) +
    geom_point(data = modeldat,aes(x = date, y = modxPeriodic, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = modxPeriodic, colour = "periodic")) +
    geom_point(data = modeldat, aes(x = date, y = nnxn, colour = "nn")) + 
    geom_line(data = modeldat, aes(x = date, y = nnxn, colour = "nn")) +
    gg_scale_xy + 
    guides(colour=guide_legend(ncol=1,nrow=3,byrow=TRUE),
           fill=guide_legend(ncol=1,nrow=1,byrow=TRUE)) +
    scale_fill_manual(values = c("actual" = cols$xn), 
                      labels = c("actual" = labs$xn)) +
    scale_colour_manual(values = c("base" = cols$basexn, "nn" = cols$nn, "periodic" = cols$periodic), 
                        labels = c("base" = labs$basexn, "nn" = labs$nn, "periodic" = labs$periodic)) +
    xntheme()
  return(p)
}

plot_nny <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat) + 
    geom_point(aes(x = date, y = yn, colour = "blue")) + 
    geom_line(aes(x = date, y = yn, colour = "blue")) +
    geom_point(data = modeldat, aes(x = date, y = baseyn, colour = "base"), shape = 1) + 
    geom_line(data = modeldat, aes(x = date, y = baseyn, colour = "base")) +
    geom_point(data = modeldat, aes(x = date, y = nnyn, colour = "nn"), shape = 0) + 
    geom_line(data = modeldat, aes(x = date, y = nnyn, colour = "nn")) +
    gg_scale_xy + 
    scale_colour_manual(values = c("base" = cols$baseyn, "blue" = cols$yn, "nn" = cols$nn), 
                        labels = c("base" = labs$baseyn, "blue" = labs$yn, "nn" = labs$nny),
                        guide = guide_legend(override.aes = list(
                          shape = c("base" = 1, "blue"=16, "nn" = 0)))) +
    yntheme()
  return(p)
}

plot_multixn <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth = 0) + 
    geom_bar(aes(x = date, y = xn, fill = cols$xn), stat = "identity") + 
    geom_point(data = modeldat, aes(x = date, y = multixn, colour = cols$multixn)) + 
    geom_line(data = modeldat, aes(x = date, y = multixn, colour = cols$multixn)) +
    gg_scale_xy + 
    scale_fill_manual(  name = "leg",values = cols$xn, labels = labs$xn) +
    scale_colour_manual(name = "leg",values = cols$multixn, labels = labs$multixn) +
    xntheme()
  return(p)
}

plot_multiyn <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat) + 
    geom_point(aes(x = date, y = yn, colour = "blue")) + 
    geom_line(aes(x = date, y = yn, colour = "blue")) +
    geom_point(data = modeldat, aes(x = date, y = multiyn, colour = "base"), shape = 1) + 
    geom_line(data = modeldat, aes(x = date, y = multiyn, colour = "base")) +
    gg_scale_xy + 
    scale_colour_manual(name = "leg",values = c("blue" = cols$yn, "base" = cols$multiyn), 
                        labels = c("blue" = labs$yn, "base" = labs$multiyn),
                        guide = guide_legend(override.aes = list(
                          shape = c("blue"=1, "base" = 16)))) +
    yntheme()
  return(p)
}

plot_multiperxn <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth = 0) + 
    geom_bar(aes(x = date, y = xn, fill = cols$xn), stat = "identity") + 
    geom_point(data = modeldat, aes(x = date, y = multipxn, colour = "multi")) + 
    geom_line(data = modeldat, aes(x = date, y = multipxn, colour = "multi")) +
    geom_line(data = countrydat, aes(x = date, y = mavgx3, colour = "x3")) +
    gg_scale_xy + 
    guides(colour=guide_legend(ncol=1,nrow=2,byrow=TRUE),
           fill=guide_legend(ncol=1,nrow=1,byrow=TRUE)) +
    scale_fill_manual(values = cols$xn, labels = labs$xn) +
    scale_colour_manual(values = c("multi" = cols$multip, "x3" = cols$x3), 
                        labels = c("multi" = labs$multipxn, "x3" = labs$x3)) +
    guides(colour = guide_legend(override.aes = list(shape = c("multi" = 16, "x3" = NA)))) +
    xntheme()
  return(p)
}

plot_multiperyn <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat) + 
    geom_point(aes(x = date, y = yn, colour = "blue"), shape = 16) + 
    geom_line(aes(x = date, y = yn, colour = "blue")) +
    geom_point(data = modeldat, aes(x = date, y = multipyn, colour = "mult"), shape = 1) + 
    geom_line(data = modeldat, aes(x = date, y = multipyn, colour = "mult")) +
    gg_scale_xy + 
    scale_colour_manual(name = "leg",values = c("blue" = cols$yn, "mult" = cols$multip), 
                        labels = c("blue" = labs$yn, "mult" = labs$multipyn),
                        guide = guide_legend(override.aes = list(
                          shape = c("blue"=16, "mult" = 1)))) +
    yntheme()
  return(p)
}

plot_worldtotal <- function(dat){
  p <- ggplot(dat, binwidth = 0) + 
    geom_bar(aes(x = Date, y = Cases),  fill = wes_palettes$Zissou1[1], stat = "identity") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0))+
    scale_y_continuous(expand = c(0,0)) +
    ggtitle(wt_title) + xntheme()
  return(p)
}

modnorm <- function(x,modx) return(floor(sum(abs(x-modx))/length(x)))

xntheme <- function(){
  p <- theme(axis.text.x        = element_text(vjust = 0.5),
             axis.title         = element_blank(),
             axis.line          = element_line(),
             panel.background   = element_rect(fill  = "grey"),
             panel.grid         = element_line(colour = "darkgrey"),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             legend.title       = element_blank(),
             legend.margin      = margin(0,4,0,4,"pt"),
             legend.background  = element_blank(),
             legend.text.align  = 0,
             legend.box.background  = element_rect(linetype="solid", colour ="darkgrey", size = 0.1, fill = "white"),
             legend.spacing     = unit(0, "cm"),
             legend.key.size    = unit(0.8,"line"),
             legend.key         = element_blank(),
             legend.text        = element_text(size = 6),
             legend.direction   = "vertical",
             legend.box         = "vertical",
             legend.box.just    ='left',
             legend.position    = "top") 
  return(p)
}

yntheme <- function(){
  p <- theme(axis.text.x        = element_text(vjust = 0.5),
             axis.title         = element_blank(),
             panel.background   = element_rect(fill  = "grey"),
             panel.grid         = element_line(colour = "darkgrey"),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             legend.title       = element_blank(),
             legend.margin      = margin(4,0,0,4,"pt"),
             legend.background  = element_blank(),
             legend.key         = element_blank(),
             legend.box.background  = element_rect(linetype="solid", colour ="darkgrey", size = 0.1, fill = "white"),
             legend.spacing     = unit(0, "cm"),
             legend.key.size    = unit(0.8,"line"),
             legend.text        = element_text(size = 6),
             legend.direction   = "vertical",
             legend.box         = "vertical",
             legend.box.just    ='left',
             legend.position    = "top")
  return(p)
}

xntoyn <- function(xn) return(cumsum(xn))

gg_scale_xy <- list(
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b", expand = c(0,0)),
  scale_y_continuous(expand = c(0,0)))

basicmodx <- function(x, pars, len = 0){
  q <- floor(pars[1])
  a <- pars[2]
  b <- pars[3]
  modx <- x[1:q]
  for(i in (q+1):(length(x)+len)){
    modx[i] <- (1-b)*modx[i-1] + a*modx[i-q]
  }
  return(modx)
}

norm <- function(par, x) return(modnorm(x,basicmodx(x, par)))

normy <- function(par, x, y) return(modnorm(y,xntoyn(basicmodx(x, par))))

normalize <- function(x) return((x-min(x))/(max(x)-min(x)))

basef <- function(lambda,par) {
  return(lambda^(par[1]+1)-(1-par[3])*lambda^(par[1])-par[2])
}

basefprime <- function(lambda,par) {
  return((par[1]+1)*lambda^(par[1])-(1-par[3])*par[1]*lambda^(par[1]-1))
} 

normC <- function(par,x,r){
  return(modnorm(x, par*r^(0:(length(x[!is.na(x)])-1))))
}

movingavg <- function(x){
  mavgx <- (x[1]+x[2])/2
  for(i in 2:(length(x)-1)){
    mavgx[i] <- sum(x[(i-1):(i+1)])/3
  }
  mavgx[length(x)] <- (x[length(x)-1]+x[length(x)])/2
  return(mavgx)
}

normper <- function(par, q, x) return(modnorm(x,modxper(par,q,x)))

modxper <- function(par, q, x, len = 0){
  #a,b,c1,c2,p1,p2,n1,n2
  an   <- par[1]*(1+par[3]*sin(2*pi*(1:(length(x)+len) - par[7])/par[5]))
  bn   <- par[2]*(1+par[4]*sin(2*pi*(1:(length(x)+len) - par[8])/par[6]))
  modx <- x[1:q]
  for(i in (q+1):(length(x)+len)){
    modx[i] <- (bn[i]*(1-bn[i-1]))*modx[i-1]/bn[i-1] +
      (an[i-q]*bn[i])*modx[i-q]/bn[i-q]
  }
  return(modx)
}

covidPlots <- function(country, dateBounds, data){
  plots <- list()
  countryrows <- grep(country, data$location)
  countrydat <- data.frame(date = data$date[countryrows], 
                           xn   = data$new_cases[countryrows],
                           yn   = data$total_cases[countryrows])
  countrydatfull <- countrydat[countrydat$date <= dateBounds[2],]
  
  if(nrow(countrydat[countrydat$date < dateBounds[1],]) == 0)
    prevcases <- 0
  else
    prevcases <- sum(countrydat$xn[countrydat$date < dateBounds[1]])
  
  #Specific dates
  countrydat <- countrydat[countrydat$date >= dateBounds[1] & countrydat$date <= dateBounds[2],]
  #countrydat$xn[countrydat$xn < 0] <- 0
  latest_date <- countrydat$date[nrow(countrydat)]
  
  cols <- list(
    xn       = wes_palettes$Zissou1[1],
    yn       = wes_palettes$Darjeeling2[2],
    basexn   = wes_palettes$Darjeeling1[1],
    baseyn   = wes_palettes$Darjeeling1[1],
    x3       = wes_palettes$FantasticFox1[2],
    Crn      = wes_palettes$FantasticFox1[2],
    arima    = wes_palettes$Darjeeling1[4],
    arimapi  = wes_palettes$Darjeeling1[3],
    hw       = wes_palettes$Moonrise1[2],
    hwpi     = wes_palettes$Moonrise1[1],
    periodic = "magenta4", #wes_palettes$IsleofDogs1[1], #
    nn       = wes_palettes$FantasticFox1[4]
  )
  
  labs <- list(
    xn = list(bquote(.(country)*","~x[n]*"*=new cases/day, actual till"~.(format.Date(latest_date,"%d.%m.%Y")))),
    yn = list(bquote(.(country)*","~y[n]*"*=cumulative cases, actual till"~.(format.Date(latest_date,"%d.%m.%Y"))))
  )
  
  plots[["xn"]] <- plot_xn(countrydatfull, cols, labs)
  
  plots[["yn"]] <- plot_yn(countrydatfull, cols, labs)
  
  #Basic model: need a,b,q,r using ||x-x*|| and ||y-y*||
  ##q - Any infected person becomes ill and infectious on the q-th day after infection.
  ##a - During each day, each ill person at large infects on average a other persons.
  ##b - During each day, a fraction b of ill people at large gets isolated 
  
  forecastlen <- 14
  
  aseq    <- seq(from = 0.1, to = 2.5, length.out = 80)
  bseq    <- seq(from = 0.1, to = 0.9, length.out = 80)
  qseq    <- 6:8
  normdat <- expand.grid(q = qseq, a = aseq, b = bseq)
  abnorm  <- apply(normdat, 1, function(x) norm(x, countrydat$xn))

  normdat$abnorm  <- abnorm
  
  newnormdat <- normdat %>% 
    top_n(abnorm ,n = -0.07*nrow(.))
  
  col_grad <- wes_palette("Zissou1", 20, type = "continuous")
  
  tileoptim <- normdat[which.min(normdat$abnorm),1:3]
  #tileoptim <- newnormdat[which.min(newnormdat$abnormy),1:3]
  optimpars <- c(tileoptim$q, tileoptim$a, tileoptim$b)
  plots[["combnorm"]] <- ggplot(newnormdat, aes(x = a, y = b, z = abnorm)) +
    geom_contour_filled() +
    scale_fill_brewer(palette = "Spectral")

  basexn   <- basicmodx(countrydat$xn, optimpars, len = forecastlen)
  
  modeldat <- data.frame(date = c(countrydat$date,latest_date + 1:forecastlen),
                         basexn = basexn, baseyn = xntoyn(basexn) + prevcases)
  
  #Newtons method, r_1 = r_0 - f(r_0)/f'(r_0)
  base_r_zero <- (optimpars[2]/optimpars[3])^(1/(2*optimpars[1]))
  base_r_one  <- base_r_zero -basef(base_r_zero,optimpars)/basefprime(base_r_zero,optimpars)
  base_r_one  <- round(base_r_one,3)
  
  roptimpars  <- round(optimpars,3)
  labs$basexn <- list(bquote("basic model, "~x[n]*"=new cases/day; a="*.(roptimpars[2])*", b="*.(roptimpars[3])*", q="*.(roptimpars[1])*";  r="*.(base_r_one)*"; ||x*-x||="*.(norm(optimpars,countrydat$xn))))
  
  ynnorm      <- modnorm(countrydat$yn, modeldat$baseyn[1:length(countrydat$yn)])
  labs$baseyn <- list(bquote("basic model, "~y[n]*"=cumulative cases; a="*.(roptimpars[2])*", b="*.(roptimpars[3])*", q="*.(roptimpars[1])*", ||y*-y||="*.(ynnorm)))

  plots[["basexn"]] <- plot_basexn(countrydat, modeldat, cols, labs)
  
  plots[["baseyn"]] <- plot_baseyn(countrydat, modeldat, cols, labs)

  optimC <- optim(par = countrydat$xn[1], normC, method = "Brent",
                  lower = 1, upper = 2*max(countrydat$xn[!is.na(countrydat$xn)]),
                  x = basexn[1:nrow(countrydat)], r = base_r_one)$par
  
  modeldat$Crn <- optimC*base_r_one^(1:nrow(modeldat))
  
  labs$Crn <- list(bquote(Cr^n*", r="*.(base_r_one)*", C="*.(floor(optimC))))
  
  plots[["Crn"]] <- plot_crn(countrydat, modeldat, cols, labs)

  mavgx1 <- movingavg(countrydat$xn[!is.na(countrydat$xn)])

  countrydat$mavgx3 <- movingavg(mavgx1)
  
  x3norm  <- modnorm(countrydat$xn,countrydat$mavgx3)
  labs$x3 <- list(bquote("moving average x*(3); ||x*(3)-x*||="*.(x3norm)))
  
  plots[["mavgx3"]] <- plot_mavgx3(countrydat, modeldat, cols, labs)
  
  #parameters of the form (ci,pi,ni)
  ##an = a1 + c1 sin(2??/p1 (n ??? n1))
  aseqper <- optimpars[2]*seq(from = 0.8, to = 1.2, length.out = 5)
  bseqper <- optimpars[3]*seq(from = 0.8, to = 1.2, length.out = 5)
  c_1seq  <- c_2seq <- seq(0.04, 0.2, length.out = 10)
  n_1seq  <- n_2seq <- c(1,7)
  p_1seq  <- p_2seq <- 6:7
  normdatp <- expand.grid(a  = aseqper, b  = bseqper,
                          c1 = c_1seq,  c2 = c_2seq,
                          p1 = p_1seq,  p2 = p_2seq,
                          n1 = n_1seq,  n2 = n_2seq)
  
  
  pernorm <- apply(normdatp, 1, function(x) normper(x, q = optimpars[1], countrydat$xn))
  normdatp$pernorm <- pernorm
  
  peroptim   <- normdatp[which.min(pernorm),1:8]
  optpernorm <- normdatp[which.min(pernorm),9]
  modeldat$modxPeriodic <- modxper(as.numeric(peroptim),countrydat$xn, q = optimpars[1], forecastlen)
  
  peroptim <- round(as.numeric(peroptim),3)
  
  perparamdat <- data.frame(
    x  = modeldat$date,
    an = peroptim[1]*(1+peroptim[3]*sin(2*pi*(1:(nrow(modeldat)) - peroptim[7])/peroptim[5])),
    bn = peroptim[2]*(1+peroptim[4]*sin(2*pi*(1:(nrow(modeldat)) - peroptim[8])/peroptim[6]))
  )
  
  plots[["perparam"]] <- ggplot(perparamdat) +
    geom_line(aes(x=x,y=an,col="a_n")) +
    geom_line(aes(x=x,y=bn,col="b_n")) +
    geom_hline(aes(yintercept = peroptim[1], col = "a"), linetype="dashed") +
    geom_hline(aes(yintercept = peroptim[2], col = "b"), linetype="dashed") +
    xlab("date") + ylab("") +
    scale_color_manual(values = wes_palettes$Rushmore1[c(3,3,5,5)]) +
    guides(colour = guide_legend(override.aes = list(linetype = 
         c("a"="dashed", "a_n"="solid", "b"="dashed", "b_n"="solid")))) +
    xntheme() + theme(legend.position = "right")
  
  #a,b,c1,c2,p1,p2,n1,n2
  labs$periodic <- list(bquote("periodic model,"~x[n]*"=new cases/day;"
                              ~a*"="*.(peroptim[1])*","~ b*"="*.(peroptim[2])*","
                              ~q*"="*.(optimpars[1])*";"~"||x*-x||="*.(optpernorm)*";"
                              ~c[1]*"="*.(peroptim[3])*","~p[1]*"="*.(peroptim[5])*","
                              ~n[1]*"="*.(peroptim[7])*","~c[2]*"="*.(peroptim[4])*","
                              ~p[2]*"="*.(peroptim[6])*","~n[2]*"="*.(peroptim[8])))
  
  plots[["periodic"]] <- plot_periodic(countrydat, modeldat, cols, labs)
  
  dat_ts <- ts(data = countrydat$xn, frequency = optimpars[1])
  
  countrydat$resid <- residuals(naive(dat_ts))
  
  g1 <- autoplot(dat_ts) + theme(axis.title = element_blank())
  g2 <- ggAcf(dat_ts) + ggtitle("")
  g3 <- ggPacf(dat_ts) + ggtitle("")
  
  plots[["tsdisplay"]] <- grid.arrange(grobs = list(g1,g2,g3),
    layout_matrix = rbind(c(1, 1), c(2, 3)))
  
  plots[["residuals"]] <- gghistogram(dat_ts, add.normal = TRUE)
  
  plots[["tsdecompose"]] <- autoplot(decompose(dat_ts))
  
  if(any(countrydat$xn <= 0)){
    plots[["hw"]] <- ggplot(data.frame(x = 0,y = 0)) + 
      geom_label(x = 0, y = 0, label = "Error: Country data has nonpositive values",
                 color = "red", size = 5 , fontface = "bold" ) +
      xlim(-1,1) + ylim(-1,1) + 
      theme(axis.line  = element_blank(),
            axis.text  = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
  } else{
    hwmethod   <- "additive"
    xnonlylast <- c(rep(NA, nrow(countrydat)-1), countrydat$xn[length(countrydat$xn)])
    #lambda=0 ensures values stay positive
    hwfcst     <- forecast::hw(dat_ts, h = forecastlen, seasonal = hwmethod, lambda = 0)
    hwfcst$lower[hwfcst$lower[,2] < 0,2] <- 0
    #hwfcst$upper[hwfcst$upper[,2] < 0,2] <- 0
    #hwfcst$mean[hwfcst$mean < 0] <- 0
    hwfcst$fitted[1:optimpars[1]] <- countrydat$xn[1:optimpars[1]]
    modeldat$hwxn <- c(hwfcst$fitted, hwfcst$mean)
    modeldat$hwlo <- c(hwfcst$fitted, hwfcst$lower[,2])
    modeldat$hwhi <- c(hwfcst$fitted, hwfcst$upper[,2])
    
    modeldat$hwyn  <- xntoyn(modeldat$hwxn)+prevcases
    modeldat$hwylo <- xntoyn(modeldat$hwlo)+prevcases
    modeldat$hwyhi <- xntoyn(modeldat$hwhi)+prevcases
    
    hwnorm <- modnorm(countrydat$xn,hwfcst$fitted)

    labs$hw   <- paste0("HoltWinters algorithm,  ||x*-x||=", modnorm(countrydat$xn,hwfcst$fitted))
    labs$hwy  <- paste0("HoltWinters algorithm,  ||y*-y||=", modnorm(countrydat$yn,modeldat$hwyn[1:nrow(countrydat)]))
    labs$hwpi <- "HW 95% Prediction Interval"
    
    plots[["hw"]]  <- plot_hw(countrydat, modeldat, cols, labs)
    plots[["hwy"]] <- plot_hwy(countrydat, modeldat, cols, labs)
  }
    
  auto.fit <- auto.arima(dat_ts, lambda = 0) #keep values positive
  getArmaModel <- function(arma){
    return(paste0("ARIMA(", paste0(arma[c(1,6,2)],collapse = ","), ")(",
                  paste0(arma[c(3,7,4)], collapse = ","), ")[", arma[5], "]"))
  }
  
  arima.fcst <- forecast(auto.fit, level = c(80, 95), h = forecastlen)
  arima.fcst$lower[arima.fcst$lower[,2] < 0,2] <- 0
  #arima.fcst$upper[arima.fcst$upper[,2] < 0,2] <- 0
  arima.fcst$fitted[1:optimpars[1]] <- countrydat$xn[1:optimpars[1]]
  
  #arima.fcst$mean[arima.fcst$mean < 0] <- 0
  
  arimanorm <- modnorm(countrydat$xn,arima.fcst$fitted)
  
  arimalabs    <- getArmaModel(auto.fit$arma)
  labs$arima   <- paste0(arimalabs, ", ||x*-x||=", arimanorm)
  labs$arimapi <- "ARIMA 95% Prediction Interval"
  
  modeldat$arimaxn <- c(auto.fit$fitted, arima.fcst$mean)
  modeldat$arimalo <- c(auto.fit$fitted,arima.fcst$lower[,2])
  modeldat$arimahi <- c(auto.fit$fitted,arima.fcst$upper[,2])
  
  modeldat$arimaxn[1:optimpars[1]] <- countrydat$xn[1:optimpars[1]]
  modeldat$arimalo[1:optimpars[1]] <- countrydat$xn[1:optimpars[1]]
  modeldat$arimahi[1:optimpars[1]] <- countrydat$xn[1:optimpars[1]]
  
  modeldat$arimayn  <- xntoyn(modeldat$arimaxn) + prevcases
  modeldat$arimaylo <- xntoyn(modeldat$arimalo) + prevcases
  modeldat$arimayhi <- xntoyn(modeldat$arimahi) + prevcases

  labs$arimay  <- paste0(arimalabs, ", ||y*-y||=", modnorm(countrydat$yn,modeldat$arimayn[1:nrow(countrydat)]))
  
  plots[["arima"]] <- plot_arima(countrydat, modeldat, cols, labs)
  
  plots[["arimay"]] <- plot_arimay(countrydat, modeldat, cols, labs)
  
  plots[["hwarima"]] <- plot_hwarima(countrydat, modeldat, cols, labs)
  
  nHidden <- max(1,floor(0.5*(1+auto.fit$arma[1]+auto.fit$arma[3])))
  #Box-Cox transformation with lambda=0 to ensure the forecasts stay positive.
  nnfit   <- nnetar(dat_ts, p = auto.fit$arma[1], P = auto.fit$arma[3], size = nHidden, lambda = 0, repeats = 20, maxit = 50) 
  nn.fcst <- forecast(nnfit, h = forecastlen)

  nn.fcst$mean[nn.fcst$mean < 0] <- 0
  nn.fcst$fitted[1:optimpars[1]] <- countrydat$xn[1:optimpars[1]]
 
  modeldat$nnxn <- c(nn.fcst$fitted, nn.fcst$mean)
  modeldat$nnyn <- xntoyn(modeldat$nnxn) + prevcases
  
  labs$nn  <- paste0(nnfit$method, ", ||x*-x||=", modnorm(countrydat$xn,nn.fcst$fitted))
  labs$nny <- paste0(nnfit$method, ", ||y*-y||=", modnorm(countrydat$yn,modeldat$nnyn[1:nrow(countrydat)]))
  
  plots[["nn"]] <- plot_nn(countrydat, modeldat, cols, labs)
  
  plots[["nny"]] <- plot_nny(countrydat, modeldat, cols, labs)
  
  return(plots)
}

grigorDates <- c("2020-04-26", "2020-06-09")
datebounds <- list(
  "Italy"         = c("2021-01-02", "2021-02-16"),
  "United States" = c("2021-01-06", "2021-02-16"), 
  "Ireland"       = c("2021-01-12", "2021-02-16"),
  "Germany"       = c("2021-01-06", "2021-02-16") 
  #"Netherlands"   = c("2021-01-06", "2021-02-16"), 
  #"Spain"         = c("2021-01-06", "2021-02-16"), 
  #"UK"            = c("2021-01-06", "2021-02-16")
)

owiddat    <- owiddat[!is.na(owiddat$new_cases),]
totaldates <- owiddat$date
totaldat   <- aggregate(owiddat$new_cases, by=list(totaldates), sum)
colnames(totaldat) <- c("Date", "Cases")
latest_date <- totaldat$Date[nrow(totaldat)]
wt_title <- sprintf('Global Total =%s as at %s', 
                    format(sum(totaldat$Cases), big.mark=",", scientific=FALSE), 
                    format.Date(latest_date, "%B %d, %Y"))

plotslist[["WorldTotal"]][["xn"]] <- plot_worldtotal(totaldat)

for(country in names(datebounds)){
  plotslist[[country]] <- covidPlots(country, datebounds[[country]], owiddat)
}

multidates <- list(
  "Italy"         = list(c("2020-12-16", "2021-01-01"),
                   c("2021-01-02", "2021-01-30")),
  "United States" = list(c("2020-12-16", "2021-01-08"),
                   c("2021-01-09", "2021-01-30")),
  "Ireland"       = list(c("2020-12-16", "2021-01-07"),
                   c("2021-01-08", "2021-01-30"))
)

multiphasePlots <- function(country, dates, data){
  plots <- list()
  crows <- grep(country, data$location)
  countrydat <- data.frame(date = data$date[crows], 
                           xn = data$new_cases[crows], yn = data$total_cases[crows])
  if(nrow(countrydat[countrydat$date < dates[[1]][1],]) == 0)
    beforecumcases <- 0
  else
    beforecumcases <- sum(countrydat$xn[countrydat$date < dates[[1]][1]])

  countrydat  <- countrydat[countrydat$date >= dates[[1]][1],]
  countrydat  <- countrydat[countrydat$date <= dates[[length(dates)]][2],]
  latest_date <- countrydat$date[nrow(countrydat)]
  
  forecastlen <- 5
  
  multimodx <- function(x, multix, pars, oldp = rep(1,10), start=FALSE, len = 0){
    q <- floor(pars[1])
    a <- pars[2]
    b <- pars[3]
    fitstd <- length(multix)+1
    
    if(start){
      multix[fitstd:(fitstd+q-1)] <- x[1:q]
      for(i in (fitstd+q):(fitstd+length(x)+len-1)){
        multix[i] <- (1-b)*multix[i-1] + a*multix[i-q]
      }
    } else {
      for(i in (fitstd):(fitstd+length(x)+len-1)){
        multix[i] <- (1-b)*multix[i-1] + a*multix[i-q]
      }
      #multix[fitstd] <- b/oldp[[3]]*((1-oldp[[3]])*multix[fitstd-1] + oldp[[2]]*multix[fitstd-q])
      #for(i in (fitstd+1):(fitstd+q-1)){
      #  multix[i] <- (1-b)*multix[i-1] + b/oldp[[3]]*oldp[[2]]*multix[i-q]
      #}
      #for(i in (fitstd+q):(fitstd+length(x)+len-1)){
      #  multix[i] <- (1-b)*multix[i-1] + a*multix[i-q]
      #}
    }
    return(multix)
  }
  
  multimodxper <- function(par, q=7, x, multix, oldp = rep(1,10), start=FALSE, len = 0){
    #a,b,c1,c2,p1,p2,n1,n2
    #first day of this phase
    fitstd <- length(multix)+1
    an  <- par[1]*(1+par[3]*sin(2*pi*(1:(fitstd+length(x)+len-1) - par[7])/par[5]))
    bn  <- par[2]*(1+par[4]*sin(2*pi*(1:(fitstd+length(x)+len-1) - par[8])/par[6]))
    
    if(start){
      multix[fitstd:(fitstd+q-1)] <- x[1:q]
      for(i in (fitstd+q):(fitstd+length(x)+len-1)){
        multix[i] <- (bn[i]*(1-bn[i-1]))*multix[i-1]/bn[i-1] +
          (an[i-q]*bn[i])*multix[i-q]/bn[i-q]
      }
    } else {
      for(i in fitstd:(fitstd+length(x)+len-1)){
        multix[i] <- (bn[i]*(1-bn[i-1]))*multix[i-1]/bn[i-1] +
          (an[i-q]*bn[i])*multix[i-q]/bn[i-q]
      }
    }
    return(multix)
  }
  
  #Specific dates
  multimodel  <- c()
  multimodelp <- c()
  phasepars   <- list()
  for(i in 1:length(dates)){
    phase    <- dates[[i]]
    phasedat <- countrydat[countrydat$date >= phase[1] & countrydat$date <= phase[2],]
    
    #get basic model for each phase first in order to get 
    # starting a and b to guess for periodic an and bn
    
    aseq    <- seq(from = 0.1, to = 2.5, length.out = 50)
    bseq    <- seq(from = 0.1, to = 0.9, length.out = 50)
    qseq    <- 6:8
    normdat <- expand.grid(q = qseq, a = aseq, b = bseq)   
    
    if(i == 1)
      abnorm <- apply(normdat, 1, function(x) modnorm(multimodx(phasedat$xn, multimodel, x, start=TRUE), phasedat$xn))
    else
      abnorm <- apply(normdat, 1, function(x) modnorm(multimodx(phasedat$xn, multimodel, x, oldp = phasepars[[i-1]])[length(multimodel) + 1:nrow(phasedat)], phasedat$xn))
    
    normalize <- function(x){
      return((x-min(x))/(max(x)-min(x)))
    }
    normdat$abnorm  <- abnorm
    #newnormdat      <- normdat %>% top_n(abnorm,n = -0.1*nrow(.))
    
    #if(i == 1)
    #  abnormy <- apply(newnormdat, 1, function(x) modnorm(beforecumcases + xntoyn(multimodx(phasedat$xn, multimodel, x[1:3], start=TRUE)), phasedat$yn))
    #else
    #  abnormy <- apply(newnormdat, 1, function(x) modnorm(beforecumcases + xntoyn(multimodx(phasedat$xn, multimodel, x[1:3], oldp = phasepars[[i-1]]))[length(multimodel)+1:nrow(phasedat)], phasedat$yn))
    
    #newnormdat$abnormy  <- normalize(abnormy)
    #newnormdat$combnorm <- newnormdat$abnorm + newnormdat$abnormy
    tileoptim <- normdat[which.min(normdat$abnorm),1:3]
    #tileoptim <- newnormdat[which.min(newnormdat$combnorm),1:3]
    optimpars <- c(tileoptim$q, tileoptim$a, tileoptim$b)
    
    phasepars[[i]] <- round(optimpars,3)
    
    if(i == 1 & i != length(dates))
      multimodel <- multimodx(phasedat$xn, multimodel, optimpars, start=TRUE)
    if(i == 1 & i == length(dates))
      multimodel <- multimodx(phasedat$xn, multimodel, optimpars, start=TRUE, len=forecastlen)
    if(i > 1 & i == length(dates))
      multimodel <- multimodx(phasedat$xn, multimodel, optimpars, oldp = phasepars[[i-1]], len=forecastlen)
    if(length(dates) > 2 & i %in% 2:(length(dates)-1))
      multimodel <- multimodx(phasedat$xn, multimodel, optimpars, oldp = phasepars[[i-1]])

    aseqper <- optimpars[2]*seq(from = 0.7, to = 1.3, length.out = 10)
    bseqper <- optimpars[3]*seq(from = 0.7, to = 1.3, length.out = 10)
    c_1seq  <- c_2seq <- seq(0.04, 0.2, length.out = 10)
    n_1seq  <- n_2seq <- c(1,7)
    p_1seq  <- p_2seq <- 6:7
    normdatp <- expand.grid(a  = aseqper, b  = bseqper,
                            c1 = c_1seq,  c2 = c_2seq,
                            p1 = p_1seq,  p2 = p_2seq,
                            n1 = n_1seq,  n2 = n_2seq)
    

    if(i == 1)
      pernorm <- apply(normdatp, 1, function(par) modnorm(multimodxper(par, q = optimpars[1], phasedat$xn, multimodelp, start=TRUE), phasedat$xn))
    else
      pernorm <- apply(normdatp, 1, function(par) modnorm(multimodxper(par, q = optimpars[1], phasedat$xn, multimodelp)[length(multimodelp)+1:nrow(phasedat)], phasedat$xn))
    
    normdatp$pernorm <- normalize(pernorm)
    
    newnormdatp <- normdatp %>% top_n(pernorm,n = -0.05*nrow(.))
    
    if(i == 1)
      pernormy <- apply(newnormdatp, 1, function(par) modnorm(beforecumcases+xntoyn(multimodxper(par, q = optimpars[1], phasedat$xn, multimodelp, start=TRUE)), phasedat$yn))
    else
      pernormy <- apply(newnormdatp, 1, function(par) modnorm(beforecumcases+xntoyn(multimodxper(par, q = optimpars[1], phasedat$xn, multimodelp))[length(multimodelp)+1:nrow(phasedat)], phasedat$yn))
    
    newnormdatp$pernormy <- normalize(pernormy)
    newnormdatp$combnorm <- newnormdatp$pernorm + newnormdatp$pernormy

    peroptim <- as.numeric(newnormdatp[which.min(newnormdatp$combnorm),1:8])
    
    phasepars[[i]] <- c(phasepars[[i]],round(peroptim,3))
    
    if(i == 1 & i != length(dates))
      multimodelp <- multimodxper(peroptim, q = optimpars[1], phasedat$xn, multimodelp, start=TRUE)
    if(i == 1 & i == length(dates))
      multimodelp <- multimodxper(peroptim, q = optimpars[1], phasedat$xn, multimodelp, start=TRUE, len=forecastlen)
    if(i > 1 & i == length(dates))
      multimodelp <- multimodxper(peroptim, q = optimpars[1], phasedat$xn, multimodelp, len=forecastlen)
    if(length(dates) > 2 & i %in% 2:(length(dates)-1))
      multimodelp <- multimodxper(peroptim, q = optimpars[1], phasedat$xn, multimodelp)
  }
  
  cols <- list(
    xn       = wes_palettes$Zissou1[1],
    yn       = wes_palettes$Darjeeling2[2],
    multixn  = wes_palettes$Darjeeling1[1],
    multiyn  = wes_palettes$Darjeeling1[1],
    multip   = "magenta4",
    x3       = wes_palettes$FantasticFox1[2]
  )
  
  labs <- list(
    xn = list(bquote(.(country)*","~x[n]*"*=new cases/day, actual till"~.(format.Date(latest_date,"%d.%m.%Y")))),
    yn = list(bquote(.(country)*","~y[n]*"*=cumulative cases, actual till"~.(format.Date(latest_date,"%d.%m.%Y"))))
  )
  
  multimodnormval  <- modnorm(multimodel[1:length(countrydat$xn)], countrydat$xn)
  multimodnormyval <- modnorm(beforecumcases+xntoyn(multimodel[1:length(countrydat$xn)]), countrydat$yn)
  
  b.roman <- function(x){ return(paste0("(", as.roman(x), ")"))}

  multilabd <- c()
  for(i in 1:length(dates)){
    multilabd <- c(multilabd, paste(b.roman(i), "from", format.Date(as.Date(as.character(dates[[i]][1])),"%d.%m")))
  }
  multilabd <- paste0(multilabd, collapse = "; ")
  
  multilabp <- c()
  for(i in 1:length(dates)){
    multilabp <- c(multilabp, paste0(b.roman(i), " a=", phasepars[[i]][[2]], ", b=", phasepars[[i]][[3]]))
  }
  multilabp <- paste0(multilabp, paste0("; q=", phasepars[[i]][[1]]), collapse = "; ")
  
  labs$multixn <- list(bquote("base"~.(length(dates))*"-phase model"~x[n]*"=new cases/day: "*
                               .(multilabd)*
                               "; ||x*-x||="*.(multimodnormval)*"  "*
                               .(multilabp)))
  labs$multiyn <- list(bquote("base"~.(length(dates))*"-phase model"~y[n]*"=cumulative cases: "*
                               .(multilabd)*
                               "; ||y*-y||="*.(multimodnormyval)*"  "*
                               .(multilabp)))
  
  mavgx1 <- movingavg(countrydat$xn[!is.na(countrydat$xn)])
  
  countrydat$mavgx3 <- movingavg(mavgx1)
  
  x3norm  <- modnorm(countrydat$xn,countrydat$mavgx3)
  labs$x3 <- list(bquote("moving average x*(3); ||x*(3)-x*||="*.(x3norm)))
  
  modeldat <- data.frame(date     = c(countrydat$date,as.Date(latest_date) + 1:forecastlen),
                         multixn  = multimodel,
                         multiyn  = beforecumcases + xntoyn(multimodel),
                         multipxn = multimodelp,
                         multipyn = beforecumcases + xntoyn(multimodelp))
  
  plots[["xn"]] <- plot_multixn(countrydat, modeldat, cols, labs)
  
  plots[["yn"]] <- plot_multiyn(countrydat, modeldat, cols, labs)
  
  multilabp <- c()
  for(i in 1:length(dates)){
    multilabp <- c(multilabp,  paste0(b.roman(i),  " a=", phasepars[[i]][[2]], ", b=", phasepars[[i]][[3]]))
  }
  multilabp <- paste0(multilabp, paste0("; q=", phasepars[[i]][[1]]), collapse = "; ")
  
  labs$multipxn <- list(bquote("periodic"~.(length(dates))*"-phase model"~x[n]*"=new cases/day: "*
                                .(multilabd)*
                                "; ||x*-x||="*.(multimodnormval)*"  "*
                                .(multilabp)))
  labs$multipyn <- list(bquote("periodic"~.(length(dates))*"-phase model"~y[n]*"=cumulative cases: "*
                                .(multilabd)*
                                "; ||y*-y||="*.(multimodnormyval)*"  "*
                                .(multilabp)))
  
  plots[["perxn"]] <- plot_multiperxn(countrydat, modeldat, cols, labs)
  
  plots[["peryn"]] <- plot_multiperyn(countrydat, modeldat, cols, labs)

  return(plots)
}

multilist <- list()

for(country in names(multidates)){
  multilist[[country]] <- multiphasePlots(country, multidates[[country]], owiddat)
}

