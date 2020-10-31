require(ggplot2)
require(forecast)
require(dplyr)
require(wesanderson)
webdat <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
# Change some country names to acronyms
webdat$countriesAndTerritories[webdat$countriesAndTerritories=="United_States_of_America"] <- "USA"
webdat$countriesAndTerritories[webdat$countriesAndTerritories=="United_Kingdom"] <- "UK"
webdat$countriesAndTerritories[webdat$countriesAndTerritories=="New_Zealand"] <- "NZ"

plotslist <- list()

plot_xn <- function(countrydat, cols, labs){
  p <- ggplot(countrydat, binwidth=0) + 
    geom_bar(aes(x = date, y = xn, fill = cols$xn), stat="identity") + 
    gg_scale_xy + 
    scale_fill_manual(values = cols$xn, name = "", labels = labs$xn) +
    xntheme()
  return(p)
}

plot_yn <- function(countrydat, cols, labs){
  p <- ggplot(countrydat) + 
    geom_line(aes(x = date, y = yn, colour = "blue")) + 
    geom_point(aes(x = date, y = yn, colour = "blue"))+
    gg_scale_xy + 
    scale_colour_manual(values = cols$yn, name = "", labels = labs$yn) +
    yntheme()
  return(p)
}

plot_basexn <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth=0) + 
    geom_bar(aes(x = date, y = xn, fill = cols$xn), stat="identity") + 
    geom_point(data = modeldat, aes(x = date, y = basexn, colour = cols$basexn)) + 
    geom_line(data = modeldat, aes(x = date, y = basexn, colour = cols$basexn)) +
    gg_scale_xy + 
    scale_fill_manual(  name = "leg",values = cols$xn, labels = labs$xn) +
    scale_colour_manual(name = "leg",values = cols$basexn, labels = labs$basexn) +
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
  p <- ggplot(countrydat, binwidth=0) +
    geom_bar(aes(x = date, y = xn, fill = cols$xn), stat="identity") + 
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
  p <- ggplot(countrydat, binwidth=0) + 
    geom_bar(aes(x = date, y = xn, fill = cols$xn), stat="identity") + 
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
  p <- ggplot(countrydat, binwidth=0) + 
    geom_bar(aes(x = date, y = xn, fill = cols$xn), stat="identity") + 
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
                        labels = c("base" = labs$basexn, "periodic" = labs$periodic, "x3" = labs$x3),
                        #guide = guide_legend(override.aes = list(
                        #shape = c("base" = 16, "periodic" = 16, "x3" = NA)))
                        ) +
    guides(colour = guide_legend(override.aes = list(shape = c("base" = 16, "periodic" = 16, "x3" = NA)))) +
    xntheme()
  return(p)
}

plot_hw <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth=0) + 
    geom_bar(aes(x = date, y = xn, fill = "actual"), stat="identity") + 
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
    geom_point(data = modeldat, aes(x = date, y = baseyn, colour = "base"), shape = 1) + 
    geom_line(data = modeldat, aes(x = date, y = baseyn, colour = "base")) +
    geom_point(data = modeldat, aes(x = date, y = hwyn, colour = "hw"), shape = 5) + 
    geom_line(data = modeldat, aes(x = date, y = hwyn, colour = "hw")) +
    geom_ribbon(data = modeldat, aes(x = date, ymin = hwylo, ymax = hwyhi, fill = "pi"), alpha = 0.5) +
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
  p <- ggplot(countrydat, binwidth=0) + 
    geom_bar(aes(x = date, y = xn, fill = "actual"), stat="identity") + 
    geom_ribbon(data = modeldat, aes(x = date, ymin = arimalo, ymax = arimahi, fill = "pi"), alpha = 0.5) +
    geom_point(data = modeldat, aes(x = date, y = basexn, colour = "base")) + 
    geom_line(data = modeldat, aes(x = date, y = basexn, colour = "base")) +
    geom_point(data = modeldat,aes(x = date, y = modxPeriodic, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = modxPeriodic, colour = "periodic")) +
    geom_point(data = modeldat, aes(x = date, y = arimaxn, colour = "arima")) + 
    geom_line(data = modeldat, aes(x = date, y = arimaxn, colour = "arima")) +
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
    geom_point(data = modeldat, aes(x = date, y = baseyn, colour = "base"), shape = 1) + 
    geom_line(data = modeldat, aes(x = date, y = baseyn, colour = "base")) +
    geom_point(data = modeldat, aes(x = date, y = arimayn, colour = "arima"), shape = 2) + 
    geom_line(data = modeldat, aes(x = date, y = arimayn, colour = "arima")) +
    geom_ribbon(data = modeldat, aes(x = date, ymin = arimaylo, ymax = arimayhi, fill = "pi"), alpha = 0.5) +
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

plot_nn <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth=0) + 
    geom_bar(aes(x = date, y = xn, fill = "actual"), stat="identity") + 
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

modnorm <- function(x,modx){
  return(floor(sum(abs(x-modx))/length(x)))
}

xntheme <- function(){
  p <- theme(axis.text.x        = element_text(angle = 90),
             axis.title         = element_blank(),
             axis.line          = element_line(),
             panel.background   = element_rect(fill  = "grey"),
             panel.grid         = element_line(colour = "darkgrey"),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             legend.title       = element_blank(),
             legend.margin = margin(0,4,0,4,"pt"),
             #legend.spacing.y   = unit(-0.4, "cm"),
             legend.background  = element_blank(),
             legend.text.align = 0,
             legend.box.background  = element_rect(linetype="solid", colour ="darkgrey", size = 0.1, fill = "white"),
             legend.spacing     = unit(0, "cm"),
             legend.key.size    = unit(0.8,"line"),
             #legend.key         = element_blank(),
             legend.text        = element_text(size = 6),
             legend.direction   = "vertical",
             legend.box         = "vertical",
             #legend.justification=c(0,1),
             legend.box.just    ='left',
             legend.position    = "top") #c(0.7, 0.8))
  return(p)
}

xntoyn <- function(xn){
  #yn <- c(0)
  yn <- cumsum(xn)
  #for(i in 1:(length(xn)-1)){
  #  yn[i+1] <- yn[i] + xn[i]
  #}
  return(yn)
}

xngrowth <- function(xn,yr){
  N <- length(xn)
  #diff_yr <- yr - lag(yr) # Difference in time (just in case there are gaps)
  diff_xn  <- xn - c(NA,xn[1:(N-1)]) # Difference in route between years
  return((diff_xn)/xn * 100)
}

gg_scale_xy <- list(
  scale_x_date(date_breaks = "5 day", date_labels = "%d-%b", expand = c(0,0)),
  scale_y_continuous(expand = c(0,0)))

gg_legend_ord <- function(){
  p <- guides(fill = guide_legend(order = 1),colour = guide_legend(order = 2))
}

yntheme <- function(){
  p <- theme(axis.text.x        = element_text(angle = 90),
             axis.title         = element_blank(),
             panel.background   = element_rect(fill  = "grey"),
             panel.grid         = element_line(colour = "darkgrey"),
             panel.grid.major.x = element_blank(),
             panel.grid.minor.x = element_blank(),
             legend.title       = element_blank(),
             legend.margin = margin(4,0,0,4,"pt"),
             legend.background = element_blank(),
             #legend.spacing.y   = unit(-0.4, "cm"),
             legend.key  = element_blank(),
             legend.box.background  = element_rect(linetype="solid", colour ="darkgrey", size = 0.1, fill = "white"),
             legend.spacing     = unit(0, "cm"),
             legend.key.size    = unit(0.8,"line"),
             legend.text        = element_text(size = 6),
             legend.direction   = "vertical",
             legend.box         = "vertical",
             #legend.justification=c(0,1),
             legend.box.just    ='left',
             legend.position    = "top")#c(0.7, 0.2))
  return(p)
}

norm <- function(par, x) {
  x <- x[!is.na(x)]
  q <- floor(par[1])
  a <- par[2]
  b <- par[3]
  modx <- x[1:q]
  for(i in (q+1):length(x)){
    modx[i] <- (1-b)*modx[i-1] + a*modx[i-q]
  }
  return(modnorm(x,modx))
}

normy <- function(par, x,y) {
  x <- x[!is.na(x)]
  q <- floor(par[1])
  a <- par[2]
  b <- par[3]
  modx <- x[1:q]
  for(i in (q+1):length(x)){
    modx[i] <- (1-b)*modx[i-1] + a*modx[i-q]
  }
  mody <- xntoyn(modx)
  return(modnorm(y,mody))
}

myoptim <- function(x,q = 7, abound = c(0.01,1.3), bbound = c(0.1,0.9)){
  mynorm <- function(q,a,b,x){
    modx <- x[1:q]
    for(ind in (q+1):length(x)){
      modx[ind] <- (1-b)*modx[ind-1] + a*modx[ind-q]
    }
    return(sum(abs(x-modx))/(length(x)+1))
  }
  N <- 1000 #500
  aseq <- seq(abound[1], abound[2], length.out= N)
  bseq <- seq(bbound[1], bbound[2], length.out= N)
  mymat <- sapply(1:N, function(ai) sapply(1:N, function(bj) mynorm(q,ai,bj,x)))
  
  return(mymat)
  #min_ind <- which(mymat == min(mymat), arr.ind = TRUE)
  #return(c("a" = aseq[min_ind[1]], "b" = bseq[min_ind[2]]))
}

basicmodx <- function(x, pars, len){
  q <- floor(pars[1])
  a <- pars[2]
  b <- pars[3]
  modx <- x[1:q]
  for(i in (q+1):len){
    modx[i] <- (1-b)*modx[i-1] + a*modx[i-q]
  }
  return(modx)
}

randmodx <- function(par, x){
  modx <- c()
  modx[1:(q+1)] <- x[1:(q+1)]
  a <- par
  sigma <- 1/3
  epsilon <- 0.2
  
}

movingavg <- function(x){
  mavgx <- (x[1]+x[2])/2
  N <- length(x)
  for(i in 2:(N-1)){
    mavgx[i] <- sum(x[(i-1):(i+1)])/3
  }
  mavgx[N] <- (x[N-1]+x[N])/2
  return(mavgx)
}

normPeriodic <- function(par, q, x){
  c_1 <- par[1]
  p_1 <- floor(par[2])
  n_1 <- floor(par[3])
  c_2 <- par[4]
  p_2 <- floor(par[5])
  n_2 <- floor(par[6])
  an  <- par[7]*(1+c_1*sin(2*pi*(1:length(x) - n_1)/p_1))
  bn  <- par[8]*(1+c_2*sin(2*pi*(1:length(x) - n_2)/p_2))
  modx <- x[1:q]
  for(i in (q+1):length(x)){
    modx[i] <- (bn[i]*(1-bn[i-1]))*modx[i-1]/bn[i-1] +
      (an[i-q]*bn[i])*modx[i-q]/bn[i-q]
  }
  return(floor(sum(abs(x-modx))/(length(x)+1)))
}

modxPeriodicfn <- function(par, q, x, len){
  c_1 <- par[1]
  p_1 <- floor(par[2])
  n_1 <- floor(par[3])
  c_2 <- par[4]
  p_2 <- floor(par[5])
  n_2 <- floor(par[6])
  an  <- par[7]*(1+c_1*sin(2*pi*(1:(length(x)+len) - n_1)/p_1))
  bn  <- par[8]*(1+c_2*sin(2*pi*(1:(length(x)+len) - n_2)/p_2))
  modx <- x[1:q]
  for(i in (q+1):(length(x)+len)){
    modx[i] <- (bn[i]*(1-bn[i-1]))*modx[i-1]/bn[i-1] +
      (an[i-q]*bn[i])*modx[i-q]/bn[i-q]
  }
  return(modx)
}

covidPlots <- function(country, dateBounds, data){
  plots <- list()
  countryrows <- grep(country, data$countriesAndTerritories)
  countrydat <- data.frame(date = as.Date(data$dateRep[countryrows], tryFormats = c("%d/%m/%Y")), 
                           xn   = data$cases[countryrows])
  countrydat <- countrydat[nrow(countrydat):1,]
  countrydat <- countrydat[which(countrydat$xn != 0)[1]:nrow(countrydat),]
  countrydat <- countrydat[countrydat$xn >= 0,]
  countrydatfull <- countrydat[countrydat$date <= dateBounds[2],]
  #Specific dates
  countrydat <- countrydat[countrydat$date >= dateBounds[1] & countrydat$date <= dateBounds[2],]
  countrydat$xn[countrydat$xn < 0] <- 0
  latest_date <- as.Date(countrydat$date[nrow(countrydat)], tryFormats = c("%d/%m/%Y"))
  
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
    xn = list(bquote(.(country)*","~x[n]*"*=new cases/day, actual till"~.(format(latest_date,"%d.%m.%Y")))),
    yn = list(bquote(.(country)*","~y[n]*"*=cumulative cases, actual till"~.(format(latest_date,"%d.%m.%Y"))))
  )
  
  plots[["xn*"]] <- plot_xn(countrydatfull, cols, labs)
  
  countrydat$yn <- xntoyn(countrydat$xn)
  countrydatfull$yn <- xntoyn(countrydatfull$xn)
  
  plots[["yn*"]] <- plot_yn(countrydatfull, cols, labs)
  
  #Basic model: need a,b,q,r using ||x-x*||
  ##q - Any infected person becomes ill and infectious on the q-th day after infection.
  ##a - During each day, each ill person at large infects on average a other persons.
  ##b - During each day, a fraction b of ill people at large gets isolated 
  
  forecastlen <- 14
  
  aseq <- seq(from = 0.1, to = 2.5, length.out = 80)
  bseq <- seq(from = 0.1, to = 0.9, length.out = 80)
  normdat <- expand.grid(a = aseq, b = bseq)
  abnorm  <- apply(normdat, 1, function(x) norm(c(7,x[1],x[2]), countrydat$xn))
  #abnormy <- apply(normdat, 1, function(x) norm(c(7,x[1],x[2]), countrydat$xn))
  
  normalize <- function(x){
    return((x-min(x))/(max(x)-min(x)))
  }
  normdat$abnorm  <- normalize(abnorm)
  #normdat$abnormy <- normalize(abnormy)
  #normdat$combnorm <- normdat$abnorm+normdat$abnormy
  newnormdat <- normdat %>% 
    top_n(abnorm
          ,n = -0.05*nrow(.))
  
  abnormy <- apply(newnormdat, 1, function(x) normy(c(7,x[1],x[2]), countrydat$xn, countrydat$yn))
  
  newnormdat$abnormy <- normalize(abnormy)
  newnormdat$combnorm <- newnormdat$abnorm + newnormdat$abnormy
  tileoptim <- newnormdat[which.min(newnormdat$combnorm),1:2]
  optimpars <- c(7,tileoptim$a, tileoptim$b)
  plots[["combnorm"]] <- ggplot(newnormdat, aes(x = a, y = b, fill = combnorm)) + 
    geom_tile() 
    #geom_point(x = 0.232, y = 0.325, col = "white")
  
  #tileoptim <- normdat[which.min(normdat$abnorm),1:2]
  #tileoptimy <- normdat[which.min(normdat$abnormy),1:2]
  #optimpars <- c(7,(tileoptim$a+tileoptimy$a)/2,(tileoptim$b+tileoptimy$b)/2)
  
  #tileoptim <- normdat[which.min(normdat$abnorm),1:2]
  #optimpars <- c(7,tileoptim$a, tileoptim$b)
  basexn <- basicmodx(countrydat$xn, optimpars, len = length(countrydat$xn)+forecastlen)
  
  #par is of form c(q,a,b)
  #optimpars <- optim(c(7,0.2,0.35), norm, x = countrydat$xn, method = "L-BFGS-B",
  #                   lower=c(5,0.1,0.1), upper=c(7,1.5,0.9))$par
  #optimpars[1] <- floor(optimpars[1])
  
  #forecastlen <- 20#40
  #optimpars[1] <- 7
  #basexn <- basicmodx(countrydat$xn, optimpars, len = length(countrydat$xn)+forecastlen)
  
  modeldat <- data.frame(date = c(countrydat$date,as.Date(latest_date) + 1:forecastlen),
                         basexn = basexn)
  
  basef <- function(lambda,par) {
    return(lambda^(par[1]+1)-(1-par[3])*lambda^(par[1])-par[2])
  }
  basefprime <- function(lambda,par) {
    return((par[1]+1)*lambda^(par[1])-(1-par[3])*par[1]*lambda^(par[1]-1))
  } 
  #Newtons method, r_1 = r_0 - f(r_0)/f'(r_0)
  base_r_zero <- (optimpars[2]/optimpars[3])^(1/(2*optimpars[1]))
  base_r_one <- base_r_zero -basef(base_r_zero,optimpars)/basefprime(base_r_zero,optimpars)
  base_r_one <- round(base_r_one,3)
  
  roptimpars <- round(optimpars,3)
  labs$basexn <- list(bquote("basic model, "~x[n]*"=new cases/day; a="*.(roptimpars[2])*", b="*.(roptimpars[3])*", q="*.(roptimpars[1])*";  r="*.(base_r_one)*"; ||x*-x||="*.(norm(optimpars,countrydat$xn))))
  
  plots[["basexn"]] <- plot_basexn(countrydat, modeldat, cols, labs)
  
  baseyn   <- xntoyn(modeldat$basexn)
  modeldat$baseyn <- baseyn
  
  ynnorm <- floor(sum(abs(countrydat$yn-baseyn[1:length(countrydat$yn)]))/(length(countrydat$yn)+1))
  labs$baseyn <- list(bquote("basic model, "~y[n]*"=cumulative cases; a="*.(roptimpars[2])*", b="*.(roptimpars[3])*", q="*.(roptimpars[1])*", ||y*-y||="*.(ynnorm)))

  plots[["baseyn"]] <- plot_baseyn(countrydat, modeldat, cols, labs)
  
  
  normC <- function(par,x,r){
    x <- x[!is.na(x)]
    modx <- par*r^(1:(length(x)))
    return(floor(sum(abs(x-modx))/(length(x)+1)))
  }
  optimC <- optim(par = countrydat$xn[1], normC, method = "Brent",
                     lower = 1, upper = 2*max(countrydat$xn[!is.na(countrydat$xn)]),
                     x = countrydat$xn, r = base_r_one)$par
  
  modeldat$Crn <- optimC*base_r_one^(1:nrow(modeldat))
  
  labs$Crn <- list(bquote(Cr^n*", r="*.(base_r_one)*", C="*.(floor(optimC))))
  
  plots[["Crn"]] <- plot_crn(countrydat, modeldat, cols, labs)

  mavgx1 <- movingavg(countrydat$xn[!is.na(countrydat$xn)])
  mavgx3 <- movingavg(mavgx1)
  
  countrydat$mavgx3 <- mavgx3
  
  x3norm <- floor(sum(abs(countrydat$xn-mavgx3)/(nrow(countrydat)+1)))
  labs$x3 <- list(bquote("moving average x*(3); ||x*(3)-x*||="*.(x3norm)))
  
  plots[["mavgx3"]] <- plot_mavgx3(countrydat, modeldat, cols, labs)
  
  #parameters of the form (ci,pi,ni)
  aseqper <- seq(from = optimpars[2]*0.8, to = optimpars[2]*1.2, length.out = 5)
  bseqper <- seq(from = optimpars[3]*0.8, to = optimpars[2]*1.2, length.out = 5)
  c_1seq  <- c_2seq <- seq(0.04, 0.2, length.out = 10)
  n_1seq  <- n_2seq <- 1#1:8
  p_1seq  <- p_2seq <- 6:7
  normdatp <- expand.grid(a  = aseqper,   b  = bseqper,
                          c1 = c_1seq, c2 = c_2seq,
                          p1 = p_1seq, p2 = p_2seq,
                          n1 = n_1seq, n2 = n_2seq)
  
  normper <- function(par, q, x){
    #a,b,c1,c2,p1,p2,n1,n2
    an  <- par[1]*(1+par[3]*sin(2*pi*(1:length(x) - par[7])/par[5]))
    bn  <- par[2]*(1+par[4]*sin(2*pi*(1:length(x) - par[8])/par[6]))
    modx <- x[1:q]
    for(i in (q+1):length(x)){
      modx[i] <- (bn[i]*(1-bn[i-1]))*modx[i-1]/bn[i-1] +
        (an[i-q]*bn[i])*modx[i-q]/bn[i-q]
    }
    return(floor(sum(abs(x-modx))/(length(x)+1)))
  }
  
  modxper <- function(par, q, x, len){
    #a,b,c1,c2,p1,p2,n1,n2
    an  <- par[1]*(1+par[3]*sin(2*pi*(1:(length(x)+len) - par[7])/par[5]))
    bn  <- par[2]*(1+par[4]*sin(2*pi*(1:(length(x)+len) - par[8])/par[6]))
    modx <- x[1:q]
    for(i in (q+1):(length(x)+len)){
      modx[i] <- (bn[i]*(1-bn[i-1]))*modx[i-1]/bn[i-1] +
        (an[i-q]*bn[i])*modx[i-q]/bn[i-q]
    }
    return(modx)
  }
  
  pernorm <- apply(normdatp, 1, function(x) normper(x, q = 7, countrydat$xn))
  normdatp$pernorm <- pernorm
  
  peroptim <- normdatp[which.min(normdatp$pernorm),1:8]
  optpernorm <- normdatp[which.min(normdatp$pernorm),9]
  modeldat$modxPeriodic <- modxper(as.numeric(peroptim),countrydat$xn, q = optimpars[1], forecastlen)
  
  peroptim <- round(as.numeric(peroptim),3)
  #a,b,c1,c2,p1,p2,n1,n2
  labs$periodic <- list(bquote("periodic model,"~x[n]*"=new cases/day;"
                              ~a*"="*.(peroptim[1])*","~ b*"="*.(peroptim[2])*","
                              ~q*"="*.(optimpars[1])*";"~"||x*-x||="*.(optpernorm)*";"
                              ~c[1]*"="*.(peroptim[3])*","~p[1]*"="*.(peroptim[5])*","
                              ~n[1]*"="*.(peroptim[7])*","~c[2]*"="*.(peroptim[4])*","
                              ~p[2]*"="*.(peroptim[6])*","~n[2]*"="*.(peroptim[8])))
  
  plots[["periodic"]] <- plot_periodic(countrydat, modeldat, cols, labs)
  
  dat_ts   <- ts(data = countrydat$xn, frequency = 7)
  
  if(any(countrydat$xn <= 0)){
    plots[["hw"]] <- ggplot(data.frame(x=0,y=0)) + 
      geom_label(x=0, y=0, label="Error: Country data has nonpositive values",
                 color="red", size=5 , fontface="bold" ) +
      xlim(-1,1) + ylim(-1,1) + 
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
  } else{
    hw.add   <- HoltWinters(dat_ts, seasonal = "additive")
    #hw.times <- HoltWinters(dat_ts, seasonal = "multiplicative")
    hwmethod <- "multiplicative"
    #hwmethod <- ifelse(hw.add$SSE < hw.times$SSE, "additive", "multiplicative")
    xnonlylast <- c(rep(NA, nrow(countrydat)-1), countrydat$xn[length(countrydat$xn)])
    hwfcst   <- forecast::hw(dat_ts, h = forecastlen, seasonal = hwmethod)
    hwfcst$lower[hwfcst$lower[,2] < 0,2] <- 0
    hwfcst$upper[hwfcst$upper[,2] < 0,2] <- 0
    hwfcst$mean[hwfcst$mean < 0] <- 0
    modeldat$hwxn <- c(hwfcst$fitted, hwfcst$mean)
    modeldat$hwlo <- c(hwfcst$fitted, hwfcst$lower[,2])
    modeldat$hwhi <- c(hwfcst$fitted, hwfcst$upper[,2])
    
    modeldat$hwyn  <- xntoyn(modeldat$hwxn)
    modeldat$hwylo <- xntoyn(modeldat$hwlo)
    modeldat$hwyhi <- xntoyn(modeldat$hwhi)
    
    hwnorm <- floor(sum(abs(countrydat$xn-hwfcst$fitted))/(length(countrydat$xn)+1))
    
    labs$hw  <- paste0("HoltWinters algorithm,  ||x*-x||=", modnorm(countrydat$xn,hwfcst$fitted))
    labs$hwy <- paste0("HoltWinters algorithm,  ||y*-y||=", modnorm(countrydat$yn,modeldat$hwyn[1:nrow(countrydat)]))
    labs$hwpi <- "HW 95% Prediction Interval"
    
    plots[["hw"]] <- plot_hw(countrydat, modeldat, cols, labs)
    
    plots[["hwy"]] <- plot_hwy(countrydat, modeldat, cols, labs)
  }
    
  auto.fit <- auto.arima(dat_ts)
  getArmaModel <- function(arma){
    return(paste0("ARIMA(", paste0(c(arma[1], arma[3], arma[2]), 
                                   collapse = ","), ")(",
                  paste0(c(arma[6], arma[4], arma[7]), collapse = ","), 
                  ")[", arma[5], "]"))
  }
  
  arima.fcst <- forecast(auto.fit, level = c(80, 95), h = forecastlen)
  arima.fcst$lower[arima.fcst$lower[,2] < 0,2] <- 0
  arima.fcst$upper[arima.fcst$upper[,2] < 0,2] <- 0
  arima.fcst$mean[arima.fcst$mean < 0] <- 0
  
  arimanorm <- modnorm(countrydat$xn,arima.fcst$fitted)
  
  arimalabs   <- getArmaModel(auto.fit$arma)
  labs$arima  <- paste0(arimalabs, ", ||x*-x||=", arimanorm)
  labs$arimapi  <- "ARIMA 95% Prediction Interval"
  
  modeldat$arimaxn <- c(auto.fit$fitted, arima.fcst$mean)
  modeldat$arimalo <- c(auto.fit$fitted,arima.fcst$lower[,2])
  modeldat$arimahi <- c(auto.fit$fitted,arima.fcst$upper[,2])
  
  modeldat$arimayn  <- xntoyn(modeldat$arimaxn)
  modeldat$arimaylo <- xntoyn(modeldat$arimalo)
  modeldat$arimayhi <- xntoyn(modeldat$arimahi)

  labs$arimay  <- paste0(arimalabs, ", ||y*-y||=", modnorm(countrydat$yn,modeldat$arimayn[1:nrow(countrydat)]))
  
  plots[["arima"]] <- plot_arima(countrydat, modeldat, cols, labs)
  
  plots[["arimay"]] <- plot_arimay(countrydat, modeldat, cols, labs)
  
  nnfit <- nnetar(dat_ts, lambda=0)
  nn.fcst <- forecast(nnfit, h=forecastlen)

  nn.fcst$mean[nn.fcst$mean < 0] <- 0
  nn.fcst$fitted[1:7] <- countrydat$xn[1:7]

 
  modeldat$nnxn <- c(nn.fcst$fitted, nn.fcst$mean)
  modeldat$nnyn <- xntoyn(modeldat$nnxn)
  
  labs$nn  <- paste0(nnfit$method, ", ||x*-x||=", modnorm(countrydat$xn,nn.fcst$fitted))
  labs$nny <- paste0(nnfit$method, ", ||y*-y||=", modnorm(countrydat$yn,modeldat$nnyn[1:nrow(countrydat)]))
  
  plots[["nn"]] <- plot_nn(countrydat, modeldat, cols, labs)
  
  plots[["nny"]] <- plot_nny(countrydat, modeldat, cols, labs)
  
  return(plots)
}

grigorDates <- c("2020-04-26", "2020-06-09")
datebounds <- list(
  "Italy"       = c("2020-09-21", "2020-10-30"),
  "USA"         = c("2020-09-21", "2020-10-30"), 
  "Ireland"     = c("2020-09-21", "2020-10-30")
  #"Germany"     = c("2020-09-21", "2020-10-30"), 
  #"Netherlands" = c("2020-09-21", "2020-10-27"), 
  #"Spain"       = c("2020-09-21", "2020-10-27"), 
  #"UK"          = c("2020-09-21", "2020-10-27")
)

totaldates <- as.Date(webdat$dateRep, tryFormats = c("%d/%m/%Y"))
totaldat <- aggregate(webdat$cases, by=list(totaldates), sum)
colnames(totaldat) <- c("Date", "Cases")
latest_date <- as.Date(totaldat$Date[nrow(totaldat)], tryFormats = c("%d/%m/%Y"))
plotslist[["WorldTotal"]][["xn*"]] <- ggplot(totaldat, binwidth=0) + 
  geom_bar(aes(x = Date, y = Cases), 
           fill = wes_palettes$Zissou1[1], stat="identity") + 
  gg_scale_xy + 
  labs(title = sprintf('Global Total =%s as at %s', 
                       format(sum(totaldat$Cases), big.mark=",", scientific=FALSE), 
                       format.Date(latest_date, "%B %d, %Y"))) +
  xntheme()

for(country in names(datebounds)){
  plotslist[[country]] <- covidPlots(country, 
                                     dateBounds = datebounds[[country]], 
                                     webdat)
}
