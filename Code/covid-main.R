require(ggplot2)
require(forecast)
require(dplyr)
require(wesanderson)
require(gridExtra)

owiddat <- read.csv("Data/owid-covid-data.csv")
owiddat$date <- as.Date(owiddat$date, tryFormats = c("%Y-%m-%d"))

plotslist <- list()

source("Code/covid-plotutils.R")
source("Code/covid-modelutils.R")

covidPlots <- function(country, dateBounds, data){
  plots <- list()
  countryrows <- grep(country, data$location)
  countrydat <- data.frame(date = data$date[countryrows], 
                           xn   = data$new_cases[countryrows],
                           yn   = data$total_cases[countryrows])
  countrydatfull <- countrydat[countrydat$date <= dateBounds[2],]
  
  prevcases <- countrydat$yn[countrydat$date == as.Date(dateBounds[1])-1]
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
    top_n(abnorm, n = -0.07*nrow(.))
  
  col_grad <- wes_palette("Zissou1", 20, type = "continuous")
  
  tileoptim <- normdat[which.min(normdat$abnorm),1:3]
  #tileoptim <- newnormdat[which.min(newnormdat$abnormy),1:3]
  optimpars <- c(tileoptim$q, tileoptim$a, tileoptim$b)
  plots[["combnorm"]] <- ggplot(newnormdat, aes(x = a, y = b, z = abnorm)) +
    geom_contour_filled() + labs(fill = "||x-x*||") +
    scale_fill_brewer(palette = "Spectral")
  
  q <- optimpars[1]
  a <- optimpars[2]
  b <- optimpars[3]

  basexn   <- basicmodx(countrydat$xn, optimpars, len = forecastlen)
  
  modeldat <- data.frame(date = c(countrydat$date,latest_date + 1:forecastlen),
                         basexn = basexn, baseyn = xntoyn(basexn) + prevcases)
  
  #Newtons method, r_1 = r_0 - f(r_0)/f'(r_0)
  base_r_zero <- (a/b)^(1/(2*q))
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
  ##an = a(1 + c1 sin(2pi/p1 (n - n1)))
  aseqper <- a*seq(from = 0.8, to = 1.2, length.out = 5)
  bseqper <- b*seq(from = 0.8, to = 1.2, length.out = 5)
  c_1seq  <- c_2seq <- seq(0.04, 0.2, length.out = 10)
  n_1seq  <- n_2seq <- c(1,7)
  p_1seq  <- p_2seq <- 6:7
  normdatp <- expand.grid(a  = aseqper, b  = bseqper,
                          c1 = c_1seq,  c2 = c_2seq,
                          p1 = p_1seq,  p2 = p_2seq,
                          n1 = n_1seq,  n2 = n_2seq)
  
  pernorm <- apply(normdatp, 1, function(x) normper(x, q = q, countrydat$xn))
  normdatp$pernorm <- pernorm
  
  peroptim   <- normdatp[which.min(pernorm),1:8]
  optpernorm <- normdatp[which.min(pernorm),9]
  
  modeldat$periodic  <- modxper(as.numeric(peroptim),countrydat$xn, q = q, forecastlen)
  modeldat$periodicy <- xntoyn(modeldat$periodic) + prevcases
  
  optpernormy <- modnorm(countrydat$yn, modeldat$periodicy[1:nrow(countrydat)])
  
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
                              ~q*"="*.(q)*";"~"||x*-x||="*.(optpernorm)*";"
                              ~c[1]*"="*.(peroptim[3])*","~p[1]*"="*.(peroptim[5])*","
                              ~n[1]*"="*.(peroptim[7])*","~c[2]*"="*.(peroptim[4])*","
                              ~p[2]*"="*.(peroptim[6])*","~n[2]*"="*.(peroptim[8])))
  
  labs$periodicy <- list(bquote("periodic model,"~y[n]*"=new cases/day;"
                               ~a*"="*.(peroptim[1])*","~ b*"="*.(peroptim[2])*","
                               ~q*"="*.(q)*";"~"||x*-x||="*.(optpernormy)*";"
                               ~c[1]*"="*.(peroptim[3])*","~p[1]*"="*.(peroptim[5])*","
                               ~n[1]*"="*.(peroptim[7])*","~c[2]*"="*.(peroptim[4])*","
                               ~p[2]*"="*.(peroptim[6])*","~n[2]*"="*.(peroptim[8])))
  
  plots[["periodic"]] <- plot_periodic(countrydat, modeldat, cols, labs)
  
  plots[["periodicy"]] <- plot_periodicy(countrydat, modeldat, cols, labs)
  
  #Statistical methods using timeseries forecasting 
  
  dat_ts <- ts(data = countrydat$xn, frequency = q)
  
  g1 <- autoplot(dat_ts) + theme(axis.title = element_blank())
  g2 <- ggAcf(dat_ts)    + ggtitle("")
  g3 <- ggPacf(dat_ts)   + ggtitle("")
  
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
    #lambda=0 ensures values stay positive
    hwfcst     <- forecast::hw(dat_ts, h = forecastlen, seasonal = hwmethod, lambda = 0)
    
    hwfcst$fitted[1:q] <- countrydat$xn[1:q]
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
  
  getArmaModel <- function(arma, pdq = c(1,6,2), PDQ = c(3,7,4), s=5){
    return(paste0("ARIMA(", paste0(arma[pdq], collapse = ","), ")(",
                  paste0(arma[PDQ], collapse = ","), ")[", arma[s], "]"))
  }
  
  arima.fcst <- forecast(auto.fit, level = c(80, 95), h = forecastlen)
  arima.fcst$fitted[1:q] <- countrydat$xn[1:q]
  
  arimanorm <- modnorm(countrydat$xn,arima.fcst$fitted)
  
  arimalabs    <- getArmaModel(auto.fit$arma)
  labs$arima   <- paste0(arimalabs, ", ||x*-x||=", arimanorm)
  labs$arimapi <- "ARIMA 95% Prediction Interval"
  
  modeldat$arimaxn <- c(auto.fit$fitted, arima.fcst$mean)
  modeldat$arimalo <- c(auto.fit$fitted,arima.fcst$lower[,2])
  modeldat$arimahi <- c(auto.fit$fitted,arima.fcst$upper[,2])
  
  modeldat$arimaxn[1:q] <- countrydat$xn[1:q]
  modeldat$arimalo[1:q] <- countrydat$xn[1:q]
  modeldat$arimahi[1:q] <- countrydat$xn[1:q]
  
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
  nn.fcst$fitted[1:q] <- countrydat$xn[1:q]
 
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

owiddat     <- owiddat[!is.na(owiddat$new_cases),]
totaldat    <- owiddat[owiddat$location == "World",]
latest_date <- totaldat$date[nrow(totaldat)]
wt_title <- sprintf('Global Total =%s as at %s', 
                    format(sum(totaldat$new_cases), big.mark=",", scientific=FALSE), 
                    format.Date(latest_date, "%B %d, %Y"))

plotslist[["WorldTotal"]][["xn"]] <- plot_worldtotal(totaldat)

for(country in names(datebounds)){
  plotslist[[country]] <- covidPlots(country, datebounds[[country]], owiddat)
}