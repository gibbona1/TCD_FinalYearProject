require(ggplot2)
require(forecast)
require(dplyr)
require(wesanderson)
require(gridExtra)
require(xtable)

owiddat <- read.csv("Data/owid-covid-data.csv")
owiddat$date <- as.Date(owiddat$date, tryFormats = c("%Y-%m-%d"))

plotslist <- list()

source("Code/covid-plotutils.R")
source("Code/covid-modelutils.R")

covidPlots <- function(country, dateBounds, data){
  plots <- list()
  dateBounds <- as.Date(dateBounds)
  countryrows <- grep(country, data$location)
  countrydat <- data.frame(date = data$date[countryrows], 
                           xn   = data$new_cases[countryrows],
                           yn   = data$total_cases[countryrows])

  forecastlen <- 14
  
  prevcases <- countrydat$yn[countrydat$date == as.Date(dateBounds[1])-1]
  #Specific dates
  countrydattest  <- countrydat[countrydat$date >= dateBounds[1] & countrydat$date <= (dateBounds[2] + forecastlen),]
  countrydat <- countrydat[countrydat$date >= dateBounds[1] & countrydat$date <= dateBounds[2],]
  latest_date <- dateBounds[2]
  testBounds <- dateBounds[2] + c(1:forecastlen)
  
  countrydesc <- paste0(country, ", ", dateBounds[1], " to ", dateBounds[2])
  
  cols <- list(
    xn       = wes_palettes$Zissou1[1],
    xntest   = wes_palettes$Darjeeling1[2],
    yn       = wes_palettes$Darjeeling2[2],
    yntest   = wes_palettes$BottleRocket2[4],
    basexn   = wes_palettes$Darjeeling1[1],
    baseyn   = wes_palettes$Darjeeling1[1],
    x3       = wes_palettes$BottleRocket2[1],
    Crn      = wes_palettes$FantasticFox1[2],
    arima    = wes_palettes$Darjeeling1[4],
    arimapi  = wes_palettes$Darjeeling1[3],
    hw       = wes_palettes$Moonrise1[2],
    hwpi     = wes_palettes$Moonrise1[1],
    periodic = wes_palettes$GrandBudapest1[3], #"magenta",
    nn       = wes_palettes$FantasticFox1[4],
    nnpi     = wes_palettes$GrandBudapest1[4]
  )
  
  labs <- list(
    train    = "Training set",
    test     = "Test set",
    base     = "Base",
    x3       = "Average x*(3)",
    periodic = "Periodic",
    hw       = "HoltWinters",
    hwpi     = "HW 95% PI",
    arimapi  = "ARIMA 95% PI",
    nnpi     = "NNAR 95% PI"
  )
  
  #Basic model: need a,b,q,r using ||x-x*|| and ||y-y*||
  ##q - Any infected person becomes ill and infectious on the q-th day after infection.
  ##a - During each day, each ill person at large infects on average a other persons.
  ##b - During each day, a fraction b of ill people at large gets isolated 
  
  aseq    <- seq(from = 0.1, to = 1.5, length.out = 40)
  bseq    <- seq(from = 0.1, to = 0.9, length.out = 40)
  qseq    <- 6:8
  normdat <- expand.grid(q = qseq, a = aseq, b = bseq)
  abnorm  <- apply(normdat, 1, function(x) norm(x, countrydat$xn))

  normdat$abnorm  <- abnorm
  normdat$abnormn <- normalize(abnorm)
  
  newnormdat <- normdat %>% top_n(abnorm, n = -0.07*nrow(.))
  
  abnormy  <- apply(newnormdat, 1, function(x) normy(x, countrydat$xn, countrydat$yn))
  newnormdat$abnormyn <- normalize(abnormy)
  
  newnormdat$combnorm <- 1*newnormdat$abnormn + 0.00*newnormdat$abnormyn
  
  col_grad <- wes_palette("Zissou1", 20, type = "continuous")

  #newnormdat$abnormy <- apply(newnormdat, 1, function(x) normy(x, countrydat$xn, countrydat$yn))
  
  tileoptim <- newnormdat[which.min(newnormdat$abnorm),1:3]
  optimpars <- c(tileoptim$q, tileoptim$a, tileoptim$b)
  plots[["combnorm"]] <- ggplot(newnormdat, aes(x = a, y = b, z = abnorm)) +
    geom_contour_filled() + labs(fill = "||x-x*||") +
    scale_fill_brewer(palette = "Spectral")
  
  q <- optimpars[1]
  a <- optimpars[2]
  b <- optimpars[3]

  basexn   <- basicmodx(countrydat$xn, optimpars, len = forecastlen)
  
  modeldat <- data.frame(date = c(countrydat$date,latest_date + 1:forecastlen),
                         basexn = basexn, baseyn = xntoyn(basexn) + prevcases,
                         xn = countrydattest$xn, yn = countrydattest$yn)
  
  modeldat$tgroup <-  ifelse(modeldat$date <= dateBounds[2], "train", "test")
  
  #Newtons method, r_1 = r_0 - f(r_0)/f'(r_0)
  base_r_zero <- (a/b)^(1/(2*q))
  base_r_one  <- base_r_zero -basef(base_r_zero,optimpars)/basefprime(base_r_zero,optimpars)
  base_r_one  <- round(base_r_one,3)
  
  trainrange  <- 1:nrow(countrydat)
  testrange   <- nrow(countrydat) + 1:forecastlen
  
  roptimpars  <- round(optimpars,3)
  xnorm       <- norm(optimpars,countrydat$xn)
  xnormtest   <- modnorm(basexn[testrange], countrydattest$xn[testrange])

  ynorm       <- modnorm(countrydat$yn, modeldat$baseyn[1:length(countrydat$yn)])
  ynormtest   <- modnorm(modeldat$baseyn[testrange], countrydattest$yn[testrange])

  plots[["basexn"]] <- plot_basexn(modeldat, cols, labs)
  plots[["baseyn"]] <- plot_baseyn(modeldat, cols, labs)
  
  summarydf <- data.frame(model = "Basic Recursion", xnorm = xnorm, ynorm = ynorm,
                          xnormt = xnormtest, ynormt = ynormtest)

  optimC <- optim(par = countrydat$xn[1], normC, method = "Brent",
                  lower = 1, upper = 2*max(countrydat$xn[!is.na(countrydat$xn)]),
                  x = basexn[trainrange], r = base_r_one)$par
  
  modeldat$Crn <- optimC*base_r_one^(1:nrow(modeldat))
  
  labs$Crn <- list(bquote(Cr^n*", r="*.(base_r_one)*", C="*.(floor(optimC))))
  
  plots[["Crn"]] <- plot_crn(modeldat, cols, labs)

  mavgx1 <- movingavg(modeldat$xn[!is.na(modeldat$xn)])

  modeldat$mavgx3  <- movingavg(mavgx1)
  modeldat$mavgx3y <- xntoyn(modeldat$mavgx3) + prevcases
  
  x3norm   <- modnorm(modeldat$xn[trainrange], modeldat$mavgx3[trainrange])
  x3normt  <- modnorm(modeldat$xn[testrange], modeldat$mavgx3[testrange])
  x3normy  <- modnorm(modeldat$yn[trainrange], modeldat$mavgx3y[trainrange])
  x3normyt <- modnorm(modeldat$yn[testrange], modeldat$mavgx3y[testrange])
  
  plots[["mavgx3"]] <- plot_mavgx3(modeldat, cols, labs)
  
  summarydf <- rbind(summarydf, data.frame(model = "Moving Average", xnorm = x3norm, ynorm = x3normy,
                                           xnormt = x3normt, ynormt = x3normyt))
  
  #parameters of the form (ci,pi,ni)
  ##an = a(1 + c1 sin(2pi/p1 (n - n1)))
  c_1seq  <- c_2seq <- seq(0.04, 0.2, length.out = 17)
  n_1seq  <- n_2seq <- 1:q
  p_1seq  <- p_2seq <- 1:q
  normdatp <- expand.grid(a  = a,       b  = b,
                          c1 = c_1seq,  c2 = c_2seq,
                          p1 = p_1seq,  p2 = p_2seq,
                          n1 = n_1seq,  n2 = n_2seq)
  
  pernorm <- apply(normdatp, 1, function(x) normper(x, q = q, countrydat$xn))
  normdatp$pernorm <- pernorm
  
  peroptim <- normdatp[which.min(pernorm),1:8]
  pernorm  <- normdatp[which.min(pernorm),9]
  
  modeldat$periodic  <- modxper(as.numeric(peroptim),countrydat$xn, q = q, forecastlen)
  modeldat$periodicy <- xntoyn(modeldat$periodic) + prevcases
  
  pernormy  <- modnorm(countrydat$yn, modeldat$periodicy[trainrange])
  pernormt  <- modnorm(modeldat$xn[testrange], modeldat$periodic[testrange])
  pernormyt <- modnorm(modeldat$yn[testrange], modeldat$periodicy[testrange])
  
  peroptim <- round(as.numeric(peroptim),3)
  
  perparamdat <- data.frame(
    x  = modeldat$date,
    an = peroptim[1]*(1+peroptim[3]*sin(2*pi*(1:(nrow(modeldat)) - peroptim[7])/peroptim[5])),
    bn = peroptim[2]*(1+peroptim[4]*sin(2*pi*(1:(nrow(modeldat)) - peroptim[8])/peroptim[6]))
  )
  
  plots[["perparam"]] <- ggplot(perparamdat) +
    geom_line(aes(x = x, y = an, col = "a_n")) +
    geom_line(aes(x = x, y = bn, col = "b_n")) +
    geom_hline(aes(yintercept = peroptim[1], col = "a"), linetype="dashed") +
    geom_hline(aes(yintercept = peroptim[2], col = "b"), linetype="dashed") +
    xlab("date") + ylab("") +
    scale_color_manual(values = wes_palettes$Rushmore1[c(3,3,5,5)]) +
    guides(colour = guide_legend(override.aes = list(linetype = 
         c("a"="dashed", "a_n"="solid", "b"="dashed", "b_n"="solid")))) +
    xntheme() + theme(legend.position = "right")
  
  #a,b,c1,c2,p1,p2,n1,n2
  
  mathperamdat <- data.frame(a = peroptim[1], b  = peroptim[2], q  = q, r = base_r_one,
                            c1 = peroptim[3], p1 = peroptim[5], n1 = peroptim[7],
                            c2 = peroptim[4], p2 = peroptim[6], n2 = peroptim[8])
  
  colnames(mathperamdat) <- c("$a$", "$b$", "$q$", "$r$", 
                              "$c_1$", "$p_1$", "$n_1$", "$c_2$", "$p_2$", "$n_2$")
  
  plots[["periodic"]]  <- plot_periodic(modeldat, cols, labs)
  plots[["periodicy"]] <- plot_periodicy(modeldat, cols, labs)
  
  summarydf <- rbind(summarydf, data.frame(model = "Periodic", xnorm = pernorm, ynorm = pernormy,
                                           xnormt = pernormt, ynormt = pernormyt))
  
  #Statistical methods using timeseries forecasting 
  
  dat_ts <- ts(data = countrydat$xn, frequency = q)
  
  g1 <- autoplot(dat_ts) + theme(axis.title = element_blank())
  g2 <- ggAcf(dat_ts)    + ggtitle("")
  g3 <- ggPacf(dat_ts)   + ggtitle("")
  
  plots[["tsdisplay"]] <- grid.arrange(grobs = list(g1,g2,g3), layout_matrix = rbind(c(1, 1), c(2, 3)))
  
  plots[["residuals"]] <- gghistogram(log(dat_ts), add.normal = TRUE, bins=10)
  
  plots[["tsdecompose"]] <- autoplot(decompose(dat_ts))
  
  if(any(countrydat$xn <= 0)){
    plots[["hw"]] <- plots[["hwy"]] <- error_plot()
  } else{
    hwmethod <- "additive"
    #lambda=0 ensures values stay positive
    hwfcst   <- forecast::hw(dat_ts, h = forecastlen, seasonal = hwmethod, lambda = 0)
    
    hwfcst$fitted[1:q] <- countrydat$xn[1:q]
    modeldat$hwxn <- c(hwfcst$fitted, hwfcst$mean)
    modeldat$hwlo <- c(hwfcst$fitted, hwfcst$lower[,2])
    modeldat$hwhi <- c(hwfcst$fitted, hwfcst$upper[,2])
    
    modeldat$hwyn  <- xntoyn(modeldat$hwxn) + prevcases
    modeldat$hwylo <- xntoyn(modeldat$hwlo) + prevcases
    modeldat$hwyhi <- xntoyn(modeldat$hwhi) + prevcases
    
    hwnorm   <- modnorm(countrydat$xn, hwfcst$fitted)
    hwnormt  <- modnorm(modeldat$xn[testrange], hwfcst$mean)
    hwnormy  <- modnorm(countrydat$yn, modeldat$hwyn[trainrange])
    hwnormyt <- modnorm(modeldat$yn[testrange], modeldat$hwyn[testrange])
   
    plots[["hw"]]  <- plot_hw(modeldat, cols, labs)
    plots[["hwy"]] <- plot_hwy(modeldat, cols, labs)
  }
    
  auto.fit <- auto.arima(dat_ts, lambda = 0) #keep values positive
  
  getArmaModel <- function(arma, pdq = c(1,6,2), PDQ = c(3,7,4), s=5){
    return(paste0("ARIMA(", paste0(arma[pdq], collapse = ","), ")(",
                  paste0(arma[PDQ], collapse = ","), ")[", arma[s], "]"))
  }
  
  summarydf <- rbind(summarydf, data.frame(model = "HoltWinters", xnorm = hwnorm, ynorm = hwnormy,
                                           xnormt = hwnormt, ynormt = hwnormyt))
  
  arima.fcst <- forecast(auto.fit, level = c(80, 95), h = forecastlen)
  arima.fcst$fitted[1:q] <- countrydat$xn[1:q]
  
  arimanorm  <- modnorm(countrydat$xn,arima.fcst$fitted)
  arimanormt <- modnorm(modeldat$xn[testrange], arima.fcst$mean)
  
  arimalabs    <- getArmaModel(auto.fit$arma)
  labs$arima   <-  arimalabs
  
  modeldat$arimaxn <- c(auto.fit$fitted, arima.fcst$mean)
  modeldat$arimalo <- c(auto.fit$fitted,arima.fcst$lower[,2])
  modeldat$arimahi <- c(auto.fit$fitted,arima.fcst$upper[,2])
  
  modeldat$arimayn  <- xntoyn(modeldat$arimaxn) + prevcases
  modeldat$arimaylo <- xntoyn(modeldat$arimalo) + prevcases
  modeldat$arimayhi <- xntoyn(modeldat$arimahi) + prevcases

  arimanormy   <- modnorm(countrydat$yn,modeldat$arimayn[trainrange])
  arimanormyt  <- modnorm(modeldat$yn[testrange], modeldat$arimayn[testrange])
  
  plots[["arima"]]   <- plot_arima(modeldat, cols, labs)
  plots[["arimay"]]  <- plot_arimay(modeldat, cols, labs)
  plots[["hwarima"]] <- plot_hwarima(modeldat, cols, labs)
  
  summarydf <- rbind(summarydf, data.frame(model = "ARIMA", xnorm = arimanorm, ynorm = arimanormy,
                                           xnormt = arimanormt, ynormt = arimanormyt))
  
  nHidden <- max(1,floor(0.5*(1+auto.fit$arma[1]+auto.fit$arma[3])))
  #Box-Cox transformation with lambda=0 to ensure the forecasts stay positive.
  nnfit   <- nnetar(dat_ts, p = auto.fit$arma[1], P = auto.fit$arma[3], size = nHidden, lambda = 0, repeats = 20, maxit = 50) 
  nn.fcst <- forecast(nnfit, PI=TRUE, h = forecastlen)
  labs$nn <- nnfit$method

  nn.fcst$fitted[1:q] <- countrydat$xn[1:q]
 
  modeldat$nnxn <- c(nn.fcst$fitted, nn.fcst$mean)
  modeldat$nnlo <- c(nn.fcst$fitted, nn.fcst$lower[,2])
  modeldat$nnhi <- c(nn.fcst$fitted, nn.fcst$upper[,2])
  
  modeldat$nnyn  <- xntoyn(modeldat$nnxn) + prevcases
  modeldat$nnylo <- xntoyn(modeldat$nnlo) + prevcases
  modeldat$nnyhi <- xntoyn(modeldat$nnhi) + prevcases
  
  nnnorm      <- modnorm(countrydat$xn, nn.fcst$fitted)
  nnnormtest  <- modnorm(modeldat$xn[testrange], nn.fcst$mean)
  nnnormy     <- modnorm(countrydat$yn,modeldat$nnyn[trainrange])
  nnnormytest <- modnorm(modeldat$yn[testrange], modeldat$nnyn[testrange])
  
  plots[["nn"]]  <- plot_nn(modeldat, cols, labs)
  plots[["nny"]] <- plot_nny(modeldat, cols, labs)
  
  summarydf <- rbind(summarydf, data.frame(model = "Neural Network", xnorm = nnnorm, ynorm = nnnormy,
                                           xnormt = nnnormtest, ynormt = nnnormytest))
  summarydf$xnorm  <- as.integer(summarydf$xnorm)
  summarydf$ynorm  <- as.integer(summarydf$ynorm)
  summarydf$xnormt <- as.integer(summarydf$xnormt)
  summarydf$ynormt <- as.integer(summarydf$ynormt)
  
  colnames(summarydf) <- c("model", "$||x-x^*||_{train}$", "$||y-y^*||_{train}$",
                           "$||x-x^*||_{test}$", "$||y-y^*||_{test}$")
  return(list("plots" = plots, "summary" = summarydf, "desc" = countrydesc, 
              "dat" = modeldat, "mathmodelpars" = mathperamdat))
}

grigorDates <- c("2020-04-26", "2020-06-09")
datebounds <- list(
  "Italy"         = c("2021-01-02", "2021-02-16"),
  "United States" = c("2021-01-06", "2021-02-16"), 
  "Ireland"       = c("2021-01-12", "2021-02-16")
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
