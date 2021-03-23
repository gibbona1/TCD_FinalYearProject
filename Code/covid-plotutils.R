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
    geom_point(data = modeldat,aes(x = date, y = periodic, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = periodic, colour = "periodic")) +
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

plot_periodicy <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat) + 
    geom_point(aes(x = date, y = yn, colour = "blue")) + 
    geom_line(aes(x = date, y = yn, colour = "blue")) +
    geom_point(data = modeldat, aes(x = date, y = baseyn, colour = "base"), shape = 1) + 
    geom_line(data = modeldat, aes(x = date, y = baseyn, colour = "base")) +
    geom_point(data = modeldat,aes(x = date, y = periodicy, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = periodicy, colour = "periodic")) +
    gg_scale_xy + 
    guides(colour=guide_legend(ncol=1,nrow=3,byrow=TRUE)) +
    scale_colour_manual(values = c("base" = cols$baseyn, "blue" = cols$yn, "periodic" = cols$periodic), 
                        labels = c("base" = labs$baseyn, "blue" = labs$yn, "periodic" = labs$periodicy),
                        guide = guide_legend(override.aes = list(
                          shape = c("base" = 1, "blue"=16, "periodic"=16)))) +
    yntheme()
  return(p)
}

plot_hw <- function(countrydat, modeldat, cols, labs){
  p <- ggplot(countrydat, binwidth = 0) + 
    geom_bar(aes(x = date, y = xn, fill = "actual"), stat = "identity") +
    geom_ribbon(data = modeldat, aes(x = date, ymin = hwlo, ymax = hwhi, fill = "hw"), alpha = 0.5) +
    geom_point(data = modeldat, aes(x = date, y = hwxn, colour = "hw"), shape = 5) + 
    geom_line(data = modeldat, aes(x = date, y = hwxn, colour = "hw")) +
    geom_point(data = modeldat, aes(x = date, y = basexn, colour = "base")) + 
    geom_line(data = modeldat, aes(x = date, y = basexn, colour = "base")) +
    geom_point(data = modeldat, aes(x = date, y = periodic, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = periodic, colour = "periodic")) +
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
    geom_point(data = modeldat,aes(x = date, y = periodicy, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = periodicy, colour = "periodic")) +
    gg_scale_xy + 
    guides(colour=guide_legend(ncol=1,nrow=4,byrow=TRUE),
           fill=guide_legend(ncol=1,nrow=1,byrow=TRUE)) +
    scale_fill_manual(values = c("pi" = cols$hwpi), 
                      labels = c("pi" = labs$hwpi)) +
    scale_colour_manual(values = c("base" = cols$baseyn, "blue" = cols$yn, "hw" = cols$hw, "periodic" = cols$periodic), 
                        labels = c("base" = labs$baseyn, "blue" = labs$yn, "hw" = labs$hwy, "periodic" = labs$periodicy),
                        guide = guide_legend(override.aes = list(
                          shape = c("base" = 1, "blue"=16, "hw" = 5, "periodic"=16)))) +
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
    geom_point(data = modeldat,aes(x = date, y = periodic, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = periodic, colour = "periodic")) +
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
    geom_point(data = modeldat,aes(x = date, y = periodicy, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = periodicy, colour = "periodic")) +
    gg_scale_xy + 
    scale_fill_manual(values = c("pi" = cols$arimapi), 
                      labels = c("pi" = labs$arimapi)) +
    scale_colour_manual(values = c("arima" = cols$arima, "base" = cols$baseyn, "blue" = cols$yn, "periodic" = cols$periodic), 
                        labels = c("arima" = labs$arimay, "base" = labs$baseyn, "blue" = labs$yn, "periodic" = labs$periodicy),
                        guide = guide_legend(override.aes = list(
                          shape = c("arima" = 2, "base" = 1, "blue"=16, "periodic" = 16)))) +
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
    geom_point(data = modeldat,aes(x = date, y = periodic, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = periodic, colour = "periodic")) +
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
    geom_point(data = modeldat,aes(x = date, y = periodicy, colour = "periodic")) +
    geom_line(data = modeldat, aes(x = date, y = periodicy, colour = "periodic")) +
    gg_scale_xy + 
    scale_colour_manual(values = c("base" = cols$baseyn, "blue" = cols$yn, "nn" = cols$nn, "periodic" = cols$periodic), 
                        labels = c("base" = labs$baseyn, "blue" = labs$yn, "nn" = labs$nny, "periodic" = labs$periodicy),
                        guide = guide_legend(override.aes = list(
                          shape = c("base" = 1, "blue"=16, "nn" = 0, "periodic" = 16)))) +
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
    geom_bar(aes(x = date, y = new_cases),  fill = wes_palettes$Zissou1[1], stat = "identity") + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0,0))+
    scale_y_continuous(expand = c(0,0)) +
    ggtitle(wt_title) + xntheme()
  return(p)
}

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
             axis.line          = element_line(),
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

gg_scale_xy <- list(
  scale_x_date(date_breaks = "1 week", date_labels = "%d-%b", expand = c(0,0)),
  scale_y_continuous(expand = c(0,0)))

error_plot <- function(){
  p <- ggplot(data.frame(x = 0,y = 0)) + 
    geom_label(x = 0, y = 0, color = "red", size = 5 , fontface = "bold",
               label = "Error: Country data has nonpositive values") +
    xlim(-1,1) + ylim(-1,1) + 
    theme(axis.line  = element_blank(),
          axis.text  = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
  return(p)
}
