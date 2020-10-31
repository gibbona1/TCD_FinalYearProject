source("covidplots-group.r")

for(country in names(plotslist)){
  for(p in names(plotslist[[country]])){
    ggsave(filename = paste0(country, "-", gsub("*", "", p, fixed = TRUE), ".png"),
           plot = plotslist[[country]][[p]])
  }
}

