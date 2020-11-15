source("covidplots-group.r")

for(country in names(plotslist)){
  for(p in names(plotslist[[country]])){
    ggsave(filename = paste0(country, "-", gsub("*", "", p, fixed = TRUE), ".png"),
           plot = plotslist[[country]][[p]],
           path = "Plots",
           height = 10,
           width  = 14,
           units  = "cm"
           )
  }
}

for(country in names(multilist)){
  for(p in names(multilist[[country]])){
    ggsave(filename = paste0(country, "-", p, "mult.png"),
           plot = multilist[[country]][[p]],
           path = "Plots",
           height = 10,
           width  = 16,
           units  = "cm"
    )
  }
}
