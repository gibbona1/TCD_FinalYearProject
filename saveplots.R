source("covidplots-group.r")

for(country in names(plotslist)){
  for(p in names(plotslist[[country]])){
    ggsave(filename = paste0(country, "-", p, ".png"),
           plot     = plotslist[[country]][[p]],
           path     = "Plots",
           height   = 10,
           width    = 14,
           units    = "cm"
           )
  }
}

for(country in names(multilist)){
  for(p in names(multilist[[country]])){
    ggsave(filename = paste0(country, "-", p, "mult.png"),
           plot     = multilist[[country]][[p]],
           path     = "Plots",
           height   = 10,
           width    = 16,
           units    = "cm"
    )
  }
}

for(pair in names(comparelist)){
  ggsave(filename = paste0("compare-", pair, ".png"),
         plot     = comparelist[[pair]],
         path     = "Plots",
         height   = 10,
         width    = 16,
         units    = "cm"
  )
}
