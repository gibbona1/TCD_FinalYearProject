setwd("~/GitHub/TCD_FinalYearProject")
source("Code/covid-main.r")
source("Code/covid-multimodel.r")
source("Code/covid-mapplots.r")
source("Code/covid-compare.r")

for(country in names(plotslist)){
  for(p in names(plotslist[[country]][["plots"]])){
    ggsave(filename = paste0(country, "-", p, ".pdf"),
           plot     = plotslist[[country]][["plots"]][[p]],
           path     = "./Plots",
           height   = 10,
           width    = 14,
           units    = "cm"
           )
  }
  if(!is.null(plotslist[[country]][["summary"]])){
    sumtable <- xtable(plotslist[[country]][["summary"]], type = "latex",
                       caption = plotslist[[country]][["desc"]],
                       label = paste0("fig:", country, "summarydf"))
    print(sumtable, sanitize.text.function=function(x){x}, 
          table.placement="H",
          file = paste0("Report/Chapters/", country, "-summarydf.tex"))
  }
}

for(country in names(multilist)){
  for(p in names(multilist[[country]])){
    ggsave(filename = paste0(country, "-", p, "mult.pdf"),
           plot     = multilist[[country]][[p]],
           path     = "Plots",
           height   = 10,
           width    = 16,
           units    = "cm"
    )
  }
}

for(pair in names(comparelist)){
  ggsave(filename = paste0("compare-", pair, ".pdf"),
         plot     = comparelist[[pair]],
         path     = "Plots",
         height   = 10,
         width    = 16,
         units    = "cm"
  )
}

for(p in names(countyplotlist)){
  ggsave(filename = paste0("county-", p, ".pdf"),
         plot     = countyplotlist[[p]],
         path     = "Plots",
         height   = 14,
         width    = 14,
         units    = "cm"
  )
}

for(p in names(worldplot)){
  ggsave(filename = paste0("world-", p, ".pdf"),
         plot     = worldplot[[p]],
         path     = "Plots",
         height   = 14,
         width    = 20,
         units    = "cm"
  )
}
