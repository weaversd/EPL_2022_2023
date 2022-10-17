library(png)
library(grid)
library(pdftools)

working_directory <- getwd()

setwd(paste0(working_directory, "weekly_analysis/weekly_team_plots/"))
plots <- lapply(ll <- list.files(),function(x){
  img <- as.raster(readPNG(x))
  rasterGrob(img, interpolate = FALSE)
})

setwd(working_directory)
library(gridExtra)
ggsave("weekly_analysis/all_team_weekly_stats.pdf", marrangeGrob(grobs=plots, nrow=5, ncol=5))


pdf_convert("weekly_analysis/all_team_weekly_stats.pdf",
            filenames = "weekly_analysis/all_team_weekly_stats.png",
            dpi = 500)
