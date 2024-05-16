#####################################################
# Description: 
#   Extreme weather events in South Brazil - Rio Grande do Sul State. September and April, May 2024
#   Historical precipitation anomaly using NASA GPM mission (2000 - 2024)
#
# Author: Mauricio Karrei (mauricio.zientar@ufl.edu)
#   Mod. Date 05/16/2024
#####################################################

library(sf)
library(stringr)
library(RColorBrewer)


shp.rs <- vect("shapefile/brrs/brrs.shp")
shp.rs <- as(shp.rs, "Spatial")
shp.cities <- vect('shapefile/RS_Municipios_2022')
shp.cities <- as(shp.cities, "Spatial")
shp.rivers <- vect('shapefile/rivers')
shp.rivers <- as(shp.rivers, "Spatial")

cidades <- read.csv('shapefile/cidades_rs.csv')
cidades <- vect(cidades, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")

# Legend and Colors
deviation.breaks <- seq(-350, 1100)
#deviation.colors <- colorRampPalette(c("darkblue","blue","lightblue","white","green", "yellow", "orange", "red", "darkred", "purple"))(length(deviation.breaks))
deviation.colors <- c(
  colorRampPalette(c("darkblue", "blue", "lightblue"))(length(deviation.breaks[deviation.breaks < 0])),
  "white",
  colorRampPalette(c("green", "yellow", "orange", "red", "darkred", "purple"))(length(deviation.breaks[deviation.breaks > 0]))
)

export.raster.map <- function(raster, title, filename, colors, breaks) {
  
  png(filename, width = 800, height = 600)
  
  labels <- seq(min(breaks), max(breaks), 100)
  
  par(mar = c(.5, .5, 2, 2))   # Remove margins bottom, left, top, right
  plot(mask(raster, shp.rs), col = colors, breaks = breaks,
       main = title,
       box = FALSE, axes = F, #interpolate = T,
       legend.shrink=1,
       axis.args=list(at = labels,
                      labels = labels, 
                      cex.axis = 1),
       legend.args=list(text='Precipitation (mm)', side=4, font=3, line=3, cex=1))
  
  lines(shp.rivers, col="darkblue", cex = .9)
  
  # Add Points to the map
  points(cidades, col = "#444444", pch = 19, cex = .9)
  text(cidades, labels = cidades$Nome_da_Cidade, pos = 4, col = "#222222")
  
  dev.off()
}


###
# Receives 2 arguments:
# 1. df = Dataframe with 2 mandatory columns:
#   1.1. year: int 
#   1.2. filename: string with the filename to be loaded
# 2. base.path: string
##
## Return:
#   RasterStack with the sum for 
### 
sum.raster.df <- function(df, base.path = 'data') {
  years.sum <- sapply(unique(df$year), function(year) {
    print(year)
    sum(stack(file.path(base.path, df[df$year == year, 'filename'])))
  })
  stack(years.sum) # from list to stack
}

