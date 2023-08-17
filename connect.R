install.packages("rgeos")
install.packages("geosphere")
install.packages("rgeos")
install.packages("raster")
install.packages("sf")
install.packages("terra")
install.packages("geosphere")
install.packages("sp")
library(rgeos)
library(rgdal)
library(raster)
library(sf)
library(terra)
library(geosphere)
library(sp)

shapefile <- readOGR("~/desktop/defor/2050_3_3.shp")
centroids <- gCentroid(shapefile, byid = TRUE)
id <- 1:length(shapefile)
centroids$centroid_id <- id

distance_matrix <- round(distm(centroids), digits = 9)
upper_triangle <- upper.tri(distance_matrix)
distance_data <- data.frame(
  centroid1 = row(distance_matrix)[upper_triangle],
  centroid2 = col(distance_matrix)[upper_triangle],
  distance = distance_matrix[upper_triangle]
)

output_file <- "~/desktop/dist_2050_3_3.txt"
write.table(distance_data, output_file, sep = "\t", quote = FALSE, row.names = FALSE)

