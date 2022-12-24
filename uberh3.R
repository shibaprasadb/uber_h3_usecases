#Load the libraries

library(sf)
library(tidyverse)
library(ggplot2)
library(h3jsr)
library(ggrepel)

number_of_points <- 1000

set.seed(04)
sample_data <- data.frame(lat = runif(number_of_points, 22, 23),
                          long = runif(number_of_points, 77, 78),
                          id = 1:number_of_points)


PointToCellFunction <- function(long, lat, input_resolution){
  
  point <- c(long, lat)
  (st_sfc(st_point(point), crs = 4326))-> point_details
  
  return(point_to_cell(point_details, res=input_resolution))
}


sample_data %>%
  mutate(cell = mapply(PointToCellFunction, long, lat, 6))-> data_with_cell


#Visualising the cells


data_with_cell %>%
  distinct(cell) %>%
  pull() -> list_cell


h3_addresses <- cell_to_polygon(list_cell, simple = FALSE)

ggplot() +
  geom_sf(
    data = h3_addresses,
    fill = 'dark blue',
    colour = 'green',
    alpha = 0.5
  ) +
  geom_point(
    data = sample_data,
    aes(x = long, y = lat),
    col = 'Red',
    alpha = 0.6
  ) +
  theme_minimal() +
  coord_sf()


#Let's wrap it in a function and see how it looks for different resolution


CellVizFunction <- function(sample_data, cell_resolution) {
  sample_data %>%
    mutate(cell = mapply(PointToCellFunction, long, lat, cell_resolution)) -> data_with_cell
  
  
  #Visualising the cells
  
  
  data_with_cell %>%
    distinct(cell) %>%
    pull() -> list_cell
  
  
  h3_addresses <- cell_to_polygon(list_cell, simple = FALSE)
  
  ggplot() +
    geom_sf(
      data = h3_addresses,
      fill = 'dark blue',
      colour = 'green',
      alpha = 0.5
    ) +
    geom_point(data = sample_data,
               aes(x = long, y = lat),
               col = 'Red',
               alpha = 0.6) +
    theme_minimal() +
    ggtitle(paste0('Resolution: ', cell_resolution)) +
    coord_sf()-> output_plot
  
  plot(output_plot)
  
}

CellVizFunction(sample_data, 6)
CellVizFunction(sample_data, 5)

#Cell to centroid

unlist(cell_to_point(data_with_cell$cell))-> lat_long_list
lat_long_list[seq(1, length(lat_long_list), 2)]-> long_list
lat_long_list[seq(2, length(lat_long_list), 2)]-> lat_list

data_with_cell %>%
  cbind(cen_lat = lat_list,
        cen_long = long_list)-> data_with_cell

data_with_cell %>%
  distinct(cen_lat, cen_long) %>%
  mutate(polygon_no = 1:n()) -> centroid_details

ggplot() +
  geom_sf(data = h3_addresses,
          fill = 'green',
          col = 'black')+
  geom_point(data = centroid_details, aes(x=cen_long, y=cen_lat), col='black')+
  theme_dark()


