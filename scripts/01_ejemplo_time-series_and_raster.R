library(leaflet)
library(terra)
library(sf)
library(tidyverse)
library(fs)

st_layers('data/vectorial/rio_claro.gpkg')

sf_le <- read_sf('data/vectorial/rio_claro.gpkg',layer = 'sectores_rio_claro') |> 
  st_transform(32719)

plot(sf_le)

plot(sf_le$geom)

files <- dir_ls('data/potencial_predict',regexp = 'rio_claro.*tif$')

potencial <- rast(files)

#variación temporal del potencial en los sectores de riego
data <- terra::extract(potencial,sf_le,fun = mean)

data |> 
  pivot_longer(-ID) |> 
  mutate(date = ymd(name)) |> 
  ggplot(aes(date,value,color = ID)) + 
  geom_point(size=.1) + 
  geom_line(lwd = .1) +
  theme_bw()

data |> count(ID)


#mapear en leaflet

r <- potencial[[1]]
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(r),
na.color = "transparent")

leaflet()  |>  
  addTiles() |> 
  addRasterImage(r, colors = pal, opacity = 0.8) |> 
  addLegend(pal = pal, values = values(r),
            title = "Potencial")
