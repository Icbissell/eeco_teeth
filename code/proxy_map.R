library(tidyverse)
library(rgdal)
library(broom)
library(ggplot2)
library(ggthemes)

coastline_gws_url <-
  "http://gws.gplates.org/reconstruct/coastlines/?time=55&model=GOLONKA"
polygons_gws_url <-
  "http://gws.gplates.org/reconstruct/static_polygons/?time=55&model=GOLONKA"

eocene_coastlines <-
  rgdal::readOGR(coastline_gws_url)
eocene_polygons <-
  rgdal::readOGR(polygons_gws_url)


eocene_coastlines <-
  broom::tidy(eocene_coastlines)
eocene_polygons <-
  broom::tidy(eocene_polygons)

eocene_map <-
  ggplot() +
  geom_map(
    data = eocene_polygons, map = eocene_polygons,
    aes(x = long, y = lat, map_id = id),
    size = 0.15, fill = "#d8d8d8"
  ) +
  geom_map(
    data = eocene_coastlines, map = eocene_coastlines,
    aes(x = long, y = lat, map_id = id),
    size = 0.15, fill = NA, colour = "grey30"
  ) +
  geom_rect(
    data = data.frame(xmin = -180, xmax = 180, ymin = -90, ymax = 90),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    color = 1, fill = NA, size = 0.3
  ) +
  coord_map("mollweide") +
  ggthemes::theme_map()

proxies_path <- "/Users/icbissell/Downloads/Eocene_Productivity_Proxies_Sheet4.csv"
proxies <- read_csv(proxies_path)

eocene_map +
  geom_point(
    data = proxies,
    aes(x = Latitude, y = Longitude)
  )