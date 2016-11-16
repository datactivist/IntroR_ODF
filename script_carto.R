library(rgdal)
library(tmap)
library(spdplyr)
# source : http://data.iledefrance.fr/explore/dataset/accidentologie-paris/export/?location=17,48.83318,2.26283
accidents <- read_shape("./data/accidentologie-paris.shp")

accidents <- accidents %>% 
  mutate(velo = ifelse(vehicule_1_ == "Bicy" | vehicule_2_ == "Bicy" | vehicule_3_ == "Bicy",
                       "bicy",
                       "autres")) %>% 
  filter(!is.na(velo))

paris <- read_osm(bbox(accidents))

tm_shape(paris) +
  tm_raster() +
tm_shape(accidents) +
  tm_bubbles(size = 0.2, col = "velo", palette = "Set1")
