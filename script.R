library(tidyverse)
library(sf)

# importamos datos

temp <- tempfile()

download.file("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/relevamiento-usos-del-suelo/relevamiento-usos-del-suelo-2017-zip.zip",
              temp)

unzip(temp,overwrite = T)

RUS_df <- st_read("relevamiento-usos-suelo-2017.shp") %>% 
  mutate_at(vars(contains("_DIG")), ~as.numeric(as.character(.))) %>% 
  st_transform(crs = 5347)

corredores <- st_read("corredores_lineales_modif.shp") %>% 
  st_transform(crs = 5347)

buffers <- st_buffer(corredores,dist = 20) 


ggplot(RUS_df %>% sample_n(5000))+
  geom_sf(col = 'steelblue', alpha = .1)+
  geom_sf(data = buffers)


# calculamos intersecciones

intersected_rows <- which(st_intersects(RUS_df, buffers, sparse = F) %>% 
  rowSums() > 0)
