library(tidyverse)
library(sf)
library(janitor)
library(rio)

# importamos datos

temp <- tempfile()

download.file("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/relevamiento-usos-del-suelo/relevamiento-usos-del-suelo-2017-zip.zip",
              temp)
unzip(temp,overwrite = T)

temp <- tempfile()

download.file("http://cdn.buenosaires.gob.ar/datosabiertos/datasets/parcelas/parcelas-zip.zip",
              temp)

unzip(temp,overwrite = T)

RUS_df <- st_read("relevamiento-usos-suelo-2017.shp") %>% 
  mutate_at(vars(contains("_DIG")), ~as.numeric(as.character(.))) %>% 
  st_transform(crs = 5347)

corredores <- st_read("corredores_lineales_modif.shp") %>% 
  st_transform(crs = 5347)

buffers <- st_buffer(corredores,dist = 20) 




# calculamos intersecciones

intersected_rows <- which(st_intersects(RUS_df, buffers, sparse = F) %>% 
  rowSums() > 0)

### problema: los puntos están asignados al centroide de la parcela, hay que intersectar con parcela

parcelas <- st_read("parcelas.shp")%>% 
  st_transform(crs = 5347)

# ggplot(RUS_df %>% sample_n(5000))+
#   geom_sf(col = 'steelblue', alpha = .1)+
#   geom_sf(data = buffers)+
#   geom_sf(data = parcelas %>% sample_n(5000))


intersected_parcelas <- st_intersects(parcelas, buffers) %>% lengths() > 0


parcelas <- parcelas %>% 
  filter(intersected_parcelas)


#intersectamos puntos con parcelas

intersected_points <- st_intersects(RUS_df, parcelas) %>% lengths() > 0

excluded_vect <- c("ABANDONADO", "ACTIVIDADES RELIGIOSAS", "ASOCIACION CIVIL", "ASOCIACIONES (OTRAS)",
                   "ASOCIACIONES EMPRESARIALES Y DE EMPLEADORES", "BAJO AUTOPISTA","BOMBEROS",
                   "CASA", "CERRADO", "CIUDADANIA ITALIANA", "COLEGIO")

data <- RUS_df %>% 
  filter(intersected_points,
         !TIPO1_16 %in% c("E", "G", "GA", "GAP", "EP", "LOTE", "U", "EDU"),
         !TIPO2_16 %in% c("LOCAL CERRADO"),
         !X3_DIG %in% c(0))

data_2 <- RUS_df %>% 
  filter(!intersected_points,
         !TIPO1_16 %in% c("E", "G", "GA", "GAP", "EP", "LOTE", "U", "EDU"),
         !TIPO2_16 %in% c("LOCAL CERRADO"),
         !X3_DIG %in% c(0))

table_list <- list()

table_list$corredores_rama <- data %>% 
  st_drop_geometry() %>% 
  group_by(RAMA) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  adorn_totals()

table_list$corredores_subrama <- data %>% 
  st_drop_geometry() %>% 
  group_by(SUBRAMA) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  adorn_totals()


table_list$corredores_ssrama <- data %>% 
  st_drop_geometry() %>% 
  group_by(SSRAMA) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  adorn_totals()



table_list$resto_caba_rama <- data_2 %>% 
  st_drop_geometry() %>% 
  group_by(RAMA) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  adorn_totals()


table_list$resto_caba_subrama <- data_2 %>% 
  st_drop_geometry() %>% 
  group_by(SUBRAMA) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  adorn_totals()

table_list$resto_caba_ssrama <- data_2 %>% 
  st_drop_geometry() %>% 
  group_by(SSRAMA) %>% 
  summarise(total = n()) %>% 
  arrange(desc(total)) %>% 
  adorn_totals()


table_list$resumen_tablas <- data.frame(zona = c("Corredores", "Resto de la CABA"),
                                        comercios = c(nrow(data), nrow(data_2))) %>% 
  adorn_totals()

table_list$corredores <- corredores$nombre


export(table_list, "comercios_rus.xlsx")
