library(tidyverse)
library(sf)
library(leaflet)

# 0 seteo ####
options(scipen = 999)

# 1 levanto data ####
df_data <- read.csv("datos_tierras_extranjerizacion.csv")

df_geo <- st_read("deptos_argentina.geojson")

st_crs(df_geo)

## 1.2 joineo
df <- df_geo%>%left_join(df_data)


## 2 Exploracion ####


## 3 MAPA ####
df <- st_read("departamentos.geojson")%>%
  mutate(id = row_number())


df_data <- df%>%
  as.data.frame()%>%
  select(-geometry)

write.csv(df_data, "datos_tierras_extranjerizacion.csv", row.names = FALSE)

df_geo <- df%>%select(id, geometry)

st_write(df_geo,"deptos_argentina.geojson",delete_dsn = TRUE)
