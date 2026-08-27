library(tidyverse)
library(sf)
library(leaflet)

# 0 seteo ####
options(scipen = 999)

# 1 levanto data ####
df_data <- read.csv("datos_tierras_extranjerizacion.csv")

df_geo <- st_read("deptos_argentina.geojson")

st_crs(df_geo)

## 1.2 Exploramos el df ####

head(df_data)
glimpse(df_data)

head(df_geo)
glimpse(df_geo)

## 1.2 Join
df <- df_geo%>%left_join(df_data)


# 2 Exploracion los datos ####
ggplot(df_data, aes(x = extranjerizada_ha )) +
  geom_histogram(bins = 100, fill = "salmon") +
  labs(
    title = "Distribución de la superficie extranjerizada",
    x = "Superficie extranjerizada",
    y = "Cantidad de departamentos"
  ) +
  theme_minimal()

ggplot(df_data, aes(
  x = reorder(provincia, extranjerizada_ha),
  y = extranjerizada_ha
)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Superficie extranjerizada por provincia",
    x = NULL,
    y = "Superficie extranjerizada"
  ) +
  theme_minimal()+
  coord_flip()

ggplot(
  df_data,
  aes(
    x = total_ha,
    y = extranjerizada_ha,
    col = provincia
  )
) +
  geom_point(aes(size = porcentaje), alpha = 0.5) +
  # geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Superficie total y superficie extranjerizada",
    x = "Superficie total",
    y = "Superficie extranjerizada"
  ) +
  theme(legend.position = "bottom") +
  theme_minimal()


# 3 MAPA ####

## 3.1 Mapa base ####
leaflet() %>%
  addTiles() 

## 3.2 Mapa con datos ####
leaflet(df) %>%
  addTiles() %>%
  addPolygons()

## 3.3 Mapa con datos y vista centrada ####
leaflet(df) %>%
  addTiles() %>%
  addPolygons()%>%
  setView(
    lng = -64,
    lat = -35,
    zoom = 4
  )

## 3.3 Mapa con datos y paleta de colores ####
### paleta numerica
pal <- colorNumeric(
  palette = "YlOrRd",
  domain = df$porcentaje,
  na.color = "transparent"
)


leaflet(df) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(porcentaje),
    fillOpacity = 0.7,
    color = "black",
    weight = 1
  ) %>%
  setView(
    lng = -64,
    lat = -35,
    zoom = 4
  )

### paleta categórica
pal <- colorQuantile(
  palette = "YlOrRd",
  domain = df$porcentaje,
  probs = seq(0, 1, 0.2),
  na.color = "transparent"
)


leaflet(df) %>%
  addTiles() %>%
  setView(
    lng = -64,
    lat = -35,
    zoom = 4
  ) %>%
  addPolygons(
    fillColor = ~pal(porcentaje),
    fillOpacity = 0.7,
    color = "black",
    weight = 1
  ) 
  
## 3.4 Mapa con datos, paleta de colores y resaltado ####
leaflet(df) %>%
  addTiles() %>%
  setView(
    lng = -64,
    lat = -35,
    zoom = 4
  ) %>%
  addPolygons(
    fillColor = ~pal(porcentaje),
    fillOpacity = 0.7,
    color = "black",
    weight = 1,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  )


## 3.5 Mapa con datos, paleta de colores, resaltado y popup ####
leaflet(df) %>%
  addTiles() %>%
  setView(
    lng = -64,
    lat = -35,
    zoom = 4
  ) %>%
  addPolygons(
    fillColor = ~pal(porcentaje),
    fillOpacity = 0.7,
    color = "black",
    weight = 1,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    popup = ~paste0(
      "<b>Departamento:</b> ", nombre,
      "<br><b>Provincia:</b> ", provincia,
#      "<br><b>Superficie extranjerizada:</b> ",
#      round(extranjerizada_ha, 0), " ha",
      "<br><b>Porcentaje:</b> ",
      round(porcentaje, 1), "%"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~porcentaje,
    title = "% de tierras extranjerizadas"
  )

# 4 Guardo el mapa ####
#install.packages("htmlwidgets")
htmlwidgets::saveWidget(mapa,
                        "./varios/mapa.html",
                        selfcontained = T)

# 4 Exporto un screen ####
install.packages("mapview")
library(mapview)

mapa <- leaflet(df) %>%
  addTiles() %>%
  setView(lng = -64, lat = -35, zoom = 4) %>%
  addPolygons(
    fillColor = ~pal(porcentaje),
    fillOpacity = 0.7,
    weight = 1,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  )

mapshot(
  mapa,
  file = "mapa_extranjerizacion.png"
)
