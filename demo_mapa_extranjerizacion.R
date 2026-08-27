# =========================================================
# DEMO: ¿Qué se puede hacer con R? (mapas y visualización)
# Fuente de datos: github.com/thomasartopoulos/mapa_extranjerizacion
# =========================================================

# 0. Seteo ####
# (instalar una sola vez, antes de la clase, no durante la demo)
# install.packages(c("tidyverse", "sf", "leaflet", "plotly", "DT"))

library(tidyverse)
library(sf)
library(leaflet)

options(scipen = 999)

# 1. Cargamos los datos ####
# Un mismo dataset separado en dos archivos, para mostrar el uso de joins

df_data <- read.csv("./data/datos_tierras_extranjerizacion.csv")
df_geo  <- st_read("./data/deptos_argentina.geojson")

# 1.1 Primer vistazo ####
head(df_data)
head(df_geo)

# 1.2 Unimos ambas fuentes (join) ####
# Uso 'by' explícito: buena práctica, evita advertencias/ambigüedades
df <- df_geo %>%
  left_join(df_data, by = c("id"))

# 2. Exploración visual de los datos ####

# Distribución general ####
ggplot(df_data, aes(x = extranjerizada_ha)) +
  geom_histogram(bins = 100, fill = "salmon") +
  labs(
    title = "Distribución de la superficie extranjerizada",
    x = "Superficie extranjerizada",
    y = "Cantidad de departamentos"
  ) +
  theme_minimal()

# Comparación por provincia ####
ggplot(df_data, aes(x = reorder(provincia, extranjerizada_ha), y = extranjerizada_ha)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Superficie extranjerizada por provincia",
    x = NULL,
    y = "Superficie extranjerizada"
  ) +
  theme_minimal()

# Relación entre superficie total y extranjerizada ####
ultimo_grafico <- ggplot(df_data, aes(x = total_ha, y = extranjerizada_ha, col = provincia)) +
  geom_point(aes(size = porcentaje), alpha = 0.5) +
  labs(
    title = "Superficie total y superficie extranjerizada",
    x = "Superficie total",
    y = "Superficie extranjerizada"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Un gráfico de ggplot2 que se vuelve interactivo con una sola línea ####
library(plotly)
ggplotly(ultimo_grafico)

# 3. Mapas: de lo simple a lo completo ####

## 3.1 Mapa base, sin datos ####
leaflet() %>%
  addTiles()

## 3.2 Mapa con los polígonos de los departamentos ####
leaflet(df) %>%
  addTiles() %>%
  addPolygons()

## 3.3 Centramos la vista sobre Argentina ####
leaflet(df) %>%
  addTiles() %>%
  addPolygons() %>%
  setView(lng = -64, lat = -35, zoom = 4)

## 3.4 Coloreamos según el % de tierra extranjerizada ####
pal <- colorNumeric(
  palette = c("darkgreen", "yellow", "orange", "red", "darkred"),
  domain = df$porcentaje,
  na.color = "transparent"
)

# Opción alternativa de paleta (más "profesional", muy usada en R): viridis
pal <- colorNumeric(
  palette = "viridis",
  domain = df$porcentaje,
  na.color = "transparent"
)



leaflet(df) %>%
  addTiles() %>%
  setView(lng = -64, lat = -35, zoom = 4) %>%
  addPolygons(
    fillColor = ~pal(porcentaje),
    fillOpacity = 0.7,
    color = "black",
    weight = 1
  )

## 3.5 Agregamos resaltado al pasar el mouse ####
leaflet(df) %>%
  addTiles() %>%
  setView(lng = -64, lat = -35, zoom = 4) %>%
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

## 3.6 Capa base oficial (IGN) ####
# Cartografía oficial del Instituto Geográfico Nacional (Ley 22.963,
# "Ley de la Carta"): a diferencia de OpenStreetMap, respeta la
# representación territorial oficial argentina (incluye Malvinas
# Argentinas como parte del territorio nacional).
ign_url <- "https://wms.ign.gob.ar/geoserver/gwc/service/tms/1.0.0/capabaseargenmap@EPSG:3857@png/{z}/{x}/{-y}.png"
ign_attribution <- 'Mapa base: <a href="https://www.ign.gob.ar">Instituto Geográfico Nacional</a>'

# misma paleta que la web 
pal <- colorBin(
  palette = c("#2E7D32", "#FBC02D", "#FFEB3B", "#EF5350", "#8B0000"),
  domain = df$porcentaje,
  bins = c(0, 4, 8, 14, 30, Inf),
  na.color = "transparent"
)

mapa <- leaflet(df) %>%
  addTiles(
    urlTemplate = ign_url,
    attribution = ign_attribution,
    options = tileOptions(minZoom = 3, maxZoom = 18)
  ) %>%
  setView(lng = -64, lat = -35, zoom = 4) %>%
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
      "<br><b>Porcentaje:</b> ", round(porcentaje, 1), "%"
    )
  ) %>%
  addLegend(
    pal = pal,
    values = ~porcentaje,
    title = "% de tierras extranjerizadas"
  )

mapa


# 4. Guardamos el resultado ####

## 4.1 Como archivo HTML interactivo, para compartir o abrir en el navegador ####
library(htmlwidgets)
saveWidget(mapa, "mapa.html", selfcontained = TRUE)

## 4.2 Como imagen estática (requiere el paquete 'mapview' + 'webshot2') ####
library(mapview)
library(webshot2)
mapshot(mapa, file = "mapa_extranjerizacion.png")
