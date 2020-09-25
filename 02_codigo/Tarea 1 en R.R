## TAREA 1 ##

## Dalila Sarabia ##

# Librerias

library(sf) # Abrir bases de datos geograficas
library(leaflet) # Visualización interactiva (mapas)
library(tidyverse) # Manejo de bases de datos
library(htmlwidgets) # Para guardar paginas HTML
library(webshot) # Para imprimir paginas HTML
library(ggplot2)
library(pacman)
p_load(plotly, 
       readxl, 
       scales)

library(plotly)

### DATOS ###

tiraderos <- read_csv("01_datos/tiraderos-clandestinos-al-cierre-de-2017.csv")

### EJERCICIO 1. UNA GRÁFICA EN GGPLOT

tiraderos %>% 
 count(alcaldia) %>% 
ggplot(aes(x = reorder(alcaldia, n), y = n, fill= alcaldia, alpha=.03)) +
  geom_col() +
  coord_flip() +
    labs(title = "Tiraderos clandestinos por Alcaldía en 2017", x = "Alcaldia", y = "No. de tiraderos", caption = "Fuente: Portal de Datos Abiertos CDMX") +
    theme(legend.position = "none")

### UNA GRÁFICA EN GGPLOT (VERSIÓN 2) ESTA ES INTERACTIVA

wifi <- read_csv("01_datos/ubicacion-acceso-gratuito-internet-wifi-c5.csv")

plo <- wifi %>% 
  count(ALCALDIA) %>% 
  ggplot(aes(x = reorder (ALCALDIA,n), y = n, fill= ALCALDIA, alpha= 0.5, text= paste0("Número de postes:", n))) +
  geom_col() +
  coord_flip() +
  labs(tit = "Puntos de WiFi gratuitos por Alcaldia en CDMX", x = "Alcaldia ", y = "No. de postes con internet gratuto", caption = "Fuente: Portal de Datos Abiertos CDMX") +
  theme(legend.position = "none")

ggplotly(plo, tooltip = "text")  %>% 
  config(displayModeBar = F)


### UN MAPA ESTÁTICO EN GGPLOT


library(pacman)
p_load(tidyverse, sf, ggplot2, viridis)

tiraderos <- read_csv("01_datos/tiraderos-clandestinos-al-cierre-de-2017.csv")

mpios <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios2.geojson", quiet = TRUE)

mpios <- mpios %>% 
  filter(CVE_ENT == "09")

class(mpios)

plot(mpios, max.plot = 1)

### Buscamos atributo de interés

glimpse(tiraderos)
glimpse(mpios)

summary(tiraderos$CVE_MUN)
summary(mpios$CVE_MUN)

#Construimos el mapa estático

tiraderos <- tiraderos %>%
  mutate(CVE_MUN = case_when
         (str_length(no_alcaldi) == 1 ~ paste0('00', no_alcaldi),
          str_length(no_alcaldi) == 2 ~ paste0('0', no_alcaldi), 
          str_length(no_alcaldi) == 3 ~ paste0('', no_alcaldi)))


tiraderos_alcaldia <- tiraderos %>% 
  group_by(alcaldia, CVE_MUN) %>% 
  count()


tiradero_mapa <- merge(mpios, tiraderos_alcaldia,by.x="CVE_MUN", by.y = "CVE_MUN", all.x=TRUE)

tiradero_mapa %>% 
  ggplot(aes(fill=n))+
  geom_sf()+
  scale_fill_gradient(low = "grey", high = "red") + 
  labs(title = "Tiraderos clandestinos en CDMX",
       subtitle = "Concentración por Alcaldia en 2017", 
       caption = "Fuente: Datos Abiertos CDMX") + 
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) + 
  theme_bw() + 
  theme(legend.position = "bottom") + 
  theme(axis.text = element_blank(), 
        panel.grid = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_rect(),
        axis.ticks = element_blank()) + 
  theme(plot.title = element_text(hjust = 0.5, 
                                  colour = "gray10", 
                                  family = "Arial", 
                                  face = "bold", 
                                  size = 15), 
        plot.subtitle = element_text(hjust = 0.5, 
                                     colour = "gray50", 
                                     family = "Arial", 
                                     face = "bold", 
                                     size = 10), 
        plot.caption = element_text(colour = "gray50", 
                                    hjust = 1))




#### UN MAPA EN LEAFLET

wifi_geo <- st_read("01_datos/ubicacion-acceso-gratuito-internet-wifi-c5.geojson", quiet = TRUE)

mpios <- st_read("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/mpios2.geojson", quiet = TRUE)

mpios <- mpios %>% 
  filter(CVE_ENT == "09")

edo <- st_read("https://raw.githubusercontent.com/JuveCampos/MexicoSinIslas/master/Sin_islas.geojson", quiet = TRUE) %>% 
  filter(ENTIDAD == "CIUDAD DE MÉXICO")


### Revisamos CRS 

st_crs(wifi_geo)
st_crs(edo)
st_crs(mpios)

### HACEMOS MAPA

leaflet() %>% 
  addPolygons(data = edo, color = "grey", fill = NA) %>% 
  addPolygons(data = mpios, color = "black", fill = NA) %>% 
  addCircleMarkers(data = wifi_geo, color = "green", radius = 1) 

### Le ponemos un mapa base

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = edo, color = "black", fill = NA) %>% 
  addPolygons(data = mpios, color = "black", fill = NA) %>% 
  addCircleMarkers(data = wifi_geo, color = "green", radius = 1)

### Le ponemos un popup

leaflet() %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolygons(data = edo, color = "black", fill = NA, opacity = .2) %>% 
  addPolygons(data = mpios, color = "black", fill = NA, opacity = .2) %>% 
  addCircleMarkers(data = wifi_geo, color = "green", radius = 1, opacity = 1, popup = paste0("<b> Alcaldía. </b>", wifi_geo$alcaldia,"<br>", "<b> Col. </b>",wifi_geo$colonia, "<br>", "<b> Esquina </b>",wifi_geo$esquina)) 


#### HACER UNA TABLA


| Alcaldia            | No. postes con wifi gratis |
  |---------------------|----------------------------|
  | Iztapalapa          | 2100                       |
  | Gustavo A. Madero   | 1790                       |
  | Cuauhtemoc          | 1448                       |
  | Venustiano Carranza | 990                        |
  | Miguel Hidalgo      | 926                        |
  | Coyoacan            | 907                        |
  | Alvaro Obregon      | 899                        |
  | Benito Juarez       | 796                        |
  | Tlalpan             | 753                        |
  | Azcapotzalco        | 707                        |
  | Iztacalco           | 664                        |
  | Tlahuac             | 540                        |
  | Xochimilco          | 416                        |
  | Magdalena Contreras | 305                        |
  | Cuajimalpa          | 239                        |
  | Milpa Alta          | 214                        |




