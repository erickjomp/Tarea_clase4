library(tidyverse)
library(sf)
library(tmap)
library(ggrepel)
library(textclean)
library(geodata)
theme_set(theme_bw())

### directorios ###
wd <- list()
wd$out_plots <- "02_outputs/plots/"
wd$out_data <- "02_outputs/data/"


#### lectura data ####
df_data <-
  read_delim(
    "https://drive.minsa.gob.pe/s/PigmdwnCGEdyqos/download",
    delim = "|",
    locale = locale(encoding = "UTF-8")
  )
# correccion de nombres (quitando espacios iniciales y finales)
df_data <- 
  df_data %>% mutate(across(where(is.character), trimws))

# vista de la estrucura
glimpse(df_data)


#### descarga obejtos sf de peru ####
sf_peru1 <- gadm("PER", 1, path = tempdir(), resolution = 2) %>% 
  st_as_sf()
sf_peru2 <- gadm("PER", 2, path = tempdir(), resolution = 2) %>% 
  st_as_sf()

plot(sf_peru1$geometry)
plot(sf_peru2$geometry)

#### lectura data de poblacion ####
# Fuente: https://www.datosabiertos.gob.pe/dataset/poblaci%C3%B3n-peru
#   (proyeccion INEI 2021)
df_pob_peru <- read_csv("https://cloud.minsa.gob.pe/s/Jwck8Z59snYAK8S/download")
df_pob_peru_deps <- df_pob_peru %>% group_by(Departamento) %>% 
  summarise(poblacion = sum(Cantidad))


#### graficos exploratorios por departamentos


 
# homegenizando objeto sf de departamentos Peru 
# (union de geometrias de LIMA y LIMA PROVINCE )
sf_peru1 <- sf_peru1 %>%
  mutate(NAME_1 = gsub("Lima Province","Lima", NAME_1)) %>% 
  group_by(NAME_1) %>% 
  summarise()

# trasnformando nombres a mayusculas y sin tildes
sf_peru1 <- sf_peru1 %>% 
  mutate(departamento = replace_non_ascii(toupper(NAME_1)))
  
df_data_sum1 <- 
  df_data %>% 
  filter(`PAIS DOMICILIO` == "PERU",
         !(`DEPARTAMENTO DOMICILIO` %in% c("SIN REGISTRO","")),
         AÑO < 2023) %>%   
  group_by(AÑO,`DEPARTAMENTO DOMICILIO`) %>% 
  summarise(n_fallecidos = n())

# uniendo data
sf_peru1 <- sf_peru1 %>% 
  left_join(df_pob_peru_deps,by = c("departamento" = "Departamento")) %>% 
  full_join(df_data_sum1, 
            by = c("departamento" = "DEPARTAMENTO DOMICILIO"))

# calculando tasa de muertes por cada 100000 habitantes
sf_peru1 <- 
  sf_peru1 %>% 
  mutate(tasa_cienmil = n_fallecidos / poblacion * 100000)

# descargando paises adicionales para mapa
sf_paises <- gadm(
  c("BRA", "CHL", "BOL", "ECU", "COL"),
  level = 0,
  path = tempdir(),
  resolution = 2
) %>% st_as_sf()

# creando sf de centroides
sf_centroids <- st_centroid(sf_peru1) %>%
  mutate(x = map_dbl(geometry, 1),
         y = map_dbl(geometry, 2))

(
  plot_mapa <-
    ggplot(data = sf_peru1) +
    geom_sf(data = sf_paises, fill = "white") +
    geom_sf(col = "black", aes(fill = tasa_cienmil)) +
    geom_text_repel(data = sf_centroids,
                    mapping = aes(x, y, label = NAME_1)) +
    labs(x = "Longitud", y = "Latitud",
         fill = "Número de\nmuertes por \ncada 100 000 \nhabitantes",
         title = "Mapa de muertes por departamente",
         caption = "Fuente: SINADEF") +
    scale_fill_continuous(type = "viridis") +
    facet_wrap(~AÑO) + 
    coord_sf(xlim = st_bbox(sf_peru1)[c(1, 3)],
             ylim = st_bbox(sf_peru1)[c(2, 4)]) +
    theme(panel.background = element_rect(fill = "lightblue"))
)

ggsave(
  file.path(wd$out_plots, 
            "mapa_muertes_cienmil_por_departamento_y_año.pdf"),
  height = 250, width = 350, units = "mm")
