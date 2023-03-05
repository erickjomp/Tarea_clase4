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


#### graficos exploratorios a nivel nacional ####

# muertos por año 
table(df_data$`PAIS DOMICILIO`)
df_data %>% 
  filter(`PAIS DOMICILIO` == "PERU",
         AÑO < 2023) %>%   
  group_by(AÑO) %>% 
  summarise(n_fallecidos = n()) %>% 
  ggplot(aes(AÑO, n_fallecidos / 1000)) + 
  geom_line(col = "royalblue") + 
  geom_point(col = "royalblue")  +
  labs(x = " Año", y = "Número de fallecidos (miles)",
       title = "Muertos por años a nivel nacional",
       caption = "Fuente: SINADEF")

ggsave(file.path(wd$out_plots,
                 "serie_muertos_anuales_nacional.pdf"),
       height = 150, width = 220, units = "mm")


# muertos por meses
df_data %>% 
  filter(`PAIS DOMICILIO` == "PERU",
         FECHA < "2023-03-01") %>% 
  mutate(meses = trunc(FECHA, "months")) %>% 
  group_by(meses) %>% 
  summarise(n_fallecidos = n()) %>% 
  ggplot(aes(meses, n_fallecidos / 1000)) + 
  geom_line(col = "royalblue") + 
  geom_point(col = "royalblue")  +
  scale_x_date(breaks = "1 year",date_labels = "%Y-%b") + 
  labs(x = NULL, y = "Número de fallecidos (miles)",   
       title = "Muertos por meses a nivel nacional", 
       caption = "Fuente: SINADEF")

ggsave(file.path(wd$out_plots,
                 "serie_muertos_por_meses_nacional.pdf"),
       height = 150, width = 220, units = "mm")

# muertos por dias
df_data %>% 
  filter(`PAIS DOMICILIO` == "PERU",
         FECHA < "2023-03-01") %>% 
  group_by(FECHA) %>% 
  summarise(n_fallecidos = n()) %>% 
  ggplot(aes(FECHA, n_fallecidos / 1000)) + 
  geom_line(col = "royalblue") + 
  scale_x_date(breaks = "1 year",date_labels = "%Y-%b-%d") + 
  labs(x = NULL, y = "Número de fallecidos (miles)",
       title = "Muertos por días a nivel nacional",
       caption = "Fuente: SINADEF")

ggsave(file.path(wd$out_plots,
                 "serie_muertos_por_dias_nacional.pdf"),
       height = 150,width = 220, units = "mm")




#### graficos exploratorios por departamentos ####

# muertos por año 
table(df_data$`PAIS DOMICILIO`)
df_data %>% 
  filter(`PAIS DOMICILIO` == "PERU",
         !(`DEPARTAMENTO DOMICILIO` %in% c("SIN REGISTRO","")),
         AÑO < 2023) %>%   
  group_by(AÑO,`DEPARTAMENTO DOMICILIO`) %>% 
  summarise(n_fallecidos = n()) %>% 
  ggplot(aes(AÑO, n_fallecidos / 1000)) + 
  geom_line(col = "royalblue") + 
  geom_point(col = "royalblue")  +
  labs(x = " Año", y = "Número de fallecidos (miles)",
       title = "Muertos por años por departamentps",
       caption = "Fuente: SINADEF") +
  facet_wrap(~`DEPARTAMENTO DOMICILIO`,scales = "free_y")

ggsave(file.path(wd$out_plots,
                 "serie_muertos_anuales_departamentos.pdf"),
       height = 200, width = 300, units = "mm")


# muertos por meses
df_data %>% 
  filter(`PAIS DOMICILIO` == "PERU",
         !(`DEPARTAMENTO DOMICILIO` %in% c("SIN REGISTRO","")),
         FECHA < "2023-03-01") %>%   
  mutate(meses = trunc(FECHA, "months")) %>% 
  group_by(meses, `DEPARTAMENTO DOMICILIO`) %>% 
  summarise(n_fallecidos = n()) %>% 
  ggplot(aes(meses, n_fallecidos / 1000)) + 
  geom_line(col = "royalblue") + 
  labs(x = NULL, y = "Número de fallecidos (miles)",   
       title = "Muertos por meses por departamentos", 
       caption = "Fuente: SINADEF") +
  # scale_x_date(breaks = "1 year",date_labels = "%Y-%b") + 
  facet_wrap(~`DEPARTAMENTO DOMICILIO`,scales = "free_y")

ggsave(file.path(wd$out_plots,
                 "serie_muertos_por_meses_departamentos.pdf"),
       height = 200, width = 300, units = "mm")

# muertos por dias
df_data %>% 
  filter(`PAIS DOMICILIO` == "PERU",
         !(`DEPARTAMENTO DOMICILIO` %in% c("SIN REGISTRO","")),
         FECHA < "2023-03-01") %>%   
  group_by(FECHA, `DEPARTAMENTO DOMICILIO`) %>% 
  summarise(n_fallecidos = n()) %>% 
  ggplot(aes(FECHA, n_fallecidos / 1000)) + 
  geom_line(col = "royalblue") + 
  labs(x = NULL, y = "Número de fallecidos (miles)",
       title = "Muertos por días a nivel nacional",
       caption = "Fuente: SINADEF") +
  facet_wrap(~`DEPARTAMENTO DOMICILIO`,scales = "free_y")

ggsave(file.path(wd$out_plots,
                 "serie_muertos_por_dias_departamentos.pdf"),
       height = 200, width = 300, units = "mm")
