# sobre: limpieza archivo de input "Base de datos 01-2018.xlsx"
library(tidyverse)
library(hrbrthemes)
library(janitor)
library(magrittr)
library(lubridate)
library(kableExtra)
library(highcharter)
library(ggrepel)
library(plotly)

# Importar y estructurar Ventas
ventas <- rio::import("G:/My Drive/ANALISIS/Base de Datos 2019.xlsx", which = "Ventas") %>% 
  remove_empty(c("cols", "rows")) %>%  # quita las columnas y filas en blanco
  clean_names() %>% select("fecha", "vcd","codigo", "cliente", "volumen_m3", "tipo", "cono_9", "c_bomba", "precio_bs_m3", "precio_total_venta_bs")

colnames(ventas)[7] <- c("cono")

#Importar y estructurar Producci?n
produccion <- rio::import("G:/My Drive/ANALISIS/Base de Datos 2019.xlsx", which = "Produccion") %>% 
  remove_empty(c("cols", "rows")) %>%  # quita las columnas y filas en blanco
  clean_names() %>% select("codigo", "costo_total_bs", "costo_cemento_bs", "costo_aditivo_bs", "costo_total_bs_m3",
                           "costo_3_4_bs_m3", "costo_3_8_bs_m3", "costo_arena_bs_m3", 
                           "costo_cemento_bs_m3","costo_aditivo_bs_m3", "observaciones", "tipo_cemento", "cemento_kg_m3", 
                           "tipo_aditivo","aditivos_planta_kg", "aditivos_obra_kg")



# temp_prod <- produccion %>% clean_names() %>% select("codigo", "costo_total_bs", 
#                                                      "costo_total_bs_m3", "observaciones", "tipo_cemento")

#Creacion tabla general con margen  
margen <- left_join(ventas, produccion, by = "codigo") %>% 
  mutate(margen_bruto = precio_total_venta_bs - costo_total_bs)
margen <- margen %>% mutate(margen_bs_m3 = precio_bs_m3 - costo_total_bs_m3,
                            porcentaje_margen = margen_bruto / precio_total_venta_bs,
                            fecha = as.Date(fecha),
                            tipo = substr(tipo,1,4),
                            margen_bs_m3 = round(margen_bs_m3,2),
                            costo_total_bs_m3 = round(costo_total_bs_m3,2))

#margen %>% rio::export(., "margen.xlsx")

#Tabla de info aditivos                          
adit <- margen %>%  
  select("codigo", "fecha","costo_total_bs", "costo_total_bs_m3", "costo_aditivo_bs", "tipo_aditivo", "costo_aditivo_bs_m3", 
         "aditivos_planta_kg", "aditivos_obra_kg", "volumen_m3") %>% 
  mutate(aditivos_obra_kg = as.numeric(aditivos_obra_kg),
         aditivos_planta_kg = as.numeric(aditivos_planta_kg),
         aditivo_planta_bs = ((costo_aditivo_bs/(aditivos_planta_kg + aditivos_obra_kg)))*aditivos_planta_kg,
         aditivo_obra_bs = ((costo_aditivo_bs/(aditivos_planta_kg + aditivos_obra_kg)))*aditivos_obra_kg,
         aditivo_planta_bs_m3 = ((costo_aditivo_bs_m3/(aditivos_planta_kg + aditivos_obra_kg)))*aditivos_planta_kg ,
         aditivo_obra_bs_m3 = ((costo_aditivo_bs_m3/(aditivos_planta_kg + aditivos_obra_kg)))*aditivos_obra_kg) %>% 
  filter(!is.na(aditivos_obra_kg))


#Tabla Vertical con costos por aditivo  obra vs planta 
tabla_4 <- adit %>% select("codigo", "fecha", "tipo_aditivo", 
                           "aditivo_planta_bs", "aditivo_obra_bs", "volumen_m3") %>% 
  gather(lugar_aditivo, costo_bs, -fecha, -codigo, - tipo_aditivo, -volumen_m3)


#Tabla Vertical con cantidades por aditivo  obra vs planta 
tabla_5 <- adit %>% select("codigo", "fecha", "tipo_aditivo", 
                           "aditivos_planta_kg", "aditivos_obra_kg") %>% 
  gather(lugar_aditivo, aditivo_kg, -fecha, -codigo, - tipo_aditivo)

temp1 <- tabla_4 %>% filter(!is.na(costo_bs)) %>% 
  group_by(fecha = month(fecha), tipo_aditivo, lugar_aditivo) %>% 
  summarise(costo_bs = sum(costo_bs),
            volumen_m3 = sum(volumen_m3))
temp1$lugar_aditivo %<>%
  gsub("aditivo_planta_bs", "planta",.) %>% 
  gsub("aditivo_obra_bs","obra",.)

temp2 <- tabla_5 %>% filter(!is.na(aditivo_kg)) %>% 
  group_by(fecha = month(fecha), tipo_aditivo, lugar_aditivo) %>% 
  summarise(aditivo_kg = sum(aditivo_kg))
temp2$lugar_aditivo %<>%
  gsub("aditivos_planta_kg", "planta",.) %>% 
  gsub("aditivos_obra_kg","obra",.)

tabla_6 <- left_join(temp1, temp2, by = c("fecha", "tipo_aditivo", "lugar_aditivo")) %>% 
  mutate(costo_bs_aditivo = round(costo_bs,2),
         aditivo_kg = round(aditivo_kg,2)) %>%  select(-costo_bs)

temp1 <- tabla_4 %>% filter(!is.na(costo_bs)) %>% 
  group_by(fecha = month(fecha), lugar_aditivo) %>% 
  summarise(costo_bs = sum(costo_bs),
            volumen_m3 = sum(volumen_m3))
temp1$lugar_aditivo %<>%
  gsub("aditivo_planta_bs", "planta",.) %>% 
  gsub("aditivo_obra_bs","obra",.)

temp2 <- tabla_5 %>% filter(!is.na(aditivo_kg)) %>% 
  group_by(fecha = month(fecha), lugar_aditivo) %>% 
  summarise(aditivo_kg = sum(aditivo_kg))
temp2$lugar_aditivo %<>%
  gsub("aditivos_planta_kg", "planta",.) %>% 
  gsub("aditivos_obra_kg","obra",.)


tabla_7 <- left_join(temp1, temp2, by = c("fecha", "lugar_aditivo")) %>% 
  mutate(costo_bs_aditivo = round(costo_bs,2),
         aditivo_kg = round(aditivo_kg,2)) %>%  select(-costo_bs)

remove(temp1,temp2)

