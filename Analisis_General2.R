source("datos_analisis.R")

#Graficar Margen Produccion
graf_1 <- margen %>% filter(margen_bruto > 0) %>%  ggplot(aes(fecha, margen_bruto)) +
  geom_point(aes(color = tipo),size = 3, alpha = 0.5) + geom_smooth() +
  xlab("Fecha") + scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  ylab("Bs/mixer") + ggtitle("Margen Bruto por Mixer") +
  theme_ipsum_tw()

#Graficar Margen por m3 según tipo
graf_2 <- margen %>% filter(margen_bs_m3 > 0) %>%  ggplot(aes(fecha, margen_bs_m3)) + 
  geom_smooth() +
  geom_point(aes(color = tipo),size = 3, alpha = 0.5) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  xlab("Fecha") + ylab("Bs/m3") + ggtitle("Margen Bruto por m3") +
  theme_ipsum_tw()

# graf_2 <- ggplotly(graf_2)

graf_2.1 <- margen %>% filter(margen_bs_m3 > 0) %>%
  hchart(type = "scatter", hcaes(x = fecha, y = margen_bs_m3, group = tipo, alpha = 0.5)) %>%
  hc_title(text = "Margen Bruto por M3") %>%
  hc_yAxis(title = list(text = "Bs/M3")) %>%
  hc_xAxis(title = list(text = "Fecha")) 

# hc_add_theme(hc_theme_538()) %>%


tabla_1 <- margen %>% filter(margen_bs_m3 > 0) %>% group_by(tipo) %>%  
  summarize(cantidad=n()) 

#Pie Chart de Tipos
graf_3 <- tabla_1 %>%  
  mutate(per = round(cantidad/sum(cantidad)*100,2)) %>% 
  ggplot(aes(x="", y = cantidad, fill = tipo)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) + 
  geom_text_repel(aes(label = paste(per, "%")), position = position_stack(vjust = 0.5)) + 
  xlab("") +
  ggtitle("Distribución por tipo de Hormigón")


  
#Identificar margenes negativos
tabla_2 <- margen %>% filter(margen_bs_m3 < 0) %>% 
  select("fecha", "vcd", "codigo", "volumen_m3", "precio_total_venta_bs", "cliente", "observaciones") 

#Density Plot Graficar Margen por m3
graf_4 <- margen %>% filter(margen_bs_m3 > 0, tipo %in% c("H-21", "H-25")) %>%  
  ggplot(aes(margen_bs_m3,color = tipo, fill = tipo)) + 
  geom_histogram(aes(y = ..density..), alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.1) +
  ylab("distribucion") + xlab("Bs/m3") +
  ggtitle("Distribución Margen Bruto Bs/m3")


# graf_4.1 <- margen %>% filter(margen_bs_m3 > 0, tipo %in% c("H-21", "H-25")) %>% 
#   hchart(margen$margen_bs_m3, color = tipo, fill = tipo, name = "Distribución Margen Bruto Bs/m3")
  

#Graficar Margen por m3 según tipo cemento


graf_5 <- margen %>% filter(margen_bs_m3 > 0) %>%  ggplot(aes(fecha, margen_bs_m3)) +
  geom_smooth() +
  geom_point(aes(color = tipo_cemento),size = 3, alpha = 0.5) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  xlab("Fecha") + ylab("Bs/m3") + ggtitle("Margen Bruto por m3") +
  theme_ipsum_tw()


graf_5.1 <- margen %>% filter(margen_bs_m3 > 0) %>%
  hchart(type = "scatter", hcaes(x = fecha, y = margen_bs_m3, group = tipo_cemento, alpha = 0.5)) %>%
  hc_title(text = "Margen Bruto por m3") %>%
  hc_yAxis(title = list(text = "Bs/M3")) %>%
  hc_xAxis(title = list(text = "Fecha"))
#hc_add_theme(hc_theme_538()) %>%





graf_6 <- margen %>% ggplot(aes(fecha, costo_total_bs_m3)) + 
  geom_smooth() +
  geom_point(aes(color = tipo_cemento),size = 3, alpha = 0.5) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  xlab("Fecha") + ylab("Bs/m3") + ggtitle("Costo Producción por m3") +
  theme_ipsum_tw()

graf_6.1 <- margen %>% filter(costo_total_bs_m3 > 0) %>% 
  hchart(type = "scatter", hcaes(x = fecha, y = costo_total_bs_m3, group = tipo_cemento, alpha = 0.5)) %>%
  hc_title(text = "Costo Produccion por m3") %>%
  hc_yAxis(title = list(text = "Bs/M3")) %>%
  hc_xAxis(title = list(text = "Fecha"))


graf_7 <- margen %>% filter(tipo_cemento %in% c("SOBOCE IP-40", "ECEBOL IP-40")) %>%  
  ggplot(aes(fecha, costo_total_bs_m3)) + 
  geom_smooth() +
  geom_point(aes(color = cemento_kg_m3),size = 3, alpha = 0.5) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  facet_grid(tipo_cemento~.) +
  xlab("Fecha") + ylab("Bs/m3") + ggtitle("Costo Producción por m3 - Cantidad de Cemento/m3") +
  theme_ipsum_tw()


#Grafico Costo Produccion por item

prod_soboce <- margen %>%  filter(tipo_cemento %in% "SOBOCE IP-40") %>% 
  select("codigo", "fecha", "volumen_m3", "costo_3_4_bs_m3", "costo_3_8_bs_m3",
                      "costo_arena_bs_m3", "costo_cemento_bs_m3","costo_aditivo_bs_m3", "costo_total_bs_m3") %>%
  gather(item, costo_m3, -fecha, -codigo, - volumen_m3) %>% 
  mutate(costo_total = volumen_m3*costo_m3)




tabla_3 <- prod_soboce %>% filter(item != "costo_total_bs_m3", !is.na(costo_m3)) %>% 
  group_by(item) %>%  
  summarize(costo_bs= sum(costo_total)) %>% 
  mutate(per = round(costo_bs/sum(costo_bs)*100,2))

tabla_3$item %<>% gsub("_bs_m3","", .)

graf_8 <- tabla_3 %>%  
  ggplot(aes(x = "", y = costo_bs, fill = item)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste(per, "%")), position = position_stack(vjust = 0.5)) +
  xlab("") +
  ggtitle("Distribución Costos Producción")

temp <- margen %>% filter(tipo_cemento %in% "SOBOCE IP-40") %>% 
  select("codigo", "fecha", "costo_cemento_bs_m3","costo_aditivo_bs_m3", "costo_total_bs_m3") %>%
  ggplot(aes(x = fecha)) + 
  geom_point(aes(y = costo_total_bs_m3, colour = "Total"),size = 3, alpha = 0.5) + geom_smooth(aes(y = costo_total_bs_m3)) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") 
temp <- temp + geom_point(aes(y = costo_cemento_bs_m3, colour = "Cemento"),size = 3,alpha = 0.5) + 
  geom_smooth(aes(y = costo_cemento_bs_m3))
graf_9 <- temp + geom_point(aes(y = costo_aditivo_bs_m3, colour = "Aditivo"),size = 3,alpha = 0.5) + 
  geom_smooth(aes(y = costo_aditivo_bs_m3)) + 
  ggtitle("Costos Bs por m3 ")


#Aanalisis Costos Aditivos Planta Vs Atitivos Obra adit y tabla_6


#Grafico Costo bs/m3 según tipo de aditivo
graf_12 <- adit %>%  ggplot() + 
  geom_point(aes(fecha, costo_total_bs_m3, color = tipo_aditivo),size = 3, alpha = 0.5) + ggtitle("Costo bs/m3 por aditivos")

graf_12.1 <- adit %>% 
  hchart(type = "scatter", hcaes(x = fecha, y = costo_total_bs_m3, group = tipo_aditivo, alpha = 0.5)) %>%
  hc_title(text = "Costo bs/m3 segun tipo de aditivo") %>%
  hc_yAxis(title = list(text = "Bs/m3 hormigon")) %>%
  hc_xAxis(title = list(text = "Fecha"))

#Barras Costo aditivo por m3 mensual

temp <- tabla_7 %>% select("fecha", "lugar_aditivo", "volumen_m3", "costo_bs_aditivo") %>% 
  gather(what, cantidad, -fecha, -lugar_aditivo)
graf_10 <-  temp %>% 
  ggplot(aes(x = month(fecha, label=TRUE), y = cantidad)) +
  facet_grid(what~., scale = "free") + 
  geom_col(aes(fill = lugar_aditivo), data = temp[temp$what == "costo_bs_aditivo",],width = 0.8, stat = "identity") +
  geom_point(data = temp[temp$what == "volumen_m3",],size = 5, alpha = 0.5, color = "Blue") +
  xlab("Mes") + ylab("") + ggtitle("Costo Aditivo vs Volumen Mezcla")


  

#Grafico General de Aditivos
graf_11 <- adit %>%
  ggplot(aes(x = fecha)) + 
  geom_point(aes(y = costo_aditivo_bs_m3, colour = "Total Aditivo"),size = 3, alpha = 0.5) + 
  geom_smooth(aes(y = costo_aditivo_bs_m3, fill = "Total Aditivo"), alpha = 0.6) +
  geom_point(aes(y = aditivo_planta_bs_m3, colour = tipo_aditivo),size = 3,alpha = 0.5) + 
  geom_smooth(aes(y = aditivo_planta_bs_m3, fill = "Aditivo Planta"), alpha = 0.6) +
  geom_point(aes(y = aditivo_obra_bs_m3, colour = tipo_aditivo),size = 3,alpha = 0.5) + 
  geom_smooth(aes(y = aditivo_obra_bs_m3, fill = "Aditivo Obra"), alpha = 0.6) + 
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  ggtitle("Costos de Aditivo por m3 de Hormigon")
 




graf_13 <- adit %>%  ggplot() + 
  geom_col(aes(x = month(fecha, label=TRUE),y = volumen_m3, fill = "Volumen en m3"), 
           alpha = 0.5, width = 0.8, stat = "identity") +
  xlab("Mes") + ylab("Volumen m3") + ggtitle("Volumen mensual mezcla en m3")


    
#  margen %>% filter(tipo_cemento %in% "SOBOCE IP-40", tipo_aditivo %in% "SIKAPLAST2000") %>% 
#   ggplot() + geom_point(aes(x = month(fecha, label = TRUE), y = costo_total_bs_m3, colour = tipo) )
#   
# str(temp)


# 
# #Grafico Precio de Venta
# margen %>%  filter(precio_bs_m3 > 0) %>% ggplot(aes(fecha, precio_bs_m3)) +
#   geom_point(aes(col=tipo)) + geom_smooth() +
#   xlab("mes") + ylab("bs / m3") + ggtitle("Precio de Venta por m3") +
#   theme_ipsum_tw()



#Graficar Costo Materiales por m3
#Agregar Tipo Cemento y tipo aditivo
# a <- rio::import("Base de Datos 2019.xlsx", which = "Produccion") %>%
#   remove_empty(c("cols", "rows")) %>%  # quita las columnas y filas en blanco
#   clean_names() %>% select("codigo", "tipo_cemento", "tipo_aditivo")
# #Graficar Costo Cemento por m3
# costo_m3 %>% left_join(a, costo_m3, by = "codigo") %>%
#   ggplot(aes(day(fecha), .$cemento_bs_m3)) +
#   geom_point(aes(col=tipo_cemento)) + geom_smooth() + facet_grid(.~month(fecha)) +
#   xlab("mes") + ylab("bs / m3") + ggtitle("Costo Mensual Cemento por m3") +
#   theme_ipsum_tw()
# #Graficar Costo Aditivo 1 por m3
# costo_m3 %>% left_join(a, costo_m3, by = "codigo") %>%
#   ggplot(aes(day(fecha), .$aditivo_bs_m3)) +
#   geom_point(aes(col=tipo_aditivo)) + geom_smooth() + facet_grid(.~month(fecha)) +
#   xlab("mes") + ylab("bs / m3") + ggtitle("Costo Mensual Aditivo por m3") +
#   theme_ipsum_tw()
# 
# costo_m3 %>% left_join(a, costo_m3, by = "codigo") %>%
#   ggplot(aes(fecha, .$aditivo_bs_m3)) +
#   geom_point(aes(col=tipo_aditivo)) + geom_smooth() +
#   xlab("mes") + ylab("bs / m3") + ggtitle("Costo Mensual Aditivo por m3") +
#   theme_ipsum_tw()
# remove(a)
# # 
# 
# 
