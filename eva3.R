file.choose()
install.packages("dplyr")
install.packages("stringr")
install.packages("readxl")
install.packages("ggplot2")
install.packages("tidyr")


library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

rutas = c("D:\\6to semestre\\dataminin\\eva3\\16.xlsx",
          "D:\\6to semestre\\dataminin\\eva3\\17.xlsx",
          "D:\\6to semestre\\dataminin\\eva3\\18.xlsx",
            "D:\\6to semestre\\dataminin\\eva3\\19.xlsx",
            "D:\\6to semestre\\dataminin\\eva3\\20.xlsx",
            "D:\\6to semestre\\dataminin\\eva3\\21.xlsx",
            "D:\\6to semestre\\dataminin\\eva3\\22.xlsx",
            "D:\\6to semestre\\dataminin\\eva3\\23.xlsx",
            "D:\\6to semestre\\dataminin\\eva3\\24.xlsx",
            "D:\\6to semestre\\dataminin\\eva3\\25.xlsx")

# Leer todos los archivos y combinarlos

df_completo <- lapply(rutas, read_excel) %>% 
  bind_rows()


# Ver el resultado

print(dim(df_completo))
head(df_completo)
columnas = names(df_completo)
print(columnas)

# revisar valores null o n/a

sum(is.null(df_completo))
sum(is.na(df_completo))


df_limpieza = df_completo

# eliminar columna "ID region"

df_limpieza = df_limpieza %>% select(-`ID region`)

# cambiar el nombre del campo "Variedad / Tipo" a "Variedad_tipo", "Unidad de comercializacion" a "Unidad_comercializacion", "Precio minimo" a "Precio_min", "Precio maximo" a "Precio_max" y "Precio promedio" a "Precio_prom"

df_limpieza = df_limpieza %>%
  rename(
    Variedad_tipo = `Variedad / Tipo`,
    Unidad_comercializacion = `Unidad de comercializacion`,
    Precio_min = `Precio minimo`,
    Precio_max = `Precio maximo`,
    Precio_prom = `Precio promedio`
  )

# comvertir columnas de precios y volumen a numérico

df_limpieza = df_limpieza %>%
  mutate(
    Volumen = as.numeric(Volumen),
    Precio_min = as.numeric(Precio_min),
    Precio_max = as.numeric(Precio_max),
    Precio_prom = as.numeric(Precio_prom)
  )

# crear columnas año, mes y día a partir de la columna "Fecha"

df_limpieza <- df_limpieza %>%
  mutate(
    Fecha = as.Date(Fecha, format = "%Y-%m-%d") 
  )
df_limpieza = df_limpieza %>%
  mutate(
    Año = as.numeric(format(Fecha, "%Y")),
    Mes = as.numeric(format(Fecha, "%m")),
    Día = as.numeric(format(Fecha, "%d"))
  )

# en el campo "Variedad_tipo", eliminar "(o)" en los valores

df_limpieza = df_limpieza %>%
  mutate(
    Variedad_tipo = str_replace_all(Variedad_tipo, "\\(o\\)", "")
  )

# crear columna binaria de nacional/extranjero a partir de la columna "Origen"

origenes = unique(df_limpieza$Origen)
print(origenes)

extranjero = c(
  "China",
  "Ecuador",
  "Perú",
  "Bolivia",
  "Colombia",
  "Panamá",
  "EE.UU.",
  "Brasil",
  "México",
  "Costa Rica",
  "Argentina",
  "Italia",
  "Guatemala",
  "Paraguay",
  "Importada(o)"
)

#crear columna binaria llamada "Nacional_extranjero" teniendo como valores 1 para nacional y 0 para extranjero
df_limpieza = df_limpieza %>%
  mutate(
    Nacional_extranjero = ifelse(Origen %in% extranjero, 0, 1)
  )
# crear 2 dataframes de ("Frutas"/"Hortalizas") a partir de la columna "Subsector"
catergorias = unique(df_limpieza$Subsector)
print(catergorias)

df_frutas = df_limpieza %>%
  filter(Subsector == "Frutas")
df_hortalizas = df_limpieza %>%
  filter(Subsector == "Hortalizas y tubérculos")

# crear columna de rangos de precios juntando los datos de "Precio_min" y "Precio_max". ejemplo: columna minimo "399.9999" columna maximo "419.9999"


df_limpieza = df_limpieza %>%
  mutate(
    Rango_precios = paste0(round(Precio_min,2), " - ", round(Precio_max,2))
  )

# en el campo "Unidad de comercializacion", eliminar "$/"... en los valores (el simbolo está siempre al inicio)

df_limpieza = df_limpieza %>%
  mutate(
    Unidad_comercializacion = str_replace_all(Unidad_comercializacion, "^\\$/", "")
  )


#ver distintos datos de la columna "Unidad_comercializacion"

unidades = unique(df_limpieza$Unidad_comercializacion)
print(unidades)

# del dataframe de frutas y hortalizas mostrar todas las frutas y hortalizas sin repetir los nombres y tambien que me muestre las columnas de "Subsector" y "Unidad_comercializacion"

frutas_unicas = df_frutas %>%
  select(Producto, Subsector, Unidad_comercializacion) %>%
  distinct()
hortalizas_unicas = df_hortalizas %>%
  select(Producto, Subsector, Unidad_comercializacion) %>%
  distinct()


#crear dataframes finales de 5 frutas ('Mora', 'Tumbo', 'Platano', 'Maracuya', 'Breva') y 5 hortalizas ('Ciboulette', 'Coliflor', 'Orégano', 'Bruselas (repollito)', 'Repollo') 

df_5_frutas = df_limpieza %>%
  filter(Producto %in% c('Mora', 'Tumbo', 'Plátano', 'Maracuyá', 'Breva'))

df_5_hortalizas = df_limpieza %>%
  filter(Producto %in% c('Ciboulette', 'Coliflor', 'Orégano', 'Bruselas (repollito)', 'Repollo'))


# crear una columna "Volumen_anual" que sea la suma de todos los volúmenes por año para cada producto y otra columna "Precio_promedio_anual" que sea el promedio de los precios promedios por año para cada producto en los dataframes de frutas y hortalizas finales

df_5_frutas = df_5_frutas %>%
  group_by(Producto,Unidad_comercializacion, Año) %>%
  mutate(
    Volumen_anual = sum(Volumen, na.rm = TRUE),
    Precio_promedio_anual = mean(Precio_prom, na.rm = TRUE)
  ) %>%
  ungroup()

df_5_hortalizas = df_5_hortalizas %>%
  group_by(Producto,Unidad_comercializacion, Año) %>%
  mutate(
    Volumen_anual = sum(Volumen, na.rm = TRUE),
    Precio_promedio_anual = mean(Precio_prom, na.rm = TRUE)
  ) %>%
  ungroup()

# crear columna "Variacio_porcentual_año" en el año 2016 deberia ser 0% y en los años siguientes el porcentaje de VARIACION DE PRECIOS respecto al año anterior para cada producto en los dataframes de frutas y hortalizas finales

df_5_frutas = df_5_frutas %>%
  arrange(Producto,Unidad_comercializacion, Año) %>%
  group_by(Producto, Unidad_comercializacion) %>%
  mutate(
    V_porcentual_año = ifelse(Año == 2016, 0, (Precio_promedio_anual - lag(Precio_promedio_anual)) / lag(Precio_promedio_anual) * 100)
  ) %>%
  ungroup()

df_5_hortalizas = df_5_hortalizas %>%
  arrange(Producto,Unidad_comercializacion, Año) %>%
  group_by(Producto, Unidad_comercializacion) %>%
  mutate(
    V_porcentual_año = ifelse(Año == 2016, 0, (Precio_promedio_anual - lag(Precio_promedio_anual)) / lag(Precio_promedio_anual) * 100)
  ) %>%
  ungroup()


# crear una view que muestre solo las columnas de "Producto", "Unidad_comercializacion", "Año" y "Variacion_porcentual_año" sin repetir filas en los dataframes de frutas y hortalizas finales

view_frutas = df_5_frutas %>%
  select(Producto, Unidad_comercializacion, Año, V_porcentual_año) %>%
  distinct()
view_hortalizas = df_5_hortalizas %>%
  select(Producto, Unidad_comercializacion, Año, V_porcentual_año) %>%
  distinct()

View(view_frutas)
View(view_hortalizas)

# a los valores n/a en la columna "Variacion_porcentual_año" asignarles el valor de 0 en los dataframes de frutas y hortalizas finales ya que no hay variacion en el primer año

df_5_frutas = df_5_frutas %>%
  mutate(
    V_porcentual_año = ifelse(is.na(V_porcentual_año), 0, V_porcentual_año)
  )

df_5_hortalizas = df_5_hortalizas %>%
  mutate(
    V_porcentual_año = ifelse(is.na(V_porcentual_año), 0, V_porcentual_año)
  )
  
#B-------------------------------------------------------------------------------------------

resumen_descriptivo <- function(df, categoria) {
  df_resumen <- df %>%

    group_by(Producto, Unidad_comercializacion) %>%
    summarise(

      Media_Precio_Anual = mean(Precio_promedio_anual, na.rm = TRUE),
      Mediana_Precio_Anual = median(Precio_promedio_anual, na.rm = TRUE),
      Min_Precio_Anual = min(Precio_promedio_anual, na.rm = TRUE),
      Max_Precio_Anual = max(Precio_promedio_anual, na.rm = TRUE),
      Rango_Precio_Anual = Max_Precio_Anual - Min_Precio_Anual,
      

      Media_Variacion_Precio = mean(V_porcentual_año[V_porcentual_año != 0], na.rm = TRUE),

      Rango_Variacion_Precio = max(V_porcentual_año[V_porcentual_año != 0], na.rm = TRUE) - 
        min(V_porcentual_año[V_porcentual_año != 0], na.rm = TRUE),
      

      Percentil_25_Precio = quantile(Precio_promedio_anual, 0.25, na.rm = TRUE),
      
      .groups = 'drop'
    ) %>%

    mutate(Categoria = categoria)
  
  return(df_resumen)
}

resumen_frutas <- resumen_descriptivo(df_5_frutas, "Frutas")
resumen_hortalizas <- resumen_descriptivo(df_5_hortalizas, "Hortalizas")

resumen_combinado <- bind_rows(resumen_frutas, resumen_hortalizas)

print(resumen_combinado)

write.csv2(resumen_combinado, "C:/Users/ferna/OneDrive/Escritorio/I.I/3ro/2SM/Mineria/RESUMEN.csv", row.names = FALSE)

# Tablas de frecuencia para la columna Unidad_comercializacion
frecuencia_unidades <- bind_rows(df_5_frutas, df_5_hortalizas) %>%
  group_by(Subsector, Producto, Unidad_comercializacion) %>%
  summarise(
    Total_Registros = n(),
    .groups = 'drop'
  ) %>%
  arrange(Subsector, Producto, desc(Total_Registros))

print("### Tablas de Frecuencia de Unidades de Comercialización ###")
print(frecuencia_unidades)


# C ------------------------------------
#REGRESION LINEAL PARA LA FRUTA MORA
df_regresion_mora <- df_5_frutas %>%
  filter(Producto == 'Mora' & Unidad_comercializacion == 'bandeja 2 kilos') %>%
  distinct(Año, .keep_all = TRUE)

ggplot(df_regresion_mora, aes(x = Año, y = Precio_promedio_anual)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(
    method = "lm", 
    se = TRUE, 
    color = "blue", 
    linetype = "dashed"
  ) +

  labs(
    title = "Regresión Lineal: Tendencia de Precios Anuales de la Mora (Unidad: Bandeja 2 kilos)",
    x = "Año",
    y = "Precio Promedio Anual ($)",
    caption = "Fuente: Elaboración propia, fluctuación precio de la Mora 2016-2025"
  ) +
  
  scale_x_continuous(breaks = unique(df_regresion_mora$Año)) +
  theme_gray()

#REGRESION LINEAL PARA LA HORTALIZA REPOLLO

df_regresion_repollo <- df_5_hortalizas %>%
  filter(Producto == 'Repollo' & Unidad_comercializacion == 'unidad') %>%
  distinct(Año, .keep_all = TRUE)

ggplot(df_regresion_repollo, aes(x = Año, y = Precio_promedio_anual)) +
  geom_point(color = "red", size = 3) +
  geom_smooth(
    method = "lm", 
    se = TRUE, 
    color = "blue", 
    linetype = "dashed"
  ) +
  
  labs(
    title = "Regresión Lineal: Tendencia de Precios Anuales del Repollo (Unidad: unidad)",
    x = "Año",
    y = "Precio Promedio Anual ($)",
    caption = "Fuente: Elaboración propia, fluctuación precio del Repollo 2016-2025"
  ) +
  
  scale_x_continuous(breaks = unique(df_regresion_repollo$Año)) +
  theme_gray()

# D --------------------------------------------
# Gráfico 1: Gráfico de Precio Promedio de Mora en Base al Origen

df_maracuya_origen_volumen <- df_5_frutas %>%
  filter(Producto == 'Maracuyá') %>%
  mutate(
    Origen_Etiqueta = case_when(
      Nacional_extranjero == 1 ~ "Nacional",
      Nacional_extranjero == 0 ~ "Extranjero",
      TRUE ~ "No Definido" 
    )
  ) %>%
  
  group_by(Origen_Etiqueta) %>%
  summarise(
    Volumen_Total_Acumulado = sum(Volumen_anual, na.rm = TRUE),
    .groups = 'drop'
  )
ggplot(df_maracuya_origen_volumen, aes(x = Origen_Etiqueta, y = Volumen_Total_Acumulado, fill = Origen_Etiqueta)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(Volumen_Total_Acumulado, big.mark = ".")), vjust = -0.5, size = 4) + 
  labs(
    title = "Volumen Total de la Maracuyá: Comparación por Origen",
    x = "Origen del Producto",
    y = "Volumen Total Acumulado (Unidades)", 
    fill = "Origen",
    caption = "Fuente: Elaboración propia, Volumen Total de Maracuyá (Extranjero Vs Nacional) 2016-2025"
  ) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M"))
  theme_gray()

# Gráfico 2: Top 3 Frutas (2020 a 2025)
  
  df_frutas_top_periodo <- df_5_frutas %>%
    filter(Año >= 2020 & Año <= 2025) %>%
    group_by(Producto) %>%
    summarise(
      Volumen_Total_Periodo = sum(Volumen_anual, na.rm = TRUE), 
      .groups = 'drop'
    ) %>%
    arrange(desc(Volumen_Total_Periodo)) %>%
    slice_head(n = 3)
  
  ggplot(df_frutas_top_periodo, aes(x = reorder(Producto, Volumen_Total_Periodo), y = Volumen_Total_Periodo, fill = Producto)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::comma(Volumen_Total_Periodo, big.mark = ".")), vjust = -0.5, size = 4) + 
    labs(
      title = "Top 3 Frutas por Volumen Total Acumulado (2020 - 2025)",
      x = "Fruta",
      y = "Volumen Total Acumulado",
      fill = "Fruta",
      caption = "Fuente: Elaboración propia, Top 3 frutas (Volumen) 2020-2025"
    ) +
    scale_y_continuous(labels = scales::label_comma(big.mark = ".")) +
    theme_minimal()
  
  
# Grafico 3: Top 3 Hortalizas (Vega Central Mapocho, 2025)

df_hortalizas_top_vega <- df_5_hortalizas %>%
    filter(
      Año == 2025,
      Mercado == "Vega Central Mapocho de Santiago" 
    ) %>%
    group_by(Producto) %>%
    summarise(
      Volumen_Total_Mercado = sum(Volumen_anual, na.rm = TRUE), 
      .groups = 'drop'
    ) %>%
    arrange(desc(Volumen_Total_Mercado)) %>%
    slice_head(n = 3)

  ggplot(df_hortalizas_top_vega, aes(x = reorder(Producto, Volumen_Total_Mercado), y = Volumen_Total_Mercado, fill = Producto)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = scales::comma(Volumen_Total_Mercado, big.mark = ".")), vjust = -0.5, size = 4) + 
    labs(
      title = "Top 3 Hortalizas Comercializadas en Vega Central Mapocho (2025)",
      x = "Hortaliza",
      y = "Volumen Total en Mercado",
      fill = "Hortaliza",
      caption = "Fuente: Elaboración propia, Top 3 Hortalizas Comercializadas 2025 (Vega Central Mapocho de Santiago)"
    ) +
    scale_y_continuous(
      labels = scales::label_comma(big.mark = "."),
      expand = expansion(mult = c(0, 0.1)) 
    ) +
    theme_gray()
  
# Grafico 4: Top 3 hortalizas mas comercializadas entre 2016-2018 (PORCENTAJE)
  
df_vol_prod_perc <- df_5_hortalizas %>%
    filter(Año >= 2016 & Año <= 2018) %>%
    group_by(Producto, Subsector) %>%
    summarise(Volumen_Total = sum(Volumen_anual, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(Volumen_Total)) %>%
    slice_head(n = 3) %>% 
    mutate(
      Porcentaje = Volumen_Total / sum(Volumen_Total),
      Etiqueta_Posicion = cumsum(Porcentaje) - 0.5 * Porcentaje 
    )
ggplot(df_vol_prod_perc, aes(x = "", y = Porcentaje, fill = Producto)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) + 
    geom_text(aes(y = Etiqueta_Posicion, label = scales::percent(Porcentaje, accuracy = 0.1)), color = "black", size = 4) +
    
    labs(
      title = "TOP 3: Hortalizas más comercializadas (Porcentual, 2016-2018)",
      x = NULL,
      y = NULL,
      fill = "Producto",
      caption = "Fuente: Elaboración Propia, Top 3 Hortalizas más Comercializadas (Volumen, Porcentaje 2016-2018) "
    ) +
    theme_void()

#Gráfico 5: precio minimo de las 5 hortalizas en el año 2024

df_2024_hortalizas <- df_5_hortalizas %>%
  filter(Año == 2024) %>%
  group_by(Producto, Unidad_comercializacion) %>%
  summarise(
    Precio_min_2024 = min(Precio_min, na.rm = TRUE),
    .groups = 'drop'
  )
ggplot(df_2024_hortalizas, aes(x = Producto, y = Precio_min_2024, fill = Unidad_comercializacion)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Precio_min_2024), position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Precio Mínimo de Hortalizas en 2024 por Unidad de Comercialización",
    x = "Hortaliza",
    y = "Precio Mínimo ($)",
    fill = "Unidad de Comercialización",
    caption = "Fuente: Elaboración propia, precios mínimos de hortalizas en 2024"
  ) +
  theme_minimal()

#Grafico 6: Distribución porcentual del volumen (Frutas vs. Hortalizas) 2021

df_frutas_2021 <- df_5_frutas %>%
  filter(Año == 2021) %>%
  summarise(Volumen_Total = sum(Volumen_anual, na.rm = TRUE)) %>%
  mutate(Categoria = "Frutas")

df_hortalizas_2021 <- df_5_hortalizas %>%
  filter(Año == 2021) %>%
  summarise(Volumen_Total = sum(Volumen_anual, na.rm = TRUE)) %>%
  mutate(Categoria = "Hortalizas")

df_vol_categoria_2021 <- bind_rows(df_frutas_2021, df_hortalizas_2021)
df_vol_categoria_2021_perc <- df_vol_categoria_2021 %>%
  mutate(
    Porcentaje = Volumen_Total / sum(Volumen_Total),
    Etiqueta_Posicion = cumsum(Porcentaje) - 0.5 * Porcentaje
  )
ggplot(df_vol_categoria_2021_perc, aes(x = "", y = Porcentaje, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = Etiqueta_Posicion, label = scales::percent(Porcentaje, accuracy = 0.1)), color = "black", size = 5) +
  
  labs(
    title = "Distribución porcentual del volumen (Frutas vs. Hortalizas) en 2021",
    x = NULL,
    y = NULL,
    fill = "Categoría",
    caption = "Fuente: Elaboracion Propia, distribución porcentual del volumen total de las 5 frutas y 5 hortalizas en el año 2021. "
  ) +
  theme_void()

#Gráfico 7:  Precio Promedio de la Mora por Región en 2018

df_mora_region <- df_5_frutas %>%
  filter(Producto == 'Mora' & Año == 2018) %>%
  group_by(Region) %>%
  summarise(
    Precio_Prom_Region = mean(Precio_promedio_anual, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(Precio_Prom_Region))

ggplot(df_mora_region, aes(x = reorder(Region, Precio_Prom_Region), y = Precio_Prom_Region, fill = Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(Precio_Prom_Region, big.mark = ".")), hjust = -0.1, size = 4) +
  labs(
    title = "Precio Promedio de la Mora por Región (Año 2018)",
    x = "Región",
    y = "Precio Promedio Anual ($)",
    fill = "Región",
    caption = "Fuente: Elaboración Propia, Comparación del precio promedio de la Mora por región en el año 2018"
  ) +
  coord_flip() +
  scale_y_continuous(labels = scales::label_comma(big.mark = "."), expand = expansion(mult = c(0, 0.1))) +
  theme_minimal()

#Gráfico 8: cantidad de repollo en el año 2020 en base a la calidad

df_repollo_2020 <- df_5_hortalizas %>%
  filter(Producto == 'Repollo' & Año == 2020) %>%
  group_by(Calidad) %>%
  summarise(
    Total_Volumen = sum(Volumen, na.rm = TRUE),
    .groups = 'drop'
  )
ggplot(df_repollo_2020, aes(x = Calidad, y = Total_Volumen, fill = Calidad)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(Total_Volumen, big.mark = ".")), vjust = -0.5, size = 3.5) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)), 
                     labels = scales::label_comma(big.mark = ".")) + 
  coord_cartesian(clip = "off") +
  labs(
    title = "Volumen Total de Repollo por Calidad en 2020",
    x = "Calidad",
    y = "Volumen Total",
    caption = "Fuente: Elaboración Propia, Volumen total de Repollo por calidad en el año 2020"
  ) +
  theme_minimal()


#Gráfico 9: Precio Máximo de Ciboulette por Región en 2025

df_ciboulette_2025 <- df_5_hortalizas %>%
  filter(Producto == 'Ciboulette' & Año == 2025) %>%
  group_by(Region) %>%
  summarise(
    Precio_max = max(Precio_max, na.rm = TRUE)
  ) %>%
  ungroup()

ggplot(df_ciboulette_2025, aes(x = reorder(Region, -Precio_max), y = Precio_max, fill = Region)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(Precio_max, big.mark = ".")), 
            vjust = -0.5, 
            size = 3.5) + 
  labs(
    title = "Precio Máximo de Ciboulette por Región en 2025",
    x = "Región",
    y = "Precio Máximo ($)",
    caption = "Fuente: Elaboración Propia, Precio Máximo de Ciboulette (Región, 2025)"
  ) +
  scale_y_continuous(labels = NULL, expand = expansion(mult = c(0, 0.15))) + 
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Gráfico 10: Precio Máximo de Bruselas (repollito) por Unidad de Comercialización en 2025

df_bruselas_2025 <- df_5_hortalizas %>%
  filter(Producto == 'Bruselas (repollito)' & Año == 2025) %>%
  group_by(Unidad_comercializacion) %>%
  summarise(
    Precio_max = max(Precio_max, na.rm = TRUE)
  ) %>%
  ungroup()
ggplot(df_bruselas_2025, aes(x = reorder(Unidad_comercializacion, -Precio_max), y = Precio_max)) +
  geom_bar(stat = "identity", aes(fill = Unidad_comercializacion)) +
  geom_text(aes(label = scales::comma(Precio_max, big.mark = ".")), 
            vjust = -0.5, 
            color = "black", 
            size = 3.5) +
  labs(
    title = "Precio Máximo de Bruselas (repollito) por Unidad de Comercialización en 2025",
    x = "Unidad de Comercialización",
    y = "Precio Máximo ($)",
    caption = "Fuente: Elaboración Propia, Precios máximos de Bruselas (repollito) en 2025.",
    fill = "Unidad" 
  ) +
  scale_y_continuous(labels = scales::label_comma(big.mark = "."), 
                     expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5) 
  )


