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



#B-------------------------------------------------------------------------------------------



