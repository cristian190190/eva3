file.choose()
install.packages("dplyr")
install.packages("stringr")
install.packages("readxl")

library(readxl)
library(stringr)
library(dplyr)

rutas = c("C:\\Users\\ferna\\OneDrive\\Escritorio\\I.I\\3ro\\2SM\\Mineria\\EVA3\\eva3\\16.xlsx",
          "C:\\Users\\ferna\\OneDrive\\Escritorio\\I.I\\3ro\\2SM\\Mineria\\EVA3\\eva3\\17.xlsx",
          "C:\\Users\\ferna\\OneDrive\\Escritorio\\I.I\\3ro\\2SM\\Mineria\\EVA3\\eva3\\18.xlsx",
            "C:\\Users\\ferna\\OneDrive\\Escritorio\\I.I\\3ro\\2SM\\Mineria\\EVA3\\eva3\\19.xlsx",
            "C:\\Users\\ferna\\OneDrive\\Escritorio\\I.I\\3ro\\2SM\\Mineria\\EVA3\\eva3\\20.xlsx",
            "C:\\Users\\ferna\\OneDrive\\Escritorio\\I.I\\3ro\\2SM\\Mineria\\EVA3\\eva3\\21.xlsx",
            "C:\\Users\\ferna\\OneDrive\\Escritorio\\I.I\\3ro\\2SM\\Mineria\\EVA3\\eva3\\22.xlsx",
            "C:\\Users\\ferna\\OneDrive\\Escritorio\\I.I\\3ro\\2SM\\Mineria\\EVA3\\eva3\\23.xlsx",
            "C:\\Users\\ferna\\OneDrive\\Escritorio\\I.I\\3ro\\2SM\\Mineria\\EVA3\\eva3\\24.xlsx",
            "C:\\Users\\ferna\\OneDrive\\Escritorio\\I.I\\3ro\\2SM\\Mineria\\EVA3\\eva3\\25.xlsx")

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

# eliminar columna "ID region"

# cambiar el nombre del campo "Variedad / Tipo" a "Variedad_tipo", "ID region" a "ID_region", "Unidad de comercializacion" a "Unidad_comercializacion", "Precio minimo" a "Precio_min", "Precio maximo" a "Precio_max" y "Precio promedio" a "Precio_prom"

# comvertir columnas de precios y volumen a numérico

# crear columnas año, mes y día a partir de la columna "Fecha"
df_completo <- df_completo %>%
  mutate(
    Fecha = as.Date(Fecha, format = "%Y-%m-%d") 
  )
df_completo <- df_completo %>%
  mutate(
    Año = format(Fecha, "%Y"), 
    Mes = format(Fecha, "%m"), 
    Dia = format(Fecha, "%d")  
  )
head(df_completo[, c("Fecha", "Año", "Mes", "Dia")])


# en el campo "Variedad_tipo", eliminar "(o)" en los valores

# crear columna binaria de nacional/extranjero a partir de la columna "Origen"

# crear columna agrupadora de categoria (fruta/hortaliza) a partir de la columna "Subsector"

# crear columna de rangos de precios con "Precio_min" y Precio_max

# en el campo "Unidad de comercializacion", eliminar "$/" en los valores

# en los campos "Precio_prom", "Precio_max" y "Precio_min", remplazar "," por "." y convertir a numérico


