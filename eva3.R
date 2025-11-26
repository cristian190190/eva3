file.choose()
install.packages("dplyr")
install.packages("stringr")
install.packages("readxl")

library(readxl)
library(stringr)
library(dplyr)

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
# crear columna agrupadora de categoria ("Frutas"/"Hortalizas") a partir de la columna "Subsector"
catergorias = unique(df_limpieza$Subsector)
print(catergorias)

df_limpieza = df_limpieza %>%
  mutate(
    Categoria = ifelse(Subsector %in% c("Frutas"), "Frutas", "Hortalizas")
  )

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

# preguntar si se pueden trabajar con 5 unidades ejemplo: KG, atado, docena de paquetes, etc y hacer un dataframe para cada tipo de unidad





