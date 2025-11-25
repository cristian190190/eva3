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

# eliminar columna "ID region"

# cambiar el nombre del campo "Variedad / Tipo" a "Variedad_tipo"

# en el campo "Variedad_tipo", eliminar "(o)" en los valores

# en el campo "Unidad de comercializacion", eliminar "$/" en los valores

# en los campos "Precio minimo", "Precio maximo" y "Precio promedio", remplazar "," por "." y convertir a numérico



