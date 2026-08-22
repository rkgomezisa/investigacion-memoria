# ==============================================================================
# SCRIPT DE PLANIFICACIÓN DE EXPERIMENTO
# Experimento: Alfonso-1
# ==============================================================================

# 1. Instalar y cargar librerías necesarias
if (!require("dplyr")) install.packages("dplyr")
if (!require("glue")) install.packages("glue")
library(dplyr)
library(glue)

# 2. Definición de Grupos Experimentales
grupos <- c(
  "Exp_RepiteContexto", 
  "Exp_NoRepiteContexto", 
  "Control_Lista1", 
  "Control_Lista2"
)

formatos <- c("Unisono", "Seriado")
sexo_sujeto <- c("Femenino", "Masculino")
sexo_investigador <- c("Femenino", "Masculino")
lugares <- c("IIPSI", "Sala_Profes")

# 3. Crear todas las combinaciones posibles de grupos y contrabalanceo
bloque_base <- expand.grid(
  Grupo_Experimental = grupos,
  Formato_Presentacion = formatos,
  Sexo_Sujeto = sexo_sujeto,
  Sexo_Investigador = sexo_investigador,
  Lugar_Evaluacion = lugares
)

# 4. Definir tamaño de muestra total
num_bloques <- 1 

matriz_experimento <- do.call(rbind, replicate(num_bloques, bloque_base, simplify = FALSE))

# 5. Ordenar sistemáticamente la tabla para facilitar el control visual
# Se ordena progresivamente por cada factor
matriz_ordenada <- matriz_experimento[
  order(
    matriz_experimento$Grupo_Experimental,
    matriz_experimento$Lugar_Evaluacion,
    matriz_experimento$Formato_Presentacion,
    matriz_experimento$Sexo_Sujeto,
    matriz_experimento$Sexo_Investigador
  ), 
]

# 6. Agregar ID sujeto
matriz_ordenada$ID_Sujeto <- sprintf("SUJ-%03d", 1:nrow(matriz_ordenada))

# 7. Reorganizar columnas
matriz_final <- matriz_ordenada[, c(
  "ID_Sujeto",
  "Grupo_Experimental",
  "Lugar_Evaluacion",
  "Formato_Presentacion",
  "Sexo_Sujeto",
  "Sexo_Investigador"
)]

# 7. Exportar el CSV
nombre_matriz <- "Matriz_Experimental_Alfonso_1"
nombre_archivo <- glue("{nombre_matriz}.csv")
write.csv(matriz_final, nombre_archivo, row.names = FALSE)
cat(glue("\n¡Matriz generada y ordenada con éxito! Se guardó como {nombre_archivo}\n"))