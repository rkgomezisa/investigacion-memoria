# ==============================================================================
# SCRIPT DE PLANIFICACIÓN DE EXPERIMENTO
# Experimento: Alfonso-2
nombre_experimento <- "Alfonso_2"
# ==============================================================================

# 1. Instalar y cargar librerías necesarias
if (!require("dplyr")) install.packages("dplyr")
if (!require("glue")) install.packages("glue")
if (!require("here")) install.packages("here")
library(dplyr)
library(glue)
library(here)

# 2. Definición de Grupos Experimentales
grupos <- c(
  "IIPSI", 
  "Sala_Profe", 
  "Box_Neuro"
)

sexo_sujeto <- c("Femenino", "Masculino")
sexo_investigador <- c("Femenino", "Masculino")

# 3. Crear todas las combinaciones posibles de grupos y contrabalanceo
bloque_base <- expand.grid(
  Grupo_Experimental = grupos,
  Sexo_Sujeto = sexo_sujeto,
  Sexo_Investigador = sexo_investigador
)

# 4. Definir tamaño de muestra total
num_bloques <- 3

matriz_experimento <- do.call(rbind, replicate(num_bloques, bloque_base, simplify = FALSE))

# 5. Ordenar sistemáticamente la tabla para facilitar el control visual
# Se ordena progresivamente por cada factor
matriz_ordenada <- matriz_experimento[
  order(
    matriz_experimento$Grupo_Experimental,
    matriz_experimento$Sexo_Sujeto,
    matriz_experimento$Sexo_Investigador
  ), 
]


# 5. Generar Orden de Llegada Aleatorizado independiente por Sexo_Sujeto
set.seed(123) # Cambiá o quitá este número para una nueva semilla aleatoria
# Asignar turno aleatorio dentro de Femenino y Masculino
matriz_ordenada$Orden_Llegada_Sujeto <- 0
# Aleatorizar para Femenino
filas_fem <- que_filas <- which(matriz_ordenada$Sexo_Sujeto == "Femenino")
matriz_ordenada$Orden_Llegada_Sujeto[filas_fem] <- sample(seq_along(filas_fem))
# Aleatorizar para Masculino
filas_masc <- which(matriz_ordenada$Sexo_Sujeto == "Masculino")
matriz_ordenada$Orden_Llegada_Sujeto[filas_masc] <- sample(seq_along(filas_masc))
# Formatear el turno como código visual (ej: FEM-01, MASC-03)
matriz_ordenada$ID_Sujeto_Asignado <- sprintf(
  "%s-%02d", 
  ifelse(matriz_ordenada$Sexo_Sujeto == "Femenino", "FEM", "MASC"), 
  matriz_ordenada$Orden_Llegada_Sujeto
)


# 6. Agregar ID sujeto
matriz_ordenada$ID_Sujeto <- sprintf("SUJ-%03d", 1:nrow(matriz_ordenada))

# 7. Reorganizar columnas
matriz_final <- matriz_ordenada[, c(
  "ID_Sujeto",
  "ID_Sujeto_Asignado",
  "Grupo_Experimental",
  "Sexo_Sujeto",
  "Sexo_Investigador"
)]

# 7. Exportar el CSV
nombre_archivo <- glue("Matriz_Experimental_{nombre_experimento}.csv")
write.csv(matriz_final, file = here::here(nombre_experimento, nombre_archivo), row.names = FALSE)
cat(glue("\n¡Matriz generada y ordenada con éxito! Se guardó como {nombre_archivo}\n"))