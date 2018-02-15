# --------------------------------------------------
# Clase 4: Manipulación de Datos con DPLYR. Referencia: http://r4ds.had.co.nz/transform.html
# Plantilla para uso de datos IICA INDEC
# https://comex.indec.gov.ar/public-api/dlc/exports_2017_M.zip
# 2-2018 FERGD --------------------------------------------

# Librerias
library(dplyr)          # para manipulacion de datos
library(stringr)        # para el padding de strings (agregar un cero en los NCM "cortos")
library(scales)         # para formatos de texto (pasar a %)
library(readr)          # para funcion read_csv y read_csv2

"%+%" <- function(x,y) paste(x,y,sep="") # se define %+% como concatenacion

# ------------------------------------------------------------
# I. Definiciones
# ------------------------------------------------------------
data_dir <- getwd()%+%"/data"
temp_dir <- getwd()%+%"/temp"
results_dir <- getwd()%+%"/results/"
# TODO: hay muchas posiciones incompletas entre 2002 y 2015!!!!
nomencladores <- read_csv(data_dir %+% "/nomencladores.csv") # Cargar nomencladores para ver rapido

# -------------------------------------------
# 1- Descarga de datos de la web y carga en R 
# -------------------------------------------

# DESCARGA ONLINE
temp <- tempfile(tmpdir =  temp_dir %+% "/")
download.file("https://comex.indec.gov.ar/public-api/dlc/imports_2017_M.zip", temp)
unzip(temp, exdir = temp_dir)
# CARGA DEL ARCHIVO
Datos <- read_csv2(temp_dir %+% "/impom17.csv",col_types = cols(),  col_names = TRUE, locale(encoding = "UTF-8"))
colnames(Datos) <- c("year","month","ncm", "country","kg","value")
unlink(temp)     # Borro el archivo temporal (para no acumular archivos de basura)

# --------------------------------------
# 2. PROCESAMIENTO DE DATOS 
# --------------------------------------

# library(dplyr) <- ya cargado en tidyquant

# Mirada básica de datos (i.e. ver si cargó bien, etc.)
head(Datos,3)
summary(Datos)
Datos$value                  #<- distintas formas de acceder a la misma columna
Datos["value"]               #<- distintas formas de acceder a la misma columna
Datos[6]                     #<- distintas formas de acceder a la misma columna
sum(Datos$value)/1000000     # Prueba rápida: suma de impos totales

# funciones importantes de DPLYR: select, filter, group_by, summarise
select(Datos,value)          # seleccionar columnas
filter(Datos, month == "01") # filtrar filas

# Gramatica de DPLYR: concatenar las funciones para pasos sucesivos
Datos %>% filter(month =="01") %>% select(value) %>% sum()/1000000

# **************************************
# EMPALMO LOS DATOS (PASO IMPORTANTE!!!)  <siempre chequear que sea correcto
Datos <- Datos %>% left_join(nomencladores, by = c("ncm"="ncm_cod"))                     # left_join usa las filas de la primer tabla
# Tengo que convertir la columna ncm_cod en texto y ademas agregarle el cero adelante si tiene 7 digitos
nomencladores$ncm_cod <-  str_pad(nomencladores$ncm_cod, width=8, side="left", pad="0")  # pad con cero los de menos digitos
Datos <- Datos %>% left_join(nomencladores, by = c("ncm"="ncm_cod"))
# Test basico de carga y empalme: SIEMPRE MIRAR SI EL EMPALME FUE CORRECTO!!
print("Cargados " %+% length(Datos$year) %+% " datos")
print("Registros sin match: " %+% sum(is.na(Datos$ncm_desc)) %+% " (" %+% percent(sum(is.na(Datos$ncm_desc))/length(Datos$year)) %+% ")")

# Resumen y manipulacion de datos: group_by , summarise y mutate
totales.mensuales <- Datos %>% select(month, value) %>% group_by(month) %>% summarise(impos = sum(value)) %>%
  mutate(impos_mill = expos/1000000)

total.rubros <- Datos %>% select(value, rubro_cod_let) %>% group_by(rubro_cod_let) %>% summarise(M = sum(value) / 1000000)
