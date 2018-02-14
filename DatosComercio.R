# --------------------------------------------------
# Clase 4: Manipulación de Datos con DPLYR. Referencia: http://r4ds.had.co.nz/transform.html
# Plantilla para uso de datos IICA INDEC
# https://comex.indec.gov.ar/public-api/dlc/exports_2017_M.zip
# 2-2018 FERGD --------------------------------------------

# Librerias
library(tidyquant)      # Loads tidyquant, tidyverse, lubridate, xts, quantmod, TTR (principalmente para read_csv2)
library(stringr)        # para el padding de strings (agregar un cero en los NCM "cortos")
library(scales)         # para formatos de texto (pasar a %)

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
Datos <- read_csv2(temp_dir %+% "/impom17.csv")
colnames(Datos) <- c("year","month","ncm", "country","kg","value")
unlink(temp)     # Borro el archivo temporal (para no acumular archivos de basura)
# Proceso los NCM de los nomencladores para que empalmen
nomencladores$ncm_cod <-  str_pad(nomencladores$ncm_cod, width=8, side="left", pad="0")  # pad con cero los de menos digitos
# **************************************
# EMPALMO LOS DATOS (PASO IMPORTANTE!!!)  <siempre chequear que sea correcto
# **************************************
Datos <- Datos %>% left_join(nomencladores, by = c("ncm"="ncm_cod"))
# Test basico de carga y empalme
print("Cargados " %+% length(Datos$year) %+% " datos")
print("Registros sin match: " %+% sum(is.na(Datos$ncm_desc)) %+% " (" %+% 
            percent(sum(is.na(Datos$ncm_desc))/length(Datos$year)) %+% ")")

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

# Gramatica de DPLYR: 
total.anual <- Datos %>% select(year, value, XM) %>% group_by(year) %>%
  summarise(X = sum(ifelse(XM == "X", value, 0)) / 1000000,
            M = sum(ifelse(XM == "M", value, 0)) / 1000000) %>% mutate(saldo = X - M)

total.rubro.anual <- Datos %>% select(year, value, XM, rubro_cod_let) %>% 
  mutate(year = as.Date(as.character(year),"%Y")) %>%
  group_by(year, rubro_cod_let) %>% summarise(X = sum(ifelse(XM == "X", value, 0)) / 1000000,
            M = sum(ifelse(XM == "M", value, 0)) / 1000000) %>% mutate(saldo = X - M)
# calcular las variaciones interanuales (usa quantmod)
total.rubro.anual$dX <- with(total.rubro.anual, ave(X, rubro_cod_let, FUN=Delt))

# calcular los acumulados anuales
total.rubro.acum <- Datos %>% select(year, month, value, XM, rubro_cod_let) %>% 
  filter(month <= last(month)) %>%
  mutate(year = as.Date(as.character(year),"%Y")) %>%
  group_by(year, rubro_cod_let) %>% 
  summarise(X = sum(ifelse(XM == "X", value, 0))/1000000,
            M = sum(ifelse(XM == "M", value, 0))/ 1000000) %>% 
  mutate(saldo = X - M)
# calcular las variaciones interanuales (usa quantmod)
total.rubro.acum$dX <- with(total.rubro.acum, ave(X, rubro_cod_let, FUN=Delt))

# Totales mensuales
total.mensual <- Datos %>% select(year, month, value, XM) %>% group_by(year, month) %>%
  summarise(X = sum(ifelse(XM == "X", value, 0)) / 1000000,
            M = sum(ifelse(XM == "M", value, 0)) / 1000000) %>% mutate(saldo = X - M)

total.rubro.mensual <- Datos %>% select(year,month,value,XM,rubro_cod_let) %>% 
            group_by(year,month, rubro_cod_let) %>% summarise(X = sum(ifelse(XM == "X",value,0))/1000000,
            M = sum(ifelse(XM == "M",value,0))/1000000) %>% mutate(saldo = X - M)




