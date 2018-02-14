#***********************************************
# CLASE DE R: #5 Hacer un mapa de Argentina
#***********************************************

# -------------------
# 1. Carga de Librerias
# -------------------#
library(ggmap)     # google maps (para cargar el fondo)
library(rgeos)     # procesamiento geografico (funcion gSimplify)
library(raster)    # libreria de rasters (funcion getData) 
library(rgdal)     # para leer OGRs (funcion readOGR)

"%+%" <- function(x,y) paste(x,y,sep="") # se define %+% como concatenacion

# Elegir WD para acceder a archivos
data_dir <- getwd() %+% "/data/"
results_dir <- getwd() %+% "/results/"

# ---------------------------------
# 2. Carga de MAPAS y Procesamiento
# ---------------------------------
# Cargar mapa de Fondo de Google
Fondo      <- get_googlemap(center = as.numeric(geocode("Argentina")), color = "bw", source = "osm", maptype = "roadmap", zoom = 4)
# Cargar shapes de Argentina y de Municipios
Argentina  <- getData('GADM', country='ARG', level=2) # Cargar datos de Municipios
Provincias <- getData('GADM', country='ARG', level=1) # Cargar datos de Provincias
ArgentinaSimple  <- gSimplify(Argentina,  tol = 0.01 , topologyPreserve = TRUE) # reducrir complejidad
ProvinciasSimple <- gSimplify(Provincias, tol = 0.01 , topologyPreserve = TRUE) # reducir complejidad

# ------------------------------------------
# 3. Carga de Datos Economicos para el PLOTEO
# -------------------------------------------
# (Cargar y Pegar datos seleccionados al DataFrame con JOIN, y otras capas)
# Siempre se transforma en data frame para trabajar con GGPLOT
# Ejemplo: trenes
RedFerroCompleja <- readOGR(dsn = data_dir %+% "/RED_FERROVIARIA", layer = "RED_FERROVIARIA")
RedFerro <- gSimplify(RedFerroCompleja , tol = 0.01 , topologyPreserve = TRUE)   # simplificar los shapes (trabajar a mas velocidad)

# Aca ya podria plotear con un metodo sencillo pero poco flexible
plot(ProvinciasSimple)
plot(RedFerro, col= 'blue', add=TRUE)

# Pero quiero usar otra libreria que requere transformar los datos un poco
# transformaciones para ggplot (no importa que hacen, hay que hacerlo para que funcionen con ggplot)
RedFerro         <- SpatialLinesDataFrame(RedFerro, data.frame(RedFerroCompleja))
ProvinciasSimple <- SpatialPolygonsDataFrame(ProvinciasSimple, data.frame(Provincias))
ArgentinaSimple  <- SpatialPolygonsDataFrame(ArgentinaSimple, data.frame(Argentina)) # le pego el data frame
#Pasar a DATA FRAME (mantiene los datos de las columnas de provincia y municipio)
ARG2 <- merge(fortify(ArgentinaSimple),as.data.frame(ArgentinaSimple), by.x="id" , by.y=0)
PROV <- merge(fortify(ProvinciasSimple),as.data.frame(ProvinciasSimple), by.x="id" , by.y=0)
RedFerro.df <- fortify(RedFerro)

# -------------------------------------------
# 4. Ploteo de shapes (municipios + provincias + trenes)
# -------------------------------------------
g <- ggmap(Fondo)  # Fondo
# Municipios
g <-g + geom_polygon(aes(x = long, y = lat, group = group , fill = NAME_1), data = ARG2, 
  color = "gray", size = 0.2, alpha = 0.5) + scale_fill_discrete() + guides(fill = FALSE)  
# Provincias
g <- g + geom_polygon(aes(x = long, y = lat, group = group , fill = NA) , 
                      data = PROV , color = "black", fill = NA , size = 0.3)
# Red Ferroviaria
g <- g + geom_path(aes(x = long, y = lat, group = group) , data = RedFerro.df , 
                   size = 0.4 , color="yellow", alpha = 0.6)
g

