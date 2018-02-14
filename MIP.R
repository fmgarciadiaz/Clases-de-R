# ************************************************************
# Clase de R: 
# 3 - Obtener multiplicadores de empleo
# FERGD 1/2017
# ************************************************************

# ------------------------------------------------------------
# I. Carga de Datos y armado de matrices basicas
# ------------------------------------------------------------

"%+%" <- function(x,y) paste(x,y,sep="") # se define %+% como concatenacion

# Elegir WD para acceder a archivos
data_dir <- getwd() %+% "/data/"
results_dir <- getwd() %+% "/results/"

# Cargar sistema MIP y Matriz de Generacion del Ingreso
IP_sys <- data.matrix(read.csv( data_dir %+% "MIP1997.csv"  , header=TRUE,row.names=1,numerals = "no.loss") )
MGI_sys <- data.matrix(read.csv(data_dir %+% "MGI1997.csv",header=TRUE,row.names=1,numerals = "no.loss"))

# Definir cantidades para el borde
q_tot <- 124 # Q de sectores // Ajustar en caso de utilizar otra MIP

# ------------------------------------------------------------
# II. Definicion de Matrices
# ------------------------------------------------------------
# MIP
e_tot <- matrix(1, ncol=1, nrow=q_tot) # Vector unidad dimension 1xq_tot
Xi  <- IP_sys[1:q_tot,1:q_tot]   # Matriz de Utilizacion Intermedia
Df  <- IP_sys[1:q_tot,"DF_TOT"]  # Demanda Final. DF_TOT debe estar apuntado al vector de demanda final en el CSV(obvio)
y_int <- IP_sys["VA",1:q_tot]    # Vector de valor agregado. VA debe estar apuntado al vector de VAB a precios basicos (obvio)
M   <- IP_sys["M",1:q_tot]       # Vector de Importaciones CIF.Idem anterior. El CSV debe tener la fila correspondiente
TN  <- IP_sys["TN",1:q_tot]      # Vector de Impuestos Netos.Idem anterior. El CSV debe tener la fila correspondiente
x   <- IP_sys["VBP",1:q_tot]     # Vector de VBPs.Idem anterior. El CSV debe tener la fila correspondiente
inv_x <- 1/x
inv_x[is.infinite(inv_x)] <- 0
diag_inv_x <- diag(c(inv_x))
# MGI
L   <- MGI_sys["L",1:q_tot]   # Requerimientos Directos de Trabajo (El CSV debe tener la linea L)
W   <- MGI_sys["W",1:q_tot]   # Masa salarial (Idem linea W)
EBE <- MGI_sys["EBE",1:q_tot] # Excedente Bruto e Ingreso Mixto (Idem linea W)


# ------------------------------------------------------------
# III. Calculo de Matrices y Linkages
# ------------------------------------------------------------

A <- Xi %*% diag_inv_x # Matriz de Coeficientes Directos
colnames(A)<-colnames(Xi)
I_x <- diag(c(e_tot))  # Matriz Identidad
B <- solve(I_x - A)    # Matriz de Coeficientes Directos e Indirectos (Inversa de Leontieff)
D <- diag_inv_x %*% Xi # Matriz de Cuotas de Mercado ("Market Share")
G <- solve (I_x - D)   # Matriz de Ghosh
#write.csv (A, results_dir %+% "A.csv")
#write.csv (B, results_dir %+% "B.csv")
#write.csv (D, results_dir %+% "D.csv")
#write.csv (G, results_dir %+% "G.csv")

# ------------------------------------------------------------
# VI. MODULO DE ANALISIS SECTORIAL (MIP 1997 124 sectores)
# ------------------------------------------------------------

# BASICOS ****************************************************

BLS   <- as.matrix(colSums(B))          # Backward Linkages
FLS   <- as.matrix(rowSums(B))          # Forward Linkages Normales
FLS2  <- as.matrix(rowSums(G))         # Forward Linkages de Jones
L_DIR <- as.matrix((L/x))            # Requerimientos Directos de Empleo
L_TOT <- t(t(L/x) %*% B)   # Requerimientos Totales de Empleo
L_MUL <- L_TOT/L_DIR       # Multiplicadores de Empleo


# FUNCIONES DE VISUALIZACION *****************************
#dibujar los BLS y FLS
  library(ggrepel)
  library(hrbrthemes)
  vbp_baseline <-  B %*% Df  # VBP baseline para comparar (es un poco diferente de x)
  puntos <- data.frame(BLS, FLS, gsub("_"," ",rownames(as.matrix(BLS))))
  colnames(puntos)<-c("BLS","FLS","Labels")
  graf <- ggplot(puntos, aes( x = BLS , y = FLS ) ) + geom_hline(yintercept=mean(puntos$BLS),color="darkgray") + 
       geom_vline(xintercept=mean(puntos$FLS), color="darkgray") + 
       geom_point(aes(size=vbp_baseline), color="red", alpha = 0.7) +
       geom_text(aes(label = Labels), color = 'black', size=1.5, check_overlap = TRUE) +
       scale_size_area(guide = FALSE) +
       ggtitle("Encadenamientos") + theme_ipsum() + ylim(c(1,6))
  # Mostrar el grafico
  graf  

