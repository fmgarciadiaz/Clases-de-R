# ******************************************
# Clase de R: Conceptos básicos
# Ejemplo #1
# Este es un script de R
# <- el # define comentarios
# ******************************************

# Defino mis variables ********************* se asigna con <- o con = (pero lo comun es <-)
x_num <- 1                            # numeros
x_vec <- c(1,2,3,4,5,6,7,8,9,10,11)   # vectores
matriz <-matrix(c(1,2,3,4),ncol = 2)  # matrices
error <- rnorm(10000,0,1)             # un vector con la distribucion normal
y_vec <- 2 * x_vec + error[1:10]      # y es igual a 2x + un error normal
media_de_y <- mean(y_vec)             # un numero con la media de y (usando la funcion "mean")

# defino mis funciones: una vez definidas se corren solo si aparecen en el codigo
oddcount <- function(x) 
{
  k<-0                # variable local a la funcion, arranca en cero
  for(n in x)         # estructura de control for n in x ("for")
    {
     if (n %% 2 == 1) k <- k+1 # sumar uno si el n que miro es impar (control logico con "if")
    }
  return(k)           # devolver el k hasta donde haya llegado
}

# ejecuto los procedimientos ********************
impares <- oddcount(x_vec)      # contar impares en X_vec con la funcion que defini
plot(error)                     # dibujar y
hist(error)                     # histograma de y
plot(x_vec,y_vec)               # dibujar y en funcion de x
