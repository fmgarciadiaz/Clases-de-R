# Clase de R: 
# Ejemplo 2 # 
# 2. Ejemplo de regresion

data(women)  # cargar data de ejemplo, alto vs peso de personas
fit <-lm(weight ~ height, data=women) # correr la regresion (funcion lm)
summary(fit)
plot(fitted(fit) , col="red")
points(women$weight, col="blue")
