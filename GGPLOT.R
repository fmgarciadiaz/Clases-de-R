# --------------------------------------------------
# Clase 5: Graficos en R: GGPLOT 2
# Luego de procesar los datos: cómo armar un gráfico?
# 2-2018 FERGD --------------------------------------------

library(ggplot2)                      # CARGAR libreria
data("midwest", package = "ggplot2")  # cargar datos de ejemplo (vienen con ggplot)

# 1 BASICOS
# los graficos de ggplot se organizan en capas
# en aes se asocian las propiedades de los graficos con datos (i.e. columnas de una tabla)
# Init Ggplot
ggplot(midwest, aes(x=area, y=poptotal)) 
# ggplot solo dibuja la capa de base
# Sobre eso se pueden agregar capas de figuras ("geoms") con el operador "+"
ggplot(midwest, aes(x=percadultpoverty)) +  geom_histogram(alpha = 0.4 , fill = "blue", color = "blue", size=0.2)
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point()
g     # solo se va a mostrar cuando llame el objeto g o con print(g)
g <- g + geom_smooth(method="lm") + xlim(c(0, 0.1)) + ylim(c(0, 1000000)) # y parametros
g <- g + ggtitle("Area Vs Poblacion", subtitle="Del dataset midwest") + xlab("Area") + ylab("Poblacion")
g

# 2 AGREGAR CATEGORIAS: se puede definir en aes()
# asociando las propiedades: color, fill, size, alpha a otras variables
gg <- ggplot(midwest, aes(x=area, y=poptotal)) +
  geom_point(aes(color=state), size=3, alpha = 0.7) +    #asocio el color al estado
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area vs. Población", subtitle="From midwest dataset", y="Población", x="Area", 
       caption="Fuente: Elaboración propia en base a ...")
plot(gg)

# 3 temas
gg <- gg + theme_minimal()
gg

# 4 estadísticas y labels
gg <- gg + geom_density2d(color="blue", alpha=0.6) + scale_colour_brewer(palette = "Set1") +
      geom_text(aes(label=county) , size = 2, vjust=-2.5, check_overlap = TRUE)# change color palette
gg

