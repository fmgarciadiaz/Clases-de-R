# --------------------------------------------------
# Clase 5: Graficos en R: GGPLOT 2
# Luego de procesar los datos: cómo armar un gráfico?
# 2-2018 FERGD --------------------------------------------

library(ggplot2)                      # CARGAR libreria
data("midwest", package = "ggplot2")  # cargar datos de ejemplo (vienen con ggplot)

# 1) BASICOS
# los graficos de ggplot se organizan en capas
# en aes se asocian las propiedades de los graficos con datos (i.e. columnas de una tabla)
# Init Ggplot
ggplot(midwest, aes(x=area, y=poptotal)) 
# ggplot solo dibuja la capa de base
# Sobre eso se pueden agregar capas de figuras ("geoms") con el operador "+"
# HISTOGRAMA
ggplot(midwest, aes(x=percadultpoverty)) +  geom_histogram()
# LUEGO HAY QUE REFINAR CADA GRAFICO ELEMENTO POR ELEMENTO!
ggplot(midwest, aes(x=percadultpoverty)) +  geom_histogram(alpha = 0.4 , fill = "blue", color = "blue", size=0.2)
# SCATTERPLOT
g <- ggplot(midwest, aes(x=area, y=poptotal)) + geom_point()    # Se puede enviar a una variable
g     # solo se va a mostrar cuando llame el objeto g o con print(g)
# Mejoramos con parámetros
g <- g + geom_smooth(method="lm") + xlim(c(0, 0.1)) + ylim(c(0, 1000000))
# Títulos y subtítulos
g <- g + ggtitle("Area Vs Poblacion", subtitle="Del dataset midwest") + xlab("Area") + ylab("Poblacion")
g

# 2) AGREGAR CATEGORIAS: se puede definir en aes()
# asociando las propiedades: color, fill, size, alpha a otras variables
gg <- ggplot(midwest, aes(x=area, y=poptotal)) +
  geom_point(aes(color=state), size=3, alpha = 0.7) +    #asocio el color al estado
  coord_cartesian(xlim=c(0, 0.1), ylim=c(0, 1000000)) + 
  labs(title="Area vs. Población", subtitle="From midwest dataset", y="Población", x="Area", 
       caption="Fuente: Elaboración propia en base a ...")
plot(gg)

# 3) Y TEMAS (tipo de letra, tamaños, formato de los ejes)
gg <- gg + theme_minimal()
gg
gg

# 4) estadísticas, labels y escalas (para recapitular, pongo una capa en cada renglón)
gg <- ggplot(midwest, aes(x=area, y=poptotal)) +
  stat_density_2d(aes(fill = ..level..), alpha = 0.15, geom = "polygon") +
       geom_point(color="white", size=3.7, alpha = 0.7) +         # dibujo un borde blanco (estética)..ojo el orden importa!!!
       geom_point(aes(color=state), size=3, alpha = 0.7) +        # asocio el color al estado
       coord_cartesian(xlim=c(0.01, 0.05), ylim=c(0, 150000)) +   # ejes
       labs(title="Area vs. Población", subtitle="From midwest dataset", y="Población", x="Area", 
        caption="Fuente: Elaboración propia en base a ...") +     # titulos y referencias
       geom_text(aes(label=county) , size = 2, vjust=-2.5, check_overlap = TRUE) + 
       scale_colour_brewer(palette = "Set1") +                    # escala para colores
       scale_fill_distiller(palette = "RdBu", guide = FALSE)  +   # escala para fills
       theme_minimal()
gg

# Ejemplo cool: TREEMAPS (hay miles de opciones buscando en internet)
library(treemapify)
gg <- ggplot(midwest, aes(area=poptotal, label=county, subgroup = state, fill=state)) + 
  geom_treemap() + 
  geom_treemap_text(min.size = 5, grow = FALSE, family = "Arial", color = "white") +
  scale_fill_brewer(palette = "Set1") +
  labs(title="Población por Condado", subtitle="From midwest dataset",
       caption="Fuente: Elaboración propia en base a ...")     # titulos y referencias
gg
# 5) GUARDAR EN PNG
png("Poblacion.png", width=12,height=8, units='in', res=300)
gg
dev.off()
