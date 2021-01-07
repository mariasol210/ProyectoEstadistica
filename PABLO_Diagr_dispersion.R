library(tidyverse)

# Crear un data frame conservando el original (he llamado excel a la tabla original)
mydf <- read.csv("owid-covid-data.csv", TRUE, ",")

# Eliminar los datos que no se van a utilizar en este estudio
mydf <- mydf[, -c(15:52)]
mydf <- mydf[, -c(5:10, 12, 13)]

# Nombrar las columnas restantes para mayor claridad
names(mydf) <- c("codigo", "continente", "pais", "fecha", "casos_por_millon", "muertes_por_millon")

# Quedarnos solo con el dato mas reciente de cada pais
mydf <- mydf %>% filter(fecha == "2020-12-31")

# Eliminar la columna de fechas porque ya no la necesitamos
mydf <- mydf[, -c(4)]

# Nombrar el continente del dato global para poder incluirlo en Europa y America
mydf[188,2] <- "World"

# Filtrar por los continentes estudiados
europa <- mydf %>% filter (continente == "Europe" | continente == "World")
america <- mydf %>% filter (continente == "North America" | continente == "South America" | continente == "World")

# Corregir datos vacios o con nombres demasiado largos para los graficos
europa[46,5] <- 0
europa[47,1] <- "WRL"
europa[21,1] <- "KOS"
america[36,1] <- "WRL"
america[13,5] <- 0
america[17,5] <- 0
america[28,5] <- 0
america[30,5] <- 0

# Dibujar los graficos
ggplot(europa, aes(x = casos_por_millon, y = muertes_por_millon, color = continente)) + geom_point(size = 7) + geom_text(aes(label = codigo), vjust = 0.4, hjust = 0.5, size = 3, col = "Black") + scale_x_log10() + scale_y_log10() + theme(legend.text = element_text(size = 10), legend.title = element_text(size = 10)) + geom_smooth(method = lm, se = FALSE)
ggplot(america, aes(x = casos_por_millon, y = muertes_por_millon, color = continente)) + geom_point(size = 7) + geom_text(aes(label = codigo), vjust = 0.4, hjust = 0.5, size = 3, col = "Black") + scale_x_log10() + scale_y_log10() + theme(legend.text = element_text(size = 10), legend.title = element_text(size = 10)) + geom_smooth(method = lm, se = FALSE)
