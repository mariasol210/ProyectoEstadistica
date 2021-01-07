library(tidyverse)
library(dplyr)

#Lectura archivo
owid_covid_data <- read.csv("owid-covid-data.csv", TRUE, ",")

# Europa
# Creamos una base de datos con los paises europeos. owid_covid_data es la base de datos escogido en la entrega 0.
Europa <- owid_covid_data%>%filter(continent == "Europe")

# Sacamos los casos totales y las muertes totales por país en Europa
casos_muertes_totales_europa <- Europa%>%group_by(location)%>%filter(!is.na(total_deaths), !is.na(total_cases))%>%summarize(casos_totales = max(total_cases), muertes_totales = max(total_deaths))

# Empleamos la fórmula de la tasa de letalidad y se guarda en una nueva columna
casos_muertes_totales_europa <- casos_muertes_totales_europa%>%mutate(letalidad = muertes_totales / casos_totales * 100)

# América
# La base de datos para los países americanos está dividida en América del norte y sur pero nuestro objetivo es calcular la tasa de letalidad de América en un histograma y diagrama de caja.
# Sacamos los países que corresponden a América del norte y sur.
North_America <- owid_covid_data%>%filter(continent == "North America")
South_America <- owid_covid_data%>%filter(continent == "South America")
# Juntamos las dos tablas en una sola tabla
America <- rbind(North_America, South_America)

# Sacamos los casos totales y las muertes totales por país en América
casos_muertes_totales_america <- America%>%group_by(location)%>%filter(!is.na(total_deaths), !is.na(total_cases))%>%summarize(casos_totales = max(total_cases), muertes_totales = max(total_deaths))

# Empleamos la fórmula de la tasa de letalidad y se guarda en una nueva columna
casos_muertes_totales_america <- casos_muertes_totales_america%>%mutate(letalidad = muertes_totales / casos_totales * 100)

# Histograma
# Se muestra el histograma la tasa de letalidad por país según el continentey el número de países que se encuentran en un intervalo.
ggplot(casos_muertes_totales_europa, aes(x = letalidad))+geom_histogram(bins = 25)+scale_x_log10()
ggplot(casos_muertes_totales_america, aes(x = letalidad))+geom_histogram(bins = 30)+scale_x_log10()

# Diagrama de cajas
# Se muestra el diagrama de cajas la tasa de letalidad por país según el continente
ggplot(casos_muertes_totales_europa, aes(x = letalidad))+geom_boxplot()
ggplot(casos_muertes_totales_america, aes(x = letalidad))+geom_boxplot()

