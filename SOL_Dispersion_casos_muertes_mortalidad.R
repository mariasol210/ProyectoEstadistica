library(tidyverse)
library(dplyr)
library(lubridate)
library(moments)

#LEO el csv, TRUE dice que la primera fila es de titulos y la coma el separador
mydata <- read.csv("owid-covid-data.csv", TRUE, ",")
#Convierto la columna date a tipo de dato Date
mydata$date = as.Date(mydata$date, format = "%Y-%m-%d") 
#Ordeno el dataframe por fecha
mydata <- arrange(mydata, date)
#Agrego la columna que indica el inicio de cada semana
mydata$week <- floor_date(mydata$date, "week")


#-- DATAFRAME PARA TODOS LOS DATOS DE AMERICA --
america <- mydata %>%
  filter(continent == "South America" | continent == "North America") %>%
  group_by(week) 

#-- DATAFRAME PARA LOS NUEVOS CASOS Y NUEVAS MUERTES DE AMERICA --
america_weekly <- america %>%
  summarize(totalcases=sum(new_cases, na.rm = TRUE),
            totaldeath=sum(new_deaths, na.rm = TRUE))

#CALCULO de las medidas de los nuevos casos semanales
media <- mean(america_weekly$totalcases,na.rm = TRUE)
mediana_Q2 <- median(america_weekly$totalcases, na.rm = TRUE)
cuartiles <- quantile(america_weekly$totalcases, probs = c(0.25, 0.5,0.75))
desviacion_tipica <- sd(america_weekly$totalcases, na.rm = TRUE)
kurtosis <- kurtosis(america_weekly$totalcases, na.rm = TRUE)
asimetria <- skewness(america_weekly$totalcases, na.rm = TRUE)

Results <- data.frame(media,Q1=cuartiles[1],  mediana_Q2,
                            Q3=cuartiles[3],desviacion_tipica, kurtosis, asimetria)

rownames(Results) = c("america_cases")
                             
#GRAFICOS
america_ncGraf <- ggplot(data=america_weekly, aes(x=week, y=totalcases)) + 
  geom_line(color="#FC4E07", size=1) + geom_point() +
  labs(title="Nuevos casos reportados semanalmente en America", x="Semana", y="Nuevos casos", 
       caption="(Basado en los datos de Our World in Data[1])") 
america_ncGraf


#CALCULO de las medidas de las nuevas muertes semanales
media <- mean(america_weekly$totaldeath,na.rm = TRUE)
mediana_Q2 <- median(america_weekly$totaldeath, na.rm = TRUE)
cuartiles <- quantile(america_weekly$totaldeath, probs = c(0.25, 0.5,0.75))
desviacion_tipica <- sd(america_weekly$totaldeath, na.rm = TRUE)
kurtosis <- kurtosis(america_weekly$totaldeath, na.rm = TRUE)
asimetria <- skewness(america_weekly$totaldeath, na.rm = TRUE)

resultados_deaths <- c(media,Q1=cuartiles[1],  mediana_Q2,
                       Q3=cuartiles[3],desviacion_tipica, kurtosis, asimetria)
Results <- rbind(Results, america_deaths = resultados_deaths)

#GRAFICOS
america_ndGraf <- ggplot(data=america_weekly, aes(x=week, y=totaldeath)) + 
  geom_line(color="#FC4E07", size=1) + geom_point() +
  labs(title="Muertes reportadas semalmente en America", x="Semana", y="Muertes reportadas", 
       caption="(Basado en los datos de Our World in Data[1])") 
america_ndGraf



#-- DATAFRAME PARA TODOS LOS DATOS DE EUROPA --
europa <- mydata %>%
  filter(continent == "Europe") %>%
  group_by(week)

#-- DATAFRAME PARA LOS NUEVOS CASOS Y NUEVAS MUERTES DE EUROPA --
europa_weekly <- europa %>%
  summarize(totalcases=sum(new_cases, na.rm = TRUE),
            totaldeath=sum(new_deaths, na.rm = TRUE))

#CALCULO de las medidas de los nuevos casos semanales
media <- mean(europa_weekly$totalcases,na.rm = TRUE)
mediana_Q2 <- median(europa_weekly$totalcases, na.rm = TRUE)
cuartiles <- quantile(europa_weekly$totalcases, probs = c(0.25, 0.5,0.75))
desviacion_tipica <- sd(europa_weekly$totalcases, na.rm = TRUE)
kurtosis <- kurtosis(europa_weekly$totalcases, na.rm = TRUE)
asimetria <- skewness(europa_weekly$totalcases, na.rm = TRUE)

resultados_cases <- c(media,Q1=cuartiles[1],  mediana_Q2,
                       Q3=cuartiles[3],desviacion_tipica, kurtosis, asimetria)
Results <- rbind(Results, europa_cases = resultados_cases)

#GRAFICOS
europa_ncGraf <- ggplot(data=europa_weekly, aes(x=week, y=totalcases)) + 
  geom_line(color="#00AFBB", size=1) + geom_point() +
  labs(title="Nuevos casos reportados semanalmente en Europa", x="Semana", y="Nuevos casos", 
       caption="(Basado en los datos de Our World in Data[1])") 
europa_ncGraf


#CALCULO de las medidas de las nuevas muertes semanales
media <- mean(europa_weekly$totaldeath,na.rm = TRUE)
mediana_Q2 <- median(europa_weekly$totaldeath, na.rm = TRUE)
cuartiles <- quantile(europa_weekly$totaldeath, probs = c(0.25, 0.5,0.75))
desviacion_tipica <- sd(europa_weekly$totaldeath, na.rm = TRUE)
kurtosis <- kurtosis(europa_weekly$totaldeath, na.rm = TRUE)
asimetria <- skewness(europa_weekly$totaldeath, na.rm = TRUE)

resultados_deaths <- c(media,Q1=cuartiles[1],  mediana_Q2,
                       Q3=cuartiles[3],desviacion_tipica, kurtosis, asimetria)
Results <- rbind(Results, europa_deaths = resultados_deaths)

#GRAFICOS
europa_ndGraf <- ggplot(data=europa_weekly, aes(x=week, y=totaldeath)) + 
  geom_line(color="#00AFBB", size=1) + geom_point() +
  labs(title="Muertes reportadas semalmente en Europa", x="Semana", y="Muertes reportadas", 
       caption="(Basado en los datos de Our World in Data[1])") 
europa_ndGraf



#INFORMACION TOTAL CONCLUSION

mydata2 <- read.csv("WHO COVID-19 global table data December 23rd 2020 at 6.15.30 PM.csv", TRUE, ",")

total_america <- mydata2 %>%
  filter(WHO.Region == "Americas") %>%
  summarize(ï..Name, Cases...cumulative.total,Deaths...cumulative.total)

total_europa <- mydata2 %>%
  filter(WHO.Region == "Europe") %>%
  summarize(ï..Name, Cases...cumulative.total,Deaths...cumulative.total)

total <- c(sum(total_america$Cases...cumulative.total), sum(total_america$Deaths...cumulative.total), 
           sum(total_europa$Cases...cumulative.total), sum(total_europa$Deaths...cumulative.total))
Results$total <- total

porcentaje <- c((total[1]/as.numeric(mydata2[1,3]))*100, 
                (total[2]/as.numeric(mydata2[1,7]))*100,
                (total[3]/as.numeric(mydata2[1,3]))*100,
                (total[4]/as.numeric(mydata2[1,7]))*100)

Results$porcentaje_mundial <- porcentaje

Results_ncGraf <- 
  ggplot(data=america_weekly, aes(x=week, y=totalcases)) +
  geom_line(color="#FC4E07",aes(color="America"), size=1) + geom_point()+
  geom_line(data=europa_weekly,aes(color="Europa"), color="#00AFBB",size=1)  +
  geom_point(data=europa_weekly)  +
  labs(title="Nuevos casos reportados semanalmente en America y Europa", x="Semana", 
       y="Nuevos casos", caption="(Basado en los datos de Our World in Data[1])") 
Results_ncGraf

Results_ndGraf <-
ggplot(data=america_weekly, aes(x=week, y=totaldeath)) +
  geom_line(color="#FC4E07",aes(color="America"), size=1) + geom_point()+
  geom_line(data=europa_weekly,aes(color="Europa"), color="#00AFBB",size=1)  +
  geom_point(data=europa_weekly) +
  labs(title="Muertes reportadas semalmente en America y Europa", fill = "Region",
       x="Semana", y="Muertes reportadas", caption="(Basado en los datos de Our World in Data[1])") 
Results_ndGraf


rm(total_america, total_europa, porcentaje, total, mydata2, mydata)
rm(asimetria,cuartiles,desviacion_tipica, kurtosis, media, mediana_Q2,
   resultados_cases,resultados_deaths)


# Esta es la parte de Adi pero la hice yo
# Mortalidad America
America_mortalidad <- america %>%
 mutate(month=format(date, "%m")) %>%
  group_by(month) %>%
  summarize(totalcases=sum(new_cases, na.rm = TRUE), 
            totaldeaths=sum(new_deaths, na.rm = TRUE), 
            total=(totaldeaths/totalcases)*100)

america_mortGraf <- ggplot(data=America_mortalidad, aes(x=month, y=total)) + 
  geom_line(color="red", size=1,aes(group = 1)) + geom_point() +
  labs(title="Tasa de letalidad mensualmente en America", fill = "Region",
       x="Mes", y="Tasa de letalidad", caption="(Basado en los datos de Our World in Data[1])") 
america_mortGraf

mortalidad_mean <- mean(America_mortalidad$total,na.rm = TRUE)
mortalidad_medianaQ2 <- median(America_mortalidad$total, na.rm = TRUE)
mortalidad_cuartiles <- quantile(America_mortalidad$total, probs = c(0.25, 0.5,0.75))
mortalidad_desviacion_tipica <- sd(America_mortalidad$total, na.rm = TRUE)
mortalidad_kurtosis <- kurtosis(America_mortalidad$total, na.rm = TRUE)
mortalidad_asimetria <- skewness(America_mortalidad$total, na.rm = TRUE)

# Mortalidad Europa
Europa_mortalidad <- europa %>%
  mutate(month=format(date, "%m")) %>%
  group_by(month) %>%
  summarize(totalcases=sum(new_cases, na.rm = TRUE), 
            totaldeaths=sum(new_deaths, na.rm = TRUE), 
            total=(totaldeaths/totalcases)*100)

eruopa_mortGraf <- ggplot(data=Europa_mortalidad, aes(x=month, y=total)) + 
  geom_line(color="purple", size=1, aes(group = 1)) + geom_point()+
  labs(title="Tasa de letalidad mensualmente en Europa", fill = "Region",
       x="Mes", y="Tasa de letalidad", caption="(Basado en los datos de Our World in Data[1])") 
eruopa_mortGraf

mortalidad_mean <- mean(Europa_mortalidad$total,na.rm = TRUE)
mortalidad_medianaQ2 <- median(Europa_mortalidad$total, na.rm = TRUE)
mortalidad_cuartiles <- quantile(Europa_mortalidad$total, probs = c(0.25, 0.5,0.75))
mortalidad_desviacion_tipica <- sd(Europa_mortalidad$total, na.rm = TRUE)
mortalidad_kurtosis <- kurtosis(Europa_mortalidad$total, na.rm = TRUE)
mortalidad_asimetria <- skewness(Europa_mortalidad$total, na.rm = TRUE)

rm(mortalidad_mean, mortalidad_medianaQ2, mortalidad_cuartiles, 
   mortalidad_desviacion_tipica, mortalidad_kurtosis, mortalidad_asimetria )
