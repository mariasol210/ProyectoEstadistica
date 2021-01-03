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


#-- DATAFRAME PARA LOS NUEVOS CASOS DE AMERICA --
#Creo un nuevo dataframe sumando la columnas de new cases de cada semana
america_newCases <- america %>%
  summarize(total=sum(new_cases, na.rm = TRUE))
  
#CALCULO de las medidas de los nuevos casos semanales
media <- mean(america_newCases$total,na.rm = TRUE)
mediana_Q2 <- median(america_newCases$total, na.rm = TRUE)
cuartiles <- quantile(america_newCases$total, probs = c(0.25, 0.5,0.75))
desviacion_tipica <- sd(america_newCases$total, na.rm = TRUE)
kurtosis <- kurtosis(america_newCases$total, na.rm = TRUE)
asimetria <- skewness(america_newCases$total, na.rm = TRUE)

Results <- data.frame(media,Q1=cuartiles[1],  mediana_Q2,
                            Q3=cuartiles[3],desviacion_tipica, kurtosis, asimetria)

rownames(Results) = c("america_cases")
                             
#GRAFICOS
america_ncGraf <- ggplot(data=america_newCases, aes(x=week, y=total)) + 
  geom_line(color="#FC4E07", size=1) + geom_point() +
  labs(title="Nuevos casos reportados semanalmente en America", x="Semana", y="Nuevos casos", 
       caption="(Basado en los datos de Our World in Data[1])") 
america_ncGraf


#-- DATAFRAME PARA LAS MUERTES DE AMERICA --
america_newDeaths <- america %>%
  summarize(total=sum(new_deaths, na.rm = TRUE))

#CALCULO de las medidas de los nuevos casos semanales
media <- mean(america_newDeaths$total,na.rm = TRUE)
mediana_Q2 <- median(america_newDeaths$total, na.rm = TRUE)
cuartiles <- quantile(america_newDeaths$total, probs = c(0.25, 0.5,0.75))
desviacion_tipica <- sd(america_newDeaths$total, na.rm = TRUE)
kurtosis <- kurtosis(america_newDeaths$total, na.rm = TRUE)
asimetria <- skewness(america_newDeaths$total, na.rm = TRUE)

resultados_deaths <- c(media,Q1=cuartiles[1],  mediana_Q2,
                       Q3=cuartiles[3],desviacion_tipica, kurtosis, asimetria)
Results <- rbind(Results, america_deaths = resultados_deaths)

#GRAFICOS
america_ndGraf <- ggplot(data=america_newDeaths, aes(x=week, y=total)) + 
  geom_line(color="#FC4E07", size=1) + geom_point() +
  labs(title="Muertes reportadas semalmente en America", x="Semana", y="Muertes reportadas", 
       caption="(Basado en los datos de Our World in Data[1])") 
america_ndGraf



#-- DATAFRAME PARA TODOS LOS DATOS DE EUROPA --
europa <- mydata %>%
  filter(continent == "Europe") %>%
  group_by(week)


#-- DATAFRAME PARA LOS NUEVOS CASOS DE EUROPA --
europa_newCases <- europa %>%
  summarize(total=sum(new_cases, na.rm = TRUE))

#CALCULO de las medidas de los nuevos casos semanales
media <- mean(europa_newCases$total,na.rm = TRUE)
mediana_Q2 <- median(europa_newCases$total, na.rm = TRUE)
cuartiles <- quantile(europa_newCases$total, probs = c(0.25, 0.5,0.75))
desviacion_tipica <- sd(europa_newCases$total, na.rm = TRUE)
kurtosis <- kurtosis(europa_newCases$total, na.rm = TRUE)
asimetria <- skewness(europa_newCases$total, na.rm = TRUE)

resultados_cases <- c(media,Q1=cuartiles[1],  mediana_Q2,
                       Q3=cuartiles[3],desviacion_tipica, kurtosis, asimetria)
Results <- rbind(Results, europa_cases = resultados_cases)

#GRAFICOS
europa_ncGraf <- ggplot(data=europa_newCases, aes(x=week, y=total)) + 
  geom_line(color="#00AFBB", size=1) + geom_point() +
  labs(title="Nuevos casos reportados semanalmente en Europa", x="Semana", y="Nuevos casos", 
       caption="(Basado en los datos de Our World in Data[1])") 
europa_ncGraf


#-- DATAFRAME PARA LAS MUERTES DE EUROPA --
europa_newDeaths <- europa %>%
  summarize(total=sum(new_deaths, na.rm = TRUE))

#CALCULO de las medidas de los nuevos casos semanales
media <- mean(europa_newDeaths$total,na.rm = TRUE)
mediana_Q2 <- median(europa_newDeaths$total, na.rm = TRUE)
cuartiles <- quantile(europa_newDeaths$total, probs = c(0.25, 0.5,0.75))
desviacion_tipica <- sd(europa_newDeaths$total, na.rm = TRUE)
kurtosis <- kurtosis(europa_newDeaths$total, na.rm = TRUE)
asimetria <- skewness(europa_newDeaths$total, na.rm = TRUE)

resultados_deaths <- c(media,Q1=cuartiles[1],  mediana_Q2,
                       Q3=cuartiles[3],desviacion_tipica, kurtosis, asimetria)
Results <- rbind(Results, europa_deaths = resultados_deaths)

#GRAFICOS
europa_ndGraf <- ggplot(data=europa_newDeaths, aes(x=week, y=total)) + 
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
  ggplot(data=america_newCases, aes(x=week, y=total)) +
  geom_line(color="#FC4E07",aes(color="America"), size=1) + geom_point()+
  geom_line(data=europa_newCases,aes(color="Europa"), color="#00AFBB",size=1)  +
  geom_point(data=europa_newCases)  +
  labs(title="Nuevos casos reportados semanalmente en America y Europa", x="Semana", 
       y="Nuevos casos", caption="(Basado en los datos de Our World in Data[1])") 
Results_ncGraf

Results_ndGraf <-
ggplot(data=america_newDeaths, aes(x=week, y=total)) +
  geom_line(color="#FC4E07",aes(color="America"), size=1) + geom_point()+
  geom_line(data=europa_newDeaths,aes(color="Europa"), color="#00AFBB",size=1)  +
  geom_point(data=europa_newDeaths) +
  labs(title="Muertes reportadas semalmente en America y Europa", fill = "Region",
       x="Semana", y="Muertes reportadas", caption="(Basado en los datos de Our World in Data[1])") 
Results_ndGraf

rm(total_america, total_europa, porcentaje, total, mydata2, mydata)
rm(asimetria,cuartiles,desviacion_tipica, kurtosis, media, mediana_Q2,
   resultados_cases,resultados_deaths)
