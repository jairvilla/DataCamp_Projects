#***************************************************
# Proyecto: Calculos renales y parádoja de Simpsons
# Curso: Ciencias de datos para el sector salud
# Tema:  Regresión Logística
#***************************************************

# Cargar packages
if(!require(dplyr)) install.packages("dplyr")
if(!require(readr)) install.packages("readr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(broom)) install.packages("broom")

# Tarea 1

# Cargar data 
library(readr); library(dplyr)
data <- read_csv("datasets/kidney_stone_data.csv") %>%
                as.data.frame()

# Leer el dataset las primeras 6 onservaciones
head(kidney_stone_data,6)


# Tarea 2: Calcular la frecuencia de exito de cada tratamiento

data %>% 
  group_by(treatment, success) %>%
  summarise(N = n()) %>%
  mutate(Freq = round(N/sum(N),3))

# Tarea 3: Tratamiento vs tamaño del cálculo
# Calular el numero y la frecuencia de exito y no exito por tamaño del calculo para 
# cada tramiento. 

sum_data <- data %>%
  group_by(treatment, stone_size, success) %>%
  summarise(N = n()) %>%
  mutate(Freq = round(N/sum(N),3))
sum_data

# Tarea 4: Cree una gráfica

library(ggplot2)
sum_data %>%
  ggplot(aes(x = treatment, y = N)) + 
  geom_bar(aes(fill = stone_size), stat = "identity")

# Tarea 5:  

library(broom)
trt_ss <- chisq.test(data$treatment, data$stone_size)
tidy(trt_ss)


# Tarea 6

# modelo de regresion 
m <- glm(data = data, success ~ stone_size + treatment, family = 'binomial')

# Print out model coefficient table in tidy format
tidy(m)

# Tarea 7


# grafica 
tidy_m <- tidy(m)

tidy_m %>%
  ggplot(aes(x=term, y=estimate)) + 
  geom_pointrange(aes(ymin=estimate-1.96*std.error, 
                      ymax=estimate+1.96*std.error)) +
  geom_hline(yintercept = 0)

# Tarea 8

#Is small stone more likely to be a success after controlling for treatment option effect?
# Options: Yes, No (as string)
  small_high_success <- "Yes"

# Is treatment A significantly better than B?
# Options: Yes, No (as string)
A_B_sig <- "No"









