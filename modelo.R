#Carga de librerias
library(dplyr)
library(skimr)
library(lubridate)
library(tidyr)
library(ggplot2)

#carga del dataset
df <- read.csv(file = 'DataSetFallosMaquina.csv',sep=';')

#Visión general del dataset
glimpse(df)

#tener una gran vison de los datos, nos muestra las distribuciones de los datos
skim(df) 

#grafico este grafico nos permite ver si hay otliers
# GRAFICO DE LA TEMPERATURA
ggplot(df,x=1) + geom_boxplot(aes(y=Temperature))

#Conclusión: efectivamente vemos que son sólo 4 puntos

#4.Calidad de datos

#Corregimos los tipos de variables y los atípicos
df <- df %>%
  mutate(Measure2 = as.factor(Measure2), #Corregimos Measure2
         Measure3 = as.factor(Measure3)) %>% #Corregimos Measure3 
  filter(Temperature > 50) #eliminamos los 4 atípicos de temperatura


#5.Análisis exploratorio de variables (EDA
#Exploramos las de tipo factor
df %>%
  select_if(is.factor) %>%
  gather() %>%
  ggplot(aes(value)) + geom_bar() + facet_wrap(~key,scales='free') +
  theme(axis.text=element_text(size=6))#esto es para cambiar el tamaño del texto del eje y que se lea bien

#Y las de tipo entero
df %>%
  select_if(is.integer) %>%
  gather() %>%
  ggplot(aes(value)) + geom_density() + facet_wrap(~key,scales='free') +
  theme(axis.text=element_text(size=6))#esto es para cambiar el tamaño del texto del eje y que se lea bien

#Hacemos análisis de correlaciones
df %>%
  select_if(is.integer) %>%
  cor() %>% 
  round(digits = 2)

#Hacemos un zoom sobre el desbalanceo de la variable target
table(df$Failure)

'Conclusiones:
-No se perciben patrones raros en las variables
-Las variables de medidas no correlacionan
-La variable target está muy desbalanceada'

'6.Transformación de variables
No son necesarias grandes transformaciones porque el fichero 
ya viene muy limpio (no pasa así en la realidad)

Tampoco vamos a crear variables sintéticas (nuevas variables) que sí 
haríamos en la realidad (por ej número de fallos del mismo equipo, etc.)

Pero sí vamos a tener que trabajar sobre el balanceo de la variable target'

#Vamos a balancear usando la técnica del inframuestreo:
#Comprobamos la penetración exacta de la target
#Tenemos 81 sis que sobre el total de casos son un 0,9%:
81/nrow(df)*100

#Para tener casi un 10% necesitaríamos incrementar la proporción aprox en x10
#Entonces vamos a reducir los nos para que salga aprox esa proporción
#Nuevo df de nos

set.seed(1234) #para que nos salga lo mismo

df_nos <- df %>% 
  filter(Failure == 'No')  %>% 
  sample_frac(size= 0.08)
dim(df_nos)

#Df de sis
df_sis <- df %>% filter(Failure == 'Yes')

#Y los unimos de nuevo en un nuevo df reducido
df_red <- rbind(df_nos,df_sis)

#Comprobamos de nuevo la penetación de la target
count(df_red,Failure)
81/nrow(df_red) * 100


# Ahora ya tenmos un dataset donde la target tiene un 10% de penetración (que sigue siendo poco pero lo dejaremos así)

# 7.Modelización

# 7.1 Dividir en entrentamiento y validación:
# No lo vamos a hacer por simplicidad y porque tenemos pocos casos

# 7.2 Roles de las variables


target <- 'Failure'
indep <- names(df_red)[-20] #la variable 20 es  failure
formula <- reformulate(indep, target) #le decimos a R que nos haga una formula para que nos desarrolloe# el modelo de que tiene como variable independiente a target y que depende de indep
formula 

df2<- as.factor(df_red$Failure)
df3 <- df %>% df2
df_red <- df_red %>%
  mutate(Failure = as.factor(Failure))
# Se realiza la regresión logística
rl <- glm(formula,df_red,family=binomial(link='logit')) #glm= modelo lineal generalizado

options(scipen=999)#Desactiva la notacion cientifica
summary(rl) #Vemos el resultado

# Sólo resultan predictivas al menos al 95% tres variables, 
# que vamos a seleccionar como finales
indep_fin <- c('Temperature','Humidity', 'Measure10')

formula <- reformulate(indep_fin,target) #actualizamos la fórmula


# Modelizamos otra vez
rl<- glm(formula,df_red,family = binomial(link='logit'))
summary(rl)


# Aplicamos nuestro modelo a los datos
df$scoring <- predict(rl,df,type='response')

# Tomamos la decision de si pensamos que será un fallo o no
#Como la penetracion inicial era del 1%, vamos a poner un punto
# un punto de corte muy alto, ejemplo encima del 80'%

df$prediccion<- ifelse(df$scoring> 0.8,1,0) # si se cumple la condicion 1 sino 0

# 8 Evaluacion del modelo

# Vamos a contrastar la prediccion contra la realidad

table()
