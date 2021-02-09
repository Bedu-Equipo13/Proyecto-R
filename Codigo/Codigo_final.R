
#librerias necesarias 
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(purrr)
library(ggpubr)
library(forecast)
library(stringr)

#setwd("../")
#descargando el archivo necesario 
url="https://www.inegi.org.mx/contenidos/programas/accidentes/datosabiertos/atus_anual_csv.zip"
zip_file="atus_anual_csv.zip"
if (!file.exists(zip_file)){
    download.file(url, destfile = zip_file , mode = 'wb')
}


#descomprimiendo el archivo descargado 
if (!dir.exists("atus_anual_1997_2019")) { 
    unzip("atus_anual_csv.zip") 
}

#creando una lista con los nombres de los archivos (para 10 años)
names<-sort(dir("atus_anual_1997_2019/conjunto_de_datos/"),decreasing = TRUE)[1:10]

for(i in seq_along(names)) {
     names[i]<-(paste0("atus_anual_1997_2019/conjunto_de_datos/",names[i]))
  
}

#Extrayendo el nombre de las columnas 
column_names<-fread("atus_anual_1997_2019/diccionario_de_datos/diccionario_de_datos_atus_anual_1997_2019.csv",
                   select = 1,data.table = F)[,1]

#extrayendo los archivos y almacenandolos en una lista
lista<-lapply(names,fread,select = (1:45),data.table = F,col.names = column_names, encoding = 'UTF-8')

#verificando los archivos
#lapply(lista, str)
#lapply(lista, summary)
#lapply(lista, View)

#estamos interesados en los deatos relacionados con la CDMX por lo cual en base al 
#diccionario de datos, sabemos que el numero de entidad correspiendte a la CDMX es 9
lista<-lapply(lista, function(x) filter(x,ID_ENTIDAD ==9))
#lapply(t2,dplyr::filter, ID_MUNICIPIO ==5)
#lapply(lista, head)
#str(lista[[1]])
#View(lista[[1]])

#combinando la lista de datos en un solo dataframe
data <- do.call(rbind, lista)

#as.factor(data$ID_MUNICIPIO)

#agregamos el nombre de la delegacion dependiendo el codigo Asignado
#primero extraemos los codigos de municipio:

Municipios<-fread("atus_anual_1997_2019/catalogos/tc_municipio.csv",
                  data.table = F,encoding = 'UTF-8',select = c(1:3),
                  col.names = c("Entidad","ID_MUNICIPIO","Municipio"))

#importamos el diccionario de datos para colsultas acerca de variables
dic_datos<-fread("atus_anual_1997_2019/diccionario_de_datos/diccionario_de_datos_atus_anual_1997_2019.csv",encoding = 'UTF-8')

#Filtramos los municipios Pertenecientes a la CDMX
(Municipios<-Municipios%>%filter(Entidad==9,ID_MUNICIPIO!=999)%>%select(ID_MUNICIPIO,Municipio))

#finalmente combinacios el nombre de los municipios y su codigo en el data.frama original
#creando una copia para preservar el set de datos originales.

data_clean<-data%>%left_join(Municipios,by="ID_MUNICIPIO")


#Agreando una columna con el Nombre del Mes de acuerdo al Numero de mes
data_clean$Nom_Mes<-recode(data_clean$MES,"1"="ENE","2"="FEB","3"="MAR",
       "4"="ABR","5"="MAY","6"="JUN","7"="JUL","8"="AGO","9"="SEP",
       "10"="OCT","11"="NOV","12"="DIC")

#Arreglando nombre de los dias de la Semana.
data_clean$DIASEMANA<-recode(data_clean$DIASEMANA,"lunes"="Lunes","Miercoles"="Miércoles","Sabado"="Sábado")


#Agregando columna de Fecha-hora
data_clean<-data_clean%>%mutate(Fecha=paste0(MES,"/",ID_DIA,"/",ANIO," ",ID_HORA,":",ID_MINUTO,":"))

#CAmbiando a formato Fecha-hora utilizando la libreria lubridate
data_clean$Fecha<-mdy_hm(data_clean$Fecha)
#data_clean$Fecha<-with_tz(data_clean$Fecha, "America/Mexico_City")



##Generacion de la grafica de histograma de Numero de Accidentes dependiendo el tipo 
# de vehiculo por Año


#nombre de las columnas de tipo de vehiculo
Modelo<-c("AUTOMOVIL","CAMPASAJ","MICROBUS","PASCAMION","OMNIBUS","TRANVIA","CAMIONETA","CAMION","TRACTOR","FERROCARRI","MOTOCICLET","BICICLETA","OTROVEHIC")   

#convierte las columnas en filas
md2<-gather(data_clean,Modelo,key = "Modelo",value = "Num_accidentes")
#filtra los datos para eliminar lo valores con 9 y las edades de 0  y 99
md2<-md2%>%filter(ID_EDAD!=0,Num_accidentes!=0,ID_EDAD!=99)
#agrupa por modelo y fecha y calcula el numero de elementos
md2<-md2%>%group_by(Fecha,Modelo)%>%select(Fecha,Modelo,Num_accidentes)%>%summarise(n=n())
#agrupa por modelo y fecha
md2<-md2%>%group_by(Modelo,year(Fecha))%>%summarise(n=n())

g <- ggplot(data_clean, aes(Municipio))
g+geom_bar(aes(fill=factor(ANIO)),position = "dodge",col="black")+
  theme(axis.text.x = element_text(angle=45, hjust=1))+scale_fill_brewer(palette="Paired")+ggtitle("Histograma de Accidentes por alcaldia y año") +
  ylab("Accidentes") +  xlab("Años medidos")


#HISTOGRAMA DE ACCIDENTES EN LA CDMX por año
data_clean %>% 
  ggplot() + 
  aes(x = factor(ANIO)) + geom_bar(col= "black", fill = "blue", stat = "count")+
  ggtitle("Histograma de Accidentes en la CDMX") +
  ylab("Accidentes totales") +
  xlab("Años medidos")

#GRAFICA DE ACCIDENTES TOTALES POR ALCALDIA 
data_clean %>%
  ggplot() + 
  aes(x = Municipio) + geom_bar(col= "black", fill = "red", stat = "count")+
  ggtitle("Histograma de Accidentes totales por alcaldia") +
  ylab("Accidentes") +
  xlab("Alcaldia")+theme(axis.text.x = element_text(angle=45, hjust=1))


#GRAFICA DE ACCIDENTES POR SEXO Y AÑO

data_clean %>% 
  ggplot() + 
  aes(x = factor(ANIO)) + geom_bar(aes(fill = SEXO),position = "dodge",col="black",stat = "count")+
  ggtitle("Histograma de Accidentes por Sexo y año") +
  ylab("Accidentes") +
  xlab("Años medidos")


#GRAFICA DE ACCIDENTES EN BASE AL MES Y AÑO 
#con todos lo datos unidos
data_clean %>% 
  ggplot() + 
  aes(x = month(Fecha,label = TRUE)) + geom_bar(aes(fill = factor(ANIO)),position = "dodge",col="black")+
  ggtitle("Histograma de Accidentes por Sexo y año") +
  ylab("Accidentes") +
  xlab("Mese medidos")+theme(axis.text.x = element_text(angle=45, hjust=1))+scale_fill_brewer(palette="Paired")+labs(fill = "YEAR")

#datos separados  
data_clean %>% 
    ggplot() + 
    aes(x = month(Fecha,label = TRUE)) + geom_bar(aes(fill = factor(ANIO)),position = "dodge",col="black")+
    ggtitle("Histograma de Accidentes por Sexo y año") +
    ylab("Accidentes") +theme(axis.text.x = element_text(angle=45, hjust=1))+scale_fill_brewer(palette="Paired")+facet_wrap(.~ANIO,scales = "free_x")+labs(fill = "YEAR")
  

#ACCIDENTES DE ACUERDO A LA HORA Y EL DIA 
#datos unidos

data_clean %>% 
  ggplot() + 
  aes(x = factor(ID_HORA)) + geom_bar(aes(fill = wday(Fecha,label = T,abbr = F)),position = "dodge",col="black")+
  ggtitle("Histograma de Accidentes por Dia del 2010-2019") +
  ylab("Accidentes") +
  xlab("Horas del Dia")+labs(fill = "Dia")+scale_fill_brewer(palette="Paired")#+facet_wrap(.~DIASEMANA,scales = "free_x")

#datos Separados

data_clean %>% 
  ggplot() + 
  aes(x = factor(ID_HORA)) + geom_bar(aes(fill = wday(Fecha,label = T,abbr = F)),position = "dodge",col="black")+
  ggtitle("Histograma de Accidentes por Dia y hora del 2010-2019") +
  ylab("Accidentes") +
  xlab("Horas del Dia")+labs(fill = "Dia")+scale_fill_brewer(palette="Paired")+facet_wrap(.~ wday(Fecha,label = T,abbr = F),scales = "free_x")+ 
  theme(axis.text.x = element_text(angle=45, hjust=1))



#GRAFICA DE ACCIDENTES DeACUERDO A LA EDAD 

data_clean %>% 
  ggplot() + 
  aes(x = ID_EDAD) + geom_bar(col= "black", fill = "blue", stat = "count")+
  ggtitle("Histograma de Accidentes en la CDMX") +
  ylab("Accidentes totales") +
  xlab("Edad del responsable")


#### ANALISis INFERENCIALES##

#Apartir de la utima grafica obtenida de numero de accidentes en funcion de la edad
#se comienza a analizar los datos. Primero se observa que los valores de edad 0 y 100
#albergan demasiados datos, de acuerdo a los registros del INEGI cuando el conductor se fugaba, los registros
# asignan el valor 0 a la eda. En cuanto a 99 significa que se desconoce la edad del conductor.

(data_clean%>%group_by(ID_EDAD)%>%summarise(num_acc=n())%>%filter(ID_EDAD==0|ID_EDAD==99)%>%mutate(proportion=num_acc/dim(data_clean)[1]))

#Son considerables la cantidad de datos y causan que la distribucion se ve afectada, para fines 
#practicos se decidio eliminar estos datos extremos 


df_edad<-data_clean %>% filter(ID_EDAD!=0,ID_EDAD!=99)#%>%summarise(mean=median(ID_EDAD))

#ELiminando los valores la distribucion de edades queda de la siguiente forma:

g<-data_clean %>% filter(ID_EDAD!=0,ID_EDAD!=99)%>%
  ggplot() + 
  aes(x = ID_EDAD) + geom_bar(col= "black", fill = "blue", stat = "count")+
  ggtitle("Histograma de Accidentes en la CDMX") +
  ylab("Accidentes totales") +
    xlab("Edad del Conductor")
g+geom_vline(aes(xintercept=median(ID_EDAD),color="median"),linetype="dashed",size=1) +
  geom_vline(aes(xintercept=mean(ID_EDAD),color="mean"),linetype="dashed", size=1) +
  scale_color_manual(name = "Medidas", values = c(median = "green", mean = "red"))

#teniendo los siguientes valores de media y mediana
mean(df_edad$ID_EDAD);median(df_edad$ID_EDAD)

#Para poder realizar algunas hipotesis primero determinamos si la distribucion de las
#edades se comporta como una distribucion normal, primero realizamos un grafico Q-Q plot
#PAra ver si los datos se ajustan
df_edad<-data_clean%>%filter(ID_EDAD!=0,ID_EDAD!=99)%>%group_by(ID_EDAD)%>%summarise(n=n())

qqnorm(df_edad$n, pch = 19, col = "gray50")
qqline(df_edad$n)

# de acuerdo a los graficos la distribucion no se comporta como una normal, podemos realizar un
#Shapiro test para comprobarlo

(shapiro.test(df_edad$n))

# tal y como lo supusimos no se comporta de manera normal.Por esta razon se decidio hacer 
# pruebas No parametricas entre estas se encuentra  el test de Mann–Whitney–Wilcoxon
# la cual contrasta que la probabilidad de que una observación de la población X supere a una observación de la población Y
# es igual a la probabilidad de que una observación de la población "Y" supere a una de la población X. 
#Es decir, que los valores de una población no tienden a ser mayores que los de otra.
# las hipotesis por lo tanto serian
#H0:P(X>Y)=P(Y>X)
#Ha:P(X>Y)≠P(Y>X)

# Es común encontrar mencionado que el test de Mann–Whitney–Wilcoxon compara medianas
#arreglamos el data set con las distribuciones en funcion de la edad y Dividimos la muestra
#en dos grupos el primer grupo pertenece a las personas mayores o iguales a 36 y
#el segundo grupo pertence al grupo menor de 36. se decidio usar estos valores debido
#a que la media y la mediana de la poblacion estaba entre 36 y 37.

#por lo Tanto en esta prueba se compararan estos dos grupo para saber si existe una diferencia
# entre los dos grupos.

df_edad<-data_clean%>%filter(ID_EDAD!=0,ID_EDAD!=99)%>%group_by(ID_EDAD)%>%summarise(n=n())%>%mutate(Rango_edad=ifelse(ID_EDAD<36,"menor 36","mayor igual 36"))

# si observamos los dos grupos recien creados en un grafico Q-Q podemos ver que su distribucion es muy paracida.
#por lo cual podemos aplicar esta prueba, que no requiere que los grupos tengan distribucion normal,
# pero si que tengan una asimetrica o parecida
ggplot(df_edad, aes(sample = n, col = Rango_edad))+
  stat_qq()+
  stat_qq_line()+
  facet_grid(.~ Rango_edad)

#aplicando el test wilcox

(wilcox.test(n ~ Rango_edad, data = df_edad))

#de acuerdo al p-value ibtenido por la prueba podemos rechaza la hipotesis nula la cual
#nos dice que no existe diferencia entre los dos grupos. por lo tanto se apoya la hipotesis alternativa
# y podemoss afirmar que existe una diferencia entre los grupos

#Utilizando la libreria ggpur podemos visulizar los resultados 

ggplot(df_edad, aes(x = Rango_edad, y = n))+
  geom_boxplot()+
  stat_compare_means()+ylab("Numero de accidentes")

#por los resultados obtenidos podemos observar que las medianas son diferentes
# y existe una diferencia considerebale entre los dos grupos de acuerdo al numero
# de accidentes siendo mayor en grupos de menos de 36 años

#este tipo de prueba es analoga al t_test con la diferencia que el t_tes solo se puede aplicar
#a distribuciones normales


## ¿Existe una diferencia entre grupos de edad en cuanto a la probabilidad de sufrir un accidente?

# para probar esto utilizaremos una prueba Chi-Squared Goodness of Fit para determinar la
#proporcion entre los grupos de edades 

#filtramos los datos eliminando las edades 0 y 99
mult<-data_clean%>%filter(ID_EDAD!=0,ID_EDAD!=99)%>%select(ID_EDAD)

#calculamos los quatiles de las edades para crear los grupos
(quantile(mult$ID_EDAD, prob=c(0,0.25,0.5,0.75,1)))

#dividimos la muestra en grupos de acuerdo a los quantiles
mult<-mult%>%mutate(Rango_edad = case_when((ID_EDAD==12 | ID_EDAD<28) ~ "12 a <28",
                                           (ID_EDAD==28 | ID_EDAD<36) ~ "28 a <36",
                                           (ID_EDAD==36 | ID_EDAD<45) ~ "36 a <45",
                                           (ID_EDAD==45 | ID_EDAD<=95) ~ "45 a 95",
                                           TRUE ~ "other"))

#Conteo de los diferentes grupos 
table(mult$Rango_edad)/dim(mult)[1]

#de acuerdo a los conteos para ser muy cercanas los conteos no se aprecia una diferencia tan marcada
# para comprobar si los accidentes se dan en la misma proporcion no importando el rango
# de edad realizamos un Chi-Squared Goodness of Fit
#las hipotesis de la prueba son las siguientes:
#ho todos los grupos tienen la misma probabilidad de sufrir accidentes
#ha los grupos tienen valores diferentes

(chisq.test( x = table(mult$Rango_edad) ))


###test binomial Basado en sexo: ¿Hombres o mujeres son mas propensos a sufrir accidentes?

#Debido a que el sexo es una variable independiente ya que puede tomar unicamente el valor
#de hombre o mujer, esta puede ser analizada por medio de una distribucion binomial

#primero recordando los graficos se accidentes por tipo de sexo. hay varios registros
# que reportan "se fugo" como valor a la variable sexo, analizando la proporcon de estos
# valores tenemos:

(data_clean%>%group_by(SEXO)%>%summarise(Accidentes=n())%>%mutate(Proporcion=(Accidentes/sum(Accidentes))*100))


#hay una clara diferencia en las proporciones entre hombres y mujeres, de igual forma 
#existe muchos registros de Fuga, sin embargo para fines de la hipotesis se excluiran estos valores.

#Extraemos los datos necesarios exluyendo el valor "se fugo"
h_m<-data_clean%>%filter(SEXO!="Se fugó")%>%select(SEXO)

#Primero empezaremos por analizar las proporciones 
#Hipótesis para la primer prueba 

#H0: la proporción de Accidentes es igual (50%) para ambos sexos, por lo tanto p=0.5
#Ha: la proporción de Accidentes no es igual entre ambos sexos p≠0.5
#revisando la proporcion de datos nuevamente
(tabla <- table(h_m))


#realizando el test binomial a dos colas obtenemos :
(binom.test(x = tabla, alternative = "two.sided", conf.level = 0.95))

#El test binomial rechaza la hipotesis nula en funcion del valor p, por lo tanto las 
#distribuciones entre los dos grupos no son iguales

#REalizando otra prueba binomial ahora considerando las siguientes hipotesis
#H0: la proporción de Accidentes es igual (90%) para ambos sexos, por lo tanto p=0.9
#Ha: la proporción de Accidentes no es igual entre ambos sexos p≠0.9

binom.test(x=tabla,p=0.9, alternative="less")
#podriamos afirmar que la proporcion de hombres y mujeres que sufren accidentes es menor al 90%
#por lo tanto se rechaza la hipotesis nula


#Podemos realizar otra prueba binomial por medio de otros tests como lo es la 
#Prueba de hipótesis para la proporción p de Wald

#Ahora que sabemos que las proporciones no son iguales podemos proceder a 
#realizar una prueba de proporcion considerando las siguientes hipotesis
#H0:p=0.90 H1:p<0.90
#Con un nivel α=0.05.
dim(h_m)[1]
z <- (103402/dim(h_m)[1] - 0.90) / sqrt(0.90 * (1 - 0.90) / dim(h_m)[1])
z  # Para obtener el valor del estadístico

#Para obtener el valor-P de la prueba debemos tener en cuenta el sentido en la hipótesis alternativa H1:p<0.87, por esa razón el valor-P será P(Z<z) y para obtenerlo usamos el siguiente código
pnorm(q=z, lower.tail=T)  # Para obtener el valor-P

#en base a este valor podemos rechazar la hipotesis nula y decir que la proporcion es en efecto menor a 90

#Prueba \Chi2 de Pearson
#Para realizar la prueba \Chi2 de Pearson se usa la función prop.test
#H0:p=0.85 H1:p not equal 0.85
#Con un nivel α=0.05.

(prop.test(tabla, p=0.85, alternative="two.sided",
          conf.level=0.95, correct=FALSE))


#por lo tanto nuestra proporcion estra en tre 85 y 90% 


###COMO ha cambiado la proprocion a lo largo de 10 años?

#pero como ha cambiado esta proporcion a lo largo de estos 10 años para esto utilizamos
#la funcion binom.test para realizar la prueba a las muestras filtradas por año

tt<-data_clean%>%group_by(Fecha,ANIO)%>%filter(SEXO!="Se fugó")%>%select(SEXO,ANIO)
tt<-tt%>%group_by(ANIO)%>%select(SEXO)
#hacemos data set de acuerdo al año
tt<-group_split(tt)
#creamos una tabla para diferencial a hombres y mujeres
tt<-map(tt,table)
#aplicamos la prueba binomial a todos los años
(tt<-map(tt,binom.test,p=0.8, alternative="greater"))

#en todos los casos se rechaza la hipotesis nula de la probabilidad igual a 0.8 y 
#se apoya la hipotesis de la proporcion mayo a 0.8 (hipotesus nula)

paste((map(tt,`[[`,3)),"p-value")


###Existe una relacion entre Modelo del vehiculo y Sexo?

#En el analis de datos se genero la siguiente grafica
md2<-gather(data_clean,Modelo,key = "Modelo",value = "Num_accidentes")
md2<-md2%>%filter(ID_EDAD!=0,Num_accidentes!=0,ID_EDAD!=99)
md2<-md2%>%group_by(Fecha,Modelo,ID_EDAD)%>%summarise(n=n())
md2<-md2%>%group_by(Modelo,ID_EDAD)%>%summarise(n=n())

ggplot(data = md2, aes(Modelo,n)) +
  geom_boxplot(aes(colour = ID_EDAD < 37))+ ylim(range(0:3000))+ylab("Numero de Accidentes")+
  theme(axis.text.x = element_text(angle=45, hjust=1))

#En el grafico podemos ver como existe una pequeña relacion entre el tipo de vehiculo y
#el sexo del coductor por lo cual podremos a prueba esta relacion. Por lo cual realizaremos una prueba independencia
#H0: The two variables are independent.
#H1: The two variables relate to each other.

#nombre de los vehiculos para la variable modelos
md2<-md2%>%mutate(Rango_edad = case_when((ID_EDAD==12 | ID_EDAD<28) ~ "12 a <28",
                                         (ID_EDAD==28 | ID_EDAD<36) ~ "28 a <36",
                                         (ID_EDAD==36 | ID_EDAD<45) ~ "36 a <45",
                                         (ID_EDAD==45 | ID_EDAD<=95) ~ "45 a 95",
                                         TRUE ~ "other"))
(table(md2$Modelo, md2$Rango_edad))

(chisq.test(table(md2$Modelo, md2$Rango_edad), simulate.p.value = TRUE))
#(chisq.test(md2$Modelo, md2$Rango_edad))

#De acuerdo a lo obtenido no se puede rechazar la hipetesis nula por lo cual podemos 
#decir que es probable que exista una independencia entre las variables
#edad y modelo de carro

#Se analiza el dataset para empezar con la construcción de los análisis de regresión que nos permitan evaluar los diferentes predictores del número de accidentes y otras variables de respuesta.

View(data_clean)
count(data_clean)

#Se utilizo el método de selección Forward, para la introducción de las variables dentro de los ajustes multivariables. Debido a que existe un sesgo importante de los datos (SEXO y EDAD) se considero evaluar SEXO y EDAD en la mayoría de nuestros análisis de regresión por separado.
#La metodología para todos y cada uno de los análisis de regresión fue la limpieza y estandarización de los data sets. Al tener varias variables categóricas y dada la naturaleza del data set original se requirió agrupar los datos con el fin de crear una variable continua que permitiera llevar a cabo el análisis de regresión multivariable

V_regresion1 <- data_clean %>%
  group_by(Municipio,ID_HORA, DIASEMANA) %>%
  summarize(Municipio,ID_HORA, DIASEMANA) 

gripi <- count(V_regresion1) %>% 
  mutate(DIASEMANA = str_to_upper(DIASEMANA)) %>%
  mutate(DIASEMANA = str_replace(DIASEMANA,"MIÉRCOLES","MIERCOLES")) %>%
  mutate(DIASEMANA = str_replace(DIASEMANA,"SÁBADO","SABADO"))

modu <- lm(n ~ Municipio + ID_HORA + DIASEMANA , data=gripi)
modul <- lm(n ~ Municipio*ID_HORA*DIASEMANA, data=gripi)

summary(modu)
anova(modu,modul)

#Análisis de Coeficientes:
#  Se toma como referencia por default a la alcaldía Álvaro Obregón, Observamos que los coeficientes de los factores de predicción del número de accidente durante los años 2010 - 2019 indican que la Alcaldía Cuauhtemóc tendrá más accidente que cualquiera de las otras alcaldías, de la misma manera observamos que entre más tarde sea contemplando un horario de 0-24 horas, existirá un aumento en el número de choques, finalmente el día viernes se infiere será el día con mayor número de choques.

#Significancia:
#  Unicamente los coeficientes calculados para los factores de Alcaldía Tlalpán y Venustiano Carranza no fueron significativos.
#Valor de R^2 por encima de 0.5, y valor de P-Value del ajuste menor a alpha.

#Interacción:
#  Calculamos la interacción entre los factores y encontramos que la interacción si es significativa por lo que la combinación de la Alcaldía, la hora y el día de la semana si tendrá un impacto en la cantidad de accidentes reportados.



v_regresion2 <- data_clean %>%
  group_by(ID_EDAD, SEXO, AUTOMOVIL) %>%
  summarize(ID_EDAD,SEXO, AUTOMOVIL)

grip <- count(v_regresion2) %>%
  filter(ID_EDAD<90, ID_EDAD > 15, SEXO != "Se fugó") 
automovil <- grip %>%
  group_by(ID_EDAD,SEXO) %>%
  filter(AUTOMOVIL>0) %>%
  summarize(AUTOMOVIL = sum(n))

View(automovil)

modi <- lm(AUTOMOVIL ~ ID_EDAD + SEXO, data=automovil)
modil <- lm(AUTOMOVIL ~ ID_EDAD * SEXO, data=automovil)
summary(modi)
anova(modi,modil)


#Análisis de Coeficientes: 
  
#Encontramos tras el ajuste lineal que a medida que aumenta la edad la cantidad de choques se reduce, el sexo como dicho anteriormente es un factor determinante para la cantidad de choques reportados.

#Significancia:
  
#  Todos los valores fueron significativos con valor de p-value muy por debajo del nivel de alpha, El p-value del modelo también nos indica que es significativo

#Interacción:
  
#  Si existe significancia, por lo que si existe una interacción entre el valor de la edad y el sexo, para la determinación de choques con automóvil

v_regresion3 <- data_clean %>%
  group_by(ID_EDAD, SEXO, MOTOCICLET) %>%
  summarize(ID_EDAD,SEXO, MOTOCICLET) 

gript <- count(v_regresion3) %>%
  filter(ID_EDAD<90, ID_EDAD > 15, SEXO != "Se fugó") 


MOTORAD <- gript %>%
  group_by(ID_EDAD,SEXO) %>%
  filter(MOTOCICLET>0) %>%
  summarize(MOTOCICLET = sum(n))

View(MOTORAD)


modiN <- lm(MOTOCICLET ~ ID_EDAD + SEXO, data=MOTORAD)
modilN <- lm(MOTOCICLET ~ ID_EDAD * SEXO, data=MOTORAD)

summary(modiN)
anova(modiN,modilN)


#Análisis de Coeficientes: 
  
#  Corroboramos tras el ajuste lineal que a medida que aumenta la edad, la cantidad de choques se reduce, el sexo como dicho anteriormente es un factor determinante para la cantidad de choques reportados.

#Significancia:
  
#  Todos los valores fueron significativos con valor de p-value muy por debajo del nivel de alpha, El p-value del modelo también nos indica que es significativo

#Interacción:
  
#  Si existe significancia, por lo que si existe una interacción entre el valor de la edad y el sexo, para la determinación de choques con motocicleta

#Prueba de Hipótesis

#Se efectúo una prueba de hipótesis con la finalidad de mitigar el sesgo relacionado al tipo de vehículo. Para ello se utilizo la información de otro data set del INEGI, con lo cual se encontró la cantidad de parque vehicular de las variables de Autos y Motocicletas durante los años 2010-2019. De esta manera se midió la diferencia de proporciones entre los dos vehículos con mayor número de reportes de accidentes registrados. "https://www.inegi.org.mx/contenidos/programas/vehiculosmotor/datosabiertos/vmrc_anual_csv.zip"



Prueba_de_motosautos <- prop.test(x = c(122172, 9703), n = c(4714909, 282450), alternative="greater")
Prueba_de_motosautos



#Los resultados de la prueba de hipótesis nos arrojan que la proporción de choques de autos comparada con el parque vehicular de 
#este tipo de vehículo es menor que la proporción de choques en moto comparada con el parque vehicular de este.
#De esta manera podemos inferir que efectivamente el tipo de vehiculo tendrá un impacto en la ocurrencia o probalidad de colisión
#siendo de acuerdo a esta hipótesis las motos un vehículo mas propicio a choque que los autos.

#Regresión Lineal

#Finalmente se efectuaron dos regresiones lineales más con el fin de buscar los fcatores más determinantes para los casos de accidentes con fuga de conductor y los accidentes de tipacidad fatal (Algún involucrado fallecido), a continuación se evalúan los resultados

v_regresion4 <- data_clean %>%
  group_by(Municipio,SEXO,DIASEMANA,ID_HORA, CLASACC, CINTURON) %>%
  summarize(Municipio,SEXO,DIASEMANA,ID_HORA, CLASACC, CINTURON) 

gripig <- count(v_regresion4) %>% 
  mutate(DIASEMANA = str_to_upper(DIASEMANA)) %>%
  mutate(DIASEMANA = str_replace(DIASEMANA,"MIÉRCOLES","MIERCOLES")) %>%
  mutate(DIASEMANA = str_replace(DIASEMANA,"SÁBADO","SABADO")) %>%
  filter(SEXO == "Se fugó") %>%
  group_by(Municipio)

View(gripig)

modulik <- lm(n ~ Municipio + ID_HORA + DIASEMANA , data=gripig)
modulilkt <- lm(n ~ Municipio*ID_HORA*DIASEMANA, data=gripig)
summary(modulik)
anova(modulik,modulilkt)




#Análisis de Coeficientes: 
  
#  Se reportó a la Alcaldía de Iztapalapa, seguida por Alcaldía Cuahtémoc y Gustavo A. Madero como las alcaldías con mayor tendencia a reporte de fuga de conductor. La hora es significativa con un valor de pvalue muy por debajo del valor de alpha y presenta una correlación directa, es decir que conforme las horas del día avanzan incrementa el número de choques con conductor que se fuga. El día Miércoles se infiere y espera sea el día con menor número de accidentes en el que conductar se fuga

#Significancia:
  
#  Unicamente los valores de la alcaldía  de Azcapotzalco y Tlalpan, aunados al día Viernes y Jueves no son significativos. El Modelo con una valor muy pequeño de p-value rectifica que es significativo

#Interacción:
  
#  Se comprueba la interacción entre las variables ajustadas.



v_regresion5 <- data_clean %>%
  group_by(Municipio,ALIENTO, CINTURON, CLASACC) %>%
  summarize(Municipio, ALIENTO, CINTURON, CLASACC) 

gripigb <- count(v_regresion5) %>%
  filter(CLASACC == "Fatal")  %>%
  filter(CINTURON != "Se ignora" ) %>%
  filter(ALIENTO != "Se ignora")

View(gripigb)

modulikur <- lm(n ~ Municipio + ALIENTO + CINTURON, data=gripigb)
modulilktr <- lm(n ~ Municipio*ALIENTO*CINTURON, data=gripigb)
summary(modulikur)
anova(modulikur,modulilktr)



#Análisis de Coeficientes:
  
#La alcaldía con menor proyección a albergar accidentes fatales es Magdalena Contreras, mientras que la alcaldía Gustavo Madero es la de mayor proyección de accidentes fatales. La variable de aliento alcohólico presenta una correlación inversamente proporcional (Esto debido a un sesgo en la información) Finalmente la variable de uso del cinturón si es significativa con un valor de Pvalue de .0004 y será también efectiva para la reducción del incremento en el número de choques fatales

#Significancia:
  
#EL valor de R2 es alto (>.7) por lo que el ajuste es bueno, y las variables Cinturónsí y alientosí (variables dummy creeadas por R)  fueron significativas 

#Interacción:
#La interacción es significativa entre las variables utilizadas para el ajuste 


#ANALISIS de TIempo 
#Se realizo la prediccion del numero de acccidentes para dos años, mediante el uso de series de
#tiempo. Para lo cual se ocupo un modelo ARIMA


"Prediccion de accidentes para el 2020 al 2022 "
#se utilizo la paqueteria Forecast para comparar modelos
#importamos los datos  de tiempo
#Agrupamos el numero de accidentes en funcion del mes
time<-data_clean%>%group_by(year(Fecha),month(Fecha))%>%summarise(Num_acc=n())
#creando la serie de tiempo en funcion del mes
time.ts <- ts(time$Num_acc, start =2010,freq = 12)

head(time.ts)

#Visulizamos la serie de tiempo obtenida 

#grafico de la serie de tiempo
plot(time.ts, 
     main = "Serie de tiempo diferenciada", 
     ylab = "Accidentes",
     sub = "Enero de 2010 - Diciembre de 2019")

#Analizamos la serie de tiempo de acuerdo al modelo Additivo y Multiplicativo para buscar
#estacionalidad

#Modelo aditivo
acc.decom.A <- decompose(time.ts)
plot(acc.decom.A, xlab = "Tiempo", 
     sub = "Descomposición de los datos de accidentes")
#Componentes
Tendencia <-acc.decom.A$trend
Estacionalidad <- acc.decom.A$seasonal
Aleatorio <- acc.decom.A$random

ts.plot(cbind(Tendencia, Tendencia + Estacionalidad), 
        xlab = "Tiempo", main = "Datos de Accidentes", 
        ylab = "Numero de accidentes", lty = 1:2,
        sub = "Tendencia con efectos estacionales aditivos sobrepuestos")

#Modelo Multiplicativo

acc.decom.M <- decompose(time.ts, type = "mult")

plot(acc.decom.M, xlab = "Tiempo", 
     sub = "Descomposición de los datos de producción de electricidad")
#Componentes
Trend <- acc.decom.M$trend
Seasonal <- acc.decom.M$seasonal
Random <- acc.decom.M$random

ts.plot(cbind(Trend, Trend*Seasonal), xlab = "Tiempo", main = "Datos de Producción de Electricidad", 
        ylab = "Producción de electricidad", lty = 1:2,
        sub = "Tendencia con efectos estacionales multiplicativos sobrepuestos")


#Como podemos notar esta serie de tiempo no tiene estacionalidad y una varianza constante
# por lo cual es necesario hacer una transformacion.

plot(diff(time.ts), 
     main = "Serie de tiempo diferenciada", 
     ylab = "Accidentes",
     sub = "Enero de 2010 - Diciembre de 2019")

#Se realizo una transformacion por diferencias y se obtuvo una serie de tiempo con una
#varianza mas constante buescando obteniendo estacionalidad, por lo cual es posible hacer nuestras prediccion de manera mas facil

#se intento con una tranformacion logaritmica sin embargo no hubo una buena aproximacion.
plot(log(time.ts), 
     main = "Serie de tiempo diferenciada", 
     ylab = "Accidentes",
     sub = "Enero de 2010 - Diciembre de 2019")

#Mediante la siguiente funcion buscaremos obtener los mejores ordenes y modelo 
# para realizar el pronostico de la serie de tiempo

get.best.arima <- function(x.ts, maxord = c(1, 1, 1, 1, 1, 1)){
  best.aic <- 1e8
  n <- length(x.ts)
  for(p in 0:maxord[1])for(d in 0:maxord[2])for(q in 0:maxord[3])
    for(P in 0:maxord[4])for(D in 0:maxord[5])for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p, d, q),
                   seas = list(order = c(P, D, Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2*fit$loglik + (log(n) + 1)*length(fit$coef)
      if(fit.aic < best.aic){
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p, d, q, P, D, Q)
      }
    }
  list(best.aic, best.fit, best.model)
}

#Ocupando la funcion recien declarada buscamos el mejor modelo y orden
best.arima.elec <- get.best.arima(diff(time.ts),
                                  maxord = c(2, 2, 2, 2, 2, 2))

best.fit.elec <- best.arima.elec[[2]]  # Modelo
best.arima.elec[[3]] # Tipo de modelo (órdenes)
best.fit.elec
best.arima.elec[[1]] # AIC

#Por ultimo analizamos los residuales en graficos de correlogramas de la serie residual

#ACF para residuales del ajuste
acf(resid(best.fit.elec), main = "")
title(main = "Correlograma de los residuales del ajuste")

#El grafico del correlograma luce bien, ya que se ve una distrubucion gausiana de ruido blanco
# Podria mejorarse el modelo realizando otros ajustes en los ordenes, pero se continuara
# con este modelo para generar valores predecidos de accidentes para dos años

#ts(cumsum(c(time.ts[1],pr)), start =2010,freq = 12)
#prediccion 
pr <- predict(best.fit.elec, 24)$pred 

ts.plot(cbind(window(diff(time.ts), start = 2010),
              pr), col = c("blue", "red"), xlab = "")
title(main = "Predicción para la serie Accidentes",
      xlab = "Mes",
      ylab = "Numero de accidentes")


ssd<-diff(time.ts)

#ts(cumsum(c(time.ts[1],ssd)), start =2010,freq = 12)
#ts(cumsum(c(time.ts[1],pr)), start =2019,freq = 12)
ts.plot(cbind(ts(cumsum(c(time.ts[1],ssd)), start =2010,freq = 12), ts(cumsum(c(time.ts[1],pr)), start =2020,freq = 12)), lty = 1:2, 
        col = c("blue", "red"), xlab = "Tiempo", 
        ylab = "Numero de Accidentes",
        main = "Predicción de los Accidentes",
        sub = "Predicción de 24 meses")





#Utilizando el modelo ARIMA utilizamos la funcion auto.arima de la libreria
#forecast para obtener el mejor modelo

auto1=auto.arima(time.ts,D=1,approximation = F,allowdrift = T,allowmean = T)

##este tipo de modelo ya tiene inlcuido la transformacion en el valor D que 
#significa una diferenciacion

#se hace la prediccion el funcion de 24 meses (2 años)
forecast1<-forecast(auto1,h=24)
#Valores generados
head(forecast1)
#graficamos los resultados 
plot(forecast1,ylab = "Accidentes",
     sub = "Enero de 2010 - Diciembre de 2022")

#analizamos los residuales generados
plot(forecast1$residuals)
#analizamos las tendencias de los residuales
qqnorm(forecast1$residuals)


# por ultimo analizamos los residuales en graficos de correlogramas de la serie residual acf y pacf
acf(forecast1$residuals, main = "")
pacf(forecast1$residuals, main = "")

# como sumary podemos analizar varios graficos a la vez
checkresiduals(forecast1)

#DE  igual forma podemos comprobar la precision del modelo 
summary(forecast1)



# Como notamos esta funcion ajusta mejor el modelo d e prediccion ya que nos resultaron residulaes
# que no necesitan la aplicacion de otro modelo para su analisis, por lo tanto nos quedaremos
# con el resultado de este ultimo y como podemos ver, las tendencias de accidentes en la CDMX
# se mantendran constantes segun la prediccion aunque tienen cierta tendencia 
# a disminuir, aunque de acuerdo a nuestros intervalos de confidencia geenrados por el modelo
# este tambien podria aumentar.

