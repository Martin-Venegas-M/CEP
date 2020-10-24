# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 17 - 9 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car, foreign)

# ---- Etapa 2: Cargar bases de datos ---
bd2003_2005_45 <- read_excel("input/data/2003-2005/cep45unjul2003.xls")
bd2003_2005_46 <- read_excel("input/data/2003-2005/cep46dic2003.xls")
bd2003_2005_47 <- read_excel("input/data/2003-2005/cep47jul2004.xls")
bd2003_2005_48 <- read.spss("input/data/2003-2005/cep48dic2004.sav",to.data.frame = TRUE)
bd2003_2005_49 <- read.spss("input/data/2003-2005/cep49junjul2005.sav",to.data.frame = TRUE)
bd2003_2005_50 <- read_excel("input/data/2003-2005/cep50agosep2005.xls")
bd2003_2005_51 <- read_excel("input/data/2003-2005/cep51octnov2005.xls")

# ---- Etapa 3: Procesamiento de datos ----
## Seleccionar variables a utilizar
bd2003_2005_45 <- select(bd2003_2005_45, DE17, DE4, DE2, DE1,  P17_A:P17_F, P17_J, P17_L:P17_J,  P7, P9) 
bd2003_2005_46 <- select(bd2003_2005_46, DE28, DE5, DE2, DE1, P9, P11) #no hay confianza
bd2003_2005_47 <- select(bd2003_2005_47, DE17, DE4, DE2, DE1, P9, P11) #no hay confianza
bd2003_2005_48 <- select(bd2003_2005_48, DE17, DE4, DE2, DE1, P11, P13) #no hay confianza
bd2003_2005_49 <- select(bd2003_2005_49, de29, de5, de2, de1, p8, p10) #no hay confianza
bd2003_2005_50 <- select(bd2003_2005_50, de22, de4, de2, de1, p9, p11)#no hay confianza
bd2003_2005_51 <- select(bd2003_2005_51, de22, de4, de2, de1, p9, p11)#no hay confianza

## Renombrarlas
### 2003-2005: CEP 45
bd2003_2005_45 <- rename(bd2003_2005_45,
                         nse = DE17,
                         esc = DE4,
                         edad = DE2,
                         sexo = DE1,   
                         id_part = P7,
                         pos_pol =  P9,
                         conf_iglesiacat = P17_A,
                         conf_iglesiaev = P17_B,
                         conf_partidos = P17_J,
                         conf_congreso = P17_K,
                         conf_ffaa = P17_C,
                         conf_prensa = P17_E,
                         conf_tele = P17_F,
                         conf_empr = P17_L,
                         conf_tribun = P17_D)

### 2003-2005: CEP 46
bd2003_2005_46 <- rename(bd2003_2005_46,
                         nse = DE28,
                         esc = DE5,
                         edad = DE2,
                         sexo = DE1,
                         id_part = P9,
                         pos_pol = P11)

### 2003-2005: CEP 47
bd2003_2005_47 <- rename(bd2003_2005_47,
                         nse = DE17,
                         esc = DE4,
                         edad = DE2,
                         sexo = DE1,
                         id_part = P9,
                         pos_pol = P11)

### 2003-2005: CEP 48
bd2003_2005_48 <- rename(bd2003_2005_48,
                        nse = DE17,
                        esc = DE4,
                        edad = DE2,
                        sexo = DE1,
                        id_part = P11,
                        pos_pol = P13)

### 2003-2005: CEP 49
bd2003_2005_49 <- rename(bd2003_2005_49,
                         nse = de29,
                         esc =  de5,
                         edad = de2,
                         sexo = de1,
                         id_part = p8,
                         pos_pol = p10)

### 2003-2005: CEP 50
bd2003_2005_50 <- rename(bd2003_2005_50,
                         nse = de22,
                         esc =  de4,
                         edad = de2,
                         sexo = de1,
                         id_part = p9,
                         pos_pol = p11)

### 2003-2005: CEP 51
bd2003_2005_51 <- rename(bd2003_2005_51,
                         nse = de22,
                         esc =  de4,
                         edad = de2,
                         sexo = de1,
                         id_part = p9,
                         pos_pol = p11)

#---- 3.1 Tratamiento de  sociodemográficas ----
#---- 3.1.1 Frecuencias ----
### 2003-2005: CEP 45
frq(bd2003_2005_45$nse) 
frq(bd2003_2005_45$esc) 
frq(bd2003_2005_45$edad) 
frq(bd2003_2005_45$sexo) 

### 2003-2005: CEP 46
frq(bd2003_2005_46$nse) 
frq(bd2003_2005_46$esc) 
frq(bd2003_2005_46$edad) 
frq(bd2003_2005_46$sexo) 

### 2003-2005: CEP 47
frq(bd2003_2005_47$nse) 
frq(bd2003_2005_47$esc) 
frq(bd2003_2005_47$edad) 
frq(bd2003_2005_47$sexo) 

### 2003-2005: CEP 48
frq(bd2003_2005_48$nse) 
frq(bd2003_2005_48$esc) 
frq(bd2003_2005_48$edad) 
frq(bd2003_2005_48$sexo) 

### 2003-2005: CEP 49
frq(bd2003_2005_49$nse) 
frq(bd2003_2005_49$esc) 
frq(bd2003_2005_49$edad) 
frq(bd2003_2005_49$sexo) 

### 2003-2005: CEP 50
frq(bd2003_2005_50$nse) 
frq(bd2003_2005_50$esc) 
frq(bd2003_2005_50$edad) 
frq(bd2003_2005_50$sexo) 

### 2003-2005: CEP 51
frq(bd2003_2005_51$nse) 
frq(bd2003_2005_51$esc) 
frq(bd2003_2005_51$edad) 
frq(bd2003_2005_51$sexo) 

#---- 3.1.2 Recodificación ----
### 2003-2005: CEP 45
bd2003_2005_45$nse  <- car::recode(bd2003_2005_45$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2003_2005_45$esc <- car::recode(bd2003_2005_45$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2003_2005_45$edad <- car::recode(bd2003_2005_45$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2003_2005_45$sexo <- car::recode(bd2003_2005_45$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 2003-2005: CEP 46
bd2003_2005_46$nse  <- car::recode(bd2003_2005_46$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2003_2005_46$esc <- car::recode(bd2003_2005_46$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2003_2005_46$edad <- car::recode(bd2003_2005_46$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2003_2005_46$sexo <- car::recode(bd2003_2005_46$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 2003-2005: CEP 47
bd2003_2005_47$nse  <- car::recode(bd2003_2005_47$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2003_2005_47$esc <- car::recode(bd2003_2005_47$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2003_2005_47$edad <- car::recode(bd2003_2005_47$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2003_2005_47$sexo <- car::recode(bd2003_2005_47$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 2003-2005: CEP 48
bd2003_2005_48$nse  <- car::recode(bd2003_2005_48$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2003_2005_48$esc <- car::recode(bd2003_2005_48$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2003_2005_48$edad <- car::recode(bd2003_2005_48$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2003_2005_48$sexo <- car::recode(bd2003_2005_48$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 2003-2005: CEP 49
bd2003_2005_49$nse  <- car::recode(bd2003_2005_49$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2003_2005_49$esc <- car::recode(bd2003_2005_49$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2003_2005_49$edad <- car::recode(bd2003_2005_49$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2003_2005_49$sexo <- car::recode(bd2003_2005_49$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 2003-2005: CEP 50
bd2003_2005_50$nse  <- car::recode(bd2003_2005_50$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2003_2005_50$esc <- car::recode(bd2003_2005_50$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2003_2005_50$edad <- car::recode(bd2003_2005_50$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2003_2005_50$sexo <- car::recode(bd2003_2005_50$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)
### 2003-2005: CEP 51
bd2003_2005_51$nse  <- car::recode(bd2003_2005_51$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2003_2005_51$esc <- car::recode(bd2003_2005_51$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2003_2005_51$edad <- car::recode(bd2003_2005_51$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2003_2005_51$sexo <- car::recode(bd2003_2005_51$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

#---- 3.2 Tratamiento de variables de confianza ----
#---- 3.2.1 Frecuencias ----
### 2003-2005: CEP 45 
frq(bd2003_2005_45$conf_iglesiacat)
frq(bd2003_2005_45$conf_iglesiaev)
frq(bd2003_2005_45$conf_partidos)
frq(bd2003_2005_45$conf_congreso)
frq(bd2003_2005_45$conf_conf_ffaa)
frq(bd2003_2005_45$conf_prensa)
frq(bd2003_2005_45$conf_tele)
frq(bd2003_2005_45$conf_empr)
frq(bd2003_2005_45$conf_tribun)

### 2003-2005: CEP 46 #no tiene confianza
### 2003-2005: CEP 47 #no tiene confianza
### 2003-2005: CEP 47 #no tiene confianza
### 2003-2005: CEP 49 #no tiene confianza
### 2003-2005: CEP 50 #no tiene confianza
### 2003-2005: CEP 51 #no tiene confianza

#---- 3.2.2 Recodificacion ----
### 2000-2002: CEP 44
bd2003_2005_45$conf_iglesiacat <- car::recode(bd2003_2005_45$conf_iglesiacat,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2003_2005_45$conf_iglesiaev  <- car::recode(bd2003_2005_45$conf_iglesiaev,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2003_2005_45$conf_partidos<- car::recode(bd2003_2005_45$conf_partidos, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2003_2005_45$conf_tele <- car::recode(bd2003_2005_45$conf_tele, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2003_2005_45$conf_congreso <- car::recode(bd2003_2005_45$conf_congreso, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2003_2005_45$conf_ffaa <- car::recode(bd2003_2005_45$conf_ffaa, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2003_2005_45$conf_prensa <- car::recode(bd2003_2005_45$conf_prensa, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2003_2005_45$conf_empr <- car::recode(bd2003_2005_45$conf_empr, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2003_2005_45$conf_tribun <- car::recode(bd2003_2005_45$conf_tribun, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)

# No olvidar
# Codificación original
#1. Mucha confianza
#2. Bastante confianza
#3. No mucha confianza
#4. Ninguna confianza
#8. No sabe
#9. No contesta

#---- 3.2.3 Otros ajustes ----
### Construccion variable mmc
### 2000-2002: CEP 44
bd2003_2005_45$conf_mmc[bd2003_2005_45$conf_tele == 'Mucha confianza' | bd2003_2005_45$conf_prensa == 'Mucha confianza'] <- 'Mucha confianza'
bd2003_2005_45$conf_mmc[bd2003_2005_45$conf_tele == 'Otra' & bd2003_2005_45$conf_prensa == 'Otra'] <- 'Otra'

### Construccion variable iglesia
bd2003_2005_45$conf_iglesia[bd2003_2005_45$conf_tele == 'Mucha confianza' & bd2003_2005_45$conf_prensa == 'Mucha confianza'] <- 'Mucha confianza'
bd2003_2005_45$conf_iglesia[bd2003_2005_45$conf_tele == 'Otra' & bd2003_2005_45$conf_prensa == 'Otra'] <- 'Otra'

### Sacar variables de confianza que no usaremos.
### 2000-2002: CEP 44
bd2003_2005_45 <- select(bd2003_2005_45, -conf_tele, -conf_prensa ) 

#---- 3.2.4 Guardar bases de confianza ----
save(bd2003_2005_45, file = "input/data/bd2003_2005_45.RData")

#---- 3.3 Tratamiento de variables de identificación partidaria e identificación política (o posición política)
#---- 3.3.1 Frecuencias ----

frq(bd2003_2005_45$id_part)
frq(bd2003_2005_45$pos_pol)

frq(bd2003_2005_46$id_part)
frq(bd2003_2005_46$pos_pol)

frq(bd2003_2005_47$id_part)
frq(bd2003_2005_47$pos_pol)

frq(bd2003_2005_48$id_part) # Ya esta recodificado
frq(bd2003_2005_48$pos_pol) # Ya esta recodificado

frq(bd2003_2005_49$id_part) # Ya esta recodificado
frq(bd2003_2005_49$pos_pol) # Ya esta recodificado

frq(bd2003_2005_50$id_part)
frq(bd2003_2005_50$pos_pol)

frq(bd2003_2005_51$id_part)
frq(bd2003_2005_51$pos_pol)

#---- 3.3.2 Recodificacion ----

# 2003 - 2005: CEP 45 
bd2003_2005_45$id_part <- car::recode(bd2003_2005_45$id_part, "c(2,4) = 'Derecha'; 
                                 c(1,5,6,7) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 9 = 'Ninguno'; 
                                 c(8,88,99) = NA", as.factor = T) 
bd2003_2005_45$pos_pol <- car::recode(bd2003_2005_45$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 2003 - 2005: CEP 46 
bd2003_2005_46$id_part <- car::recode(bd2003_2005_46$id_part, "c(1,8,9) = 'Derecha'; 
                                 c(3,4,5,6,7) = 'Centro-Izquierda concertación'; 
                                 2 = 'Izquierda extraconcertación'; 
                                 11 = 'Ninguno'; 
                                 c(10,88,99) = NA", as.factor = T)  
bd2003_2005_46$pos_pol <- car::recode(bd2003_2005_46$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 2003 - 2005: CEP 47 
bd2003_2005_47$id_part <- car::recode(bd2003_2005_47$id_part, "c(5,6) = 'Derecha'; 
                                 c(1,2,3,4) = 'Centro-Izquierda concertación'; 
                                 7 = 'Izquierda extraconcertación'; 
                                 c(9,11) = 'Ninguno'; 
                                 c(88,99) = NA", as.factor = T) 
bd2003_2005_47$pos_pol <- car::recode(bd2003_2005_47$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

#2003 - 2005: CEP 48 
bd2003_2005_48$id_part <- car::recode(bd2003_2005_48$id_part, "c('Unión Demócrata Independiente (UDI)','Renovación Nacional (RN)') = 'Derecha'; 
                                 c('Partido Demócrata Cristiano (PDC)', 'Partido Radical Socialdemócrata (PRSD)',
                                 'Partido Socialista de Chile (PS)','Partido Radical Socialdemócrata (PRSD)','Partido por la Democracia (PPD)',
                                'Partido Humanista (PH)') = 'Centro-Izquierda concertación'; 
                                 'Partido Comunista de Chile (PCCH)' = 'Izquierda extraconcertación'; 
                                'NINGUNO' = 'Ninguno'; 
                                 c('OTRO PARTIDO','No sabe','No contesta') = NA", as.factor = T) 
bd2003_2005_48$pos_pol <- car::recode(bd2003_2005_48$pos_pol,"'Derecha' = 'Derecha';
'Centro Derecha' = 'Centro Derecha';
'Centro' = 'Centro';
'Centro Izquierda' = 'Centro Izquierda';
'Izquierda' = 'Izquierda';
'Independiente' = 'Independiente';
'Ninguna' = 'Ninguna';
'No Sabe' = NA;
'No Contesta' = NA", as.factor = T)

#2003 - 2005: CEP 49
bd2003_2005_49$id_part <- car::recode(bd2003_2005_49$id_part, "c('Union Democrata Independiente','Renovacion Nacional') = 'Derecha'; 
                                 c('Partido Democrata Cristiano', 'Partido Radical Socialdemócrata (PRSD)',
                                 'Partido Socialista de Chile','Partido Radical Socialdemocrata','Partido por la Democracia',
                                'Partido Humanista') = 'Centro-Izquierda concertación'; 
                                 'Partido Comunista de Chile' = 'Izquierda extraconcertación'; 
                                'Ninguno' = 'Ninguno'; 
                                 c('Otro partido','No sabe','No contesta') = NA", as.factor = T) 
freq(bd2003_2005_49$pos_pol)

bd2003_2005_49$pos_pol <- car::recode(bd2003_2005_49$pos_pol,"'Derecha' = 'Derecha';
'Centro Derecha' = 'Centro Derecha';
'Centro' = 'Centro';
'Centro Izquierda' = 'Centro Izquierda';
'Izquierda' = 'Izquierda';
'Independiente' = 'Independiente';
'Ninguna' = 'Ninguna';
'No sabe' = NA;
'No contesta' = NA", as.factor = T)


# 2003 - 2005: CEP 50 
bd2003_2005_50$id_part <- car::recode(bd2003_2005_50$id_part, "c(2,4) = 'Derecha'; 
                                 c(1,5,6,7,8) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 10 = 'Ninguno'; 
                                 c(9,88,99) = NA", as.factor = T) 
bd2003_2005_50$pos_pol <- car::recode(bd2003_2005_50$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 2003 - 2005: CEP 51 
bd2003_2005_51$id_part <- car::recode(bd2003_2005_51$id_part, "c(2,4) = 'Derecha'; 
                                 c(1,5,6,7,8) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 10 = 'Ninguno'; 
                                 c(9,88,99) = NA", as.factor = T) 
bd2003_2005_51$pos_pol <- car::recode(bd2003_2005_51$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

freq(bd2003_2005_51$pos_pol)

# ---- 3.4 Guardar base de datos final ---- 
save(bd2003_2005_45, file = "input/data/bd2003_2005_45.RData")
save(bd2003_2005_46, file = "input/data/bd2003_2005_46.RData")
save(bd2003_2005_47, file = "input/data/bd2003_2005_47.RData")
save(bd2003_2005_48, file = "input/data/bd2003_2005_48.RData")
save(bd2003_2005_49, file = "input/data/bd2003_2005_49.RData")
save(bd2003_2005_50, file = "input/data/bd2003_2005_50.RData")
save(bd2003_2005_51, file = "input/data/bd2003_2005_51.RData")


