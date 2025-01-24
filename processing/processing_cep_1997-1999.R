# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 17 - 9 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car)
# ---- Etapa 2: Cargar bases de datos ---
bd1997_1999_34 <- read_excel("input/data/1997-1999/cep34junjul1997.xls")
bd1997_1999_35 <- read_excel("input/data/1997-1999/cep35dicene19971998.xls")
bd1997_1999_36 <- read_excel("input/data/1997-1999/cep36jun1998E.xls")
bd1997_1999_37 <- read_excel("input/data/1997-1999/cep37abrmay1999.xls")
bd1997_1999_38 <- read_excel("input/data/1997-1999/cep38sepoct1999.xls")

# ---- Etapa 3: Procesamiento de datos ----
## Seleccionar variables a utilizar
bd1997_1999_34<- select(bd1997_1999_34, daen_p, daen_cb, daen_a2, daen_q, p12, p15) #no hay confianza
bd1997_1999_35 <- select(bd1997_1999_35, daen_l, daen_cb, daen_a2, daen_m, p9, p13t) #no hay confianza
bd1997_1999_36 <- select(bd1997_1999_36, daen_p25, daen_p5, daen_p2, daen_p1, te_p12a:te_p12d, p8, p16)
bd1997_1999_37 <- select(bd1997_1999_37, dat_14, dat_4_a, dat_2, dat_13, p8, p7) #no hay confianza
bd1997_1999_38 <- select(bd1997_1999_38, dat_15, esco, dat_2, dat_14, p8, p7) #no hay confianza

## Renombrarlas
### 1997-1999: CEP 34
bd1997_1999_34 <- rename(bd1997_1999_34,
                         nse = daen_p,
                         esc = daen_cb,
                         edad = daen_a2,
                         sexo = daen_q,
                         id_part = p12,
                         pos_pol = p15)

### 1997-1999: CEP 35
bd1997_1999_35 <- rename(bd1997_1999_35,
                         nse = daen_l,
                         esc = daen_cb,
                         edad = daen_a2,
                         sexo = daen_m,
                         id_part = p9,
                         pos_pol = p13t)

### 1997-1999: CEP 36
bd1997_1999_36 <- rename(bd1997_1999_36,
                         nse = daen_p25,
                         esc = daen_p5,
                         edad = daen_p2,
                         sexo = daen_p1,
                         id_part = p8,
                         pos_pol = p16,
                         conf_iglesia = te_p12c,
                         conf_congreso = te_p12a,
                         conf_diremp = te_p12b,
                         conf_dircortesup = te_p12d)

### 1997-1999: CEP 37
bd1997_1999_37 <- rename(bd1997_1999_37,
                         nse = dat_14,
                         esc = dat_4_a,
                         edad = dat_2,
                         sexo = dat_13,
                         id_part = p8,
                         pos_pol = p7)

### 1997-1999: CEP 38
bd1997_1999_38 <- rename(bd1997_1999_38,
                         nse = dat_15,
                         esc = esco,
                         edad = dat_2,
                         sexo = dat_14,
                         id_part = p8,
                         pos_pol = p7)

#---- 3.1 Tratamiento de  sociodemográficas ----
#---- 3.1.1 Frecuencias ----
### 1997-1999: CEP 34
frq(bd1997_1999_34$nse) 
frq(bd1997_1999_34$esc) 
frq(bd1997_1999_34$edad) 
frq(bd1997_1999_34$sexo) 

### 1997-1999: CEP 35
frq(bd1997_1999_35$nse) 
frq(bd1997_1999_35$esc) 
frq(bd1997_1999_35$edad) 
frq(bd1997_1999_35$sexo) 


### 1997-1999: CEP 36
frq(bd1997_1999_36$nse) 
frq(bd1997_1999_36$esc) 
frq(bd1997_1999_36$edad) 
frq(bd1997_1999_36$sexo) 


### 1997-1999: CEP 37
frq(bd1997_1999_37$nse) 
frq(bd1997_1999_37$esc) 
frq(bd1997_1999_37$edad) 
frq(bd1997_1999_37$sexo) 

### 1997-1999: CEP 38
frq(bd1997_1999_38$nse) 
frq(bd1997_1999_38$esc) 
frq(bd1997_1999_38$edad) 
frq(bd1997_1999_38$sexo) 

#---- 3.1.2 Recodificación ----
### 1997-1999: CEP 34
bd1997_1999_34$nse  <- car::recode(bd1997_1999_34$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1997_1999_34$esc <- car::recode(bd1997_1999_34$esc, "c(0,1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd1997_1999_34$edad <- car::recode(bd1997_1999_34$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = NA", as.factor = T)
bd1997_1999_34$sexo <- car::recode(bd1997_1999_34$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 1997-1999: CEP 35
bd1997_1999_35$nse  <- car::recode(bd1997_1999_35$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1997_1999_35$esc <- car::recode(bd1997_1999_35$esc, "c(0,1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd1997_1999_35$edad <- car::recode(bd1997_1999_35$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = NA", as.factor = T)
bd1997_1999_35$sexo <- car::recode(bd1997_1999_35$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 1997-1999: CEP 36
bd1997_1999_36$nse  <- car::recode(bd1997_1999_36$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1997_1999_36$esc <- car::recode(bd1997_1999_36$esc, "c(88,99) = NA; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd1997_1999_36$edad <- car::recode(bd1997_1999_36$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd1997_1999_36$sexo <- car::recode(bd1997_1999_36$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 1997-1999: CEP 37
bd1997_1999_37$nse  <- car::recode(bd1997_1999_37$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1997_1999_37$esc <- car::recode(bd1997_1999_37$esc, "c(88,99) = NA; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd1997_1999_37$edad <- car::recode(bd1997_1999_37$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd1997_1999_37$sexo <- car::recode(bd1997_1999_37$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 1997-1999: CEP 38
bd1997_1999_38$nse  <- car::recode(bd1997_1999_38$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1997_1999_38$esc <- car::recode(bd1997_1999_38$esc, "c(88,99) = NA; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd1997_1999_38$edad <- car::recode(bd1997_1999_38$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd1997_1999_38$sexo <- car::recode(bd1997_1999_38$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

#---- 3.2 Tratamiento de variables de confianza ----
#---- 3.2.1 Frecuencias ----
### 1997-1999: CEP 34 #no tiene confianza
### 1997-1999: CEP 35 #no tiene confianza
### 1997-1999: CEP 36
frq(bd1997_1999_36$conf_iglesia)
frq(bd1997_1999_36$conf_congreso)
frq(bd1997_1999_36$conf_diremp)
frq(bd1997_1999_36$conf_dircortesup)

### 1997-1999: CEP 37 #no tiene confianza
### 1997-1999: CEP 38 #no tiene confianza

#---- 3.2.2 Recodificacion ----
### 1997-1999: CEP 34 #no tiene confianza
### 1997-1999: CEP 35 #no tiene confianza
### 1997-1999: CEP 36
bd1997_1999_36$conf_iglesia <- car::recode(bd1997_1999_36$conf_iglesia,"1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)
bd1997_1999_36$conf_congreso <- car::recode(bd1997_1999_36$conf_congreso, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)
bd1997_1999_36$conf_diremp <- car::recode(bd1997_1999_36$conf_diremp, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)
bd1997_1999_36$conf_dircortesup <- car::recode(bd1997_1999_36$conf_dircortesup, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)

# No olvidar
# Codificación original es la siguinte:
#1. Plena confianza
#2. Una gran cantidad de confianza
#3. Algo de confianza
#4. Muy poco de confianza
#5. Nada de confianza

### 1997-1999: CEP 37 #no tiene confianza
### 1997-1999: CEP 38 #no tiene confianza

#---- 3.2.3 Otros ajustes ----
### Construccion variable iglesia en calidad de institucion

#---- 3.2.4 Guardar bases de confianza ----
save(bd1997_1999_36, file = "input/data/bd1997_1999_36.RData")

### Sacar variables de confianza que no usaremos.

#---- 3.3 Tratamiento de variables de identificación partidaria e identificación política (o posición política)
#---- 3.3.1 Frecuencias ----
frq(bd1997_1999_34$id_part)
frq(bd1997_1999_34$pos_pol)

frq(bd1997_1999_35$id_part)
frq(bd1997_1999_35$pos_pol)

frq(bd1997_1999_36$id_part)
frq(bd1997_1999_36$pos_pol)

frq(bd1997_1999_37$id_part)
frq(bd1997_1999_37$pos_pol)

frq(bd1997_1999_38$id_part)
frq(bd1997_1999_38$pos_pol)

#---- 3.3.2 Recodificacion ----

# 1997 - 1999: CEP 34 
bd1997_1999_34$id_part <- car::recode(bd1997_1999_34$id_part, "c(2,4,7) = 'Derecha'; 
                                 c(1,5,6,8) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 10 = 'Ninguno'; 
                                 c(9,11,12) = NA", as.factor = T) 
bd1997_1999_34$pos_pol <- car::recode(bd1997_1999_34$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 1997 - 1999: CEP 35
bd1997_1999_35$id_part <- car::recode(bd1997_1999_35$id_part, "c(2,4,7,9,10) = 'Derecha'; 
                                 c(1,5,6,8) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 c(13,14) = NA", as.factor = T)  
bd1997_1999_35$pos_pol <- car::recode(bd1997_1999_35$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 1997 - 1999: CEP 36 
bd1997_1999_36$id_part <- car::recode(bd1997_1999_36$id_part, "c(2,4,7) = 'Derecha'; 
                                 c(1,5,6,8) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 10 = 'Ninguno'; 
                                 c(9,11,12) = NA", as.factor = T)  
bd1997_1999_36$pos_pol <- car::recode(bd1997_1999_36$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 1997 - 1999: CEP 37 
bd1997_1999_37$id_part <- car::recode(bd1997_1999_37$id_part, "c(2,4,7) = 'Derecha'; 
                                 c(1,5,6,8) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 10 = 'Ninguno'; 
                                 c(9,11,12) = NA", as.factor = T) 
bd1997_1999_37$pos_pol <- car::recode(bd1997_1999_37$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 1997 - 1999: CEP 38 
bd1997_1999_38$id_part <- car::recode(bd1997_1999_38$id_part, "c(2,4,7) = 'Derecha'; 
                                 c(1,5,6,8) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 10 = 'Ninguno'; 
                                 c(9,88,99) = NA", as.factor = T)  
bd1997_1999_38$pos_pol <- car::recode(bd1997_1999_38$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# ---- 3.4 Guardar base de datos final ---- 
save(bd1997_1999_34, file = "input/data/bd1997_1999_34.RData")
save(bd1997_1999_35, file = "input/data/bd1997_1999_35.RData")
save(bd1997_1999_36, file = "input/data/bd1997_1999_36.RData")
save(bd1997_1999_37, file = "input/data/bd1997_1999_37.RData")
save(bd1997_1999_38, file = "input/data/bd1997_1999_38.RData")

