# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 17 - 9 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car)
# ---- Etapa 2: Cargar bases de datos ---
bd1994_1996_29 <- read_excel("input/data/1994-1996/cep29novdic1994.xls")
bd1994_1996_30 <- read_excel("input/data/1994-1996/cep30mayjun1995.xls")
bd1994_1996_31 <- read_excel("input/data/1994-1996/cep31nov1995.xls")
bd1994_1996_32 <- read_excel("input/data/1994-1996/cep32junjul1996.xls")
bd1994_1996_33 <- read_excel("input/data/1994-1996/cep33novdic1996.xls")

# ---- Etapa 3: Procesamiento de datos ----
## Seleccionar variables a utilizar
bd1994_1996_29 <- select(bd1994_1996_29, nse, ed2, edad2, sexo, p14, p17) #no hay confianza
bd1994_1996_30 <- select(bd1994_1996_30, p47h, p47c, p47a, p47i, p12, p15) #no hay confianza
bd1994_1996_31 <- select(bd1994_1996_31, p41h, p41c, b2, b1, p13, p16a)  #no hay confianza
bd1994_1996_32 <- select(bd1994_1996_32, p38q, p38ca, b2, p38r, p15, p18) #no hay confianza
bd1994_1996_33 <- select(bd1994_1996_33, dho, dpc, b2, dhp, p12, p15) #no hay confianza

## Renombrarlas
### 1994-1996: CEP 29
bd1994_1996_29 <- rename(bd1994_1996_29,
                         edad = edad2,
                         id_part = p14,
                         pos_pol = p17,
                         nse = nse,
                         sexo = sexo,
                         esc = ed2)

### 1994-1996: CEP 30
bd1994_1996_30 <- rename(bd1994_1996_30,
                         edad = p47a,
                         id_part = p12,
                         pos_pol = p15,
                         nse = p47h,
                         sexo = p47i,
                         esc = p47c)

### 1994-1996: CEP 31
bd1994_1996_31 <- rename(bd1994_1996_31,
                         edad = b2,
                         id_part = p13,
                         pos_pol = p16a,
                         nse = p41h,
                         sexo = b1,
                         esc = p41c)
### 1994-1996: CEP 32
bd1994_1996_32 <- rename(bd1994_1996_32,
                         edad = b2,
                         id_part = p15,
                         pos_pol = p18,
                         nse = p38q,
                         sexo = p38r,
                         esc = p38ca)

### 1994-1996: CEP 33
bd1994_1996_33 <- rename(bd1994_1996_33,
                         edad = b2,
                         id_part = p12,
                         pos_pol = p15,
                         nse = dho,
                         sexo = dhp,
                         esc = dpc)

#---- 3.1 Tratamiento de  sociodemográficas ----
#---- 3.1.1 Frecuencias ----
### 1994-1996: CEP 29
frq(bd1994_1996_29$nse) 
frq(bd1994_1996_29$esc) 
frq(bd1994_1996_29$edad) 
frq(bd1994_1996_29$sexo) 

### 1994-1996: CEP 30
frq(bd1994_1996_30$nse) 
frq(bd1994_1996_30$esc) 
frq(bd1994_1996_30$edad) 
frq(bd1994_1996_30$sexo) 

### 1994-1996: CEP 31
frq(bd1994_1996_31$nse) 
frq(bd1994_1996_31$esc) 
frq(bd1994_1996_31$edad) 
frq(bd1994_1996_31$sexo) 

### 1994-1996: CEP 32
frq(bd1994_1996_32$nse) 
frq(bd1994_1996_32$esc) 
frq(bd1994_1996_32$edad) 
frq(bd1994_1996_32$sexo) 

### 1994-1996: CEP 33
frq(bd1994_1996_33$nse) 
frq(bd1994_1996_33$esc) 
frq(bd1994_1996_33$edad) 
frq(bd1994_1996_33$sexo) 

#---- 3.1.2 Recodificación ----
### 1994-1996: CEP 29
bd1994_1996_29$nse  <- car::recode(bd1994_1996_29$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1994_1996_29$esc <- car::recode(bd1994_1996_29$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1994_1996_29$edad <- car::recode(bd1994_1996_29$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1994_1996_29$sexo <- car::recode(bd1994_1996_29$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)


### 1994-1996: CEP 30
bd1994_1996_30$nse  <- car::recode(bd1994_1996_30$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1994_1996_30$esc <- car::recode(bd1994_1996_30$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1994_1996_30$edad <- car::recode(bd1994_1996_30$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1994_1996_30$sexo <- car::recode(bd1994_1996_30$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 1994-1996: CEP 31
bd1994_1996_31$nse  <- car::recode(bd1994_1996_31$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1994_1996_31$esc <- car::recode(bd1994_1996_31$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1994_1996_31$edad <- car::recode(bd1994_1996_31$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1994_1996_31$sexo <- car::recode(bd1994_1996_31$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 1994-1996: CEP 32
bd1994_1996_32$nse  <- car::recode(bd1994_1996_32$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1994_1996_32$esc <- car::recode(bd1994_1996_32$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1994_1996_32$edad <- car::recode(bd1994_1996_32$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1994_1996_32$sexo <- car::recode(bd1994_1996_32$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 1994-1996: CEP 33
bd1994_1996_33$nse  <- car::recode(bd1994_1996_33$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1994_1996_33$esc <- car::recode(bd1994_1996_33$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1994_1996_33$edad <- car::recode(bd1994_1996_33$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1994_1996_33$sexo <- car::recode(bd1994_1996_33$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

#---- 3.2 Tratamiento de variables de confianza ----
# No hay variables de confianza en este periodo

#---- 3.3 Tratamiento de variables de identificación partidaria e identificación política (o posición política)
#---- 3.3.1 Frecuencias ----
frq(bd1994_1996_29$id_part)
frq(bd1994_1996_29$pos_pol)

frq(bd1994_1996_30$id_part)
frq(bd1994_1996_30$pos_pol)

frq(bd1994_1996_31$id_part)
frq(bd1994_1996_31$pos_pol)

frq(bd1994_1996_32$id_part)
frq(bd1994_1996_32$pos_pol)

frq(bd1994_1996_33$id_part)
frq(bd1994_1996_33$pos_pol)

#---- 3.3.2 Recodificacion

# 1994 - 1996: CEP 29 
bd1994_1996_29$id_part <- car::recode(bd1994_1996_29$id_part, "c(8,9,10) = 'Derecha'; 
                                 c(1,2,3,6,7) = 'Centro-Izquierda concertación'; 
                                 c(4,5) = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 c(11,13,14) = NA", as.factor = T)
bd1994_1996_29$pos_pol <- car::recode(bd1994_1996_29$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 1994 - 1996: CEP 30 
bd1994_1996_30$id_part <- car::recode(bd1994_1996_30$id_part, "c(6,7,8) = 'Derecha'; 
                                 c(1,2,4,5) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 10 = 'Ninguno'; 
                                 c(9,11,12) = NA", as.factor = T) 
bd1994_1996_30$pos_pol <- car::recode(bd1994_1996_30$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 1994 - 1996: CEP 31
bd1994_1996_31$id_part <- car::recode(bd1994_1996_31$id_part, "c(6,7,8) = 'Derecha'; 
                                 c(1,2,4,5) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 10 = 'Ninguno'; 
                                 c(9,11,12) = NA", as.factor = T)
bd1994_1996_31$pos_pol <- car::recode(bd1994_1996_31$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 1994 - 1996: CEP 32 
bd1994_1996_32$id_part <- car::recode(bd1994_1996_32$id_part, "c(6,7,8,9,11) = 'Derecha'; 
                                 c(1,2,4,5,10) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 c(13,14) = NA", as.factor = T)
bd1994_1996_32$pos_pol <- car::recode(bd1994_1996_32$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 1994 - 1996: CEP 33 
bd1994_1996_33$id_part <- car::recode(bd1994_1996_33$id_part, "c(6,7,8,9,11) = 'Derecha'; 
                                 c(1,2,4,5,10) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 c(13,14) = NA", as.factor = T)
bd1994_1996_33$pos_pol <- car::recode(bd1994_1996_33$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)


# ---- 3.4 Guardar base de datos final ---- 
save(bd1994_1996_29, file = "input/data/bd1994_1996_29.RData")
save(bd1994_1996_30, file = "input/data/bd1994_1996_30.RData")
save(bd1994_1996_31, file = "input/data/bd1994_1996_31.RData")
save(bd1994_1996_32, file = "input/data/bd1994_1996_32.RData")
save(bd1994_1996_33, file = "input/data/bd1994_1996_33.RData")


