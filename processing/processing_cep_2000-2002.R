# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 17 - 9 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car)
# ---- Etapa 2: Cargar bases de datos ---
bd2000_2002_39 <- read_excel("input/data/2000-2002/cep39marabr2000.xls")
bd2000_2002_40 <- read_excel("input/data/2000-2002/cep40novdic.xls")
bd2000_2002_41 <- read_excel("input/data/2000-2002/cep41jun2001.xls")
bd2000_2002_42 <- read_excel("input/data/2000-2002/cep42dicene20012002.xls")
bd2000_2002_43 <- read_excel("input/data/2000-2002/cep43jul2002.xls")
bd2000_2002_44 <- read_excel("input/data/2000-2002/cep44dic2002.xls")

# ---- Etapa 3: Procesamiento de datos ----
## Seleccionar variables a utilizar
bd2000_2002_39 <- select(bd2000_2002_39, dat_30, dat_5, dat_2, dat_1, p8t, p7t) #no hay confianza
bd2000_2002_40 <- select(bd2000_2002_40, de27, de5, de2, de1, p9t, p8t) #no hay confianza
bd2000_2002_41 <- select(bd2000_2002_41, de16, de4, de2, de15, p10, p12) #no hay confianza
bd2000_2002_42 <- select(bd2000_2002_42, de14, de4, de2, de13, p7, p9) #no hay confianza
bd2000_2002_43 <- select(bd2000_2002_43, de25, de5, de2, de1, p7, p9) #no hay confianza
bd2000_2002_44 <- select(bd2000_2002_44, de28, de5, de2, de1, p7, p9, p20_a:p20_e, p20_g, p20_i, p20_j, p20_k, p19_a:p19_e, p19_h, p19_i)

## Renombrarlas
### 2000-2002: CEP 39
bd2000_2002_39 <- rename(bd2000_2002_39,
                         nse = dat_30,
                         esc = dat_5,
                         edad = dat_2,
                         sexo = dat_1,
                         id_part = p8t,
                         pos_pol = p7t)

### 2000-2002: CEP 40
bd2000_2002_40 <- rename(bd2000_2002_40,
                         nse = de27,
                         esc = de5,
                         edad = de2,
                         sexo = de1,
                         id_part = p9t,
                         pos_pol = p8t)

### 2000-2002: CEP 41
bd2000_2002_41 <- rename(bd2000_2002_41,
                         nse = de16,
                         esc = de4,
                         edad = de2,
                         sexo = de15,
                         id_part = p10,
                         pos_pol = p12)

### 2000-2002: CEP 42
bd2000_2002_42 <- rename(bd2000_2002_42,
                         nse = de14,
                         esc = de4,
                         edad = de2,
                         sexo = de13,
                         id_part = p7,
                         pos_pol = p9)

### 2000-2002: CEP 43
bd2000_2002_43 <- rename(bd2000_2002_43,
                         nse = de25,
                         esc = de5,
                         edad = de2,
                         sexo = de1,
                         id_part = p7,
                         pos_pol = p9)

### 2000-2002: CEP 44
bd2000_2002_44 <- rename(bd2000_2002_44,
                         nse = de28,
                         esc = de5,
                         edad = de2,
                         sexo = de1,
                         id_part = p7,
                         pos_pol = p9,
                         conf_iglesia = p20_a,
                         conf_partidos = p20_i,
                         conf_congreso = p20_j,
                         conf_ffaa = p20_b,
                         conf_carab = p20_g,
                         conf_prensa = p20_d,
                         conf_tele = p20_e,
                         conf_empr = p20_k,
                         conf_tribun = p20_c,
                         conf_dirpol = p19_a,
                         conf_iglesiacat_per = p19_b,
                         conf_iglesiaev_per = p19_c,
                         conf_congreso_per = p19_d,
                         conf_altosffaa = p19_e,
                         conf_diremp = p19_h,
                         conf_dircortesup = p19_i)

#---- 3.1 Tratamiento de  sociodemográficas ----
#---- 3.1.1 Frecuencias ----
### 2000-2002: CEP 39
frq(bd2000_2002_39$nse) 
frq(bd2000_2002_39$esc) 
frq(bd2000_2002_39$edad) 
frq(bd2000_2002_39$sexo) 

### 2000-2002: CEP 40
frq(bd2000_2002_40$nse) 
frq(bd2000_2002_40$esc) 
frq(bd2000_2002_40$edad) 
frq(bd2000_2002_40$sexo) 

### 2000-2002: CEP 41
frq(bd2000_2002_41$nse) 
frq(bd2000_2002_41$esc) 
frq(bd2000_2002_41$edad) 
frq(bd2000_2002_41$sexo) 

### 2000-2002: CEP 42
frq(bd2000_2002_42$nse) 
frq(bd2000_2002_42$esc) 
frq(bd2000_2002_42$edad) 
frq(bd2000_2002_42$sexo) 

### 2000-2002: CEP 43
frq(bd2000_2002_43$nse) 
frq(bd2000_2002_43$esc) 
frq(bd2000_2002_43$edad) 
frq(bd2000_2002_43$sexo) 

### 2000-2002: CEP 44
frq(bd2000_2002_44$nse) 
frq(bd2000_2002_44$esc) 
frq(bd2000_2002_44$edad) 
frq(bd2000_2002_44$sexo) 

#---- 3.1.2 Recodificación ----
### 2000-2002: CEP 39
bd2000_2002_39$nse  <- car::recode(bd2000_2002_39$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2000_2002_39$esc <- car::recode(bd2000_2002_39$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2000_2002_39$edad <- car::recode(bd2000_2002_39$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2000_2002_39$sexo <- car::recode(bd2000_2002_39$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 2000-2002: CEP 40
bd2000_2002_40$nse  <- car::recode(bd2000_2002_40$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2000_2002_40$esc <- car::recode(bd2000_2002_40$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2000_2002_40$edad <- car::recode(bd2000_2002_40$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2000_2002_40$sexo <- car::recode(bd2000_2002_40$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 2000-2002: CEP 41
bd2000_2002_41$nse  <- car::recode(bd2000_2002_41$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2000_2002_41$esc <- car::recode(bd2000_2002_41$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2000_2002_41$edad <- car::recode(bd2000_2002_41$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2000_2002_41$sexo <- car::recode(bd2000_2002_41$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 2000-2002: CEP 42
bd2000_2002_42$nse  <- car::recode(bd2000_2002_42$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2000_2002_42$esc <- car::recode(bd2000_2002_42$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2000_2002_42$edad <- car::recode(bd2000_2002_42$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2000_2002_42$sexo <- car::recode(bd2000_2002_42$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 2000-2002: CEP 43
bd2000_2002_43$nse  <- car::recode(bd2000_2002_43$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2000_2002_43$esc <- car::recode(bd2000_2002_43$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2000_2002_43$edad <- car::recode(bd2000_2002_43$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2000_2002_43$sexo <- car::recode(bd2000_2002_43$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

### 2000-2002: CEP 44
bd2000_2002_44$nse  <- car::recode(bd2000_2002_44$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2000_2002_44$esc <- car::recode(bd2000_2002_44$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd2000_2002_44$edad <- car::recode(bd2000_2002_44$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2000_2002_44$sexo <- car::recode(bd2000_2002_44$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

#---- 3.2 Tratamiento de variables de confianza ----
#---- 3.2.1 Frecuencias ----
### 2000-2002: CEP 39 #no tiene confianza
### 2000-2002: CEP 40 #no tiene confianza
### 2000-2002: CEP 41 #no tiene confianza
### 2000-2002: CEP 42 #no tiene confianza
### 2000-2002: CEP 43 #no tiene confianza
### 2000-2002: CEP 44

frq(bd2000_2002_44$conf_iglesia)
frq(bd2000_2002_44$conf_partidos)
frq(bd2000_2002_44$conf_iglesia)
frq(bd2000_2002_44$conf_congreso)
frq(bd2000_2002_44$conf_ffaa)
frq(bd2000_2002_44$conf_carab)
frq(bd2000_2002_44$conf_prensa)
frq(bd2000_2002_44$conf_tele)
frq(bd2000_2002_44$conf_empr)
frq(bd2000_2002_44$conf_tribun)

#---- 3.2.2 Recodificacion ----
### 2000-2002: CEP 44 inst
bd2000_2002_44$conf_partidos <- car::recode(bd2000_2002_44$conf_partidos,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2000_2002_44$conf_iglesia  <- car::recode(bd2000_2002_44$conf_iglesia,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2000_2002_44$conf_prensa <- car::recode(bd2000_2002_44$conf_prensa, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2000_2002_44$conf_tele <- car::recode(bd2000_2002_44$conf_tele, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2000_2002_44$conf_congreso <- car::recode(bd2000_2002_44$conf_congreso, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2000_2002_44$conf_ffaa <- car::recode(bd2000_2002_44$conf_ffaa, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2000_2002_44$conf_carab <- car::recode(bd2000_2002_44$conf_carab, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2000_2002_44$conf_empr <- car::recode(bd2000_2002_44$conf_empr, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2000_2002_44$conf_tribun <- car::recode(bd2000_2002_44$conf_tribun, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)

# per
bd2000_2002_44$conf_dirpol <- car::recode(bd2000_2002_44$conf_dirpol,"1 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd2000_2002_44$conf_iglesiacat_per <- car::recode(bd2000_2002_44$conf_iglesiacat_per,"1 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd2000_2002_44$conf_iglesiaev_per <- car::recode(bd2000_2002_44$conf_iglesiaev_per, "1 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd2000_2002_44$conf_congreso_per <- car::recode(bd2000_2002_44$conf_congreso_per, "1 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd2000_2002_44$conf_altosffaa <- car::recode(bd2000_2002_44$conf_altosffaa, "1 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd2000_2002_44$conf_diremp <- car::recode(bd2000_2002_44$conf_diremp, "1 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd2000_2002_44$conf_dircortesup <- car::recode(bd2000_2002_44$conf_dircortesup, "1 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)

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
bd2000_2002_44$conf_mmc[bd2000_2002_44$conf_tele == 'Mucha confianza' | bd2000_2002_44$conf_prensa == 'Mucha confianza'] <- 'Mucha confianza'
bd2000_2002_44$conf_mmc[bd2000_2002_44$conf_tele == 'Otra' & bd2000_2002_44$conf_prensa == 'Otra'] <- 'Otra'

### Sacar variables de confianza que no usaremos.
### 2000-2002: CEP 44
bd2000_2002_44 <- select(bd2000_2002_44, -conf_carab, -conf_tele, -conf_prensa )

bd2000_2002_44$conf_iglesia_per[bd2000_2002_44$conf_iglesiacat_per == 'Mucha confianza' & bd2000_2002_44$conf_iglesiaev_per == 'Mucha confianza'] <- 'Mucha confianza'
bd2000_2002_44$conf_iglesia_per[bd2000_2002_44$conf_iglesiacat_per == 'Otra' & bd2000_2002_44$conf_iglesiaev_per == 'Otra'] <- 'Otra'

### Eliminación de variables no utilzadas

#---- 3.2.4 Guardar bases de confianza ----
save(bd2000_2002_44, file = "input/data/bd2000_2002_44.RData")

#---- 3.3 Tratamiento de variables de identificación partidaria e identificación política (o posición política)
#---- 3.3.1 Frecuencias ----

frq(bd2000_2002_39$id_part)
frq(bd2000_2002_39$pos_pol)

frq(bd2000_2002_40$id_part)
frq(bd2000_2002_40$pos_pol)

frq(bd2000_2002_41$id_part)
frq(bd2000_2002_41$pos_pol)

frq(bd2000_2002_42$id_part)
frq(bd2000_2002_42$pos_pol)

frq(bd2000_2002_43$id_part)
frq(bd2000_2002_43$pos_pol)

frq(bd2000_2002_44$id_part)
frq(bd2000_2002_44$pos_pol)

#---- 3.3.2 Recodificacion ----

# 2000 - 2002: CEP 39
bd2000_2002_39$id_part <- car::recode(bd2000_2002_39$id_part, "c(2,4,7) = 'Derecha'; 
                                 c(1,5,6,8) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 10 = 'Ninguno'; 
                                 c(9,88,99) = NA", as.factor = T)  
bd2000_2002_39$pos_pol <- car::recode(bd2000_2002_39$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 2000 - 2002: CEP 40
bd2000_2002_40$id_part <- car::recode(bd2000_2002_40$id_part, "c(2,4,7) = 'Derecha'; 
                                 c(1,5,6,8) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 9 = 'Ninguno'; 
                                 c(10,88,99) = NA", as.factor = T)  
bd2000_2002_40$pos_pol <- car::recode(bd2000_2002_40$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 2000 - 2002: CEP 41
bd2000_2002_41$id_part <- car::recode(bd2000_2002_41$id_part, "c(2,4,7) = 'Derecha'; 
                                 c(1,5,6,8) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 10 = 'Ninguno'; 
                                 c(9,88,99) = NA", as.factor = T)   
bd2000_2002_41$pos_pol <- car::recode(bd2000_2002_41$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 2000 - 2002: CEP 42
bd2000_2002_42$id_part <- car::recode(bd2000_2002_42$id_part, "c(2,4) = 'Derecha'; 
                                 c(1,5,6,7) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 10 = 'Ninguno'; 
                                 c(8,88,99) = NA", as.factor = T)  
bd2000_2002_42$pos_pol <- car::recode(bd2000_2002_42$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 2000 - 2002: CEP 43
bd2000_2002_43$id_part <- car::recode(bd2000_2002_43$id_part, "c(2,4) = 'Derecha'; 
                                 c(1,5,6,7) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 9 = 'Ninguno'; 
                                 c(8,88,99) = NA", as.factor = T)  
bd2000_2002_43$pos_pol <- car::recode(bd2000_2002_43$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 2000 - 2002: CEP 44
bd2000_2002_44$id_part <- car::recode(bd2000_2002_44$id_part, "c(2,4) = 'Derecha'; 
                                 c(1,5,6,7) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 9 = 'Ninguno'; 
                                 c(8,88,99) = NA", as.factor = T) 
bd2000_2002_44$pos_pol <- car::recode(bd2000_2002_44$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# ---- 3.4 Guardar base de datos final ---- 
save(bd2000_2002_39, file = "input/data/bd2000_2002_39.RData")
save(bd2000_2002_40, file = "input/data/bd2000_2002_40.RData")
save(bd2000_2002_41, file = "input/data/bd2000_2002_41.RData")
save(bd2000_2002_42, file = "input/data/bd2000_2002_42.RData")
save(bd2000_2002_43, file = "input/data/bd2000_2002_43.RData")
save(bd2000_2002_44, file = "input/data/bd2000_2002_44.RData")