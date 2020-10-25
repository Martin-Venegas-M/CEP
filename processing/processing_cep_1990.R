# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 17 - 9 - 2020
# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car)
# ---- Etapa 2: Cargar bases de datos ---
bd1990_14 <- read_excel("input/data/1990/cep14mayjun1990.xls")
bd1990_15 <- read_excel("input/data/1990/cep15sepoct1990.xls")
bd1990_16 <- read_excel("input/data/1990/cep16dic1990.xls")

# ---- Etapa 3: Procesamiento de datos ----
## Seleccionar variables a utilizar
bd1990_14 <- select(bd1990_14, p26, p171, p22, p28, p29, p27) #No hay preguntas de confianza
bd1990_15 <- select(bd1990_15, p32, p17a, p21, p22, p23, p33) #No hay preguntas de confianza
bd1990_16 <- select(bd1990_16, p26, p14a, p16, p29, p30, p27, p15a:p15i)

## Renombrarlas
### 1990: CEP 14
bd1990_14 <- rename(bd1990_14,
                    edad = p26,
                    id_part = p171,
                    pos_pol = p22,
                    nse_ad = p28,
                    sexo = p29,
                    esc = p27)

### 1990: CEP 15
bd1990_15 <- rename(bd1990_15,
                    edad = p32,
                    id_part = p17a,
                    pos_pol = p21,
                    nse = p22,
                    sexo = p23,
                    esc = p33)

### 1990: CEP 16
bd1990_16 <- rename(bd1990_16,
                    edad = p26,
                    id_part = p14a,
                    pos_pol = p16,
                    nse = p29,
                    sexo = p30,
                    esc = p27,
                    conf_dirpol = p15a,
                    conf_iglesiacat = p15b,
                    conf_iglesiaev = p15c,
                    conf_congreso = p15d,
                    conf_altosffaa = p15e,
                    conf_dirsin = p15f,
                    conf_gabinete = p15g,
                    conf_diremp = p15h,
                    conf_dircortesup = p15i)

## Etiquetarlas
### 1990: CEP 14
#bd1990_14$edad <- set_label(x = bd1990_14$edad, label = "")
#bd1990_14$id_part <- set_label(x = bd1990_14$id_part, label = "")
#bd1990_14$pos_pol <- set_label(x = bd1990_14$pos_pol, label = "")
#bd1990_14$nse_ad <- set_label(x = bd1990_14$nse_ad, label = "")
#bd1990_14$sexo <- set_label(x = bd1990_14$sexo, label = "")
#bd1990_14$esc <- set_label(x = bd1990_14$esc, label = "")

### 1990: CEP 15
#bd1990_15$edad <- set_label(x = bd1990_15$edad, label = "")
#bd1990_15$id_part <- set_label(x = bd1990_15$id_part, label = "")
#bd1990_15$pos_pol <- set_label(x = bd1990_15$pos_pol, label = "")
#bd1990_15$nse <- set_label(x = bd1990_15$nse, label = "")
#bd1990_15$sexo <- set_label(x = bd1990_15$sexo, label = "")
#bd1990_15$esc <- set_label(x = bd1990_15$esc, label = "")

### 1990: CEP 16
#bd1990_16$edad <- set_label(x = bd1990_16$edad, label = "")
#bd1990_16$id_part <- set_label(x = bd1990_16$id_part, label = "")
#bd1990_16$pos_pol <- set_label(x = bd1990_16$pos_pol, label = "")
#bd1990_16$sexo <- set_label(x = bd1990_16$sexo, label = "")
#bd1990_16$esc <- set_label(x = bd1990_16$esc, label = "")

#bd1990_16$conf_dirpol <- set_label(x = bd1990_16$conf_dirpol, label = "")
#bd1990_16$conf_iglesiacat <- set_label(x = bd1990_16$conf_iglesiacat, label = "")
#bd1990_16$conf_iglesiaev <- set_label(x = bd1990_16$conf_iglesiaev, label = "")
#bd1990_16$conf_congreso <- set_label(x = bd1990_16$conf_congreso, label = "")
#bd1990_16$conf_altosffaa <- set_label(x = bd1990_16$conf_altosffaa, label = "")
#bd1990_16$conf_dirsin <- set_label(x = bd1990_16$conf_dirsin, label = "")
#bd1990_16$conf_gabinete <- set_label(x = bd1990_16$conf_gabinete, label = "")
#bd1990_16$conf_diremp <- set_label(x = bd1990_16$conf_diremp, label = "")
#bd1990_16$conf_dircortesup <- set_label(x = bd1990_16$conf_dircortesup, label = "")

#---- 3.1 Tratamiento de  sociodemográficas ----
#---- 3.1.1 Frecuencias ----
## 1990: CEP 14
frq(bd1990_14$nse_ad) # No se ocupara esta variable
frq(bd1990_14$esc) # Escolaridad
frq(bd1990_14$edad) # Edad
frq(bd1990_14$sexo) # Sexo

## 1990: CEP 15
frq(bd1990_15$nse)
frq(bd1990_15$esc)
frq(bd1990_15$edad)
frq(bd1990_15$sexo)

## 1990: CEP 16
frq(bd1990_16$nse)
frq(bd1990_16$esc)
frq(bd1990_16$edad)
frq(bd1990_16$sexo)

#---- 3.1.2 Recodificación ----
## 1990: CEP 14
bd1990_14$esc <- car::recode(bd1990_14$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd1990_14$edad <- car::recode(bd1990_14$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = NA", as.factor = T)
bd1990_14$sexo <- car::recode(bd1990_14$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 1990: CEP 15
bd1990_15$nse  <- car::recode(bd1990_15$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1990_15$esc <- car::recode(bd1990_15$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd1990_15$edad <- car::recode(bd1990_15$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = NA", as.factor = T)
bd1990_15$sexo <- car::recode(bd1990_15$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 1990: CEP 16
bd1990_16$nse  <- car::recode(bd1990_16$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = NA", as.factor = T)
bd1990_16$esc <- car::recode(bd1990_16$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = NA", as.factor = T)
bd1990_16$edad <- car::recode(bd1990_16$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = NA", as.factor = T)
bd1990_16$sexo <- car::recode(bd1990_16$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

#---- 3.2 Tratamiento de variables de confianza ----
#---- 3.2.1 Frecuencias ----
## 1990: CEP 16
frq(bd1990_16$conf_dirpol)
frq(bd1990_16$conf_iglesiacat)
frq(bd1990_16$conf_iglesiaev)
frq(bd1990_16$conf_congreso)
frq(bd1990_16$conf_altosffaa)
frq(bd1990_16$conf_dirsin) # Esta no la ocuparemos (dirigentes sindicales)
frq(bd1990_16$conf_gabinete) # Esta no la ocuparemos (ministros actuales/gabinetes)
frq(bd1990_16$conf_diremp)
frq(bd1990_16$conf_dircortesup)

#---- 3.2.2 Recodificacion ----
## 1990: CEP 16
bd1990_16$conf_dirpol <- car::recode(bd1990_16$conf_dirpol,"3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1990_16$conf_iglesiacat <- car::recode(bd1990_16$conf_iglesiacat,"3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1990_16$conf_iglesiaev <- car::recode(bd1990_16$conf_iglesiaev, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1990_16$conf_congreso <- car::recode(bd1990_16$conf_congreso, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1990_16$conf_altosffaa <- car::recode(bd1990_16$conf_altosffaa, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1990_16$conf_diremp <- car::recode(bd1990_16$conf_diremp, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1990_16$conf_dircortesup <- car::recode(bd1990_16$conf_dircortesup, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)

# No olvidar
# Codificación original

# 1. Poca confianza
# 2. Algo de confianza
# 3. Mucha confianza

#---- 3.2.3 Otros  ajustes ----
## 1990: CEP 16
### Construccion variable iglesia en calidad de institucion

bd1990_16$conf_iglesia[bd1990_16$conf_iglesiacat == 'Mucha confianza' & bd1990_16$conf_iglesiaev == 'Mucha confianza'] <- 'Mucha confianza'
bd1990_16$conf_iglesia[bd1990_16$conf_iglesiacat == 'Otra' & bd1990_16$conf_iglesiaev == 'Otra'] <- 'Otra'

### Eliminación de variables no utilzadas
bd1990_16 <- select(bd1990_16,-conf_dirsin, -conf_gabinete) # Sacar variables de confianza que no usaremos.

#---- 3.2.4 Guardar bases de confianza ----
save(bd1990_16, file = "input/data/bd1990_16.RData")

#---- 3.3 Tratamiento de variables de identificación partidaria e identificación política (o posición política)
#---- 3.3.1 Frecuencias ----
# 1990: CEP 14
frq(bd1990_14$id_part)
frq(bd1990_14$pos_pol)

#1990: CEP 15
frq(bd1990_15$id_part)
frq(bd1990_15$pos_pol)

# 1990: CEP 16
frq(bd1990_16$id_part)
frq(bd1990_16$pos_pol)

#---- 3.3.2 Recodificación ----

# 1990: CEP 14
bd1990_14$id_part <- car::recode(bd1990_14$id_part, "c(1,3) = 'Derecha'; 
                                 c(2,4,5,6) = 'Centro-Izquierda concertación'; 
                                 7 = 'Izquierda extraconcertación'; 
                                 9 = 'Ninguno'; 
                                 8 = NA", as.factor = T) 
bd1990_14$pos_pol <- car::recode(bd1990_14$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA", as.factor = T)

# 1990: CEP 15
bd1990_15$id_part <- car::recode(bd1990_15$id_part, "c(1,3) = 'Derecha'; 
                                 c(2,4,5,6) = 'Centro-Izquierda concertación'; 
                                 7 = 'Izquierda extraconcertación'; 
                                 9 = 'Ninguno'; 
                                 8 = NA", as.factor = T) 
bd1990_15$pos_pol <- car::recode(bd1990_15$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA", as.factor = T)

# 1990: CEP 16
bd1990_16$id_part <- car::recode(bd1990_16$id_part, "c(1,3) = 'Derecha'; 
                                 c(2,4,5,6) = 'Centro-Izquierda concertación'; 
                                 7 = 'Izquierda extraconcertación'; 
                                 9 = 'Ninguno'; 
                                 8 = NA", as.factor = T)
bd1990_16$pos_pol <- car::recode(bd1990_16$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA", as.factor = T)


# Recodificación
# Identificación partidaria                          #NOTA: Esta bateria es la misma para las tres encuestas de este ano
# 1 Unión Demócrata Independiente (UDI)
# 2 Partido Radical (PR)
# 3 Renovación Nacional (RN)
# 4 Partido Socialista (PS)
# 5 Democracia Cristiana (DC)
# 6 Partido por la Democracia (PPD)
# 7 Partido Comunista (PC)
# 8 Otro: ¿cuál?:
# 9 Ninguno


# Posición política
#1 Derecha
#2 Centro Derecha
#3 Centro
#4 Centro Izquierda
#5 Izquierda
#6 Independiente
#7 Ninguna
#8 No sabe

# ---- 3.4 Guardar base de datos final ---- 
save(bd1990_14, file = "input/data/bd1990_14.RData")
save(bd1990_15, file = "input/data/bd1990_15.RData")
save(bd1990_16, file = "input/data/bd1990_16.RData")