# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 17 - 9 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car)
# ---- Etapa 2: Cargar bases de datos ---
bd1991_1993_17 <- read_excel("input/data/1991-1993/cep17mar1991.xls")
bd1991_1993_18 <- read_excel("input/data/1991-1993/cep18junjul1991.xls")
bd1991_1993_19 <- read_excel("input/data/1991-1993/cep19septoct1991.xls")
bd1991_1993_20 <- read_excel("input/data/1991-1993/cep20dicenemar19911992.xls")
bd1991_1993_21 <- read_excel("input/data/1991-1993/cep21abr1992.xls")
bd1991_1993_22 <- read_excel("input/data/1991-1993/cep22ago1992.xls")
bd1991_1993_23 <- read_excel("input/data/1991-1993/cep23novdic1992.xls")
bd1991_1993_24 <- read_excel("input/data/1991-1993/cep24mar1993.xls")
bd1991_1993_25 <- read_excel("input/data/1991-1993/cep25junjul1993.xls")
bd1991_1993_28 <- read_excel("input/data/1991-1993/cep28nov1993.xls")

# ---- Etapa 3: Procesamiento de datos ----
## Seleccionar variables a utilizar
bd1991_1993_17 <- select(bd1991_1993_17, p24, p22, p21, p25, p131:p139, p12a, p14)
bd1991_1993_18 <- select(bd1991_1993_18, dpd, dpb, dpa, dpe, p17a:p17i, p10a, p18)
bd1991_1993_19 <- select(bd1991_1993_19, dpf, dpd, dpa, dpg, p11a, p14) #no hay confianza
bd1991_1993_20 <- select(bd1991_1993_20, dpf, dpd, dpa, dpg, p10a, p12) #no hay confianza
bd1991_1993_21 <- select(bd1991_1993_21, dpg, dpd, edad, sexo, p101, p13) #no hay confianza
bd1991_1993_22 <- select(bd1991_1993_22, dpg, dpd, edad, sexo, p10, p13) #no hay confianza
bd1991_1993_23 <- select(bd1991_1993_23, dpg, dpd, edad, sexo, p161:p169, p10, p13)
bd1991_1993_24 <- select(bd1991_1993_24, dpe, dpc, dpa, dpf, p10, p13) #no hay confianza
bd1991_1993_25 <- select(bd1991_1993_25, dpe, dpc, dpa, dpf, p10, p13) #no hay confianza
bd1991_1993_28 <- select(bd1991_1993_28, dpe, dpc, dpa, dpf, p9, p15) #no hay confianza

## Renombrarlas
### 1991-1993: CEP 17
bd1991_1993_17 <- rename(bd1991_1993_17, 
                         edad = p21,
                         id_part = p12a,
                         pos_pol = p14,
                         nse = p24, 
                         sexo = p25,
                         esc = p22,
                         conf_dirpol = p131,
                         conf_iglesiacat = p132,
                         conf_iglesiaev = p133,
                         conf_congreso = p134,
                         conf_altosffaa = p135,
                         conf_dirsin = p136,
                         conf_gabinete = p137,
                         conf_diremp = p138,
                         conf_dircortesup = p139)
### 1991-1993: CEP 18
bd1991_1993_18 <- rename(bd1991_1993_18, 
                         edad = dpa,
                         id_part = p10a,
                         pos_pol = p18,
                         nse = dpd,
                         sexo = dpe,
                         esc = dpb,
                         conf_dirpol = p17a,
                         conf_iglesiacat = p17b,
                         conf_iglesiaev = p17c,
                         conf_congreso = p17d,
                         conf_altosffaa = p17e,
                         conf_dirsin = p17f,
                         conf_gabinete = p17g,
                         conf_diremp = p17h,
                         conf_dircortesup = p17i)
### 1991-1993: CEP 19
bd1991_1993_19 <- rename(bd1991_1993_19,
                    edad = dpa,
                    id_part = p11a,
                    pos_pol = p14,
                    nse = dpf,
                    sexo = dpg,
                    esc = dpd)
### 1991-1993: CEP 20
bd1991_1993_20 <- rename(bd1991_1993_20,
                         edad = dpa,
                         id_part = p10a,
                         pos_pol = p12,
                         nse = dpf,
                         sexo = dpg,
                         esc = dpd)
### 1991-1993: CEP 21
bd1991_1993_21 <- rename(bd1991_1993_21,
                         edad = edad,
                         id_part = p101,
                         pos_pol = p13,
                         nse = dpg,
                         sexo = sexo,
                         esc = dpd)
### 1991-1993: CEP 22
bd1991_1993_22 <- rename(bd1991_1993_22,
                         edad = edad,
                         id_part = p10,
                         pos_pol = p13,
                         nse = dpg,
                         sexo = sexo,
                         esc = dpd)
### 1991-1993: CEP 23
bd1991_1993_23 <- rename(bd1991_1993_23,
                         edad = edad,
                         id_part = p10,
                         pos_pol = p13,
                         nse = dpg,
                         sexo = sexo,
                         esc = dpd,
                         conf_dirpol = p163,
                         conf_iglesiacat = p161,
                         conf_iglesiaev = p162,
                         conf_congreso = p164,
                         conf_altosffaa = p165,
                         conf_dirsin = p166,
                         conf_gabinete = p167,
                         conf_diremp = p168,
                         conf_dircortesup = p169)
### 1991-1993: CEP 24
bd1991_1993_24 <- rename(bd1991_1993_24,
                         edad = dpa,
                         id_part = p10,
                         pos_pol = p13,
                         nse = dpe,
                         sexo = dpf,
                         esc = dpc)
### 1991-1993: CEP 25
bd1991_1993_25 <- rename(bd1991_1993_25,
                         edad = dpa,
                         id_part = p10,
                         pos_pol = p13,
                         nse = dpe,
                         sexo = dpf,
                         esc = dpc)
### 1991-1993: CEP 28
bd1991_1993_28 <- rename(bd1991_1993_28,
                         edad = dpa,
                         id_part = p9,
                         pos_pol = p15,
                         nse = dpe,
                         sexo = dpf,
                         esc = dpc)

#---- 3.1 Tratamiento de  sociodemográficas ----
#---- 3.1.1 Frecuencias ----
## 1991-1993: CEP 17
frq(bd1991_1993_17$nse) 
frq(bd1991_1993_17$esc) 
frq(bd1991_1993_17$edad) 
frq(bd1991_1993_17$sexo) 

## 1991-1993: CEP 18
frq(bd1991_1993_18$nse) 
frq(bd1991_1993_18$esc) 
frq(bd1991_1993_18$edad) 
frq(bd1991_1993_18$sexo) 

## 1991-1993: CEP 19
frq(bd1991_1993_19$nse) 
frq(bd1991_1993_19$esc) 
frq(bd1991_1993_19$edad) 
frq(bd1991_1993_19$sexo) 

## 1991-1993: CEP 20
frq(bd1991_1993_20$nse) 
frq(bd1991_1993_20$esc) 
frq(bd1991_1993_20$edad) 
frq(bd1991_1993_20$sexo) 

## 1991-1993: CEP 21
frq(bd1991_1993_21$nse) 
frq(bd1991_1993_21$esc) 
frq(bd1991_1993_21$edad) 
frq(bd1991_1993_21$sexo) 

## 1991-1993: CEP 22
frq(bd1991_1993_22$nse) 
frq(bd1991_1993_22$esc) 
frq(bd1991_1993_22$edad) 
frq(bd1991_1993_22$sexo) 

## 1991-1993: CEP 23
frq(bd1991_1993_23$nse) 
frq(bd1991_1993_23$esc) 
frq(bd1991_1993_23$edad) 
frq(bd1991_1993_23$sexo) 

## 1991-1993: CEP 24
frq(bd1991_1993_24$nse) 
frq(bd1991_1993_24$esc) 
frq(bd1991_1993_24$edad) 
frq(bd1991_1993_24$sexo) 

## 1991-1993: CEP 25
frq(bd1991_1993_25$nse) 
frq(bd1991_1993_25$esc) 
frq(bd1991_1993_25$edad) 
frq(bd1991_1993_25$sexo) 

## 1991-1993: CEP 28
frq(bd1991_1993_28$nse) 
frq(bd1991_1993_28$esc) 
frq(bd1991_1993_28$edad) 
frq(bd1991_1993_28$sexo) 

#---- 3.1.2 Recodificación ----
## 1991-1993: CEP 17
bd1991_1993_17$nse  <- car::recode(bd1991_1993_17$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1991_1993_17$esc <- car::recode(bd1991_1993_17$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1991_1993_17$edad <- car::recode(bd1991_1993_17$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1991_1993_17$sexo <- car::recode(bd1991_1993_17$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 1991-1993: CEP 18
bd1991_1993_18$nse  <- car::recode(bd1991_1993_18$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1991_1993_18$esc <- car::recode(bd1991_1993_18$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1991_1993_18$edad <- car::recode(bd1991_1993_18$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1991_1993_18$sexo <- car::recode(bd1991_1993_18$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 1991-1993: CEP 19
bd1991_1993_19$nse  <- car::recode(bd1991_1993_19$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1991_1993_19$esc <- car::recode(bd1991_1993_19$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1991_1993_19$edad <- car::recode(bd1991_1993_19$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1991_1993_19$sexo <- car::recode(bd1991_1993_19$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 1991-1993: CEP 20
bd1991_1993_20$nse  <- car::recode(bd1991_1993_20$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1991_1993_20$esc <- car::recode(bd1991_1993_20$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1991_1993_20$edad <- car::recode(bd1991_1993_20$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1991_1993_20$sexo <- car::recode(bd1991_1993_20$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 1991-1993: CEP 21
bd1991_1993_21$nse  <- car::recode(bd1991_1993_21$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1991_1993_21$esc <- car::recode(bd1991_1993_21$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1991_1993_21$edad <- car::recode(bd1991_1993_21$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1991_1993_21$sexo <- car::recode(bd1991_1993_21$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 1991-1993: CEP 22
bd1991_1993_22$nse  <- car::recode(bd1991_1993_22$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1991_1993_22$esc <- car::recode(bd1991_1993_22$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1991_1993_22$edad <- car::recode(bd1991_1993_22$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1991_1993_22$sexo <- car::recode(bd1991_1993_22$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 1991-1993: CEP 23
bd1991_1993_23$nse  <- car::recode(bd1991_1993_23$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1991_1993_23$esc <- car::recode(bd1991_1993_23$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1991_1993_23$edad <- car::recode(bd1991_1993_23$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1991_1993_23$sexo <- car::recode(bd1991_1993_23$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 1991-1993: CEP 24
bd1991_1993_24$nse  <- car::recode(bd1991_1993_24$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1991_1993_24$esc <- car::recode(bd1991_1993_24$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1991_1993_24$edad <- car::recode(bd1991_1993_24$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1991_1993_24$sexo <- car::recode(bd1991_1993_24$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 1991-1993: CEP 25
bd1991_1993_25$nse  <- car::recode(bd1991_1993_25$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1991_1993_25$esc <- car::recode(bd1991_1993_25$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1991_1993_25$edad <- car::recode(bd1991_1993_25$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1991_1993_25$sexo <- car::recode(bd1991_1993_25$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 1991-1993: CEP 28
bd1991_1993_28$nse  <- car::recode(bd1991_1993_28$nse,  "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd1991_1993_28$esc <- car::recode(bd1991_1993_28$esc, "c(1,2) = '0-3'; c(3,4) = '4-8'; c(5,6) = '9-12'; c(7,8) = '13 y mas'; 9 = 'NC'", as.factor = T)
bd1991_1993_28$edad <- car::recode(bd1991_1993_28$edad, "1 = '18-24'; 2 = '25-34'; c(3,4) = '35-54'; c(5,6) = '55 y mas'; 7 = 'NC'", as.factor = T)
bd1991_1993_28$sexo <- car::recode(bd1991_1993_28$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

#---- 3.2 Tratamiento de variables de confianza ----
#---- 3.2.1 Frecuencias ----
## 1991-1993: CEP 17
frq(bd1991_1993_17$conf_dirpol)
frq(bd1991_1993_17$conf_iglesiacat)
frq(bd1991_1993_17$conf_iglesiaev)
frq(bd1991_1993_17$conf_congreso)
frq(bd1991_1993_17$conf_altosffaa)
frq(bd1991_1993_17$conf_dirsin) # Esta no la ocuparemos (dirigentes sindicales)
frq(bd1991_1993_17$conf_gabinete) # Esta no la ocuparemos (ministros actuales/gabinetes)
frq(bd1991_1993_17$conf_diremp)
frq(bd1991_1993_17$conf_dircortesup)

## 1991-1993: CEP 18
frq(bd1991_1993_18$conf_dirpol)
frq(bd1991_1993_18$conf_iglesiacat)
frq(bd1991_1993_18$conf_iglesiaev)
frq(bd1991_1993_18$conf_congreso)
frq(bd1991_1993_18$conf_altosffaa)
frq(bd1991_1993_18$conf_dirsin) # Esta no la ocuparemos (dirigentes sindicales)
frq(bd1991_1993_18$conf_gabinete) # Esta no la ocuparemos (ministros actuales/gabinetes)
frq(bd1991_1993_18$conf_diremp)
frq(bd1991_1993_18$conf_dircortesup)

## 1991-1993: CEP 19 #no tiene confianza
## 1991-1993: CEP 20 #no tiene confianza
## 1991-1993: CEP 21 #no tiene confianza
## 1991-1993: CEP 22 #no tiene confianza

## 1991-1993: CEP 23
frq(bd1991_1993_23$conf_dirpol)
frq(bd1991_1993_23$conf_iglesiacat)
frq(bd1991_1993_23$conf_iglesiaev)
frq(bd1991_1993_23$conf_congreso)
frq(bd1991_1993_23$conf_altosffaa)
frq(bd1991_1993_23$conf_dirsin) # Esta no la ocuparemos (dirigentes sindicales)
frq(bd1991_1993_23$conf_gabinete) # Esta no la ocuparemos (ministros actuales/gabinetes)
frq(bd1991_1993_23$conf_diremp)
frq(bd1991_1993_23$conf_dircortesup)

## 1991-1993: CEP 24 #no tiene confianza
## 1991-1993: CEP 25 #no tiene confianza
## 1991-1993: CEP 28 #no tiene confianza

#---- 3.2.2 Recodificacion ----
## 1991-1993: CEP 17
bd1991_1993_17$conf_dirpol <- car::recode(bd1991_1993_17$conf_dirpol,"3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_17$conf_iglesiacat <- car::recode(bd1991_1993_17$conf_iglesiacat,"3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_17$conf_iglesiaev <- car::recode(bd1991_1993_17$conf_iglesiaev, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_17$conf_congreso <- car::recode(bd1991_1993_17$conf_congreso, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_17$conf_altosffaa <- car::recode(bd1991_1993_17$conf_altosffaa, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_17$conf_diremp <- car::recode(bd1991_1993_17$conf_diremp, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_17$conf_dircortesup <- car::recode(bd1991_1993_17$conf_dircortesup, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)

## 1991-1993: CEP 18
bd1991_1993_18$conf_dirpol <- car::recode(bd1991_1993_18$conf_dirpol,"3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_18$conf_iglesiacat <- car::recode(bd1991_1993_18$conf_iglesiacat,"3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_18$conf_iglesiaev <- car::recode(bd1991_1993_18$conf_iglesiaev, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_18$conf_congreso <- car::recode(bd1991_1993_18$conf_congreso, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_18$conf_altosffaa <- car::recode(bd1991_1993_18$conf_altosffaa, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_18$conf_diremp <- car::recode(bd1991_1993_18$conf_diremp, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_18$conf_dircortesup <- car::recode(bd1991_1993_18$conf_dircortesup, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)

## 1991-1993: CEP 23
bd1991_1993_23$conf_dirpol <- car::recode(bd1991_1993_23$conf_dirpol,"3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_23$conf_iglesiacat <- car::recode(bd1991_1993_23$conf_iglesiacat,"3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_23$conf_iglesiaev <- car::recode(bd1991_1993_23$conf_iglesiaev, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_23$conf_congreso <- car::recode(bd1991_1993_23$conf_congreso, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_23$conf_altosffaa <- car::recode(bd1991_1993_23$conf_altosffaa, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_23$conf_diremp <- car::recode(bd1991_1993_23$conf_diremp, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)
bd1991_1993_23$conf_dircortesup <- car::recode(bd1991_1993_23$conf_dircortesup, "3 = 'Mucha confianza'; c(1,2) = 'Otra'; 4 = NA", as.factor = T)


# No olvidar
# Codificación original

# 1. Poca confianza
# 2. Algo de confianza
# 3. Mucha confianza

#---- 3.2.3 Otros ajustes ----
### Construccion variable iglesia en calidad de institucion
## 1991-1993: CEP 17
bd1991_1993_17$conf_iglesia[bd1991_1993_17$conf_iglesiacat == 'Mucha confianza' | bd1991_1993_17$conf_iglesiaev == 'Mucha confianza'] <- 'Mucha confianza'
bd1991_1993_17$conf_iglesia[bd1991_1993_17$conf_iglesiacat == 'Otra' & bd1991_1993_17$conf_iglesiaev == 'Otra'] <- 'Otra'

## 1991-1993: CEP 18
bd1991_1993_18$conf_iglesia[bd1991_1993_18$conf_iglesiacat == 'Mucha confianza' | bd1991_1993_18$conf_iglesiaev == 'Mucha confianza'] <- 'Mucha confianza'
bd1991_1993_18$conf_iglesia[bd1991_1993_18$conf_iglesiacat == 'Otra' & bd1991_1993_18$conf_iglesiaev == 'Otra'] <- 'Otra'

## 1991-1993: CEP 23
bd1991_1993_23$conf_iglesia[bd1991_1993_23$conf_iglesiacat == 'Mucha confianza' | bd1991_1993_23$conf_iglesiaev == 'Mucha confianza'] <- 'Mucha confianza'
bd1991_1993_23$conf_iglesia[bd1991_1993_23$conf_iglesiacat == 'Otra' & bd1991_1993_23$conf_iglesiaev == 'Otra'] <- 'Otra'

### Sacar variables de confianza que no usaremos.
## 1991-1993: CEP 17
bd1991_1993_17 <- select(bd1991_1993_17, -conf_iglesiaev, -conf_iglesiacat,-conf_dirsin, -conf_gabinete) 

## 1991-1993: CEP 18
bd1991_1993_18 <- select(bd1991_1993_18, -conf_iglesiaev, -conf_iglesiacat,-conf_dirsin, -conf_gabinete)

## 1991-1993: CEP 23
bd1991_1993_23 <- select(bd1991_1993_23, -conf_iglesiaev, -conf_iglesiacat,-conf_dirsin, -conf_gabinete)

#---- 3.2.4 Guardar bases de confianza ----
save(bd1991_1993_17, file = "input/data/bd1991_1993_17.RData")
save(bd1991_1993_18, file = "input/data/bd1991_1993_18.RData")
save(bd1991_1993_23, file = "input/data/bd1991_1993_23.RData")

#---- 3.3 Tratamiento de variables de identificación partidaria e identificación política (o posición política)
#---- 3.3.1 Frecuencias ----

frq(bd1991_1993_17$id_part)
frq(bd1991_1993_17$pos_pol)

frq(bd1991_1993_18$id_part)
frq(bd1991_1993_18$pos_pol)

frq(bd1991_1993_19$id_part)
frq(bd1991_1993_19$pos_pol)

frq(bd1991_1993_20$id_part)
frq(bd1991_1993_20$pos_pol)

frq(bd1991_1993_21$id_part)
frq(bd1991_1993_21$pos_pol)

frq(bd1991_1993_22$id_part)
frq(bd1991_1993_22$pos_pol)

frq(bd1991_1993_23$id_part)
frq(bd1991_1993_23$pos_pol)

frq(bd1991_1993_24$id_part)
frq(bd1991_1993_24$pos_pol)

frq(bd1991_1993_25$id_part)
frq(bd1991_1993_25$pos_pol)

frq(bd1991_1993_28$id_part)
frq(bd1991_1993_28$pos_pol)

#---- 3.3.2 Recodificacion ----

# 1991 - 1993: CEP 17
bd1991_1993_17$id_part <- car::recode(bd1991_1993_17$id_part, "c(1,3,8) = 'Derecha'; 
                                 c(2,4,5,6,9) = 'Centro-Izquierda concertación'; 
                                 7 = 'Izquierda extraconcertación'; 
                                 11 = 'Ninguno'; 
                                 10 = NA", as.factor = T)
bd1991_1993_17$pos_pol <- car::recode(bd1991_1993_17$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA", as.factor = T)

# 1991 - 1993: CEP 18
bd1991_1993_18$id_part <- car::recode(bd1991_1993_18$id_part, "c(1,3,8) = 'Derecha'; 
                                 c(2,4,5,6,9) = 'Centro-Izquierda concertación'; 
                                 c(7,10) = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 11 = NA", as.factor = T) 
bd1991_1993_18$pos_pol <- car::recode(bd1991_1993_18$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA", as.factor = T)

# 1991 - 1993: CEP 19
bd1991_1993_19$id_part <- car::recode(bd1991_1993_19$id_part, "c(1,3,8) = 'Derecha'; 
                                 c(2,4,5,6,9) = 'Centro-Izquierda concertación'; 
                                 c(7,10) = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 c(11,13) = NA", as.factor = T)  
bd1991_1993_19$pos_pol <- car::recode(bd1991_1993_19$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA", as.factor = T)

# 1991 - 1993: CEP 20
bd1991_1993_20$id_part <- car::recode(bd1991_1993_20$id_part, "c(1,3,8) = 'Derecha'; 
                                 c(2,4,5,6,9) = 'Centro-Izquierda concertación'; 
                                 c(7,10) = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 11 = NA", as.factor = T)  
bd1991_1993_20$pos_pol <- car::recode(bd1991_1993_20$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA", as.factor = T)

# 1991 - 1993: CEP 21
bd1991_1993_21$id_part <- car::recode(bd1991_1993_21$id_part, "c(1,3,8) = 'Derecha'; 
                                 c(2,4,5,6,9) = 'Centro-Izquierda concertación'; 
                                 c(7,10) = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 11 = NA", as.factor = T)
bd1991_1993_21$pos_pol <- car::recode(bd1991_1993_21$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA", as.factor = T)

# 1991 - 1993: CEP 22
bd1991_1993_22$id_part <- car::recode(bd1991_1993_22$id_part, "c(1,3,8) = 'Derecha'; 
                                 c(2,4,5,6,9) = 'Centro-Izquierda concertación'; 
                                 c(7,10) = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 11 = NA", as.factor = T)
bd1991_1993_22$pos_pol <- car::recode(bd1991_1993_22$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA", as.factor = T)

# 1991 - 1993: CEP 23
bd1991_1993_23$id_part <- car::recode(bd1991_1993_23$id_part, "c(1,3,8) = 'Derecha'; 
                                 c(2,4,5,6,9) = 'Centro-Izquierda concertación'; 
                                 c(7,10) = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 11 = NA", as.factor = T)
bd1991_1993_23$pos_pol <- car::recode(bd1991_1993_23$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA", as.factor = T)

# 1991 - 1993: CEP 24 #Aqui se introduce la categoria 9!!!
bd1991_1993_24$id_part <- car::recode(bd1991_1993_24$id_part, "c(1,3,8) = 'Derecha'; 
                                 c(2,4,5,6,9) = 'Centro-Izquierda concertación'; 
                                 c(7,10) = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 c(11,13) = NA", as.factor = T) 
bd1991_1993_24$pos_pol <- car::recode(bd1991_1993_24$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 1991 - 1993: CEP 25 
bd1991_1993_25$id_part <- car::recode(bd1991_1993_25$id_part, "c(1,3,8) = 'Derecha'; 
                                 c(2,4,5,6,9) = 'Centro-Izquierda concertación'; 
                                 c(7,10) = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 11 = NA", as.factor = T)  
bd1991_1993_25$pos_pol <- car::recode(bd1991_1993_25$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

bd1991_1993_28$id_part <- car::recode(bd1991_1993_28$id_part, "c(1,3,8) = 'Derecha'; 
                                 c(2,4,5,6,9) = 'Centro-Izquierda concertación'; 
                                 c(7,10) = 'Izquierda extraconcertación'; 
                                 12 = 'Ninguno'; 
                                 c(11, 13) = NA", as.factor = T) 
bd1991_1993_28$pos_pol <- car::recode(bd1991_1993_28$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# ---- 3.4 Guardar base de datos final ---- 
save(bd1991_1993_17, file = "input/data/bd1991_1993_17.RData")
save(bd1991_1993_18, file = "input/data/bd1991_1993_18.RData")
save(bd1991_1993_19, file = "input/data/bd1991_1993_19.RData")
save(bd1991_1993_20, file = "input/data/bd1991_1993_20.RData")
save(bd1991_1993_21, file = "input/data/bd1991_1993_21.RData")
save(bd1991_1993_22, file = "input/data/bd1991_1993_22.RData")
save(bd1991_1993_23, file = "input/data/bd1991_1993_23.RData")
save(bd1991_1993_24, file = "input/data/bd1991_1993_24.RData")
save(bd1991_1993_25, file = "input/data/bd1991_1993_25.RData")
save(bd1991_1993_28, file = "input/data/bd1991_1993_28.RData")
