# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 29 - 9 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car, haven)
# ---- Etapa 2: Cargar bases de datos ---
bd2018_2019_82 <- read_excel("input/data/2018-2019/cep82octnov2018.xlsx")
bd2018_2019_83 <- read_sav("input/data/2018-2019/cep83may2019.sav")
bd2018_2019_84 <- read_sav("input/data/2018-2019/cep84dic2019.sav")


# ---- Etapa 3: Procesamiento de datos ----
## Seleccionar variables a utilizar

bd2018_2019_82<- select(bd2018_2019_82, DS_P38, DS_P3, DS_P2_EXACTA, DS_P1, REL_7_A:REL_7_E, MB_P12, MB_P13)
bd2018_2019_83<- select(bd2018_2019_83, DS_P44, DS_P3, DS_P2_EXACTA, DS_P1, MB_P13, MB_P14)
bd2018_2019_84<- select(bd2018_2019_84, DS_P35, DS_P3, DS_P2_EXACTA, DS_P1, MB_P11_1:MB_P11_14, MB_P8, MB_P9)

## Renombrarlas
## 2018-2019: CEP 82
bd2018_2019_82 <- rename(bd2018_2019_82,
                         edad = DS_P2_EXACTA,
                         id_part = MB_P12,
                         pos_pol = MB_P13,
                         nse = DS_P38,
                         sexo = DS_P1,
                         esc = DS_P3,
                         conf_congreso = REL_7_A,
                         conf_comercio = REL_7_B,
                         conf_iglesias = REL_7_C,
                         conf_sistjust = REL_7_D,
                         conf_sistedu = REL_7_E)
## 2018-2019: CEP 83
bd2018_2019_83 <- rename(bd2018_2019_83,
                         edad = DS_P2_EXACTA,
                         id_part = MB_P13,
                         pos_pol = MB_P14,
                         nse = DS_P44,
                         sexo = DS_P1,
                         esc = DS_P3)
## 2018-2019: CEP 84
bd2018_2019_84 <- rename(bd2018_2019_84,
                         edad = DS_P2_EXACTA,
                         id_part = MB_P8,
                         pos_pol = MB_P9,
                         nse = DS_P35,
                         sexo = DS_P1,
                         esc = DS_P3,
                         conf_iglesiacat = MB_P11_1,
                         conf_ffaa = MB_P11_2,
                         conf_iglesiaev = MB_P11_3,
                         conf_partidos = MB_P11_4,
                         conf_tribunalesjust = MB_P11_5,
                         conf_diarios = MB_P11_6,
                         conf_tele = MB_P11_7,
                         conf_radios = MB_P11_8,
                         conf_rrss = MB_P11_9,
                         conf_sindicatos = MB_P11_10,
                         conf_carabineros = MB_P11_11,
                         conf_gobierno = MB_P11_12,
                         conf_congreso = MB_P11_13,
                         conf_emppriv = MB_P11_14)


#---- 3.1 Tratamiento de  sociodemográficas ----
#---- 3.1.1 Frecuencias ----

## 2018-2019: CEP 82
frq(bd2018_2019_82$nse) # NSE
frq(bd2018_2019_82$esc) # Escolaridad
frq(bd2018_2019_82$edad) # Edad
frq(bd2018_2019_82$sexo) # Sexo

## 2018-2019: CEP 83
frq(bd2018_2019_83$nse) # NSE
frq(bd2018_2019_83$esc) # Escolaridad
frq(bd2018_2019_83$edad) # Edad
frq(bd2018_2019_83$sexo) # Sexo

## 2018-2019: CEP 84
frq(bd2018_2019_84$nse) # NSE
frq(bd2018_2019_84$esc) # Escolaridad
frq(bd2018_2019_84$edad) # Edad
frq(bd2018_2019_84$sexo) # Sexo

#---- 3.1.2 Recodificacion ----
bd2018_2019_82 <- sjlabelled::remove_all_labels(bd2018_2019_82)
bd2018_2019_83 <- sjlabelled::remove_all_labels(bd2018_2019_83)
bd2018_2019_84 <- sjlabelled::remove_all_labels(bd2018_2019_84)

## 2018-2019: CEP 82
bd2018_2019_82$nse  <- car::recode(bd2018_2019_82$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2018_2019_82$esc <- car::recode(bd2018_2019_82$esc, "c(88,99) = NA; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2018_2019_82$edad <- car::recode(bd2018_2019_82$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2018_2019_82$sexo <- car::recode(bd2018_2019_82$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2018-2019: CEP 83
bd2018_2019_83$nse  <- car::recode(bd2018_2019_83$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2018_2019_83$esc <- car::recode(bd2018_2019_83$esc, "c(88,99) = NA; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2018_2019_83$edad <- car::recode(bd2018_2019_83$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2018_2019_83$sexo <- car::recode(bd2018_2019_83$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2018-2019: CEP 84
bd2018_2019_84$nse  <- car::recode(bd2018_2019_84$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2018_2019_84$esc <- car::recode(bd2018_2019_84$esc, "c(88,99) = NA; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2018_2019_84$edad <- car::recode(bd2018_2019_84$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2018_2019_84$sexo <- car::recode(bd2018_2019_84$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

#---- 3.2 Tratamiento de variables de confianza ----
#---- 3.2.1 Frecuencias ----

## 2018-2019: CEP 82
frq(bd2018_2019_82$conf_congreso)
frq(bd2018_2019_82$conf_comercio)
frq(bd2018_2019_82$conf_iglesias)
frq(bd2018_2019_82$conf_sistjust)
frq(bd2018_2019_82$conf_sistedu)

## 2018-2019: CEP 84
frq(bd2018_2019_84$conf_iglesiacat) # Combinar:iglesia
frq(bd2018_2019_84$conf_ffaa)
frq(bd2018_2019_84$conf_iglesiaev) # Combinar:iglesia
frq(bd2018_2019_84$conf_partidos)
frq(bd2018_2019_84$conf_tribunalesjust)
frq(bd2018_2019_84$conf_diarios) # Combinar: MMC
frq(bd2018_2019_84$conf_tele) # Combinar: MMC
frq(bd2018_2019_84$conf_radios)
frq(bd2018_2019_84$conf_rrss)
frq(bd2018_2019_84$conf_sindicatos) # No usar
frq(bd2018_2019_84$conf_carabineros) # Combinar: instituciones del orden
frq(bd2018_2019_84$conf_gobierno)
frq(bd2018_2019_84$conf_congreso)
frq(bd2018_2019_84$conf_emppriv)

#---- 3.2.2 Recodificacion ----
## 2018-2019: CEP 82
bd2018_2019_82$conf_congreso <- car::recode(bd2018_2019_82$conf_congreso, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)
bd2018_2019_82$conf_comercio <- car::recode(bd2018_2019_82$conf_comercio, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)
bd2018_2019_82$conf_iglesias <- car::recode(bd2018_2019_82$conf_iglesias, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)
bd2018_2019_82$conf_sistjust <- car::recode(bd2018_2019_82$conf_sistjust, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)
bd2018_2019_82$conf_sistedu <- car::recode(bd2018_2019_82$conf_sistedu, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)

## 2018-2019: CEP 84
bd2018_2019_84$conf_iglesiacat <- car::recode(bd2018_2019_84$conf_iglesiacat,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_ffaa <- car::recode(bd2018_2019_84$conf_ffaa,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_iglesiaev <- car::recode(bd2018_2019_84$conf_iglesiaev,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_partidos <- car::recode(bd2018_2019_84$conf_partidos, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_tribunalesjust <- car::recode(bd2018_2019_84$conf_tribunalesjust, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_diarios <- car::recode(bd2018_2019_84$conf_diarios, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_tele <- car::recode(bd2018_2019_84$conf_tele, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_radios <- car::recode(bd2018_2019_84$conf_radios, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_rrss <- car::recode(bd2018_2019_84$conf_rrss, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_sindicatos <- car::recode(bd2018_2019_84$conf_sindicatos, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_carabineros <- car::recode(bd2018_2019_84$conf_carabineros, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_gobierno <- car::recode(bd2018_2019_84$conf_gobierno, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_congreso <- car::recode(bd2018_2019_84$conf_congreso, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2018_2019_84$conf_emppriv <- car::recode(bd2018_2019_84$conf_emppriv, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)

# No olvidar
# Codificación original
#1. Mucha confianza
#2. Bastante confianza
#3. No mucha confianza
#4. Ninguna confianza
#8. No sabe
#9. No contesta

#---- 3.2.3 Otros ajustes ----
## 2018-2019: CEP 84

### Construccion variable iglesia en calidad de institucion

bd2018_2019_84$conf_iglesia[bd2018_2019_84$conf_iglesiacat == 'Mucha confianza' & bd2018_2019_84$conf_iglesiaev == 'Mucha confianza'] <- 'Mucha confianza'
bd2018_2019_84$conf_iglesia[bd2018_2019_84$conf_iglesiacat == 'Otra' & bd2018_2019_84$conf_iglesiaev == 'Otra'] <- 'Otra'

### Construccion variable MMC
bd2018_2019_84$conf_mmc[bd2018_2019_84$conf_diarios == 'Mucha confianza' & bd2018_2019_84$conf_radios == 'Mucha confianza'] <- 'Mucha confianza'
bd2018_2019_84$conf_mmc[bd2018_2019_84$conf_diarios == 'Mucha confianza' & bd2018_2019_84$conf_tele == 'Mucha confianza'] <- 'Mucha confianza'
bd2018_2019_84$conf_mmc[bd2018_2019_84$conf_tele == 'Mucha confianza' & bd2018_2019_84$conf_radios == 'Mucha confianza'] <- 'Mucha confianza'

bd2018_2019_84$conf_mmc[bd2018_2019_84$conf_diarios == 'Otra' & bd2018_2019_84$conf_radios == 'Otra'] <- 'Otra'
bd2018_2019_84$conf_mmc[bd2018_2019_84$conf_diarios == 'Otra' & bd2018_2019_84$conf_tele == 'Otra'] <- 'Otra'
bd2018_2019_84$conf_mmc[bd2018_2019_84$conf_tele == 'Otra' & bd2018_2019_84$conf_radios == 'Otra'] <- 'Otra'

bd2018_2019_84$conf_mmc[bd2018_2019_84$conf_tele == 'Mucha confianza' & bd2018_2019_84$conf_radios == 'Mucha confianza' & bd2018_2019_84$conf_diarios == 'Mucha confianza'] <- 'Mucha confianza'
bd2018_2019_84$conf_mmc[bd2018_2019_84$conf_tele == 'Otra' & bd2018_2019_84$conf_radios == 'Otra' & bd2018_2019_84$conf_diarios == 'Otra'] <- 'Otra'

# Ver frecuencia variable nueva
frq(bd2018_2019_84$conf_mmc)

# Eliminar variables que no se usarán
bd2018_2019_84 <- select(bd2018_2019_84, -conf_radios, -conf_tele, -conf_diarios, -conf_carabineros, -conf_sindicatos)

#---- 3.2.4 Guardar bases de confianza ----
save(bd2018_2019_82, file = "input/data/bd2018_2019_82.RData")
save(bd2018_2019_84, file = "input/data/bd2018_2019_84.RData")

#---- 3.3 Tratamiento de variables de identificación partidaria e identificación política (o posición política)
#---- 3.3.1 Frecuencias ----
frq(bd2018_2019_82$id_part)
frq(bd2018_2019_82$pos_pol)

frq(bd2018_2019_83$id_part)
frq(bd2018_2019_83$pos_pol)

frq(bd2018_2019_84$id_part)
frq(bd2018_2019_84$pos_pol)

#---- 3.3.2 Recodificacion ----

# 2018 - 2019: CEP 82 
bd2018_2019_82$id_part <- car::recode(bd2018_2019_82$id_part, "c(1,4,8) = 'Derecha'; 
                                 c(2,3,5,6) = 'Centro-Izquierda concertación';
                                 c(7,9)= 'Izquierda extraconcertación'; 
                                 77 = 'Ninguno'; 
                                 c(10,88,99) = NA", as.factor = T)  
bd2018_2019_82$pos_pol <- car::recode(bd2018_2019_82$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 2018 - 2019: CEP 83 
bd2018_2019_83$id_part <- car::recode(bd2018_2019_83$id_part, "c(1,4,8) = 'Derecha'; 
                                 c(2,3,5,6) = 'Centro-Izquierda concertación';
                                 c(7,9)= 'Izquierda extraconcertación'; 
                                 77 = 'Ninguno'; 
                                 c(10,88,99) = NA", as.factor = T)   
bd2018_2019_83$pos_pol <- car::recode(bd2018_2019_83$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 2018 - 2019: CEP 84 
bd2018_2019_84$id_part <- car::recode(bd2018_2019_84$id_part, "c(1,4,8) = 'Derecha'; 
                                 c(2,3,5,6) = 'Centro-Izquierda concertación';
                                 c(7,9)= 'Izquierda extraconcertación'; 
                                 77 = 'Ninguno'; 
                                 c(10,88,99) = NA", as.factor = T)  
bd2018_2019_84$pos_pol <- car::recode(bd2018_2019_84$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# ---- 3.4 Guardar base de datos final ---- 
save(bd2018_2019_82, file = "input/data/bd2018_2019_82.RData")
save(bd2018_2019_83, file = "input/data/bd2018_2019_83.RData")
save(bd2018_2019_84, file = "input/data/bd2018_2019_84.RData")