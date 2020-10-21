# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 27 - 9 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car, haven)
# ---- Etapa 2: Cargar bases de datos ---
bd2006_2008_52 <- read_excel("input/data/2006-2008/cep52junjul2006.xls")
bd2006_2008_54 <- read_sav("input/data/2006-2008/cep54dic2006.sav")

bd2006_2008_55 <- read_sav("input/data/2006-2008/cep55jun2007.sav")
bd2006_2008_56 <- read_sav("input/data/2006-2008/cep56novdic2007.sav")


bd2006_2008_57 <- read_sav("input/data/2006-2008/cep57jun2008.sav")
bd2006_2008_58 <- read_sav("input/data/2006-2008/cep58novdic2008.sav")

# ---- Etapa 3: Procesamiento de datos ----
## Seleccionar variables a utilizar

#base de datos <- select(base de datos, nse, escolaridad, edad, sexo, variables confianza, identificacion partidaria, posicion politica)
bd2006_2008_52 <- select(bd2006_2008_52, dd28, ESCOLARIDAD, dd2, dd1, mb11) # mb16 variable sobre escala 1 a 10 en espectro politico
bd2006_2008_54 <- select(bd2006_2008_54, DDP22, ESCOLARIDAD, DDP2, DDP1, TE_2P4a:TE_2P4h, MBP13, MBP17)

bd2006_2008_55 <- select(bd2006_2008_55, DDP30, ESCOLARIDAD, DDP2, DDP1, TE2P2_A:TE2P2_H, MBP13, MBP14) #MBP16 preguta en escala de 1 a 10
bd2006_2008_56 <- select(bd2006_2008_56, DDP21, ESCOLARIDAD, DDP2, DDP1, MBP14, MBP15)


bd2006_2008_57 <- select(bd2006_2008_57, ddp30, ESCOLARIDAD, ddp2, ddp1, te3p08_a:te3p08_e, mbp14, mbp16)
bd2006_2008_58 <- select(bd2006_2008_58, DDP23, ESCOLARIDAD, DDP2, DDP1, TE2P13_A:TE2P13_M, MBP14, MBP16) 

## Renombrarlas
### 2006_2008: CEP 52
bd2006_2008_52 <- rename(bd2006_2008_52,
                    edad = dd2,
                    id_part = mb11,
                    nse = dd28,
                    sexo = dd1,
                    esc = ESCOLARIDAD)

### 2006_2008: CEP 54
bd2006_2008_54 <- rename(bd2006_2008_54,
                    edad = DDP2,
                    id_part = MBP13,
                    pos_pol = MBP17,
                    nse = DDP22,
                    sexo = DDP1,
                    esc = ESCOLARIDAD,
                    conf_ffaa = TE_2P4a,
                    conf_tribunalesjust = TE_2P4b,
                    conf_partidos = TE_2P4d)
                    

### 2006_2008: CEP 55
bd2006_2008_55 <- rename(bd2006_2008_55,
                    edad = DDP2,
                    id_part = MBP13,
                    pos_pol = MBP14,
                    nse = DDP30,
                    sexo = DDP1,
                    esc = ESCOLARIDAD,
                    conf_partido1 = TE2P2_A,
                    conf_partido2 = TE2P2_B,
                    conf_partido3 = TE2P2_C,
                    conf_partido4 = TE2P2_D,
                    conf_partido5 = TE2P2_E,
                    conf_partido6 = TE2P2_F,
                    conf_partido7 = TE2P2_G,
                    conf_partido8 = TE2P2_H)


### 2006_2008: CEP 56
bd2006_2008_56 <- rename(bd2006_2008_56,
                    edad = DDP2,
                    id_part = MBP14,
                    pos_pol = MBP15,
                    nse = DDP21,
                    sexo = DDP1,
                    esc = ESCOLARIDAD)

### 2006_2008: CEP 57
bd2006_2008_57 <- rename(bd2006_2008_57,
                    edad = ddp2,
                    id_part = mbp14,
                    pos_pol = mbp16,
                    nse = ddp30,
                    sexo = ddp1,
                    esc = ESCOLARIDAD,
                    conf_congreso = te3p08_a,
                    conf_comercio = te3p08_b,
                    conf_iglesias = te3p08_c,
                    conf_sistjudicial = te3p08_d,
                    conf_sistemaedu = te3p08_e)

### 2006_2008: CEP 58
bd2006_2008_58 <- rename(bd2006_2008_58,
                    edad = DDP2,
                    id_part = MBP14,
                    pos_pol = MBP16,
                    nse = DDP23,
                    sexo = DDP1,
                    esc = ESCOLARIDAD,
                    conf_iglesiacat = TE2P13_A,
                    conf_ffaa = TE2P13_B,
                    conf_iglesiaev = TE2P13_C,
                    conf_partidos = TE2P13_D,
                    conf_tribunalesjust = TE2P13_E,
                    conf_diarios = TE2P13_F,
                    conf_tele = TE2P13_G,
                    conf_radios = TE2P13_H,
                    conf_sindicatos = TE2P13_I,
                    conf_carabineros = TE2P13_J,
                    conf_gobierno = TE2P13_K,
                    conf_congreso = TE2P13_L,
                    conf_emppriv = TE2P13_M)

#---- 3.1 Tratamiento de  sociodemográficas ----
#---- 3.1.1 Frecuencias ----
## 2006_2008: CEP 52
frq(bd2006_2008_52$nse) # NSE
frq(bd2006_2008_52$esc) # Escolaridad
frq(bd2006_2008_52$edad) # Edad
frq(bd2006_2008_52$sexo) # Sexo

## 2006_2008: CEP 54
frq(bd2006_2008_54$nse)
frq(bd2006_2008_54$esc)
frq(bd2006_2008_54$edad)
frq(bd2006_2008_54$sexo)

## 2006_2008: CEP 55
frq(bd2006_2008_55$nse)
frq(bd2006_2008_55$esc)
frq(bd2006_2008_55$edad)
frq(bd2006_2008_55$sexo)

## 2006_2008: CEP 56
frq(bd2006_2008_56$nse) # NSE
frq(bd2006_2008_56$esc) # Escolaridad
frq(bd2006_2008_56$edad) # Edad
frq(bd2006_2008_56$sexo) # Sexo

## 2006_2008: CEP 57
frq(bd2006_2008_57$nse)
frq(bd2006_2008_57$esc)
frq(bd2006_2008_57$edad)
frq(bd2006_2008_57$sexo)

## 2006_2008: CEP 58
frq(bd2006_2008_58$nse) # NSE
frq(bd2006_2008_58$esc) # Escolaridad
frq(bd2006_2008_58$edad) # Edad
frq(bd2006_2008_58$sexo) # Sexo


#---- 3.1.2 Recodificación ----
# Remover etiquetas
bd2006_2008_52 <- sjlabelled::remove_all_labels(bd2006_2008_52)
bd2006_2008_54 <- sjlabelled::remove_all_labels(bd2006_2008_54)

bd2006_2008_55 <- sjlabelled::remove_all_labels(bd2006_2008_55)
bd2006_2008_56 <- sjlabelled::remove_all_labels(bd2006_2008_56)

bd2006_2008_57 <- sjlabelled::remove_all_labels(bd2006_2008_57)
bd2006_2008_58 <- sjlabelled::remove_all_labels(bd2006_2008_58)

## 2006_2008: CEP 52
bd2006_2008_52$nse  <- car::recode(bd2006_2008_52$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2006_2008_52$esc <- car::recode(bd2006_2008_52$esc, "1 = '0-3'; 2 = '4-8'; 3 = '9-12'; 4 = '13 y mas'; 5 = 'NC'", as.factor = T)
bd2006_2008_52$edad <- car::recode(bd2006_2008_52$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2006_2008_52$sexo <- car::recode(bd2006_2008_52$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2006_2008: CEP 54
bd2006_2008_54$nse  <- car::recode(bd2006_2008_54$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2006_2008_54$esc <- car::recode(bd2006_2008_54$esc, "1 = '0-3'; 2 = '4-8'; 3 = '9-12'; 4 = '13 y mas'; 5 = 'NC'", as.factor = T)
bd2006_2008_54$edad <- car::recode(bd2006_2008_54$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2006_2008_54$sexo <- car::recode(bd2006_2008_54$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2006_2008: CEP 55
bd2006_2008_55$nse  <- car::recode(bd2006_2008_55$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2006_2008_55$esc <- car::recode(bd2006_2008_55$esc, "1 = '0-3'; 2 = '4-8'; 3 = '9-12'; 4 = '13 y mas'; 5 = 'NC'", as.factor = T)
bd2006_2008_55$edad <- car::recode(bd2006_2008_55$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2006_2008_55$sexo <- car::recode(bd2006_2008_55$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2006_2008: CEP 56
bd2006_2008_56$nse  <- car::recode(bd2006_2008_56$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2006_2008_56$esc <- car::recode(bd2006_2008_56$esc, "1 = '0-3'; 2 = '4-8'; 3 = '9-12'; 4 = '13 y mas'; 5 = 'NC'", as.factor = T)
bd2006_2008_56$edad <- car::recode(bd2006_2008_56$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2006_2008_56$sexo <- car::recode(bd2006_2008_56$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2006_2008: CEP 57
bd2006_2008_57$nse  <- car::recode(bd2006_2008_57$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2006_2008_57$esc <- car::recode(bd2006_2008_57$esc, "1 = '0-3'; 2 = '4-8'; 3 = '9-12'; 4 = '13 y mas'; 5 = 'NC'", as.factor = T)
bd2006_2008_57$edad <- car::recode(bd2006_2008_57$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2006_2008_57$sexo <- car::recode(bd2006_2008_57$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2006_2008: CEP 58
bd2006_2008_58$nse  <- car::recode(bd2006_2008_58$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2006_2008_58$esc <- car::recode(bd2006_2008_58$esc, "1 = '0-3'; 2 = '4-8'; 3 = '9-12'; 4 = '13 y mas'; 5 = 'NC'", as.factor = T)
bd2006_2008_58$edad <- car::recode(bd2006_2008_58$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2006_2008_58$sexo <- car::recode(bd2006_2008_58$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

#---- 3.2 Tratamiento de variables de confianza ----
#---- 3.2.1 Frecuencias ----

## 2006_2008: CEP 54

frq(bd2006_2008_54$conf_ffaa)
frq(bd2006_2008_54$conf_tribunalesjust)
frq(bd2006_2008_54$conf_partidos)

## 2006_2008: CEP 55
frq(bd2006_2008_55$conf_partido1) # No me llama la atencion el usar esto.
frq(bd2006_2008_55$conf_partido2)
frq(bd2006_2008_55$conf_partido3)
frq(bd2006_2008_55$conf_partido4)
frq(bd2006_2008_55$conf_partido5)
frq(bd2006_2008_55$conf_partido6)

## 2006_2008: CEP 57
frq(bd2006_2008_57$conf_congreso)
frq(bd2006_2008_57$conf_comercio)
frq(bd2006_2008_57$conf_iglesias)
frq(bd2006_2008_57$conf_sistjudicial)
frq(bd2006_2008_57$conf_sistemaedu)

## 2006_2008: CEP 58
frq(bd2006_2008_58$conf_iglesiacat) # Combinar:iglesia
frq(bd2006_2008_58$conf_ffaa)
frq(bd2006_2008_58$conf_iglesiaev) # Combinar:iglesia
frq(bd2006_2008_58$conf_partidos)
frq(bd2006_2008_58$conf_tribunalesjust)
frq(bd2006_2008_58$conf_diarios) # Combinar: MMC
frq(bd2006_2008_58$conf_tele) # Combinar: MMC
frq(bd2006_2008_58$conf_radios) # Combinar: MMC
frq(bd2006_2008_58$conf_sindicatos) # No usar
frq(bd2006_2008_58$conf_carabineros) # Combinar: instituciones del orden
frq(bd2006_2008_58$conf_gobierno)
frq(bd2006_2008_58$conf_congreso)
frq(bd2006_2008_58$conf_emppriv)

#---- 3.2.2 Recodificacion ----
## 2006_2008: CEP 54
bd2006_2008_54$conf_ffaa <- car::recode(bd2006_2008_54$conf_ffaa,"3 = 'Alta o media confianza'; c(1,2) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2006_2008_54$conf_tribunalesjust <- car::recode(bd2006_2008_54$conf_tribunalesjust,"3 = 'Alta o media confianza'; c(1,2) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2006_2008_54$conf_partidos <- car::recode(bd2006_2008_54$conf_partidos,"3 = 'Alta o media confianza'; c(1,2) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)

## 2006_2008: CEP 55
bd2006_2008_55$conf_partido1 <- car::recode(bd2006_2008_55$conf_partido1, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2006_2008_55$conf_partido2 <- car::recode(bd2006_2008_55$conf_partido2, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2006_2008_55$conf_partido3 <- car::recode(bd2006_2008_55$conf_partido3, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2006_2008_55$conf_partido4 <- car::recode(bd2006_2008_55$conf_partido4, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2006_2008_55$conf_partido5 <- car::recode(bd2006_2008_55$conf_partido5, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2006_2008_55$conf_partido6 <- car::recode(bd2006_2008_55$conf_partido6, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)

## 2006_2008: CEP 57
bd2006_2008_57$conf_congreso <- car::recode(bd2006_2008_57$conf_congreso, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)
bd2006_2008_57$conf_comercio <- car::recode(bd2006_2008_57$conf_comercio, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)
bd2006_2008_57$conf_iglesias <- car::recode(bd2006_2008_57$conf_iglesias, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)
bd2006_2008_57$conf_sistjudicial <- car::recode(bd2006_2008_57$conf_sistjudicial, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)
bd2006_2008_57$conf_sistemaedu <- car::recode(bd2006_2008_57$conf_sistemaedu, "1 = 'Mucha confianza'; c(2, 3, 4, 5) = 'Otra'; 8 = NA", as.factor = T)

## 2006_2008: CEP 58
bd2006_2008_58$conf_iglesiacat <- car::recode(bd2006_2008_58$conf_iglesiacat,"c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2006_2008_58$conf_ffaa <- car::recode(bd2006_2008_58$conf_ffaa,"c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2006_2008_58$conf_iglesiaev <- car::recode(bd2006_2008_58$conf_iglesiaev,"c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2006_2008_58$conf_partidos <- car::recode(bd2006_2008_58$conf_partidos, "c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2006_2008_58$conf_tribunalesjust <- car::recode(bd2006_2008_58$conf_tribunalesjust, "c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2006_2008_58$conf_diarios <- car::recode(bd2006_2008_58$conf_diarios, "c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2006_2008_58$conf_tele <- car::recode(bd2006_2008_58$conf_tele, "c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2006_2008_58$conf_radios <- car::recode(bd2006_2008_58$conf_radios, "c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2006_2008_58$conf_sindicatos <- car::recode(bd2006_2008_58$conf_sindicatos, "c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2006_2008_58$conf_carabineros <- car::recode(bd2006_2008_58$conf_carabineros, "c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2006_2008_58$conf_gobierno <- car::recode(bd2006_2008_58$conf_gobierno, "c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2006_2008_58$conf_congreso <- car::recode(bd2006_2008_58$conf_congreso, "c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2006_2008_58$conf_emppriv <- car::recode(bd2006_2008_58$conf_emppriv, "c(1, 2, 3) = 'Otra'; 4 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)

# No olvidar
# Codificación original
#1. Mucha confianza
#2. Bastante confianza
#3. No mucha confianza
#4. Ninguna confianza
#8. No sabe
#9. No contesta

#---- 3.2.3 Otros ajustes --
## 2006_2008: CEP 58
### Construccion variable iglesia en calidad de institucion

bd2006_2008_58$conf_iglesia[bd2006_2008_58$conf_iglesiacat == 'Mucha confianza' | bd2006_2008_58$conf_iglesiaev == 'Mucha confianza'] <- 'Mucha confianza'
bd2006_2008_58$conf_iglesia[bd2006_2008_58$conf_iglesiacat == 'Otra' & bd2006_2008_58$conf_iglesiaev == 'Otra'] <- 'Otra'

### Construccion variable MMC
bd2006_2008_58$conf_mmc[bd2006_2008_58$conf_diarios == 'Mucha confianza' & bd2006_2008_58$conf_radios == 'Mucha confianza'] <- 'Mucha confianza'
bd2006_2008_58$conf_mmc[bd2006_2008_58$conf_diarios == 'Mucha confianza' & bd2006_2008_58$conf_tele == 'Mucha confianza'] <- 'Mucha confianza'
bd2006_2008_58$conf_mmc[bd2006_2008_58$conf_tele == 'Mucha confianza' & bd2006_2008_58$conf_radios == 'Mucha confianza'] <- 'Mucha confianza'

bd2006_2008_58$conf_mmc[bd2006_2008_58$conf_diarios == 'Otra' & bd2006_2008_58$conf_radios == 'Otra'] <- 'Otra'
bd2006_2008_58$conf_mmc[bd2006_2008_58$conf_diarios == 'Otra' & bd2006_2008_58$conf_tele == 'Otra'] <- 'Otra'
bd2006_2008_58$conf_mmc[bd2006_2008_58$conf_tele == 'Otra' & bd2006_2008_58$conf_radios == 'Otra'] <- 'Otra'

bd2006_2008_58$conf_mmc[bd2006_2008_58$conf_tele == 'Mucha confianza' & bd2006_2008_58$conf_radios == 'Mucha confianza' & bd2006_2008_58$conf_diarios == 'Mucha confianza'] <- 'Mucha confianza'
bd2006_2008_58$conf_mmc[bd2006_2008_58$conf_tele == 'Otra' & bd2006_2008_58$conf_radios == 'Otra' & bd2006_2008_58$conf_diarios == 'Otra'] <- 'Otra'

# Ver frecuencia variable nueva
frq(bd2006_2008_58$conf_mmc)

### Sacar variables de confianza que no usaremos.
### 2006_2008-2008: CEP 58
bd2006_2008_58 <- select(bd2006_2008_58,-conf_iglesiacat, -conf_iglesiaev ,-conf_gobierno, -conf_radios, -conf_tele, -conf_sindicatos, -conf_carabineros, -conf_diarios) 

#---- 3.2.4 Guardar bases de confianza ----
save(bd2006_2008_58, file = "input/data/bd2006_2008_58.RData")

#---- 3.3 Tratamiento de variables de identificación partidaria e identificación política (o posición política)
#---- 3.3.1 Frecuencias ----

frq(bd2006_2008_52$id_part)
# No esta la bateria tipica de posicion politica

frq(bd2006_2008_54$id_part)
frq(bd2006_2008_54$pos_pol)

frq(bd2006_2008_55$id_part)
frq(bd2006_2008_55$pos_pol)

frq(bd2006_2008_56$id_part)
frq(bd2006_2008_56$pos_pol)

frq(bd2006_2008_57$id_part)
frq(bd2006_2008_57$pos_pol)

frq(bd2006_2008_58$id_part)
frq(bd2006_2008_58$pos_pol)

# ---- 3.3.2 Recodificacion ----

# 2006 - 2008: CEP 52
bd2006_2008_52$id_part <- car::recode(bd2006_2008_52$id_part "", as.factor = T) # Recodificar de acuerdo a lo que quiere avendano


# 2006 - 2008: CEP 54
bd2006_2008_54$id_part <- car::recode(bd2006_2008_54$id_part "", as.factor = T) # Recodificar de acuerdo a lo que quiere avendano
bd2006_2008_54$pos_pol <- car::recode(bd2006_2008_54$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = 'No sabe':
9 = 'No contesta'", as.factor = T)

# 2006 - 2008: CEP 55
bd2006_2008_55$id_part <- car::recode(bd2006_2008_55$id_part "", as.factor = T) # Recodificar de acuerdo a lo que quiere avendano
bd2006_2008_55$pos_pol <- car::recode(bd2006_2008_55$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = 'No sabe':
9 = 'No contesta'", as.factor = T)

# 2006 - 2008: CEP 56
bd2006_2008_56$id_part <- car::recode(bd2006_2008_56$id_part "", as.factor = T) # Recodificar de acuerdo a lo que quiere avendano
bd2006_2008_56$pos_pol <- car::recode(bd2006_2008_56$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = 'No sabe':
9 = 'No contesta'", as.factor = T)

# 2006 - 2008: CEP 57
bd2006_2008_57$id_part <- car::recode(bd2006_2008_57$id_part "", as.factor = T) # Recodificar de acuerdo a lo que quiere avendano
bd2006_2008_57$pos_pol <- car::recode(bd2006_2008_57$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = 'No sabe':
9 = 'No contesta'", as.factor = T)

# 2006 - 2008: CEP 58
bd2006_2008_58$id_part <- car::recode(bd2006_2008_58$id_part "", as.factor = T) # Recodificar de acuerdo a lo que quiere avendano
bd2006_2008_58$pos_pol <- car::recode(bd2006_2008_58$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = 'No sabe':
9 = 'No contesta'", as.factor = T)

# ---- 3.4 Guardar base de datos final ---- 
save(bd2006_2008_52, file = "input/data/bd2006_2008_52.RData")

save(bd2006_2008_54, file = "input/data/bd2006_2008_54.RData")
save(bd2006_2008_55, file = "input/data/bd2006_2008_55.RData")
save(bd2006_2008_56, file = "input/data/bd2006_2008_56.RData")
save(bd2006_2008_57, file = "input/data/bd2006_2008_57.RData")
save(bd2006_2008_58, file = "input/data/bd2006_2008_58.RData")

