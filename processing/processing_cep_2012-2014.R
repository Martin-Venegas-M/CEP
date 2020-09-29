# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 28 - 9 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car, haven)
# ---- Etapa 2: Cargar bases de datos ---
bd2012_2014_66 <- read_sav("input/data/2012-2014/cep66abril2012.sav")
bd2012_2014_67 <- read_sav("input/data/2012-2014/cep67julago2012.sav")
bd2012_2014_68 <- read_sav("input/data/2012-2014/cep68novdic2012.sav")
bd2012_2014_69 <- read_sav("input/data/2012-2014/cep69julago2013.sav")
bd2012_2014_70 <- read_sav("input/data/2012-2014/cep70sepoct2013.sav")
bd2012_2014_71 <- read_sav("input/data/2012-2014/cep71jul2014.sav")
bd2012_2014_72 <- read_sav("input/data/2012-2014/cep72nov2014.sav")

# ---- Etapa 3: Procesamiento de datos ----
## Seleccionar variables a utilizar

bd2012_2014_66<- select(bd2012_2014_66, DDP39, DDP03, DDP02, DDP01, TE1P03a:TE1P03m, MBP14, MBP16)
bd2012_2014_67<- select(bd2012_2014_67, DDP32, DDP03, DDP02, DDP01, MBP14, MBP16)
bd2012_2014_68<- select(bd2012_2014_68, DDP31, DDP03, DDP02, DDP01, MBP14, MBP16)
bd2012_2014_69<- select(bd2012_2014_69, DDP31, DDP03, DDP02, DDP01,TE1P03a:TE1P03m, MBP14, MBP16)

bd2012_2014_70<- select(bd2012_2014_70, ddp31, ddp03, ddp02, ddp01, mbp14, mbp11)
bd2012_2014_71<- select(bd2012_2014_71, ddp26, ddp03, ddp02, ddp01,te1p04a:te1p04r,mbp16, mbp18)
bd2012_2014_72<- select(bd2012_2014_72, DS_P47, DS_P3_ANOS, DS_P2_EXACTA, DS_P1, MB_P16, MB_P18)

## Renombrarlas

## 2012-2014: CEP 66
bd2012_2014_66 <- rename(bd2012_2014_66,
                    edad = DDP02,
                    id_part = MBP14,
                    pos_pol = MBP16,
                    nse = DDP39,
                    sexo = DDP01,
                    esc = DDP03,
                    conf_iglesiacat = ,
                    conf_ffaa = ,
                    conf_iglesiaev = ,
                    conf_partidos = ,
                    conf_tribunalesjust = ,
                    conf_diarios = ,
                    conf_tele = ,
                    conf_radios = ,
                    conf_sindicatos = ,
                    conf_carabineros = ,
                    conf_gobierno = ,
                    conf_congreso = ,
                    conf_emppriv = ,
                    conf_muni = ,
                    conf_fiscales = ,
                    conf_movestudiantil = ,
                    conf_pdi = ,
                    conf_emppub = ,
                    conf_colegioprof = )

## 2012-2014: CEP 67
bd2012_2014_67 <- rename(bd2012_2014_67,
                       edad = DDP02,
                       id_part = MBP14,
                       pos_pol = MBP16,
                       nse = DDP32,
                       sexo = DDP01,
                       esc = DDP03)

## 2012_2014: CEP 68
bd2012_2014_68 <- rename(bd2012_2014_68,
                       edad = DDP02,
                       id_part = MBP14,
                       pos_pol = MBP16,
                       nse = DDP31,
                       sexo = DDP01,
                       esc = DDP03)

## 2012_2014: CEP 69
bd2012_2014_69 <- rename(bd2012_2014_69,
                       edad = DDP02,
                       id_part = MBP14,
                       pos_pol = MBP16,
                       nse = DDP31,
                       sexo = DDP01,
                       esc = DDP03,
                       conf_iglesiacat = TE1P03a,
                       conf_ffaa = TE1P03b,
                       conf_iglesiaev = TE1P03c,
                       conf_partidos = TE1P03d,
                       conf_tribunalesjust = TE1P03e,
                       conf_diarios = TE1P03f,
                       conf_tele = TE1P03g,
                       conf_radios = TE1P03h,
                       conf_sindicatos = TE1P03i,
                       conf_carabineros = TE1P03j,
                       conf_gobierno = TE1P03k,
                       conf_congreso = TE1P03l,
                       conf_emppriv = TE1P03m)

## 2012_2014: CEP 70

bd2012_2014_70 <- rename(bd2012_2014_70,
                       edad = ddp02,
                       id_part = mbp14,
                       pos_pol = mbp11,
                       nse = ddp31,
                       sexo = ddp01,
                       esc = ddp03)

## 2012_2014: CEP 71

bd2012_2014_71 <- rename(bd2012_2014_71,
                       edad = ddp02,
                       id_part = mbp16,
                       pos_pol = mbp18,
                       nse = ddp26,
                       sexo = ddp01,
                       esc = ddp03,
                       conf_iglesiacat = te1p04a,
                       conf_ffaa = te1p04b,
                       conf_iglesiaev = te1p04c,
                       conf_partidos = te1p04d,
                       conf_tribunalesjust = te1p04e,
                       conf_diarios = te1p04f,
                       conf_tele = te1p04g,
                       conf_radios = te1p04h,
                       conf_sindicatos = te1p04i,
                       conf_carabineros = te1p04j,
                       conf_gobierno = te1p04k,
                       conf_congreso = te1p04l,
                       conf_emppriv = te1p04m)
## 2012_2014: CEP 72

bd2012_2014_72 <- rename(bd2012_2014_72,
                       edad = DS_P2_EXACTA,
                       id_part = MB_P16,
                       pos_pol = MB_P18,
                       nse = DS_P47,
                       sexo = DS_P1,
                       esc = DS_P3_ANOS)

#---- 3.1 Tratamiento de  sociodemográficas ----
#---- 3.1.1 Frecuencias ----
## 2012-2014: CEP 66
frq(bd2012_2014_66$nse) # NSE
frq(bd2012_2014_66$esc) # Escolaridad
frq(bd2012_2014_66$edad) # Edad
frq(bd2012_2014_66$sexo) # Sexo

## 2012-2014: CEP 67
frq(bd2012_2014_67$nse) # NSE
frq(bd2012_2014_67$esc) # Escolaridad
frq(bd2012_2014_67$edad) # Edad
frq(bd2012_2014_67$sexo) # Sexo

## 2012-2014: CEP 68
frq(bd2012_2014_68$nse) # NSE
frq(bd2012_2014_68$esc) # Escolaridad
frq(bd2012_2014_68$edad) # Edad
frq(bd2012_2014_68$sexo) # Sexo

## 2012-2014: CEP 69
frq(bd2012_2014_69$nse) # NSE
frq(bd2012_2014_69$esc) # Escolaridad
frq(bd2012_2014_69$edad) # Edad
frq(bd2012_2014_69$sexo) # Sexo

## 2012-2014: CEP 70
frq(bd2012_2014_70$nse) # NSE
frq(bd2012_2014_70$esc) # Escolaridad
frq(bd2012_2014_70$edad) # Edad
frq(bd2012_2014_70$sexo) # Sexo

## 2012-2014: CEP 71
frq(bd2012_2014_71$nse) # NSE
frq(bd2012_2014_71$esc) # Escolaridad
frq(bd2012_2014_71$edad) # Edad
frq(bd2012_2014_71$sexo) # Sexo

## 2012-2014: CEP 72
frq(bd2012_2014_72$nse) # NSE
frq(bd2012_2014_72$esc) # Escolaridad
frq(bd2012_2014_72$edad) # Edad
frq(bd2012_2014_72$sexo) # Sexo

#---- 3.1.2 Recodificación ----
# Remover etiquetas
bd2012_2014_66 <- sjlabelled::remove_all_labels(bd2012_2014_66)
bd2012_2014_67 <- sjlabelled::remove_all_labels(bd2012_2014_67)
bd2012_2014_68 <- sjlabelled::remove_all_labels(bd2012_2014_68)
bd2012_2014_69 <- sjlabelled::remove_all_labels(bd2012_2014_69)
bd2012_2014_70 <- sjlabelled::remove_all_labels(bd2012_2014_70)
bd2012_2014_71 <- sjlabelled::remove_all_labels(bd2012_2014_71)
bd2012_2014_72 <- sjlabelled::remove_all_labels(bd2012_2014_72)

## 2012-2014: CEP 66
bd2012_2014_66$nse  <- car::recode(bd2012_2014_66$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2012_2014_66$esc <- car::recode(bd2012_2014_66$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2012_2014_66$edad <- car::recode(bd2012_2014_66$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2012_2014_66$sexo <- car::recode(bd2012_2014_66$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)
## 2012-2014: CEP 67
bd2012_2014_67$nse  <- car::recode(bd2012_2014_67$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2012_2014_67$esc <- car::recode(bd2012_2014_67$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2012_2014_67$edad <- car::recode(bd2012_2014_67$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2012_2014_67$sexo <- car::recode(bd2012_2014_67$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)
## 2012-2014: CEP 68
bd2012_2014_68$nse  <- car::recode(bd2012_2014_68$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2012_2014_68$esc <- car::recode(bd2012_2014_68$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2012_2014_68$edad <- car::recode(bd2012_2014_68$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2012_2014_68$sexo <- car::recode(bd2012_2014_68$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)
## 2012-2014: CEP 69
bd2012_2014_69$nse  <- car::recode(bd2012_2014_69$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2012_2014_69$esc <- car::recode(bd2012_2014_69$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2012_2014_69$edad <- car::recode(bd2012_2014_69$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2012_2014_69$sexo <- car::recode(bd2012_2014_69$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)
## 2012-2014: CEP 70
bd2012_2014_70$nse  <- car::recode(bd2012_2014_70$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2012_2014_70$esc <- car::recode(bd2012_2014_70$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2012_2014_70$edad <- car::recode(bd2012_2014_70$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2012_2014_70$sexo <- car::recode(bd2012_2014_70$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)
## 2012-2014: CEP 71
bd2012_2014_71$nse  <- car::recode(bd2012_2014_71$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2012_2014_71$esc <- car::recode(bd2012_2014_71$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2012_2014_71$edad <- car::recode(bd2012_2014_71$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2012_2014_71$sexo <- car::recode(bd2012_2014_71$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)
## 2012-2014: CEP 72
bd2012_2014_72$nse  <- car::recode(bd2012_2014_72$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2012_2014_72$esc <- car::recode(bd2012_2014_72$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2012_2014_72$edad <- car::recode(bd2012_2014_72$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2012_2014_72$sexo <- car::recode(bd2012_2014_72$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

#---- 3.2 Tratamiento de variables de confianza ----
#---- 3.2.1 Frecuencias ----

## 2012-2014: CEP 66

## 2012-2014: CEP 69
frq(bd2012_2014_69$conf_iglesiacat) # Combinar:iglesia
frq(bd2012_2014_69$conf_ffaa)
frq(bd2012_2014_69$conf_iglesiaev) # Combinar:iglesia
frq(bd2012_2014_69$conf_partidos)
frq(bd2012_2014_69$conf_tribunalesjust)
frq(bd2012_2014_69$conf_diarios) # Combinar: MMC
frq(bd2012_2014_69$conf_tele) # Combinar: MMC
frq(bd2012_2014_69$conf_radios) # Combinar: MMC
frq(bd2012_2014_69$conf_sindicatos) # No usar
frq(bd2012_2014_69$conf_carabineros) # Combinar: instituciones del orden
frq(bd2012_2014_69$conf_gobierno)
frq(bd2012_2014_69$conf_congreso)
frq(bd2012_2014_69$conf_emppriv)
## 2012-2014: CEP 71
frq(bd2012_2014_71$conf_iglesiacat) # Combinar:iglesia
frq(bd2012_2014_71$conf_ffaa)
frq(bd2012_2014_71$conf_iglesiaev) # Combinar:iglesia
frq(bd2012_2014_71$conf_partidos)
frq(bd2012_2014_71$conf_tribunalesjust)
frq(bd2012_2014_71$conf_diarios) # Combinar: MMC
frq(bd2012_2014_71$conf_tele) # Combinar: MMC
frq(bd2012_2014_71$conf_radios) # Combinar: MMC
frq(bd2012_2014_71$conf_sindicatos) # No usar
frq(bd2012_2014_71$conf_carabineros) # Combinar: instituciones del orden
frq(bd2012_2014_71$conf_gobierno)
frq(bd2012_2014_71$conf_congreso)
frq(bd2012_2014_71$conf_emppriv)

#---- 3.2.2 Recodificacion ----
## 2012-2014: CEP 66

## 2012-2014: CEP 69
bd2012_2014_69$conf_iglesiacat <- car::recode(bd2012_2014_69$conf_iglesiacat,"c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_69$conf_ffaa <- car::recode(bd2012_2014_69$conf_ffaa,"c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_69$conf_iglesiaev <- car::recode(bd2012_2014_69$conf_iglesiaev,"c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_69$conf_partidos <- car::recode(bd2012_2014_69$conf_partidos, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_69$conf_tribunalesjust <- car::recode(bd2012_2014_69$conf_tribunalesjust, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_69$conf_diarios <- car::recode(bd2012_2014_69$conf_diarios, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_69$conf_tele <- car::recode(bd2012_2014_69$conf_tele, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_69$conf_radios <- car::recode(bd2012_2014_69$conf_radios, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_69$conf_sindicatos <- car::recode(bd2012_2014_69$conf_sindicatos, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_69$conf_carabineros <- car::recode(bd2012_2014_69$conf_carabineros, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_69$conf_gobierno <- car::recode(bd2012_2014_69$conf_gobierno, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
## 2012-2014: CEP 71

bd2012_2014_71$conf_iglesiacat <- car::recode(bd2012_2014_71$conf_iglesiacat,"c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_71$conf_ffaa <- car::recode(bd2012_2014_71$conf_ffaa,"c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_71$conf_iglesiaev <- car::recode(bd2012_2014_71$conf_iglesiaev,"c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_71$conf_partidos <- car::recode(bd2012_2014_71$conf_partidos, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_71$conf_tribunalesjust <- car::recode(bd2012_2014_71$conf_tribunalesjust, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_71$conf_diarios <- car::recode(bd2012_2014_71$conf_diarios, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_71$conf_tele <- car::recode(bd2012_2014_71$conf_tele, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_71$conf_radios <- car::recode(bd2012_2014_71$conf_radios, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_71$conf_sindicatos <- car::recode(bd2012_2014_71$conf_sindicatos, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_71$conf_carabineros <- car::recode(bd2012_2014_71$conf_carabineros, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2012_2014_71$conf_gobierno <- car::recode(bd2012_2014_71$conf_gobierno, "c(1,2) = 'Alta o media confianza'; c(3,4) = 'Baja o nula confianza'; c(8,9) = 'NS/NC'", as.factor = T)

#---- 3.2.3 Otros ajustes ----
## 2012-2014: CEP 66

## 2012-2014: CEP 69

### Construccion variable iglesia en calidad de institucion

bd2012_2014_69$conf_iglesia[bd2012_2014_69$conf_iglesiacat == 'Alta o media confianza' | bd2012_2014_69$conf_iglesiaev == 'Alta o media confianza'] <- 'Alta o media confianza'
bd2012_2014_69$conf_iglesia[bd2012_2014_69$conf_iglesiacat == 'Baja o nula confianza' | bd2012_2014_69$conf_iglesiaev == 'Baja o nula confianza'] <- 'Baja o nula confianza'

### Construccion variable MMC
bd2012_2014_69$conf_mmc[bd2012_2014_69$conf_diarios == 'Alta o media confianza' & bd2012_2014_69$conf_radios == 'Alta o media confianza'] <- 'Alta o media confianza'
bd2012_2014_69$conf_mmc[bd2012_2014_69$conf_diarios == 'Alta o media confianza' & bd2012_2014_69$conf_tele == 'Alta o media confianza'] <- 'Alta o media confianza'
bd2012_2014_69$conf_mmc[bd2012_2014_69$conf_tele == 'Alta o media confianza' & bd2012_2014_69$conf_radios == 'Alta o media confianza'] <- 'Alta o media confianza'

bd2012_2014_69$conf_mmc[bd2012_2014_69$conf_diarios == 'Baja o nula confianza' & bd2012_2014_69$conf_radios == 'Baja o nula confianza'] <- 'Baja o nula confianza'
bd2012_2014_69$conf_mmc[bd2012_2014_69$conf_diarios == 'Baja o nula confianza' & bd2012_2014_69$conf_tele == 'Baja o nula confianza'] <- 'Baja o nula confianza'
bd2012_2014_69$conf_mmc[bd2012_2014_69$conf_tele == 'Baja o nula confianza' & bd2012_2014_69$conf_radios == 'Baja o nula confianza'] <- 'Baja o nula confianza'

bd2012_2014_69$conf_mmc[bd2012_2014_69$conf_tele == 'Alta o media confianza' & bd2012_2014_69$conf_radios == 'Alta o media confianza' & bd2012_2014_69$conf_diarios == 'Alta o media confianza'] <- 'Alta o media confianza'
bd2012_2014_69$conf_mmc[bd2012_2014_69$conf_tele == 'Baja o nula confianza' & bd2012_2014_69$conf_radios == 'Baja o nula confianza' & bd2012_2014_69$conf_diarios == 'Baja o nula confianza'] <- 'Baja o nula confianza'

# Ver frecuencia variable nueva
frq(bd2012_2014_69$conf_mmc)




## 2012-2014: CEP 71

### Construccion variable iglesia en calidad de institucion

bd2012_2014_71$conf_iglesia[bd2012_2014_71$conf_iglesiacat == 'Alta o media confianza' | bd2012_2014_71$conf_iglesiaev == 'Alta o media confianza'] <- 'Alta o media confianza'
bd2012_2014_71$conf_iglesia[bd2012_2014_71$conf_iglesiacat == 'Baja o nula confianza' | bd2012_2014_71$conf_iglesiaev == 'Baja o nula confianza'] <- 'Baja o nula confianza'

### Construccion variable MMC
bd2012_2014_71$conf_mmc[bd2012_2014_71$conf_diarios == 'Alta o media confianza' & bd2012_2014_71$conf_radios == 'Alta o media confianza'] <- 'Alta o media confianza'
bd2012_2014_71$conf_mmc[bd2012_2014_71$conf_diarios == 'Alta o media confianza' & bd2012_2014_71$conf_tele == 'Alta o media confianza'] <- 'Alta o media confianza'
bd2012_2014_71$conf_mmc[bd2012_2014_71$conf_tele == 'Alta o media confianza' & bd2012_2014_71$conf_radios == 'Alta o media confianza'] <- 'Alta o media confianza'

bd2012_2014_71$conf_mmc[bd2012_2014_71$conf_diarios == 'Baja o nula confianza' & bd2012_2014_71$conf_radios == 'Baja o nula confianza'] <- 'Baja o nula confianza'
bd2012_2014_71$conf_mmc[bd2012_2014_71$conf_diarios == 'Baja o nula confianza' & bd2012_2014_71$conf_tele == 'Baja o nula confianza'] <- 'Baja o nula confianza'
bd2012_2014_71$conf_mmc[bd2012_2014_71$conf_tele == 'Baja o nula confianza' & bd2012_2014_71$conf_radios == 'Baja o nula confianza'] <- 'Baja o nula confianza'

bd2012_2014_71$conf_mmc[bd2012_2014_71$conf_tele == 'Alta o media confianza' & bd2012_2014_71$conf_radios == 'Alta o media confianza' & bd2012_2014_71$conf_diarios == 'Alta o media confianza'] <- 'Alta o media confianza'
bd2012_2014_71$conf_mmc[bd2012_2014_71$conf_tele == 'Baja o nula confianza' & bd2012_2014_71$conf_radios == 'Baja o nula confianza' & bd2012_2014_71$conf_diarios == 'Baja o nula confianza'] <- 'Baja o nula confianza'

# Ver frecuencia variable nueva
frq(bd2012_2014_71$conf_mmc)
