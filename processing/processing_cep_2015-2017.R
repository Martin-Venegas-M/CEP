# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 25 - 9 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car, haven)
# ---- Etapa 2: Cargar bases de datos ---
bd2015_2017_73 <- read_sav("input/data/2015-2017/cep73abr2015.sav")
bd2015_2017_74 <- read_sav("input/data/2015-2017/cep74ago2015.sav")
bd2015_2017_75 <- read_sav("input/data/2015-2017/cep75nov2015.sav")

bd2015_2017_76 <- read_sav("input/data/2015-2017/cep76map2016.sav")
bd2015_2017_77 <- read_sav("input/data/2015-2017/cep77julago2016.sav")
bd2015_2017_78 <- read_sav("input/data/2015-2017/cep78novdic2016.sav")


bd2015_2017_79 <- read_sav("input/data/2015-2017/cep79abrmay2017.sav")
bd2015_2017_80 <- read_sav("input/data/2015-2017/cep80julago2017.sav")
bd2015_2017_81 <- read_sav("input/data/2015-2017/cep81sepoct2017.sav")


# ---- Etapa 3: Procesamiento de datos ----
## Seleccionar variables a utilizar
bd2015_2017_73 <- select(bd2015_2017_73, DS_P41, DS_P3, DS_P2_EXACTA, DS_P1, MB_P16, MB_P18)
bd2015_2017_74 <- select(bd2015_2017_74, DS_P41, DS_P3, DS_P2_EXACTA, DS_P1, CISP_10A:CISP_10S, MB_P20, MB_P22)
bd2015_2017_75 <- select(bd2015_2017_75, DS_P41, DS_P3, DS_P2_EXACTA, DS_P1, MB_P16, MB_P18)

bd2015_2017_76 <- select(bd2015_2017_76, SDT25, SDT4_ANOS, SDT2_EXACTA, SDT1, T10A:T10J)
bd2015_2017_77 <- select(bd2015_2017_77, DS_P41, DS_P3, DS_P2_EXACTA, DS_P1, MB_P14, MB_P15)
bd2015_2017_78 <- select(bd2015_2017_78, DS_P25, DS_P3, DS_P2_EXACTA, DS_P1, SAT_9A:SAT_9R, MB_P12, MB_P13)

bd2015_2017_79 <- select(bd2015_2017_79, DS_P26, DS_P3, DS_P2_EXACTA, DS_P1, ACTI_5_1:ACTI_5_13) # MB_P8: Posicion politica en formato de escala (no se usa)
bd2015_2017_80 <- select(bd2015_2017_80, DS_P20, DS_P3, DS_P2_EXACTA, DS_P1) # MB_P9: Posicion politica en formato escala (no se usa)
bd2015_2017_81 <- select(bd2015_2017_81, DS_P19, DS_P3, DS_P2_EXACTA, DS_P1, MB_P12, MB_P14)

## Renombrarlas
### 2015_2017: CEP 73
bd2015_2017_73 <- rename(bd2015_2017_73,
                    edad = DS_P2_EXACTA,
                    id_part = MB_P16,
                    pos_pol = MB_P18,
                    nse = DS_P41,
                    sexo = DS_P1,
                    esc = DS_P3)

### 2015_2017: CEP 74
bd2015_2017_74 <- rename(bd2015_2017_74,
                    edad = DS_P2_EXACTA,
                    id_part = MB_P20,
                    pos_pol = MB_P22,
                    nse = DS_P41,
                    sexo = DS_P1,
                    esc = DS_P3,
                    conf_iglesiacat = CISP_10A,
                    conf_ffaa = CISP_10B,
                    conf_iglesiaev = CISP_10C,
                    conf_partidos = CISP_10D,
                    conf_tribunalesjust = CISP_10E,
                    conf_diarios = CISP_10F,
                    conf_tele = CISP_10G,
                    conf_radios = CISP_10H,
                    conf_sindicatos = CISP_10I,
                    conf_carabineros = CISP_10J,
                    conf_gobierno = CISP_10K,
                    conf_congreso = CISP_10L,
                    conf_emppriv = CISP_10M)
                    

### 2015_2017: CEP 75
bd2015_2017_75 <- rename(bd2015_2017_75,
                    edad = DS_P2_EXACTA,
                    id_part = MB_P16,
                    pos_pol = MB_P18,
                    nse = DS_P41,
                    sexo = DS_P1,
                    esc = DS_P3)

### 2015_2017: CEP 76
bd2015_2017_76 <- rename(bd2015_2017_76,
                    edad = SDT2_EXACTA,
                    nse = SDT25,
                    sexo = SDT1,
                    esc = SDT4_ANOS,
                    conf_ffaa = T10A,
                    conf_partidos = T10B,
                    conf_tribunalesjust = T10C,
                    conf_carabineros = T10D,
                    conf_gobierno = T10E,
                    conf_congreso = T10F,
                    conf_muni = T10G,
                    conf_fiscales = T10H,
                    conf_pdi = T10I,
                    conf_grandemp = T10J)

### 2015_2017: CEP 77
bd2015_2017_77 <- rename(bd2015_2017_77,
                    edad = DS_P2_EXACTA,
                    id_part = MB_P14,
                    pos_pol = MB_P15,
                    nse = DS_P41,
                    sexo = DS_P1,
                    esc = DS_P3)

### 2015_2017: CEP 78
bd2015_2017_78 <- rename(bd2015_2017_78,
                    edad = DS_P2_EXACTA,
                    id_part = MB_P12,
                    pos_pol = MB_P13,
                    nse = DS_P25,
                    sexo = DS_P1,
                    esc = DS_P3,
                    conf_iglesiacat = SAT_9A,
                    conf_ffaa = SAT_9B,
                    conf_iglesiaev = SAT_9C,
                    conf_partidos = SAT_9D,
                    conf_tribunalesjust = SAT_9E,
                    conf_diarios = SAT_9F,
                    conf_tele = SAT_9G,
                    conf_radios = SAT_9H,
                    conf_sindicatos = SAT_9I,
                    conf_carabineros = SAT_9J,
                    conf_gobierno = SAT_9K,
                    conf_congreso = SAT_9L,
                    conf_emppriv = SAT_9M)

### 2015_2017: CEP 79
bd2015_2017_79 <- rename(bd2015_2017_79,
                    edad = DS_P2_EXACTA,
                    nse = DS_P26,
                    sexo = DS_P1,
                    esc = DS_P3,
                    conf_iglesiacat = ACTI_5_1,
                    conf_ffaa = ACTI_5_2,
                    conf_iglesiaev = ACTI_5_3,
                    conf_partidos = ACTI_5_4,
                    conf_tribunalesjust = ACTI_5_5,
                    conf_diarios = ACTI_5_6,
                    conf_tele = ACTI_5_7,
                    conf_radios = ACTI_5_8,
                    conf_sindicatos = ACTI_5_9,
                    conf_carabineros = ACTI_5_10,
                    conf_gobierno = ACTI_5_11,
                    conf_congreso = ACTI_5_12,
                    conf_emppriv = ACTI_5_13)

### 2015_2017: CEP 80
bd2015_2017_80 <- rename(bd2015_2017_80,
                    edad = DS_P2_EXACTA,
                    nse = DS_P20,
                    sexo = DS_P1,
                    esc = DS_P3)

### 2015_2017: CEP 81
bd2015_2017_81 <- rename(bd2015_2017_81,
                    edad = DS_P2_EXACTA,
                    id_part = MB_P12,
                    pos_pol = MB_P14,
                    nse = DS_P19,
                    sexo = DS_P1,
                    esc = DS_P3)

#---- 3.1 Tratamiento de  sociodemográficas ----
#---- 3.1.1 Frecuencias ----
## 2015_2017: CEP 73
frq(bd2015_2017_73$nse) # NSE
frq(bd2015_2017_73$esc) # Escolaridad
frq(bd2015_2017_73$edad) # Edad
frq(bd2015_2017_73$sexo) # Sexo

## 2015_2017: CEP 74
frq(bd2015_2017_74$nse)
frq(bd2015_2017_74$esc)
frq(bd2015_2017_74$edad)
frq(bd2015_2017_74$sexo)

## 2015_2017: CEP 75
frq(bd2015_2017_75$nse)
frq(bd2015_2017_75$esc)
frq(bd2015_2017_75$edad)
frq(bd2015_2017_75$sexo)

## 2015_2017: CEP 76
frq(bd2015_2017_76$nse) # NSE
frq(bd2015_2017_76$esc) # Escolaridad
frq(bd2015_2017_76$edad) # Edad
frq(bd2015_2017_76$sexo) # Sexo

## 2015_2017: CEP 77
frq(bd2015_2017_77$nse)
frq(bd2015_2017_77$esc)
frq(bd2015_2017_77$edad)
frq(bd2015_2017_77$sexo)

## 2015_2017: CEP 78
frq(bd2015_2017_78$nse)
frq(bd2015_2017_78$esc)
frq(bd2015_2017_78$edad)
frq(bd2015_2017_78$sexo)

## 2015_2017: CEP 79
frq(bd2015_2017_79$nse) # NSE
frq(bd2015_2017_79$esc) # Escolaridad
frq(bd2015_2017_79$edad) # Edad
frq(bd2015_2017_79$sexo) # Sexo

## 2015_2017: CEP 80
frq(bd2015_2017_80$nse)
frq(bd2015_2017_80$esc)
frq(bd2015_2017_80$edad)
frq(bd2015_2017_80$sexo)

## 2015_2017: CEP 81
frq(bd2015_2017_81$nse)
frq(bd2015_2017_81$esc)
frq(bd2015_2017_81$edad)
frq(bd2015_2017_81$sexo)

#---- 3.1.2 Recodificación ----
# Remover etiquetas
bd2015_2017_73 <- sjlabelled::remove_all_labels(bd2015_2017_73)
bd2015_2017_74 <- sjlabelled::remove_all_labels(bd2015_2017_74)
bd2015_2017_75 <- sjlabelled::remove_all_labels(bd2015_2017_75)

bd2015_2017_76 <- sjlabelled::remove_all_labels(bd2015_2017_76)
bd2015_2017_77 <- sjlabelled::remove_all_labels(bd2015_2017_77)
bd2015_2017_78 <- sjlabelled::remove_all_labels(bd2015_2017_78)

bd2015_2017_79 <- sjlabelled::remove_all_labels(bd2015_2017_79)
bd2015_2017_80 <- sjlabelled::remove_all_labels(bd2015_2017_80)
bd2015_2017_81 <- sjlabelled::remove_all_labels(bd2015_2017_81)

## 2015_2017: CEP 73
bd2015_2017_73$nse  <- car::recode(bd2015_2017_73$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2015_2017_73$esc <- car::recode(bd2015_2017_73$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2015_2017_73$edad <- car::recode(bd2015_2017_73$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2015_2017_73$sexo <- car::recode(bd2015_2017_73$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2015_2017: CEP 74
bd2015_2017_74$nse  <- car::recode(bd2015_2017_74$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2015_2017_74$esc <- car::recode(bd2015_2017_74$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2015_2017_74$edad <- car::recode(bd2015_2017_74$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2015_2017_74$sexo <- car::recode(bd2015_2017_74$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2015_2017: CEP 75
bd2015_2017_75$nse  <- car::recode(bd2015_2017_75$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2015_2017_75$esc <- car::recode(bd2015_2017_75$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2015_2017_75$edad <- car::recode(bd2015_2017_75$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2015_2017_75$sexo <- car::recode(bd2015_2017_75$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2015_2017: CEP 76
bd2015_2017_76$nse  <- car::recode(bd2015_2017_76$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2015_2017_76$esc <- car::recode(bd2015_2017_76$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2015_2017_76$edad <- car::recode(bd2015_2017_76$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2015_2017_76$sexo <- car::recode(bd2015_2017_76$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2015_2017: CEP 77
bd2015_2017_77$nse  <- car::recode(bd2015_2017_77$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2015_2017_77$esc <- car::recode(bd2015_2017_77$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2015_2017_77$edad <- car::recode(bd2015_2017_77$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2015_2017_77$sexo <- car::recode(bd2015_2017_77$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2015_2017: CEP 78
bd2015_2017_78$nse  <- car::recode(bd2015_2017_78$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2015_2017_78$esc <- car::recode(bd2015_2017_78$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2015_2017_78$edad <- car::recode(bd2015_2017_78$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2015_2017_78$sexo <- car::recode(bd2015_2017_78$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2015_2017: CEP 79
bd2015_2017_79$nse  <- car::recode(bd2015_2017_79$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2015_2017_79$esc <- car::recode(bd2015_2017_79$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2015_2017_79$edad <- car::recode(bd2015_2017_79$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2015_2017_79$sexo <- car::recode(bd2015_2017_79$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2015_2017: CEP 80
bd2015_2017_80$nse  <- car::recode(bd2015_2017_80$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2015_2017_80$esc <- car::recode(bd2015_2017_80$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2015_2017_80$edad <- car::recode(bd2015_2017_80$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2015_2017_80$sexo <- car::recode(bd2015_2017_80$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2015_2017: CEP 81
bd2015_2017_81$nse  <- car::recode(bd2015_2017_81$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2015_2017_81$esc <- car::recode(bd2015_2017_81$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2015_2017_81$edad <- car::recode(bd2015_2017_81$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2015_2017_81$sexo <- car::recode(bd2015_2017_81$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

#---- 3.2 Tratamiento de variables de confianza ----
#---- 3.2.1 Frecuencias ----
## 2015_2017: CEP 74
frq(bd2015_2017_74$conf_iglesiacat) # Combinar:iglesia
frq(bd2015_2017_74$conf_ffaa)
frq(bd2015_2017_74$conf_iglesiaev) # Combinar:iglesia
frq(bd2015_2017_74$conf_partidos)
frq(bd2015_2017_74$conf_tribunalesjust)
frq(bd2015_2017_74$conf_diarios) # Combinar: MMC
frq(bd2015_2017_74$conf_tele) # Combinar: MMC
frq(bd2015_2017_74$conf_radios) # Combinar: MMC
frq(bd2015_2017_74$conf_sindicatos) # No usar
frq(bd2015_2017_74$conf_carabineros) # Combinar: instituciones del orden
frq(bd2015_2017_74$conf_gobierno)
frq(bd2015_2017_74$conf_congreso)
frq(bd2015_2017_74$conf_emppriv)

## 2015_2017: CEP 76
frq(bd2015_2017_76$conf_ffaa)
frq(bd2015_2017_76$conf_partidos)
frq(bd2015_2017_76$conf_tribunalesjust)
frq(bd2015_2017_76$conf_carabineros)
frq(bd2015_2017_76$conf_gobierno)
frq(bd2015_2017_76$conf_congreso)
frq(bd2015_2017_76$conf_muni)
frq(bd2015_2017_76$conf_fiscales)
frq(bd2015_2017_76$conf_pdi)
frq(bd2015_2017_76$conf_grandemp)

## 2015_2017: CEP 78
frq(bd2015_2017_78$conf_iglesiacat)
frq(bd2015_2017_78$conf_ffaa)
frq(bd2015_2017_78$conf_iglesiaev)
frq(bd2015_2017_78$conf_partidos)
frq(bd2015_2017_78$conf_tribunalesjust)
frq(bd2015_2017_78$conf_diarios)
frq(bd2015_2017_78$conf_tele)
frq(bd2015_2017_78$conf_radios)
frq(bd2015_2017_78$conf_sindicatos)
frq(bd2015_2017_78$conf_carabineros)
frq(bd2015_2017_78$conf_gobierno)
frq(bd2015_2017_78$conf_congreso)
frq(bd2015_2017_78$conf_emppriv)

## 2015_2017: CEP 79
frq(bd2015_2017_79$conf_iglesiacat)
frq(bd2015_2017_79$conf_ffaa)
frq(bd2015_2017_79$conf_iglesiaev)
frq(bd2015_2017_79$conf_partidos)
frq(bd2015_2017_79$conf_tribunalesjust)
frq(bd2015_2017_79$conf_diarios)
frq(bd2015_2017_79$conf_tele)
frq(bd2015_2017_79$conf_radios)
frq(bd2015_2017_79$conf_sindicatos)
frq(bd2015_2017_79$conf_carabineros)
frq(bd2015_2017_79$conf_gobierno)
frq(bd2015_2017_79$conf_congreso)
frq(bd2015_2017_79$conf_emppriv)

#---- 3.2.2 Recodificacion ----

## 2015_2017: CEP 74

bd2015_2017_74$conf_iglesiacat <- car::recode(bd2015_2017_74$conf_iglesiacat,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_74$conf_ffaa <- car::recode(bd2015_2017_74$conf_ffaa,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_74$conf_iglesiaev <- car::recode(bd2015_2017_74$conf_iglesiaev,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_74$conf_partidos <- car::recode(bd2015_2017_74$conf_partidos, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_74$conf_tribunalesjust <- car::recode(bd2015_2017_74$conf_tribunalesjust, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_74$conf_diarios <- car::recode(bd2015_2017_74$conf_diarios, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_74$conf_tele <- car::recode(bd2015_2017_74$conf_tele, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_74$conf_radios <- car::recode(bd2015_2017_74$conf_radios, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_74$conf_sindicatos <- car::recode(bd2015_2017_74$conf_sindicatos, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_74$conf_carabineros <- car::recode(bd2015_2017_74$conf_carabineros, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_74$conf_gobierno <- car::recode(bd2015_2017_74$conf_gobierno, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_74$conf_congreso <- car::recode(bd2015_2017_74$conf_congreso, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_74$conf_emppriv <- car::recode(bd2015_2017_74$conf_emppriv, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)

## 2015_2017: CEP 76

bd2015_2017_76$conf_ffaa <- car::recode(bd2015_2017_76$conf_ffaa,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_76$conf_partidos <- car::recode(bd2015_2017_76$conf_partidos, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_76$conf_tribunalesjust <- car::recode(bd2015_2017_76$conf_tribunalesjust, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_76$conf_carabineros <- car::recode(bd2015_2017_76$conf_carabineros, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_76$conf_gobierno <- car::recode(bd2015_2017_76$conf_gobierno, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_76$conf_congreso <- car::recode(bd2015_2017_76$conf_congreso, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_76$conf_muni <- car::recode(bd2015_2017_76$conf_muni, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_76$conf_fiscales <- car::recode(bd2015_2017_76$conf_fiscales, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_76$conf_pdi <- car::recode(bd2015_2017_76$conf_pdi, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_76$conf_grandemp <- car::recode(bd2015_2017_76$conf_grandemp, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)

## 2015_2017: CEP 78

bd2015_2017_78$conf_iglesiacat <- car::recode(bd2015_2017_78$conf_iglesiacat,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_78$conf_ffaa <- car::recode(bd2015_2017_78$conf_ffaa,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_78$conf_iglesiaev <- car::recode(bd2015_2017_78$conf_iglesiaev,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_78$conf_partidos <- car::recode(bd2015_2017_78$conf_partidos, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_78$conf_tribunalesjust <- car::recode(bd2015_2017_78$conf_tribunalesjust, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_78$conf_diarios <- car::recode(bd2015_2017_78$conf_diarios, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_78$conf_tele <- car::recode(bd2015_2017_78$conf_tele, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_78$conf_radios <- car::recode(bd2015_2017_78$conf_radios, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_78$conf_sindicatos <- car::recode(bd2015_2017_78$conf_sindicatos, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_78$conf_carabineros <- car::recode(bd2015_2017_78$conf_carabineros, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_78$conf_gobierno <- car::recode(bd2015_2017_78$conf_gobierno, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_78$conf_congreso <- car::recode(bd2015_2017_78$conf_congreso, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_78$conf_emppriv <- car::recode(bd2015_2017_78$conf_emppriv, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)

## 2015_2017: CEP 79

bd2015_2017_79$conf_iglesiacat <- car::recode(bd2015_2017_79$conf_iglesiacat,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_79$conf_ffaa <- car::recode(bd2015_2017_79$conf_ffaa,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_79$conf_iglesiaev <- car::recode(bd2015_2017_79$conf_iglesiaev,"c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_79$conf_partidos <- car::recode(bd2015_2017_79$conf_partidos, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_79$conf_tribunalesjust <- car::recode(bd2015_2017_79$conf_tribunalesjust, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_79$conf_diarios <- car::recode(bd2015_2017_79$conf_diarios, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_79$conf_tele <- car::recode(bd2015_2017_79$conf_tele, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_79$conf_radios <- car::recode(bd2015_2017_79$conf_radios, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_79$conf_sindicatos <- car::recode(bd2015_2017_79$conf_sindicatos, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_79$conf_carabineros <- car::recode(bd2015_2017_79$conf_carabineros, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_79$conf_gobierno <- car::recode(bd2015_2017_79$conf_gobierno, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_79$conf_congreso <- car::recode(bd2015_2017_79$conf_congreso, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)
bd2015_2017_79$conf_emppriv <- car::recode(bd2015_2017_79$conf_emppriv, "c(2, 3, 4) = 'Otra'; 1 = 'Mucha confianza'; c(8, 9) = NA", as.factor = T)

# No olvidar
# Codificación original
#1. Mucha confianza
#2. Bastante confianza
#3. No mucha confianza
#4. Ninguna confianza
#8. No sabe
#9. No contesta

#---- 3.2.3 Otros ajustes --
## 2015_2017: CEP 74
### Construccion variable iglesia en calidad de institucion

bd2015_2017_74$conf_iglesia[bd2015_2017_74$conf_iglesiacat == 'Mucha confianza' | bd2015_2017_74$conf_iglesiaev == 'Mucha confianza'] <- 'Mucha confianza'
bd2015_2017_74$conf_iglesia[bd2015_2017_74$conf_iglesiacat == 'Otra' & bd2015_2017_74$conf_iglesiaev == 'Otra'] <- 'Otra'

### Construccion variable MMC
bd2015_2017_74$conf_mmc[bd2015_2017_74$conf_diarios == 'Mucha confianza' & bd2015_2017_74$conf_radios == 'Mucha confianza'] <- 'Mucha confianza'
bd2015_2017_74$conf_mmc[bd2015_2017_74$conf_diarios == 'Mucha confianza' & bd2015_2017_74$conf_tele == 'Mucha confianza'] <- 'Mucha confianza'
bd2015_2017_74$conf_mmc[bd2015_2017_74$conf_tele == 'Mucha confianza' & bd2015_2017_74$conf_radios == 'Mucha confianza'] <- 'Mucha confianza'

bd2015_2017_74$conf_mmc[bd2015_2017_74$conf_diarios == 'Otra' & bd2015_2017_74$conf_radios == 'Otra'] <- 'Otra'
bd2015_2017_74$conf_mmc[bd2015_2017_74$conf_diarios == 'Otra' & bd2015_2017_74$conf_tele == 'Otra'] <- 'Otra'
bd2015_2017_74$conf_mmc[bd2015_2017_74$conf_tele == 'Otra' & bd2015_2017_74$conf_radios == 'Otra'] <- 'Otra'

bd2015_2017_74$conf_mmc[bd2015_2017_74$conf_tele == 'Mucha confianza' & bd2015_2017_74$conf_radios == 'Mucha confianza' & bd2015_2017_74$conf_diarios == 'Mucha confianza'] <- 'Mucha confianza'
bd2015_2017_74$conf_mmc[bd2015_2017_74$conf_tele == 'Otra' & bd2015_2017_74$conf_radios == 'Otra' & bd2015_2017_74$conf_diarios == 'Otra'] <- 'Otra'

# Ver frecuencia variable nueva
frq(bd2015_2017_74$conf_mmc)

# Eliminar variables
bd2015_2017_74 <- select(bd2015_2017_74, -conf_iglesiaev, -conf_iglesiacat, -conf_radios, -conf_tele, -conf_diarios, -conf_carabineros, -conf_sindicatos)

## 2015_2017: CEP 76
# No tiene variables de iglesia

## 2015_2017: CEP 78
### Construccion variable iglesia en calidad de institucion

bd2015_2017_78$conf_iglesia[bd2015_2017_78$conf_iglesiacat == 'Mucha confianza' | bd2015_2017_78$conf_iglesiaev == 'Mucha confianza'] <- 'Mucha confianza'
bd2015_2017_78$conf_iglesia[bd2015_2017_78$conf_iglesiacat == 'Otra' | bd2015_2017_78$conf_iglesiaev == 'Otra'] <- 'Otra'

### Construccion variable MMC
bd2015_2017_78$conf_mmc[bd2015_2017_78$conf_diarios == 'Mucha confianza' & bd2015_2017_78$conf_radios == 'Mucha confianza'] <- 'Mucha confianza'
bd2015_2017_78$conf_mmc[bd2015_2017_78$conf_diarios == 'Mucha confianza' & bd2015_2017_78$conf_tele == 'Mucha confianza'] <- 'Mucha confianza'
bd2015_2017_78$conf_mmc[bd2015_2017_78$conf_tele == 'Mucha confianza' & bd2015_2017_78$conf_radios == 'Mucha confianza'] <- 'Mucha confianza'

bd2015_2017_78$conf_mmc[bd2015_2017_78$conf_diarios == 'Otra' & bd2015_2017_78$conf_radios == 'Otra'] <- 'Otra'
bd2015_2017_78$conf_mmc[bd2015_2017_78$conf_diarios == 'Otra' & bd2015_2017_78$conf_tele == 'Otra'] <- 'Otra'
bd2015_2017_78$conf_mmc[bd2015_2017_78$conf_tele == 'Otra' & bd2015_2017_78$conf_radios == 'Otra'] <- 'Otra'

bd2015_2017_78$conf_mmc[bd2015_2017_78$conf_tele == 'Mucha confianza' & bd2015_2017_78$conf_radios == 'Mucha confianza' & bd2015_2017_78$conf_diarios == 'Mucha confianza'] <- 'Mucha confianza'
bd2015_2017_78$conf_mmc[bd2015_2017_78$conf_tele == 'Otra' & bd2015_2017_78$conf_radios == 'Otra' & bd2015_2017_78$conf_diarios == 'Otra'] <- 'Otra'

# Ver frecuencia variable nueva
frq(bd2015_2017_78$conf_mmc)

# Eliminar variables
bd2015_2017_78 <- select(bd2015_2017_78, -conf_iglesiaev, -conf_iglesiacat, -conf_radios, -conf_tele, -conf_diarios, -conf_carabineros, -conf_sindicatos)

## 2015_2017: CEP 79
### Construccion variable iglesia en calidad de institucion

bd2015_2017_79$conf_iglesia[bd2015_2017_79$conf_iglesiacat == 'Mucha confianza' | bd2015_2017_79$conf_iglesiaev == 'Mucha confianza'] <- 'Mucha confianza'
bd2015_2017_79$conf_iglesia[bd2015_2017_79$conf_iglesiacat == 'Otra' & bd2015_2017_79$conf_iglesiaev == 'Otra'] <- 'Otra'

### Construccion variable MMC
bd2015_2017_79$conf_mmc[bd2015_2017_79$conf_diarios == 'Mucha confianza' & bd2015_2017_79$conf_radios == 'Mucha confianza'] <- 'Mucha confianza'
bd2015_2017_79$conf_mmc[bd2015_2017_79$conf_diarios == 'Mucha confianza' & bd2015_2017_79$conf_tele == 'Mucha confianza'] <- 'Mucha confianza'
bd2015_2017_79$conf_mmc[bd2015_2017_79$conf_tele == 'Mucha confianza' & bd2015_2017_79$conf_radios == 'Mucha confianza'] <- 'Mucha confianza'

bd2015_2017_79$conf_mmc[bd2015_2017_79$conf_diarios == 'Otra' & bd2015_2017_79$conf_radios == 'Otra'] <- 'Otra'
bd2015_2017_79$conf_mmc[bd2015_2017_79$conf_diarios == 'Otra' & bd2015_2017_79$conf_tele == 'Otra'] <- 'Otra'
bd2015_2017_79$conf_mmc[bd2015_2017_79$conf_tele == 'Otra' & bd2015_2017_79$conf_radios == 'Otra'] <- 'Otra'

bd2015_2017_79$conf_mmc[bd2015_2017_79$conf_tele == 'Mucha confianza' & bd2015_2017_79$conf_radios == 'Mucha confianza' & bd2015_2017_79$conf_diarios == 'Mucha confianza'] <- 'Mucha confianza'
bd2015_2017_79$conf_mmc[bd2015_2017_79$conf_tele == 'Otra' & bd2015_2017_79$conf_radios == 'Otra' & bd2015_2017_79$conf_diarios == 'Otra'] <- 'Otra'

# Ver frecuencia variable nueva
frq(bd2015_2017_79$conf_mmc)

# Eliminar variables
bd2015_2017_79 <- select(bd2015_2017_79, -conf_iglesiaev, -conf_iglesiacat, -conf_radios, -conf_tele, -conf_diarios, -conf_carabineros, -conf_sindicatos)

#---- 3.2.4 Guardar bases de confianza ----
save(bd2015_2017_79, file = "input/data/bd2015_2017_79.RData")

#---- 3.3 Tratamiento de variables de identificación partidaria e identificación política (o posición política)
#---- 3.3.1 Frecuencias ----

frq(bd2015_2017_73$id_part)
frq(bd2015_2017_73$pos_pol)

frq(bd2015_2017_74$id_part)
frq(bd2015_2017_74$pos_pol)

frq(bd2015_2017_75$id_part)
frq(bd2015_2017_75$pos_pol)

frq(bd2015_2017_76$id_part) # Algo pasa acá
frq(bd2015_2017_76$pos_pol) # Algo pasa acá

frq(bd2015_2017_77$id_part)
frq(bd2015_2017_77$pos_pol)

frq(bd2015_2017_78$id_part)
frq(bd2015_2017_78$pos_pol)

frq(bd2015_2017_79$id_part) # Algo pasa acá
frq(bd2015_2017_79$pos_pol) # Algo pasa acá

frq(bd2015_2017_80$id_part) # Algo pasa acá
frq(bd2015_2017_80$pos_pol) # Algo pasa acá

frq(bd2015_2017_81$id_part)
frq(bd2015_2017_81$pos_pol)

#---- 3.3.2 Recodificacion ----

# 2015 - 2017: CEP 73 
bd2015_2017_73$id_part <- car::recode(bd2015_2017_73$id_part, "c(2,4) = 'Derecha'; 
                                 c(1,5,6,7) = 'Centro-Izquierda concertación'; 
                                 3 = 'Izquierda extraconcertación'; 
                                 9 = 'Ninguno'; 
                                 c(8,88,99) = NA", as.factor = T) 
bd2015_2017_73$pos_pol <- car::recode(bd2015_2017_73$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 2015 - 2017: CEP 74 
bd2015_2017_74$id_part <- car::recode(bd2015_2017_74$id_part, "c(1,4) = 'Derecha'; 
                                 c(2,3,5,6,8) = 'Centro-Izquierda concertación'; 
                                 c(7,11) = 'Izquierda extraconcertación'; 
                                 9 = 'Ninguno'; 
                                 c(8,88,99) = NA", as.factor = T)  
bd2015_2017_74$pos_pol <- car::recode(bd2015_2017_74$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)
freq(bd2015_2017_73$id_part)

# 2015 - 2017: CEP 75 
bd2015_2017_75$id_part <- car::recode(bd2015_2017_75$id_part "", as.factor = T) 
bd2015_2017_75$pos_pol <- car::recode(bd2015_2017_75$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# No estan estas variables en la 76

# 2015 - 2017: CEP 77 
bd2015_2017_77$id_part <- car::recode(bd2015_2017_77$id_part "", as.factor = T) 
bd2015_2017_77$pos_pol <- car::recode(bd2015_2017_77$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# 2015 - 2017: CEP 78 
bd2015_2017_78$id_part <- car::recode(bd2015_2017_78$id_part "", as.factor = T) 
bd2015_2017_78$pos_pol <- car::recode(bd2015_2017_78$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# No estan estas variables en a CEP 79 ni 80

# 2015 - 2017: CEP 81 
bd2015_2017_81$id_part <- car::recode(bd2015_2017_81$id_part "", as.factor = T) 
bd2015_2017_81$pos_pol <- car::recode(bd2015_2017_81$pos_pol,"1 = 'Derecha';
2 = 'Centro Derecha';
3 = 'Centro';
4 = 'Centro Izquierda';
5 = 'Izquierda';
6 = 'Independiente';
7 = 'Ninguna';
8 = NA;
9 = NA", as.factor = T)

# ---- 3.4 Guardar base de datos final ---- 
save(bd2015_2017_73, file = "input/data/bd2015_2017_73.RData")
save(bd2015_2017_74, file = "input/data/bd2015_2017_74.RData")
save(bd2015_2017_75, file = "input/data/bd2015_2017_75.RData")
save(bd2015_2017_76, file = "input/data/bd2015_2017_76.RData")
save(bd2015_2017_77, file = "input/data/bd2015_2017_77.RData")
save(bd2015_2017_78, file = "input/data/bd2015_2017_78.RData")
save(bd2015_2017_79, file = "input/data/bd2015_2017_79.RData")
save(bd2015_2017_80, file = "input/data/bd2015_2017_80.RData")
save(bd2015_2017_81, file = "input/data/bd2015_2017_81.RData")