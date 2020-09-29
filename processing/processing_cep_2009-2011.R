# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 28 - 9 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car, haven)
# ---- Etapa 2: Cargar bases de datos ---
bd2009_2011_59 <- read_sav("input/data/2009-2011/cep59mayjun2009.sav")
bd2009_2011_60 <- read_sav("input/data/2009-2011/cep60ago2009.sav")
bd2009_2011_61 <- read_sav("input/data/2009-2011/cep61oct2009.sav")
bd2009_2011_62 <- read_sav("input/data/2009-2011/cep62junjul2010.sav")
bd2009_2011_63 <- read_sav("input/data/2009-2011/cep63novdic2010.sav")
bd2009_2011_64 <- read_sav("input/data/2009-2011/cep64junjul2011.sav")
bd2009_2011_65 <- read_sav("input/data/2009-2011/cep65novdic2011.sav")
# ---- Etapa 3: Procesamiento de datos ----
## Seleccionar variables a utilizar

bd2009_2011_59 <- select(bd2009_2011_59, DDP38, ESCOLARIDAD, DDP02, DDP01, MBP14, MBP16)
bd2009_2011_60 <- select(bd2009_2011_60, DDP23, ESCOLARIDAD, DDP02, DDP01, MBP14, MBP16)
bd2009_2011_61 <- select(bd2009_2011_61, DDP23, ESCOLARIDAD, DDP02, DDP01, TE1P04_A:TE1P04_H, MBP14, MBP16)
bd2009_2011_62 <- select(bd2009_2011_62, DDP24, ESCOLARIDAD, DDP02, DDP01, MBP15, MBP17)
bd2009_2011_63 <- select(bd2009_2011_63, DDP36, DDP03, DDP02, DDP01, MBP14, MBP16)
bd2009_2011_64 <- select(bd2009_2011_64, DDP29, DDP03, DDP02, DDP01, MBP16, MBP18)
bd2009_2011_65 <- select(bd2009_2011_65, DDP36, DDP03, DDP02, DDP01, TE1P03a:TE1P03m)

## Renombrarlas

## 2009-2011: CEP 59
bd2009_2011_59 <- rename(bd2009_2011_59,
                    edad = DDP02,
                    id_part = MBP14,
                    pos_pol = MBP16,
                    nse = DDP38,
                    sexo = DDP01,
                    esc = ESCOLARIDAD)

## 2009-2011: CEP 60
bd2009_2011_60 <- rename(bd2009_2011_60,
                         edad = DDP02,
                         id_part = MBP14,
                         pos_pol = MBP16,
                         nse = DDP23,
                         sexo = DDP01,
                         esc = ESCOLARIDAD)
## 2009-2011: CEP 61
bd2009_2011_61 <- rename(bd2009_2011_61,
                         edad = DDP02,
                         id_part = MBP14,
                         pos_pol = MBP16,
                         nse = DDP23,
                         sexo = DDP01,
                         esc = ESCOLARIDAD,
                         conf_iglesiacat = TE1P04_A,
                         conf_ffaa = TE1P04_B,
                         conf_iglesiaev = TE1P04_C,
                         conf_partidos = TE1P04_D,
                         conf_tribunalesjust = TE1P04_E,
                         conf_diarios = TE1P04_F,
                         conf_tele = TE1P04_G,
                         conf_radios = TE1P04_H)
## 2009-2011: CEP 62
bd2009_2011_62 <- rename(bd2009_2011_62,
                         edad = DDP02,
                         id_part = MBP15,
                         pos_pol = MBP17,
                         nse = DDP24,
                         sexo = DDP01,
                         esc = ESCOLARIDAD)
## 2009-2011: CEP 63
bd2009_2011_63 <- rename(bd2009_2011_63,
                         edad = DDP02,
                         id_part = MBP14,
                         pos_pol = MBP16,
                         nse = DDP36,
                         sexo = DDP01,
                         esc = DDP03)
## 2009-2011: CEP 64
bd2009_2011_64 <- rename(bd2009_2011_64,
                         edad = DDP02,
                         id_part = MBP16,
                         pos_pol = MBP18,
                         nse = DDP29,
                         sexo = DDP01,
                         esc = DDP03)
## 2009-2011: CEP 65
bd2009_2011_65 <- rename(bd2009_2011_65,
                         edad = DDP02,
                         id_part = MBP14,
                         pos_pol = MBP16,
                         nse = DDP29,
                         sexo = DDP01,
                         esc = DDP03,
                         )

#---- 3.1 Tratamiento de  sociodemográficas ----
#---- 3.1.1 Frecuencias ----
## 2009-2011: CEP 59
frq(bd2009_2011_59$nse) # NSE
frq(bd2009_2011_59$esc) # Escolaridad
frq(bd2009_2011_59$edad) # Edad
frq(bd2009_2011_59$sexo) # Sexo

## 2009-2011: CEP 60
frq(bd2009_2011_60$nse)
frq(bd2009_2011_60$esc)
frq(bd2009_2011_60$edad)
frq(bd2009_2011_60$sexo)

## 2009-2011: CEP 61
frq(bd2009_2011_61$nse)
frq(bd2009_2011_61$esc)
frq(bd2009_2011_61$edad)
frq(bd2009_2011_61$sexo)

## 2009-2011: CEP 62
frq(bd2009_2011_62$nse) # NSE
frq(bd2009_2011_62$esc) # Escolaridad
frq(bd2009_2011_62$edad) # Edad
frq(bd2009_2011_62$sexo) # Sexo

## 2009-2011: CEP 63
frq(bd2009_2011_63$nse)
frq(bd2009_2011_63$esc)
frq(bd2009_2011_63$edad)
frq(bd2009_2011_63$sexo)

## 2009-2011: CEP 64
frq(bd2009_2011_64$nse) # NSE
frq(bd2009_2011_64$esc) # Escolaridad
frq(bd2009_2011_64$edad) # Edad
frq(bd2009_2011_64$sexo) # Sexo

## 2009-2011: CEP 65
frq(bd2009_2011_65$nse) # NSE
frq(bd2009_2011_65$esc) # Escolaridad
frq(bd2009_2011_65$edad) # Edad
frq(bd2009_2011_65$sexo) # Sexo


#---- 3.1.2 Recodificación ----
# Remover etiquetas
bd2009_2011_59 <- sjlabelled::remove_all_labels(bd2009_2011_59)
bd2009_2011_60 <- sjlabelled::remove_all_labels(bd2009_2011_60)
bd2009_2011_61 <- sjlabelled::remove_all_labels(bd2009_2011_61)
bd2009_2011_62 <- sjlabelled::remove_all_labels(bd2009_2011_62)
bd2009_2011_63 <- sjlabelled::remove_all_labels(bd2009_2011_63)
bd2009_2011_64 <- sjlabelled::remove_all_labels(bd2009_2011_64)
bd2009_2011_65 <- sjlabelled::remove_all_labels(bd2009_2011_65)

## 2009-2011: CEP 59
bd2009_2011_59$nse  <- car::recode(bd2009_2011_59$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2009_2011_59$esc <- car::recode(bd2009_2011_59$esc, "1 = '0-3'; 2 = '4-8'; 3 = '9-12'; 4 = '13 y mas'; 5 = 'NC'", as.factor = T)
bd2009_2011_59$edad <- car::recode(bd2009_2011_59$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2009_2011_59$sexo <- car::recode(bd2009_2011_59$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2009-2011: CEP 60
bd2009_2011_60$nse  <- car::recode(bd2009_2011_60$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2009_2011_60$esc <- car::recode(bd2009_2011_60$esc, "1 = '0-3'; 2 = '4-8'; 3 = '9-12'; 4 = '13 y mas'; 5 = 'NC'", as.factor = T)
bd2009_2011_60$edad <- car::recode(bd2009_2011_60$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2009_2011_59$sexo <- car::recode(bd2009_2011_60$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2009-2011: CEP 61

bd2009_2011_61$nse  <- car::recode(bd2009_2011_61$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2009_2011_61$esc <- car::recode(bd2009_2011_61$esc, "1 = '0-3'; 2 = '4-8'; 3 = '9-12'; 4 = '13 y mas'; 5 = 'NC'", as.factor = T)
bd2009_2011_61$edad <- car::recode(bd2009_2011_61$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2009_2011_61$sexo <- car::recode(bd2009_2011_61$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2009-2011: CEP 62
bd2009_2011_62$nse  <- car::recode(bd2009_2011_62$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2009_2011_62$esc <- car::recode(bd2009_2011_62$esc, "1 = '0-3'; 2 = '4-8'; 3 = '9-12'; 4 = '13 y mas'; 5 = 'NC'", as.factor = T)
bd2009_2011_62$edad <- car::recode(bd2009_2011_62$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2009_2011_62$sexo <- car::recode(bd2009_2011_62$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2009-2011: CEP 63
bd2009_2011_63$nse  <- car::recode(bd2009_2011_63$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2009_2011_63$esc <- car::recode(bd2009_2011_63$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2009_2011_63$edad <- car::recode(bd2009_2011_63$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2009_2011_63$sexo <- car::recode(bd2009_2011_63$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2009-2011: CEP 64
bd2009_2011_64$nse  <- car::recode(bd2009_2011_64$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2009_2011_64$esc <- car::recode(bd2009_2011_64$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2009_2011_64$edad <- car::recode(bd2009_2011_64$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2009_2011_64$sexo <- car::recode(bd2009_2011_64$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

## 2009-2011: CEP 65
bd2009_2011_65$nse  <- car::recode(bd2009_2011_65$nse, "1 = 'ABC1'; 2 = 'C2'; 3 = 'C3'; 4 = 'D'; 5 = 'E'", as.factor = T)
bd2009_2011_65$esc <- car::recode(bd2009_2011_65$esc, "c(88,99) = 'NS/NC'; 0:3 = '0-3'; 4:8 = '4-8'; 9:12 = '9-12'; else = '13 y mas'", as.factor = T)
bd2009_2011_65$edad <- car::recode(bd2009_2011_65$edad, "18:24 = '18-24'; 25:34 = '25-34'; 35:54 = '35-54'; else = '55 y mas'", as.factor = T)
bd2009_2011_65$sexo <- car::recode(bd2009_2011_65$sexo, "1 = 'Hombre'; 2 = 'Mujer'", as.factor = T)

#---- 3.2 Tratamiento de variables de confianza ----
#---- 3.2.1 Frecuencias ----

## 2009-2011: CEP 61
frq(bd2009_2011_61$conf_iglesiacat)
frq(bd2009_2011_61$conf_ffaa)
frq(bd2009_2011_61$conf_iglesiaev)
frq(bd2009_2011_61$conf_partidos)
frq(bd2009_2011_61$conf_tribunalesjust)
frq(bd2009_2011_61$conf_diarios)
frq(bd2009_2011_61$conf_tele)
frq(bd2009_2011_61$conf_radios)

#---- 3.2.2 Recodificacion ----
bd2009_2011_61$conf_iglesiacat <- car::recode(bd2009_2011_61$conf_iglesiacat,"c(1,2) = 'Baja o nula confianza';  c(3,4) = 'Alta o media confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2009_2011_61$conf_ffaa <- car::recode(bd2009_2011_61$conf_ffaa,"c(1,2) = 'Baja o nula confianza';  c(3,4) = 'Alta o media confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2009_2011_61$conf_iglesiaev <- car::recode(bd2009_2011_61$conf_iglesiaev,"c(1,2) = 'Baja o nula confianza';  c(3,4) = 'Alta o media confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2009_2011_61$conf_partidos <- car::recode(bd2009_2011_61$conf_partidos, "c(1,2) = 'Baja o nula confianza';  c(3,4) = 'Alta o media confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2009_2011_61$conf_tribunalesjust <- car::recode(bd2009_2011_61$conf_tribunalesjust, "c(1,2) = 'Baja o nula confianza';  c(3,4) = 'Alta o media confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2009_2011_61$conf_diarios <- car::recode(bd2009_2011_61$conf_diarios, "c(1,2) = 'Baja o nula confianza';  c(3,4) = 'Alta o media confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2009_2011_61$conf_tele <- car::recode(bd2009_2011_61$conf_tele, "c(1,2) = 'Baja o nula confianza';  c(3,4) = 'Alta o media confianza'; c(8,9) = 'NS/NC'", as.factor = T)
bd2009_2011_61$conf_radios <- car::recode(bd2009_2011_61$conf_radios, "c(1,2) = 'Baja o nula confianza';  c(3,4) = 'Alta o media confianza'; c(8,9) = 'NS/NC'", as.factor = T)

#---- 3.2.3 Otros ajustes ----
### Construccion variable iglesia en calidad de institucion

bd2009_2011_61$conf_iglesia[bd2009_2011_61$conf_iglesiacat == 'Alta o media confianza' | bd2009_2011_61$conf_iglesiaev == 'Alta o media confianza'] <- 'Alta o media confianza'
bd2009_2011_61$conf_iglesia[bd2009_2011_61$conf_iglesiacat == 'Baja o nula confianza' | bd2009_2011_61$conf_iglesiaev == 'Baja o nula confianza'] <- 'Baja o nula confianza'

### Construccion variable MMC
bd2009_2011_61$conf_mmc[bd2009_2011_61$conf_diarios == 'Alta o media confianza' & bd2009_2011_61$conf_radios == 'Alta o media confianza'] <- 'Alta o media confianza'
bd2009_2011_61$conf_mmc[bd2009_2011_61$conf_diarios == 'Alta o media confianza' & bd2009_2011_61$conf_tele == 'Alta o media confianza'] <- 'Alta o media confianza'
bd2009_2011_61$conf_mmc[bd2009_2011_61$conf_tele == 'Alta o media confianza' & bd2009_2011_61$conf_radios == 'Alta o media confianza'] <- 'Alta o media confianza'

bd2009_2011_61$conf_mmc[bd2009_2011_61$conf_diarios == 'Baja o nula confianza' & bd2009_2011_61$conf_radios == 'Baja o nula confianza'] <- 'Baja o nula confianza'
bd2009_2011_61$conf_mmc[bd2009_2011_61$conf_diarios == 'Baja o nula confianza' & bd2009_2011_61$conf_tele == 'Baja o nula confianza'] <- 'Baja o nula confianza'
bd2009_2011_61$conf_mmc[bd2009_2011_61$conf_tele == 'Baja o nula confianza' & bd2009_2011_61$conf_radios == 'Baja o nula confianza'] <- 'Baja o nula confianza'

bd2009_2011_61$conf_mmc[bd2009_2011_61$conf_tele == 'Alta o media confianza' & bd2009_2011_61$conf_radios == 'Alta o media confianza' & bd2009_2011_61$conf_diarios == 'Alta o media confianza'] <- 'Alta o media confianza'
bd2009_2011_61$conf_mmc[bd2009_2011_61$conf_tele == 'Baja o nula confianza' & bd2009_2011_61$conf_radios == 'Baja o nula confianza' & bd2009_2011_61$conf_diarios == 'Baja o nula confianza'] <- 'Baja o nula confianza'

# Ver frecuencia variable nueva
frq(bd2009_2011_61$conf_mmc)