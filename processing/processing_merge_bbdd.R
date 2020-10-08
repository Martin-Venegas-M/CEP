# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 5 - 10 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car)

# ---- Etapa 2: Cargar bases de datos ---
load("input/data/bd1990_16.RData")

load("input/data/bd1991_1993_17.RData") #1991
load("input/data/bd1991_1993_18.RData") #1991
load("input/data/bd1991_1993_23.RData") #1992

load("input/data/bd1997_1999_36.RData") #1998

load("input/data/bd2000_2002_44.RData") #2002

load("input/data/bd2003_2005_45.RData") #2003

load("input/data/bd2006_2008_58.RData") #2008

load("input/data/bd2009_2011_61.RData") #2009

load("input/data/bd2012_2014_67.RData") #2012

load("input/data/bd2012_2014_69.RData") #2013
load("input/data/bd2012_2014_71.RData") #2014

load("input/data/bd2015_2017_79.RData") #2017

load("input/data/bd2018_2019_84.RData") #2019

# Re asignar nombres
df1991_17 <- bd1991_1993_17
df1991_18 <- bd1991_1993_18
df1992_23 <- bd1991_1993_23

df1998_36 <- bd1997_1999_36

df2002_44 <- bd2000_2002_44

df2003_45 <- bd2003_2005_45
df2008_58 <- bd2008_58
df2009_61 <- bd2009_2011_61
df2012_67 <- bd2012_2014_67
df2013_69 <- bd2012_2014_69
df2014_71 <- bd2012_2014_71
df2017_79 <- bd2017_79
df2019_84 <- bd2018_2019_84


rm(list = ls()[grep("bd", ls())])



#---- Etapa 3: Procesamiento de datos----
#---- 3.1 Añadir variable año ----
df1991_17$year <- "1991"
df1991_18$year <- "1991"
df1992_23$year <- "1992"
df1998_36$year <- "1998"
df2002_44$year <- "2002"
df2003_45$year <- "2003"
df2008_58$year <- "2008"
df2009_61$year <- "2009"
df2012_67$year <- "2012"
df2013_69$year <- "2013"
df2014_71$year <- "2014"
df2017_79$year <- "2017"
df2019_84$year <- "2019"
  
#---- 3.2 Group and summarise bases de datos ----
# 1991a
df_conf_partidos_1991a <- df1991_17 %>%group_by(year)%>%summarise(porc_conf_dirpol= length(which(conf_dirpol== 'Alta o media confianza'))
                                                        /(length(which(conf_dirpol== 'Alta o media confianza'))+
                                                            length(which(conf_dirpol== 'Baja o nula confianza'))))



df_conf_iglesia_1991a <- df1991_17 %>%group_by(year)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Alta o media confianza'))
                                                    /(length(which(conf_iglesia== 'Alta o media confianza'))+
                                                        length(which(conf_iglesia== 'Baja o nula confianza'))))



df_conf_congreso_1991a <- df1991_17 %>%group_by(year)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Alta o media confianza'))
                                                    /(length(which(conf_congreso== 'Alta o media confianza'))+
                                                        length(which(conf_congreso== 'Baja o nula confianza'))))


df_conf_ffaa_1991a <- df1991_17 %>%group_by(year)%>%summarise(porc_conf_ffaa= length(which(conf_altosffaa== 'Alta o media confianza'))
                                              /(length(which(conf_altosffaa== 'Alta o media confianza'))+
                                                  length(which(conf_altosffaa== 'Baja o nula confianza'))))


df_conf_empresas_1991a <- df1991_17 %>%group_by(year)%>%summarise(porc_conf_empresas= length(which(conf_diremp== 'Alta o media confianza'))
                                                        /(length(which(conf_diremp== 'Alta o media confianza'))+
                                                            length(which(conf_diremp== 'Baja o nula confianza'))))


df_conf_tribunales_1991a <- df1991_17 %>%group_by(year)%>%summarise(porc_conf_tribunales= length(which(conf_dircortesup== 'Alta o media confianza'))
                                                    /(length(which(conf_dircortesup== 'Alta o media confianza'))+
                                                        length(which(conf_dircortesup== 'Baja o nula confianza'))))

# 1991b
df_conf_partidos_1991b <- df1991_17 %>%group_by(year)%>%summarise(porc_conf_dirpol= length(which(conf_dirpol== 'Alta o media confianza'))
                                                                  /(length(which(conf_dirpol== 'Alta o media confianza'))+
                                                                      length(which(conf_dirpol== 'Baja o nula confianza'))))



df_conf_iglesia_1991b <- df1991_17 %>%group_by(year)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Alta o media confianza'))
                                                                 /(length(which(conf_iglesia== 'Alta o media confianza'))+
                                                                     length(which(conf_iglesia== 'Baja o nula confianza'))))



df_conf_congreso_1991b <- df1991_17 %>%group_by(year)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Alta o media confianza'))
                                                                  /(length(which(conf_congreso== 'Alta o media confianza'))+
                                                                      length(which(conf_congreso== 'Baja o nula confianza'))))


df_conf_ffaa_1991b <- df1991_17 %>%group_by(year)%>%summarise(porc_conf_ffaa= length(which(conf_altosffaa== 'Alta o media confianza'))
                                                              /(length(which(conf_altosffaa== 'Alta o media confianza'))+
                                                                  length(which(conf_altosffaa== 'Baja o nula confianza'))))


df_conf_empresas_1991b <- df1991_17 %>%group_by(year)%>%summarise(porc_conf_empresas= length(which(conf_diremp== 'Alta o media confianza'))
                                                                  /(length(which(conf_diremp== 'Alta o media confianza'))+
                                                                      length(which(conf_diremp== 'Baja o nula confianza'))))


df_conf_tribunales_1991b <- df1991_17 %>%group_by(year)%>%summarise(porc_conf_tribunales= length(which(conf_dircortesup== 'Alta o media confianza'))
                                                                    /(length(which(conf_dircortesup== 'Alta o media confianza'))+
                                                                        length(which(conf_dircortesup== 'Baja o nula confianza'))))

# Promedio anos

# 1992
# 1998
# 2002
# 2003
# 2008
# 2009
# 2012
# 2013
# 2014
# 2017
# 2019


