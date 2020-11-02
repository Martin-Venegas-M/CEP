# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 5 - 10 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car)

# ---- Etapa 2: Cargar bases de datos ---
load("input/data/bd1990_16.RData") # 1990

load("input/data/bd1991_1993_17.RData") #1991 # Bateria personas
load("input/data/bd1991_1993_18.RData") #1991 # Bateria personas
load("input/data/bd1991_1993_23.RData") #1992 # Bateria personas

#load("input/data/bd1997_1999_36.RData") #1998 # Bateria extrana

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
df1990_16 <- bd1990_16

df1991_17 <- bd1991_1993_17
df1991_18 <- bd1991_1993_18
df1992_23 <- bd1991_1993_23

#df1998_36 <- bd1997_1999_36

df2002_44 <- bd2000_2002_44

df2003_45 <- bd2003_2005_45
df2008_58 <- bd2006_2008_58
df2009_61 <- bd2009_2011_61
df2012_67 <- bd2012_2014_67
df2013_69 <- bd2012_2014_69
df2014_71 <- bd2012_2014_71
df2017_79 <- bd2015_2017_79
df2019_84 <- bd2018_2019_84


rm(list = ls()[grep("bd", ls())])



#---- Etapa 3: Procesamiento de datos----
#---- 3.1 Añadir variable año ----
df1990_16$year <- "1990"
df1991_17$year <- "1991"
df1991_18$year <- "1991"
df1992_23$year <- "1992"
#df1998_36$year <- "1998"
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
# ---- 1990 ----
df_conf_dirpol_1990 <- df1990_16 %>%group_by(year, esc)%>%summarise(porc_conf_dirpol= length(which(conf_dirpol== 'Mucha confianza'))
                                                                    /(length(which(conf_dirpol== 'Mucha confianza'))+
                                                                        length(which(conf_dirpol== 'Otra'))))



df_conf_iglesia_1990 <- df1990_16 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                     /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                         length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_1990 <- df1990_16 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                      /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                          length(which(conf_congreso== 'Otra'))))


df_conf_altosffaa_1990 <- df1990_16 %>%group_by(year, esc)%>%summarise(porc_conf_altosffaa= length(which(conf_altosffaa== 'Mucha confianza'))
                                                                       /(length(which(conf_altosffaa== 'Mucha confianza'))+
                                                                           length(which(conf_altosffaa== 'Otra'))))


df_conf_diremp_1990 <- df1990_16 %>%group_by(year, esc)%>%summarise(porc_conf_diremp= length(which(conf_diremp== 'Mucha confianza'))
                                                                    /(length(which(conf_diremp== 'Mucha confianza'))+
                                                                        length(which(conf_diremp== 'Otra'))))


df_conf_dircortesup_1990 <- df1990_16 %>%group_by(year, esc)%>%summarise(porc_conf_dircortesup= length(which(conf_dircortesup== 'Mucha confianza'))
                                                                         /(length(which(conf_dircortesup== 'Mucha confianza'))+
                                                                             length(which(conf_dircortesup== 'Otra'))))


# ---- 1991a ----
df_conf_dirpol_1991a <- df1991_17 %>%group_by(year, esc)%>%summarise(porc_conf_dirpol= length(which(conf_dirpol== 'Mucha confianza'))
                                                                     /(length(which(conf_dirpol== 'Mucha confianza'))+
                                                                         length(which(conf_dirpol== 'Otra'))))



df_conf_iglesia_1991a <- df1991_17 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                      /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                          length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_1991a <- df1991_17 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                       /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                           length(which(conf_congreso== 'Otra'))))


df_conf_altosffaa_1991a <- df1991_17 %>%group_by(year, esc)%>%summarise(porc_conf_altosffaa= length(which(conf_altosffaa== 'Mucha confianza'))
                                                                        /(length(which(conf_altosffaa== 'Mucha confianza'))+
                                                                            length(which(conf_altosffaa== 'Otra'))))


df_conf_diremp_1991a <- df1991_17 %>%group_by(year, esc)%>%summarise(porc_conf_diremp= length(which(conf_diremp== 'Mucha confianza'))
                                                                     /(length(which(conf_diremp== 'Mucha confianza'))+
                                                                         length(which(conf_diremp== 'Otra'))))


df_conf_dircortesup_1991a <- df1991_17 %>%group_by(year, esc)%>%summarise(porc_conf_dircortesup= length(which(conf_dircortesup== 'Mucha confianza'))
                                                                          /(length(which(conf_dircortesup== 'Mucha confianza'))+
                                                                              length(which(conf_dircortesup== 'Otra'))))

# ---- 1991b ----
df_conf_dirpol_1991b <- df1991_18 %>%group_by(year, esc)%>%summarise(porc_conf_dirpol= length(which(conf_dirpol== 'Mucha confianza'))
                                                                     /(length(which(conf_dirpol== 'Mucha confianza'))+
                                                                         length(which(conf_dirpol== 'Otra'))))



df_conf_iglesia_1991b <- df1991_18 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                      /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                          length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_1991b <- df1991_18 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                       /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                           length(which(conf_congreso== 'Otra'))))


df_conf_altosffaa_1991b <- df1991_18 %>%group_by(year, esc)%>%summarise(porc_conf_altosffaa= length(which(conf_altosffaa== 'Mucha confianza'))
                                                                        /(length(which(conf_altosffaa== 'Mucha confianza'))+
                                                                            length(which(conf_altosffaa== 'Otra'))))


df_conf_diremp_1991b <- df1991_18 %>%group_by(year, esc)%>%summarise(porc_conf_diremp= length(which(conf_diremp== 'Mucha confianza'))
                                                                     /(length(which(conf_diremp== 'Mucha confianza'))+
                                                                         length(which(conf_diremp== 'Otra'))))


df_conf_dircortesup_1991b <- df1991_18 %>%group_by(year, esc)%>%summarise(porc_conf_dircortesup= length(which(conf_dircortesup== 'Mucha confianza'))
                                                                          /(length(which(conf_dircortesup== 'Mucha confianza'))+
                                                                              length(which(conf_dircortesup== 'Otra'))))

# Promedio años

# Dado que el año de 1991 cuenta con dos registros de la batería de confianza, se procede a hacerun promedio entre ambas mediciones y se guarda en una nueva base de datos

df_conf_congreso_1991a$porc_conf_congreso <- (df_conf_congreso_1991a$porc_conf_congreso + df_conf_congreso_1991b$porc_conf_congreso)/2
df_conf_diremp_1991a$porc_conf_diremp <- (df_conf_diremp_1991a$porc_conf_diremp + df_conf_diremp_1991b$porc_conf_diremp)/2
df_conf_altosffaa_1991a$porc_conf_altosffaa <- (df_conf_altosffaa_1991a$porc_conf_altosffaa + df_conf_altosffaa_1991b$porc_conf_altosffaa)/2
df_conf_iglesia_1991a$porc_conf_iglesia <- (df_conf_iglesia_1991a$porc_conf_iglesia + df_conf_iglesia_1991b$porc_conf_iglesia)/2
df_conf_dirpol_1991a$porc_conf_dirpol <- (df_conf_dirpol_1991a$porc_conf_dirpol + df_conf_dirpol_1991b$porc_conf_dirpol)/2
df_conf_dircortesup_1991a$porc_conf_dircortesup <- (df_conf_dircortesup_1991a$porc_conf_dircortesup + df_conf_dircortesup_1991b$porc_conf_dircortesup)/2

df_conf_congreso_1991 <-  df_conf_congreso_1991a
df_conf_diremp_1991 <- df_conf_diremp_1991a
df_conf_altosffaa_1991 <- df_conf_altosffaa_1991a
df_conf_iglesia_1991 <- df_conf_iglesia_1991a
df_conf_dirpol_1991 <- df_conf_dirpol_1991a
df_conf_dircortesup_1991 <- df_conf_dircortesup_1991a


# ---- 1992 ----

df_conf_dirpol_1992 <- df1992_23 %>%group_by(year, esc)%>%summarise(porc_conf_dirpol= length(which(conf_dirpol== 'Mucha confianza'))
                                                                    /(length(which(conf_dirpol== 'Mucha confianza'))+
                                                                        length(which(conf_dirpol== 'Otra'))))



df_conf_iglesia_1992 <- df1992_23 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                     /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                         length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_1992 <- df1992_23 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                      /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                          length(which(conf_congreso== 'Otra'))))


df_conf_altosffaa_1992 <- df1992_23 %>%group_by(year, esc)%>%summarise(porc_conf_altosffaa= length(which(conf_altosffaa== 'Mucha confianza'))
                                                                       /(length(which(conf_altosffaa== 'Mucha confianza'))+
                                                                           length(which(conf_altosffaa== 'Otra'))))


df_conf_diremp_1992 <- df1992_23 %>%group_by(year, esc)%>%summarise(porc_conf_diremp= length(which(conf_diremp== 'Mucha confianza'))
                                                                    /(length(which(conf_diremp== 'Mucha confianza'))+
                                                                        length(which(conf_diremp== 'Otra'))))


df_conf_dircortesup_1992 <- df1992_23 %>%group_by(year, esc)%>%summarise(porc_conf_dircortesup= length(which(conf_dircortesup== 'Mucha confianza'))
                                                                         /(length(which(conf_dircortesup== 'Mucha confianza'))+
                                                                             length(which(conf_dircortesup== 'Otra'))))


# ---- 1998 -----


#df_conf_iglesia_1998<- df1998_36 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
#/(length(which(conf_iglesia== 'Mucha confianza'))+
#length(which(conf_iglesia== 'Otra'))))



#df_conf_congreso_1998<- df1998_36 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
#/(length(which(conf_congreso== 'Mucha confianza'))+
#length(which(conf_congreso== 'Otra'))))



#df_conf_empresas_1998<- df1998_36 %>%group_by(year, esc)%>%summarise(porc_conf_empresas= length(which(conf_diremp== 'Mucha confianza'))
#/(length(which(conf_diremp== 'Mucha confianza'))+
#length(which(conf_diremp== 'Otra'))))


#df_conf_tribunales_1998<- df1998_36 %>%group_by(year, esc)%>%summarise(porc_conf_tribunales= length(which(conf_dircortesup== 'Mucha confianza'))
#/(length(which(conf_dircortesup== 'Mucha confianza'))+
#length(which(conf_dircortesup== 'Otra'))))

# ---- 2002 Instituciones ----

df_conf_partidos_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_partidos= length(which(conf_partidos== 'Mucha confianza'))
                                                                      /(length(which(conf_partidos== 'Mucha confianza'))+
                                                                          length(which(conf_partidos== 'Otra'))))



df_conf_iglesia_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                     /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                         length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                      /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                          length(which(conf_congreso== 'Otra'))))


df_conf_ffaa_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_ffaa= length(which(conf_ffaa== 'Mucha confianza'))
                                                                  /(length(which(conf_ffaa== 'Mucha confianza'))+
                                                                      length(which(conf_ffaa== 'Otra'))))


df_conf_empresas_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_empresas= length(which(conf_empr== 'Mucha confianza'))
                                                                      /(length(which(conf_empr== 'Mucha confianza'))+
                                                                          length(which(conf_empr== 'Otra'))))


df_conf_tribunales_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_tribunales= length(which(conf_tribun== 'Mucha confianza'))
                                                                        /(length(which(conf_tribun== 'Mucha confianza'))+
                                                                            length(which(conf_tribun== 'Otra'))))

df_conf_mmc_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_mmc= length(which(conf_mmc== 'Mucha confianza'))
                                                                 /(length(which(conf_mmc== 'Mucha confianza'))+
                                                                     length(which(conf_mmc== 'Otra'))))

# ---- 2002 Personas ----
df_conf_dirpol_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_dirpol= length(which(conf_dirpol== 'Mucha confianza'))
                                                                    /(length(which(conf_dirpol== 'Mucha confianza'))+
                                                                        length(which(conf_dirpol== 'Otra'))))



df_conf_iglesia_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia_per== 'Mucha confianza'))
                                                                     /(length(which(conf_iglesia_per== 'Mucha confianza'))+
                                                                         length(which(conf_iglesia_per== 'Otra'))))



df_conf_congreso_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso_per== 'Mucha confianza'))
                                                                      /(length(which(conf_congreso_per== 'Mucha confianza'))+
                                                                          length(which(conf_congreso_per== 'Otra'))))


df_conf_altosffaa_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_altosffaa= length(which(conf_altosffaa== 'Mucha confianza'))
                                                                       /(length(which(conf_altosffaa== 'Mucha confianza'))+
                                                                           length(which(conf_altosffaa== 'Otra'))))


df_conf_diremp_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_diremp= length(which(conf_diremp== 'Mucha confianza'))
                                                                    /(length(which(conf_diremp== 'Mucha confianza'))+
                                                                        length(which(conf_diremp== 'Otra'))))


df_conf_dircortesup_2002 <- df2002_44 %>%group_by(year, esc)%>%summarise(porc_conf_dircortesup= length(which(conf_dircortesup== 'Mucha confianza'))
                                                                         /(length(which(conf_dircortesup== 'Mucha confianza'))+
                                                                             length(which(conf_dircortesup== 'Otra'))))
# ---- 2003 ----

df_conf_partidos_2003 <- df2003_45 %>%group_by(year, esc)%>%summarise(porc_conf_partidos= length(which(conf_partidos== 'Mucha confianza'))
                                                                      /(length(which(conf_partidos== 'Mucha confianza'))+
                                                                          length(which(conf_partidos== 'Otra'))))



df_conf_iglesia_2003 <- df2003_45 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                     /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                         length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_2003 <- df2003_45 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                      /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                          length(which(conf_congreso== 'Otra'))))


df_conf_ffaa_2003 <- df2003_45 %>%group_by(year, esc)%>%summarise(porc_conf_ffaa= length(which(conf_ffaa== 'Mucha confianza'))
                                                                  /(length(which(conf_ffaa== 'Mucha confianza'))+
                                                                      length(which(conf_ffaa== 'Otra'))))


df_conf_empresas_2003 <- df2003_45 %>%group_by(year, esc)%>%summarise(porc_conf_empresas= length(which(conf_empr== 'Mucha confianza'))
                                                                      /(length(which(conf_empr== 'Mucha confianza'))+
                                                                          length(which(conf_empr== 'Otra'))))


df_conf_tribunales_2003 <- df2003_45 %>%group_by(year, esc)%>%summarise(porc_conf_tribunales= length(which(conf_tribun== 'Mucha confianza'))
                                                                        /(length(which(conf_tribun== 'Mucha confianza'))+
                                                                            length(which(conf_tribun== 'Otra'))))

df_conf_mmc_2003 <- df2003_45 %>%group_by(year, esc)%>%summarise(porc_conf_mmc= length(which(conf_mmc== 'Mucha confianza'))
                                                                 /(length(which(conf_mmc== 'Mucha confianza'))+
                                                                     length(which(conf_mmc== 'Otra'))))


# ---- 2008 ----

df_conf_partidos_2008 <- df2008_58 %>%group_by(year, esc)%>%summarise(porc_conf_partidos= length(which(conf_partidos== 'Mucha confianza'))
                                                                      /(length(which(conf_partidos== 'Mucha confianza'))+
                                                                          length(which(conf_partidos== 'Otra'))))



df_conf_iglesia_2008 <- df2008_58 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                     /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                         length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_2008 <- df2008_58 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                      /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                          length(which(conf_congreso== 'Otra'))))


df_conf_ffaa_2008 <- df2008_58 %>%group_by(year, esc)%>%summarise(porc_conf_ffaa= length(which(conf_ffaa== 'Mucha confianza'))
                                                                  /(length(which(conf_ffaa== 'Mucha confianza'))+
                                                                      length(which(conf_ffaa== 'Otra'))))


df_conf_empresas_2008 <- df2008_58 %>%group_by(year, esc)%>%summarise(porc_conf_empresas= length(which(conf_emppriv== 'Mucha confianza'))
                                                                      /(length(which(conf_emppriv== 'Mucha confianza'))+
                                                                          length(which(conf_emppriv== 'Otra'))))


df_conf_tribunales_2008 <- df2008_58 %>%group_by(year, esc)%>%summarise(porc_conf_tribunales= length(which(conf_tribunalesjust== 'Mucha confianza'))
                                                                        /(length(which(conf_tribunalesjust== 'Mucha confianza'))+
                                                                            length(which(conf_tribunalesjust== 'Otra'))))

df_conf_mmc_2008 <- df2008_58 %>%group_by(year, esc)%>%summarise(porc_conf_mmc= length(which(conf_mmc== 'Mucha confianza'))
                                                                 /(length(which(conf_mmc== 'Mucha confianza'))+
                                                                     length(which(conf_mmc== 'Otra'))))


# ---- 2009 ----

df_conf_partidos_2009 <- df2009_61 %>%group_by(year, esc)%>%summarise(porc_conf_partidos= length(which(conf_partidos== 'Mucha confianza'))
                                                                      /(length(which(conf_partidos== 'Mucha confianza'))+
                                                                          length(which(conf_partidos== 'Otra'))))



df_conf_iglesia_2009 <- df2009_61 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                     /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                         length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_2009 <- df2009_61 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                      /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                          length(which(conf_congreso== 'Otra'))))


df_conf_ffaa_2009 <- df2009_61 %>%group_by(year, esc)%>%summarise(porc_conf_ffaa= length(which(conf_ffaa== 'Mucha confianza'))
                                                                  /(length(which(conf_ffaa== 'Mucha confianza'))+
                                                                      length(which(conf_ffaa== 'Otra'))))


df_conf_empresas_2009 <- df2009_61 %>%group_by(year, esc)%>%summarise(porc_conf_empresas= length(which(conf_emppriv== 'Mucha confianza'))
                                                                      /(length(which(conf_emppriv== 'Mucha confianza'))+
                                                                          length(which(conf_emppriv== 'Otra'))))


df_conf_tribunales_2009 <- df2009_61 %>%group_by(year, esc)%>%summarise(porc_conf_tribunales= length(which(conf_tribunalesjust== 'Mucha confianza'))
                                                                        /(length(which(conf_tribunalesjust== 'Mucha confianza'))+
                                                                            length(which(conf_tribunalesjust== 'Otra'))))

df_conf_mmc_2009 <- df2009_61 %>%group_by(year, esc)%>%summarise(porc_conf_mmc= length(which(conf_mmc== 'Mucha confianza'))
                                                                 /(length(which(conf_mmc== 'Mucha confianza'))+
                                                                     length(which(conf_mmc== 'Otra'))))

# ---- 2012 ----

df_conf_partidos_2012 <- df2012_67 %>%group_by(year, esc)%>%summarise(porc_conf_partidos= length(which(conf_partidos== 'Mucha confianza'))
                                                                      /(length(which(conf_partidos== 'Mucha confianza'))+
                                                                          length(which(conf_partidos== 'Otra'))))



df_conf_iglesia_2012 <- df2012_67 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                     /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                         length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_2012 <- df2012_67 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                      /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                          length(which(conf_congreso== 'Otra'))))


df_conf_ffaa_2012 <- df2012_67 %>%group_by(year, esc)%>%summarise(porc_conf_ffaa= length(which(conf_ffaa== 'Mucha confianza'))
                                                                  /(length(which(conf_ffaa== 'Mucha confianza'))+
                                                                      length(which(conf_ffaa== 'Otra'))))


df_conf_empresas_2012 <- df2012_67 %>%group_by(year, esc)%>%summarise(porc_conf_empresas= length(which(conf_emppriv== 'Mucha confianza'))
                                                                      /(length(which(conf_emppriv== 'Mucha confianza'))+
                                                                          length(which(conf_emppriv== 'Otra'))))


df_conf_tribunales_2012 <- df2012_67 %>%group_by(year, esc)%>%summarise(porc_conf_tribunales= length(which(conf_tribunalesjust== 'Mucha confianza'))
                                                                        /(length(which(conf_tribunalesjust== 'Mucha confianza'))+
                                                                            length(which(conf_tribunalesjust== 'Otra'))))

df_conf_mmc_2012 <- df2012_67 %>%group_by(year, esc)%>%summarise(porc_conf_mmc= length(which(conf_mmc== 'Mucha confianza'))
                                                                 /(length(which(conf_mmc== 'Mucha confianza'))+
                                                                     length(which(conf_mmc== 'Otra'))))
# ---- 2013 ----

df_conf_partidos_2013<- df2013_69 %>%group_by(year, esc)%>%summarise(porc_conf_partidos= length(which(conf_partidos== 'Mucha confianza'))
                                                                     /(length(which(conf_partidos== 'Mucha confianza'))+
                                                                         length(which(conf_partidos== 'Otra'))))



df_conf_iglesia_2013<- df2013_69 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                    /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                        length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_2013<- df2013_69 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                     /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                         length(which(conf_congreso== 'Otra'))))


df_conf_ffaa_2013<- df2013_69 %>%group_by(year, esc)%>%summarise(porc_conf_ffaa= length(which(conf_ffaa== 'Mucha confianza'))
                                                                 /(length(which(conf_ffaa== 'Mucha confianza'))+
                                                                     length(which(conf_ffaa== 'Otra'))))


df_conf_empresas_2013<- df2013_69 %>%group_by(year, esc)%>%summarise(porc_conf_empresas= length(which(conf_emppriv== 'Mucha confianza'))
                                                                     /(length(which(conf_emppriv== 'Mucha confianza'))+
                                                                         length(which(conf_emppriv== 'Otra'))))


df_conf_tribunales_2013<- df2013_69 %>%group_by(year, esc)%>%summarise(porc_conf_tribunales= length(which(conf_tribunalesjust== 'Mucha confianza'))
                                                                       /(length(which(conf_tribunalesjust== 'Mucha confianza'))+
                                                                           length(which(conf_tribunalesjust== 'Otra'))))

df_conf_mmc_2013<- df2013_69 %>%group_by(year, esc)%>%summarise(porc_conf_mmc= length(which(conf_mmc== 'Mucha confianza'))
                                                                /(length(which(conf_mmc== 'Mucha confianza'))+
                                                                    length(which(conf_mmc== 'Otra'))))
# ---- 2014 ----


df_conf_partidos_2014 <- df2014_71 %>%group_by(year, esc)%>%summarise(porc_conf_partidos= length(which(conf_partidos== 'Mucha confianza'))
                                                                      /(length(which(conf_partidos== 'Mucha confianza'))+
                                                                          length(which(conf_partidos== 'Otra'))))



df_conf_iglesia_2014 <- df2014_71 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                     /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                         length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_2014 <- df2014_71 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                      /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                          length(which(conf_congreso== 'Otra'))))


df_conf_ffaa_2014 <- df2014_71 %>%group_by(year, esc)%>%summarise(porc_conf_ffaa= length(which(conf_ffaa== 'Mucha confianza'))
                                                                  /(length(which(conf_ffaa== 'Mucha confianza'))+
                                                                      length(which(conf_ffaa== 'Otra'))))


df_conf_empresas_2014 <- df2014_71 %>%group_by(year, esc)%>%summarise(porc_conf_empresas= length(which(conf_emppriv== 'Mucha confianza'))
                                                                      /(length(which(conf_emppriv== 'Mucha confianza'))+
                                                                          length(which(conf_emppriv== 'Otra'))))


df_conf_tribunales_2014 <- df2014_71 %>%group_by(year, esc)%>%summarise(porc_conf_tribunales= length(which(conf_tribunalesjust== 'Mucha confianza'))
                                                                        /(length(which(conf_tribunalesjust== 'Mucha confianza'))+
                                                                            length(which(conf_tribunalesjust== 'Otra'))))

df_conf_mmc_2014 <- df2014_71 %>%group_by(year, esc)%>%summarise(porc_conf_mmc= length(which(conf_mmc== 'Mucha confianza'))
                                                                 /(length(which(conf_mmc== 'Mucha confianza'))+
                                                                     length(which(conf_mmc== 'Otra'))))
# ---- 2017 ----


df_conf_partidos_2017 <- df2017_79 %>%group_by(year, esc)%>%summarise(porc_conf_partidos= length(which(conf_partidos== 'Mucha confianza'))
                                                                      /(length(which(conf_partidos== 'Mucha confianza'))+
                                                                          length(which(conf_partidos== 'Otra'))))



df_conf_iglesia_2017 <- df2017_79 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                     /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                         length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_2017 <- df2017_79 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                      /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                          length(which(conf_congreso== 'Otra'))))


df_conf_ffaa_2017 <- df2017_79 %>%group_by(year, esc)%>%summarise(porc_conf_ffaa= length(which(conf_ffaa== 'Mucha confianza'))
                                                                  /(length(which(conf_ffaa== 'Mucha confianza'))+
                                                                      length(which(conf_ffaa== 'Otra'))))


df_conf_empresas_2017 <- df2017_79 %>%group_by(year, esc)%>%summarise(porc_conf_empresas= length(which(conf_emppriv== 'Mucha confianza'))
                                                                      /(length(which(conf_emppriv== 'Mucha confianza'))+
                                                                          length(which(conf_emppriv== 'Otra'))))


df_conf_tribunales_2017 <- df2017_79 %>%group_by(year, esc)%>%summarise(porc_conf_tribunales= length(which(conf_tribunalesjust== 'Mucha confianza'))
                                                                        /(length(which(conf_tribunalesjust== 'Mucha confianza'))+
                                                                            length(which(conf_tribunalesjust== 'Otra'))))

df_conf_mmc_2017 <- df2017_79 %>%group_by(year, esc)%>%summarise(porc_conf_mmc= length(which(conf_mmc== 'Mucha confianza'))
                                                                 /(length(which(conf_mmc== 'Mucha confianza'))+
                                                                     length(which(conf_mmc== 'Otra'))))

# ---- 2019 ----


df_conf_partidos_2019<- df2019_84 %>%group_by(year, esc)%>%summarise(porc_conf_partidos= length(which(conf_partidos== 'Mucha confianza'))
                                                                     /(length(which(conf_partidos== 'Mucha confianza'))+
                                                                         length(which(conf_partidos== 'Otra'))))



df_conf_iglesia_2019<- df2019_84 %>%group_by(year, esc)%>%summarise(porc_conf_iglesia= length(which(conf_iglesia== 'Mucha confianza'))
                                                                    /(length(which(conf_iglesia== 'Mucha confianza'))+
                                                                        length(which(conf_iglesia== 'Otra'))))



df_conf_congreso_2019<- df2019_84 %>%group_by(year, esc)%>%summarise(porc_conf_congreso= length(which(conf_congreso== 'Mucha confianza'))
                                                                     /(length(which(conf_congreso== 'Mucha confianza'))+
                                                                         length(which(conf_congreso== 'Otra'))))


df_conf_ffaa_2019<- df2019_84 %>%group_by(year, esc)%>%summarise(porc_conf_ffaa= length(which(conf_ffaa== 'Mucha confianza'))
                                                                 /(length(which(conf_ffaa== 'Mucha confianza'))+
                                                                     length(which(conf_ffaa== 'Otra'))))


df_conf_empresas_2019<- df2019_84 %>%group_by(year, esc)%>%summarise(porc_conf_empresas= length(which(conf_emppriv== 'Mucha confianza'))
                                                                     /(length(which(conf_emppriv== 'Mucha confianza'))+
                                                                         length(which(conf_emppriv== 'Otra'))))


df_conf_tribunales_2019<- df2019_84 %>%group_by(year, esc)%>%summarise(porc_conf_tribunales= length(which(conf_tribunalesjust== 'Mucha confianza'))
                                                                       /(length(which(conf_tribunalesjust== 'Mucha confianza'))+
                                                                           length(which(conf_tribunalesjust== 'Otra'))))

df_conf_mmc_2019<- df2019_84 %>%group_by(year, esc)%>%summarise(porc_conf_mmc= length(which(conf_mmc== 'Mucha confianza'))
                                                                /(length(which(conf_mmc== 'Mucha confianza'))+
                                                                    length(which(conf_mmc== 'Otra'))))


# ---- 3.3 Merge bases de datos ----
# Bateria personas
df_conf_congreso_personas <- do.call("rbind", list(df_conf_congreso_1990, df_conf_congreso_1991, df_conf_congreso_1992, df_conf_congreso_2002))
df_conf_diremp_personas <- do.call("rbind", list(df_conf_diremp_1990, df_conf_diremp_1991, df_conf_diremp_1992, df_conf_diremp_2002))
df_conf_altosffaa_personas <- do.call("rbind", list(df_conf_altosffaa_1990, df_conf_altosffaa_1991, df_conf_altosffaa_1992, df_conf_altosffaa_2002))
df_conf_iglesia_personas <- do.call("rbind", list(df_conf_iglesia_1990, df_conf_iglesia_1991, df_conf_iglesia_1992, df_conf_iglesia_2002))

df_conf_dirpol_personas <- do.call("rbind", list(df_conf_dirpol_1990, df_conf_dirpol_1991, df_conf_dirpol_1992, df_conf_dirpol_2002))
df_conf_dircortesup_personas <- do.call("rbind", list(df_conf_dircortesup_1990, df_conf_dircortesup_1991, df_conf_dircortesup_1992, df_conf_dircortesup_2002))


# Bateria instituciones  

df_conf_congreso_inst <- do.call("rbind", list(df_conf_congreso_2002, df_conf_congreso_2003, df_conf_congreso_2008, df_conf_congreso_2009, df_conf_congreso_2012, df_conf_congreso_2013, df_conf_congreso_2014, df_conf_congreso_2017, df_conf_congreso_2019))

df_conf_empresas_inst <- do.call("rbind", list(df_conf_empresas_2002, df_conf_empresas_2003, df_conf_empresas_2008, df_conf_empresas_2009, df_conf_empresas_2012, df_conf_empresas_2013, df_conf_empresas_2014, df_conf_empresas_2017, df_conf_empresas_2019))

df_conf_ffaa_inst <- do.call("rbind", list(df_conf_ffaa_2002, df_conf_ffaa_2003, df_conf_ffaa_2008, df_conf_ffaa_2009, df_conf_ffaa_2012, df_conf_ffaa_2013, df_conf_ffaa_2014, df_conf_ffaa_2017, df_conf_ffaa_2019))

df_conf_iglesia_inst <- do.call("rbind", list(df_conf_iglesia_2002, df_conf_iglesia_2003, df_conf_iglesia_2008, df_conf_iglesia_2009, df_conf_iglesia_2012, df_conf_iglesia_2013, df_conf_iglesia_2014, df_conf_iglesia_2017, df_conf_iglesia_2019))

df_conf_mmc_inst <- do.call("rbind", list(df_conf_mmc_2002, df_conf_mmc_2003, df_conf_mmc_2008, df_conf_mmc_2009, df_conf_mmc_2012, df_conf_mmc_2013, df_conf_mmc_2014, df_conf_mmc_2017, df_conf_mmc_2019))

df_conf_partidos_inst <- do.call("rbind", list(df_conf_partidos_2002, df_conf_partidos_2003, df_conf_partidos_2008, df_conf_partidos_2009, df_conf_partidos_2012, df_conf_partidos_2013, df_conf_partidos_2014, df_conf_partidos_2017, df_conf_partidos_2019))

df_conf_tribunales_inst <- do.call("rbind", list(df_conf_tribunales_2002, df_conf_tribunales_2003, df_conf_tribunales_2008, df_conf_tribunales_2009, df_conf_tribunales_2009, df_conf_tribunales_2012, df_conf_tribunales_2013, df_conf_tribunales_2014, df_conf_tribunales_2017, df_conf_tribunales_2019))


# Merge all inst

df_conf_cep_inst_esc  <- merge(merge(merge(merge(merge(merge(
  df_conf_tribunales_inst, df_conf_mmc_inst, by = c("year", "esc"), all = TRUE),
  df_conf_ffaa_inst, by = c("year", "esc"), all = TRUE),
  df_conf_iglesia_inst, by = c("year", "esc"), all = TRUE),
  df_conf_empresas_inst, by = c("year", "esc"), all = TRUE),
  df_conf_partidos_inst, by = c("year", "esc"), all = TRUE),
  df_conf_congreso_inst, by = c("year", "esc"), all = TRUE)

# Merge all per

df_conf_cep_per_esc  <- merge(merge(merge(merge(merge(
  df_conf_dirpol_personas, df_conf_altosffaa_personas, by = c("year", "esc"), all = TRUE),
  df_conf_diremp_personas, by = c("year", "esc"), all = TRUE),
  df_conf_iglesia_personas, by = c("year", "esc"), all = TRUE),
  df_conf_congreso_personas, by = c("year", "esc"), all = TRUE),
  df_conf_dircortesup_personas, by = c("year", "esc"), all = TRUE)

# Eliminar rows repetidas y categorias NA

df_conf_cep_inst_esc <- df_conf_cep_inst_esc[-c(5, 10, 15, 16, 18, 20, 22, 24, 30, 35, 40, 45, 50),]
df_conf_cep_per_esc <- df_conf_cep_per_esc[-c(5, 10, 15, 20),]


# Test grafic                
df_conf_partidos_inst %>%
  tail(10) %>%
  ggplot( aes(x=year, y=porc_conf_partidos)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("Evolucion de Mucha confianza en el congreso")


partidos <- ggplot(data = df_conf_partidos_inst,
                   mapping = aes(x = year,
                                 y = porc_conf_partidos)) + geom_point(size=3) +   ggtitle("Evolucion de Mucha confianza en los partidos")

partidos


# Save database
#save(df_conf_cep_inst_esc, file = "output/Confianza-Mucha-Conf/Mucha-Conf-Escolaridad/CEP-Mucha-Conf-Inst-Escolaridad.RData")
#write.csv(df_conf_cep_inst_esc, "output/Confianza-Mucha-Conf/Mucha-Conf-Escolaridad/CEP-Mucha-Conf-Inst-Escolaridad.csv")
write.xlsx(df_conf_cep_inst_esc, "output/Confianza-Mucha-Conf/Mucha-Conf-Escolaridad/CEP-Mucha-Conf-Inst-Escolaridad.xlsx")

#save(df_conf_cep_per_esc, file = "output/Confianza-Mucha-Conf/Mucha-Conf-Escolaridad/CEP-Mucha-Conf-Per-Escolaridad.RData")
#write.csv(df_conf_cep_per_esc, "output/Confianza-Mucha-Conf/Mucha-Conf-Escolaridad/CEP-Mucha-Conf-Per-Escolaridad.csv")
write.xlsx(df_conf_cep_per_esc, "output/Confianza-Mucha-Conf/Mucha-Conf-Escolaridad/CEP-Mucha-Conf-Per-Escolaridad.xlsx")