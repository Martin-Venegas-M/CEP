# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 23 - 10 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car)

# ---- Etapa 2: Cargar bases de datos ---
load("input/data/bd1990_14.RData") #1990
load("input/data/bd1990_15.RData") #1990
load("input/data/bd1990_16.RData") #1990

load("input/data/bd1991_1993_17.RData") #1991
load("input/data/bd1991_1993_18.RData") #1991
load("input/data/bd1991_1993_19.RData") #1991
load("input/data/bd1991_1993_20.RData") #1991
load("input/data/bd1991_1993_21.RData") #1992
load("input/data/bd1991_1993_22.RData") #1992
load("input/data/bd1991_1993_23.RData") #1992
load("input/data/bd1991_1993_24.RData") #1993
load("input/data/bd1991_1993_25.RData") #1993
load("input/data/bd1991_1993_28.RData") #1993

load("input/data/bd1994_1996_29.RData") #1994
load("input/data/bd1994_1996_30.RData") #1995
load("input/data/bd1994_1996_31.RData") #1995
load("input/data/bd1994_1996_32.RData") #1996
load("input/data/bd1994_1996_33.RData") #1996

load("input/data/bd1997_1999_34.RData") #1997
load("input/data/bd1997_1999_35.RData") #1997
load("input/data/bd1997_1999_36.RData") #1998
load("input/data/bd1997_1999_37.RData") #1999
load("input/data/bd1997_1999_38.RData") #1999

load("input/data/bd2000_2002_39.RData") #2000
load("input/data/bd2000_2002_40.RData") #2000
load("input/data/bd2000_2002_41.RData") #2001
load("input/data/bd2000_2002_42.RData") #2001
load("input/data/bd2000_2002_43.RData") #2002
load("input/data/bd2000_2002_44.RData") #2002

load("input/data/bd2003_2005_45.RData") #2003
load("input/data/bd2003_2005_46.RData") #2003
load("input/data/bd2003_2005_47.RData") #2004
load("input/data/bd2003_2005_48.RData") #2004
load("input/data/bd2003_2005_49.RData") #2005
load("input/data/bd2003_2005_50.RData") #2005
load("input/data/bd2003_2005_51.RData") #2005

load("input/data/bd2006_2008_52.RData") #2006
load("input/data/bd2006_2008_54.RData") #2006
load("input/data/bd2006_2008_55.RData") #2007
load("input/data/bd2006_2008_56.RData") #2007
load("input/data/bd2006_2008_57.RData") #2008
load("input/data/bd2006_2008_58.RData") #2008

load("input/data/bd2009_2011_59.RData") #2009
load("input/data/bd2009_2011_60.RData") #2009
load("input/data/bd2009_2011_61.RData") #2009
load("input/data/bd2009_2011_62.RData") #2010
load("input/data/bd2009_2011_63.RData") #2010
load("input/data/bd2009_2011_64.RData") #2011
load("input/data/bd2009_2011_65.RData") #2011

load("input/data/bd2012_2014_66.RData") #2012
load("input/data/bd2012_2014_67.RData") #2012
load("input/data/bd2012_2014_68.RData") #2012
load("input/data/bd2012_2014_69.RData") #2013
load("input/data/bd2012_2014_70.RData") #2013
load("input/data/bd2012_2014_71.RData") #2014
load("input/data/bd2012_2014_72.RData") #2014

load("input/data/bd2015_2017_73.RData") #2015
load("input/data/bd2015_2017_74.RData") #2015
load("input/data/bd2015_2017_75.RData") #2015
load("input/data/bd2015_2017_76.RData") #2016
load("input/data/bd2015_2017_77.RData") #2016
load("input/data/bd2015_2017_78.RData") #2016
load("input/data/bd2015_2017_79.RData") #2017
load("input/data/bd2015_2017_80.RData") #2017
load("input/data/bd2015_2017_81.RData") #2017

load("input/data/bd2018_2019_82.RData") #2018
load("input/data/bd2018_2019_83.RData") #2019
load("input/data/bd2018_2019_84.RData") #2019

# Cambiar nombres de las bases de datos

df1990_14 <- bd1990_14 #1990
df1990_15 <- bd1990_15 #1990
df1990_16 <- bd1990_16 #1990

df1991_17 <- bd1991_1993_17 #1991
df1991_18 <- bd1991_1993_18 #1991
df1991_19 <- bd1991_1993_19 #1991
df1991_20 <- bd1991_1993_20 #1991
df1992_21 <- bd1991_1993_21 #1992
df1992_22 <- bd1991_1993_22 #1992
df1992_23 <- bd1991_1993_23 #1992
df1993_24 <- bd1991_1993_24 #1993
df1993_25 <- bd1991_1993_25 #1993
df1993_28 <- bd1991_1993_28 #1993

df1994_29 <- bd1994_1996_29 #1994
df1995_30 <- bd1994_1996_30 #1995
df1995_31 <- bd1994_1996_31 #1995
df1996_32 <- bd1994_1996_32 #1996
df1996_33 <- bd1994_1996_33 #1996

df1997_34 <- bd1997_1999_34 #1997
df1997_35 <- bd1997_1999_35 #1997
df1998_36 <- bd1997_1999_36 #1998
df1999_37 <- bd1997_1999_37 #1999
df1999_38 <- bd1997_1999_38 #1999

df2000_39 <- bd2000_2002_39 #2000
df2000_40 <- bd2000_2002_40 #2000
df2001_41 <- bd2000_2002_41 #2001
df2001_42 <- bd2000_2002_42 #2001
df2002_43 <- bd2000_2002_43 #2002
df2002_44 <- bd2000_2002_44 #2002

df2003_45 <- bd2003_2005_45 #2003
df2003_46 <- bd2003_2005_46 #2003
df2004_47 <- bd2003_2005_47 #2004
df2004_48 <- bd2003_2005_48 #2004
df2005_49 <- bd2003_2005_49 #2005
df2005_50 <- bd2003_2005_50 #2005
df2005_51 <- bd2003_2005_51 #2005

df2006_52 <- bd2006_2008_52 #2006
df2006_54 <- bd2006_2008_54 #2006
df2007_55 <- bd2006_2008_55 #2007
df2007_56 <- bd2006_2008_56 #2007
df2008_57 <- bd2006_2008_57 #2008
df2008_58 <- bd2006_2008_58 #2008

df2009_59 <- bd2009_2011_59 #2009
df2009_60 <- bd2009_2011_60 #2009
df2009_61 <- bd2009_2011_61 #2009
df2010_62 <- bd2009_2011_62 #2010
df2010_63 <- bd2009_2011_63 #2010
df2011_64 <- bd2009_2011_64 #2011
df2011_65 <- bd2009_2011_65 #2011

df2012_66 <- bd2012_2014_66 #2012
df2012_67 <- bd2012_2014_67 #2012
df2012_68 <- bd2012_2014_68 #2012
df2013_69 <- bd2012_2014_69 #2013
df2013_70 <- bd2012_2014_70 #2013
df2014_71 <- bd2012_2014_71 #2014
df2014_72 <- bd2012_2014_72 #2014

df2015_73 <- bd2015_2017_73 #2015
df2015_74 <- bd2015_2017_74 #2015
df2015_75 <- bd2015_2017_75 #2015
df2016_76 <- bd2015_2017_76 #2016
df2016_77 <- bd2015_2017_77 #2016
df2016_78 <- bd2015_2017_78 #2016
df2017_79 <- bd2015_2017_79 #2017
df2017_80 <- bd2015_2017_80 #2017
df2017_81 <- bd2015_2017_81 #2017

df2018_82 <- bd2018_2019_82 #2018
df2019_83 <- bd2018_2019_83 #2019
df2019_84 <- bd2018_2019_84 #2019

#---- Etapa 3: Procesamiento de datos ----
#---- 3.1 Añadir variable año----

df1990_14$year <- "1990"
df1990_15$year <- "1990"
df1990_16$year <- "1990"

df1991_17$year <- "1991"
df1991_18$year <- "1991"
df1991_19$year <- "1991"
df1991_20$year <- "1991"
df1992_21$year <- "1992"
df1992_22$year <- "1992"
df1992_23$year <- "1992"
df1993_24$year <- "1993"
df1993_25$year <- "1993"
df1993_28$year <- "1993"

df1994_29$year <- "1994"
df1995_30$year <- "1995"
df1995_31$year <- "1995"
df1996_32$year <- "1996"
df1996_33$year <- "1996"

df1997_34$year <- "1997"
df1997_35$year <- "1997"
df1998_36$year <- "1998"
df1999_37$year <- "1999"
df1999_38$year <- "1999"

df2000_39$year <- "2000"
df2000_40$year <- "2000"
df2001_41$year <- "2001"
df2001_42$year <- "2001"
df2002_43$year <- "2002"
df2002_44$year <- "2002"

df2003_45$year <- "2003"
df2003_46$year <- "2003"
df2004_47$year <- "2004"
df2004_48$year <- "2004"
df2005_49$year <- "2005"
df2005_50$year <- "2005"
df2005_51$year <- "2005"

df2006_52$year <- "2006"
df2006_54$year <- "2006"
df2007_55$year <- "2007"
df2007_56$year <- "2007"
df2008_57$year <- "2008"
df2008_58$year <- "2008"

df2009_59$year <- "2009"
df2009_60$year <- "2009"
df2009_61$year <- "2009"
df2010_62$year <- "2010"
df2010_63$year <- "2010"
df2011_64$year <- "2011"
df2011_65$year <- "2011"

df2012_66$year <- "2012"
df2012_67$year <- "2012"
df2012_68$year <- "2012"
df2013_69$year <- "2013"
df2013_70$year <- "2013"
df2014_71$year <- "2014"
df2014_72$year <- "2014"

df2015_73$year <- "2015"
df2015_74$year <- "2015"
df2015_75$year <- "2015"
df2016_76$year <- "2016"
df2016_77$year <- "2016"
df2016_78$year <- "2016"
df2017_79$year <- "2017"
df2017_80$year <- "2017"
df2017_81$year <- "2017"

df2018_82$year <- "2018"
df2019_83$year <- "2019"
df2019_84$year <- "2019"

rm(list = ls()[grep("bd", ls())])

#---- 3.2 Group and summarise bases de datos ----
# Derecha

# 1990

df_idpart_der_1990_14 <- df1990_14 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1990_15 <- df1990_15 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1990_16 <- df1990_16 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Prom

df_idpart_der_1990_14$porc_idpart_der <- (df_idpart_der_1990_14$porc_idpart_der + df_idpart_der_1990_15$porc_idpart_der + df_idpart_der_1990_16$porc_idpart_der)/3
df_idpart_der_1990 <- df_idpart_der_1990_14

# 1991-1993 

df_idpart_der_1991_17 <- df1991_17 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1991_18 <- df1991_18 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1991_19 <- df1991_19 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1991_20 <- df1991_20 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1992_21 <- df1992_21 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1992_22 <- df1992_22 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1992_23 <- df1992_23 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1993_24 <- df1993_24 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1993_25 <- df1993_25 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))


df_idpart_der_1993_28 <- df1993_28 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Proms
df_idpart_der_1991_18 <- df_idpart_der_1991_18[-5,]

df_idpart_der_1991_17$porc_idpart_der <- (df_idpart_der_1991_17$porc_idpart_der + df_idpart_der_1991_18$porc_idpart_der + df_idpart_der_1991_19$porc_idpart_der + df_idpart_der_1991_20$porc_idpart_der)/4
df_idpart_der_1991 <- df_idpart_der_1991_17

df_idpart_der_1992_21$porc_idpart_der <- (df_idpart_der_1992_21$porc_idpart_der + df_idpart_der_1992_22$porc_idpart_der + df_idpart_der_1992_23$porc_idpart_der)/3
df_idpart_der_1992 <- df_idpart_der_1992_21

df_idpart_der_1993_24$porc_idpart_der <- (df_idpart_der_1993_24$porc_idpart_der + df_idpart_der_1993_25$porc_idpart_der + df_idpart_der_1993_28$porc_idpart_der)/3
df_idpart_der_1993 <- df_idpart_der_1993_24

# 1994-1996

df_idpart_der_1994_29 <- df1994_29 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1995_30 <- df1995_30 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1995_31 <- df1995_31 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1996_32 <- df1996_32 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1996_33 <- df1996_33 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Proms
df_idpart_der_1995_31[nrow(df_idpart_der_1995_31)+1,] <- NA
df_idpart_der_1996_32[nrow(df_idpart_der_1996_32)+1,] <- NA
df_idpart_der_1996_33[nrow(df_idpart_der_1996_33)+1,] <- NA

df_idpart_der_1994 <- df_idpart_der_1994_29

df_idpart_der_1995_30$porc_idpart_der <- (df_idpart_der_1995_30$porc_idpart_der + df_idpart_der_1995_31$porc_idpart_der)/2
df_idpart_der_1995 <- df_idpart_der_1995_30

df_idpart_der_1996_32$porc_idpart_der <- (df_idpart_der_1996_32$porc_idpart_der + df_idpart_der_1996_33$porc_idpart_der)/2
df_idpart_der_1996 <- df_idpart_der_1996_32

# 1997 - 1999

df_idpart_der_1997_34 <- df1997_34 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1997_35 <- df1997_35 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1998_36 <- df1998_36 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1999_37 <- df1999_37 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_1999_38 <- df1999_38 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Prom

df_idpart_der_1997_34$porc_idpart_der <- (df_idpart_der_1997_34$porc_idpart_der + df_idpart_der_1997_35$porc_idpart_der)/2
df_idpart_der_1997 <- df_idpart_der_1997_34

df_idpart_der_1998 <- df_idpart_der_1998_36

df_idpart_der_1999_37$porc_idpart_der <- (df_idpart_der_1999_37$porc_idpart_der + df_idpart_der_1999_38$porc_idpart_der)/2
df_idpart_der_1999 <- df_idpart_der_1999_37



#2000 - 2002

df_idpart_der_2000_39 <- df2000_39 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2000_40 <- df2000_40 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2001_41 <- df2001_41 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2001_42 <- df2001_42 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2002_43 <- df2002_43 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2002_44 <- df2002_44 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Proms

df_idpart_der_2000_39$porc_idpart_der <- (df_idpart_der_2000_39$porc_idpart_der + df_idpart_der_2000_40$porc_idpart_der)/2
df_idpart_der_2000 <- df_idpart_der_2000_39

df_idpart_der_2001_41$porc_idpart_der <- (df_idpart_der_2001_41$porc_idpart_der + df_idpart_der_2001_42$porc_idpart_der)/2
df_idpart_der_2001 <- df_idpart_der_2001_41

df_idpart_der_2002_43$porc_idpart_der <- (df_idpart_der_2002_43$porc_idpart_der + df_idpart_der_2002_44$porc_idpart_der)/2
df_idpart_der_2002 <- df_idpart_der_2002_43

# 2003 - 2005

df_idpart_der_2003_45 <- df2003_45 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2003_46 <- df2003_46 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2004_47 <- df2004_47 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2004_48 <- df2004_48 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2005_49 <- df2005_49 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2005_50 <- df2005_50 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2005_51 <- df2005_51 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Proms

df_idpart_der_2003_45$porc_idpart_der <- (df_idpart_der_2003_45$porc_idpart_der + df_idpart_der_2003_46$porc_idpart_der)/2
df_idpart_der_2003 <- df_idpart_der_2003_45

df_idpart_der_2004_47$porc_idpart_der <- (df_idpart_der_2004_47$porc_idpart_der + df_idpart_der_2004_48$porc_idpart_der)/2
df_idpart_der_2004 <- df_idpart_der_2004_47

df_idpart_der_2005_49$porc_idpart_der <- (df_idpart_der_2005_49$porc_idpart_der + df_idpart_der_2005_50$porc_idpart_der + df_idpart_der_2005_51$porc_idpart_der)/3
df_idpart_der_2005 <- df_idpart_der_2005_49

# 2006-2008

df_idpart_der_2006_52 <- df2006_52 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2006_54 <- df2006_54 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2007_55 <- df2007_55 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2007_56 <- df2007_56 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2008_57 <- df2008_57 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2008_58 <- df2008_58 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

#Prom

df_idpart_der_2006_52$porc_idpart_der <- (df_idpart_der_2006_52$porc_idpart_der + df_idpart_der_2006_54$porc_idpart_der)/2
df_idpart_der_2006 <- df_idpart_der_2006_52

df_idpart_der_2007_55$porc_idpart_der <- (df_idpart_der_2007_55$porc_idpart_der + df_idpart_der_2007_56$porc_idpart_der)/2
df_idpart_der_2007 <- df_idpart_der_2007_55

df_idpart_der_2008_57$porc_idpart_der <- (df_idpart_der_2008_57$porc_idpart_der + df_idpart_der_2008_58$porc_idpart_der)/2
df_idpart_der_2008 <- df_idpart_der_2008_57

# 2009 - 2011

df_idpart_der_2009_59 <- df2009_59 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2009_60 <- df2009_60 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2009_61 <- df2009_61 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2010_62 <- df2010_62 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2010_63 <- df2010_63 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2011_64 <- df2011_64 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2011_65 <- df2011_65 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

#Prom

df_idpart_der_2009_59$porc_idpart_der <- (df_idpart_der_2009_59$porc_idpart_der + df_idpart_der_2009_60$porc_idpart_der + df_idpart_der_2009_61$porc_idpart_der)/3
df_idpart_der_2009 <- df_idpart_der_2009_59


df_idpart_der_2010_62$porc_idpart_der <- (df_idpart_der_2010_62$porc_idpart_der + df_idpart_der_2010_63$porc_idpart_der)/2
df_idpart_der_2010 <- df_idpart_der_2010_62


df_idpart_der_2011_64$porc_idpart_der <- (df_idpart_der_2011_64$porc_idpart_der + df_idpart_der_2011_65$porc_idpart_der)/2
df_idpart_der_2011 <- df_idpart_der_2011_64

# 2012 - 2014

df_idpart_der_2012_66 <- df2012_66 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2012_67 <- df2012_67 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2012_68 <- df2012_68 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2013_69 <- df2013_69 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2013_70 <- df2013_70 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2014_71 <- df2014_71 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2014_72 <- df2014_72 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Prom

df_idpart_der_2012_66$porc_idpart_der <- (df_idpart_der_2012_66$porc_idpart_der + df_idpart_der_2012_67$porc_idpart_der + df_idpart_der_2012_68$porc_idpart_der)/3
df_idpart_der_2012 <- df_idpart_der_2012_66

df_idpart_der_2013_69$porc_idpart_der <- (df_idpart_der_2013_69$porc_idpart_der + df_idpart_der_2013_70$porc_idpart_der)/2
df_idpart_der_2013 <- df_idpart_der_2013_69

df_idpart_der_2014_71$porc_idpart_der <- (df_idpart_der_2014_71$porc_idpart_der + df_idpart_der_2014_72$porc_idpart_der)/2
df_idpart_der_2014 <- df_idpart_der_2014_71

# 2015 - 2017

df_idpart_der_2015_73 <- df2015_73 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2015_74 <- df2015_74 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2015_75 <- df2015_75 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

#df_idpart_der_2016_76 <- df2016_76 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
#length(which(id_part == "Derecha"))
#                                               /(length(which(id_part == "Centro-Izquierda concertación"))+
#                                                  length(which(id_part == "Izquierda extraconcertación"))+
#                                                 length(which(id_part == "Ninguno"))+
#                                                length(which(id_part == "Derecha"))))

df_idpart_der_2016_77 <- df2016_77 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2016_78 <- df2016_78 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

#df_idpart_der_2017_79 <- df2017_79 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
#                                                  length(which(id_part == "Derecha"))
#                                               /(length(which(id_part == "Centro-Izquierda concertación"))+
#                                                  length(which(id_part == "Izquierda extraconcertación"))+
#                                                 length(which(id_part == "Ninguno"))+
#                                                length(which(id_part == "Derecha"))))

#df_idpart_der_2017_80 <- df2017_80 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
#                                                  length(which(id_part == "Derecha"))
#                                               /(length(which(id_part == "Centro-Izquierda concertación"))+
#                                                  length(which(id_part == "Izquierda extraconcertación"))+
#                                                 length(which(id_part == "Ninguno"))+
#                                                length(which(id_part == "Derecha"))))

df_idpart_der_2017_81 <- df2017_81 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Proms

#df_idpart_der__$porc_idpart_der <- (df_idpart_der__$porc_idpart_der + df_idpart_der__$porc_idpart_der + df_idpart_der__$porc_idpart_der)/3
#df_idpart_der_ <- df_idpart_der__

df_idpart_der_2015_73$porc_idpart_der <- (df_idpart_der_2015_73$porc_idpart_der + df_idpart_der_2015_74$porc_idpart_der + df_idpart_der_2015_75$porc_idpart_der)/3
df_idpart_der_2015 <- df_idpart_der_2015_73

df_idpart_der_2016_77$porc_idpart_der <- (df_idpart_der_2016_77$porc_idpart_der + df_idpart_der_2016_78$porc_idpart_der)/2
df_idpart_der_2016 <- df_idpart_der_2016_77

df_idpart_der_2017 <- df_idpart_der_2017_81

# 2018 - 2019

df_idpart_der_2018_82 <- df2018_82 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2019_83 <- df2019_83 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_der_2019_84 <- df2019_84 %>%group_by(year, edad)%>%summarise(porc_idpart_der = 
                                                                        length(which(id_part == "Derecha"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# prom

df_idpart_der_2018 <- df_idpart_der_2018_82

df_idpart_der_2019_83$porc_idpart_der <- (df_idpart_der_2019_83$porc_idpart_der + df_idpart_der_2019_84$porc_idpart_der)/2
df_idpart_der_2019 <- df_idpart_der_2019_83

# Crear base

df_idpart_der <- do.call("rbind", list(df_idpart_der_1990,
                                       df_idpart_der_1991, 
                                       df_idpart_der_1992, 
                                       df_idpart_der_1993, 
                                       df_idpart_der_1994, 
                                       df_idpart_der_1995, 
                                       df_idpart_der_1996, 
                                       df_idpart_der_1997, 
                                       df_idpart_der_1998, 
                                       df_idpart_der_1999, 
                                       df_idpart_der_2000, 
                                       df_idpart_der_2001, 
                                       df_idpart_der_2002, 
                                       df_idpart_der_2003, 
                                       df_idpart_der_2004, 
                                       df_idpart_der_2005, 
                                       df_idpart_der_2006, 
                                       df_idpart_der_2007, 
                                       df_idpart_der_2008, 
                                       df_idpart_der_2009, 
                                       df_idpart_der_2010, 
                                       df_idpart_der_2011, 
                                       df_idpart_der_2012, 
                                       df_idpart_der_2013, 
                                       df_idpart_der_2014, 
                                       df_idpart_der_2015, 
                                       df_idpart_der_2016, 
                                       df_idpart_der_2017, 
                                       df_idpart_der_2018, 
                                       df_idpart_der_2019))

# Save data base

# Save database
#save(df_idpart_der, file = "output/Idpart/Derecha/Derecha-Edad/CEP-Idpart-Derecha_Edad.RData")
#write.csv(df_idpart_der, "output/Idpart/Derecha/Derecha-Edad/CEP-Idpart-Derecha_Edad.csv")
write.xlsx(df_idpart_der, "output/Idpart/Derecha/Derecha-Edad/CEP-Idpart-Derecha_Edad.xlsx")

# Centro izquierda concertación

# 1990

df_idpart_centizq_1990_14 <- df1990_14 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1990_15 <- df1990_15 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1990_16 <- df1990_16 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

# Prom

df_idpart_centizq_1990_14$porc_idpart_centizq <- (df_idpart_centizq_1990_14$porc_idpart_centizq + df_idpart_centizq_1990_15$porc_idpart_centizq + df_idpart_centizq_1990_16$porc_idpart_centizq)/3
df_idpart_centizq_1990 <- df_idpart_centizq_1990_14

# 1991-1993 

df_idpart_centizq_1991_17 <- df1991_17 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1991_18 <- df1991_18 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1991_19 <- df1991_19 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1991_20 <- df1991_20 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1992_21 <- df1992_21 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1992_22 <- df1992_22 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1992_23 <- df1992_23 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1993_24 <- df1993_24 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1993_25 <- df1993_25 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))


df_idpart_centizq_1993_28 <- df1993_28 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

# Proms
df_idpart_centizq_1991_18 <- df_idpart_centizq_1991_18[-5,]

df_idpart_centizq_1991_17$porc_idpart_centizq <- (df_idpart_centizq_1991_17$porc_idpart_centizq + df_idpart_centizq_1991_18$porc_idpart_centizq + df_idpart_centizq_1991_19$porc_idpart_centizq + df_idpart_centizq_1991_20$porc_idpart_centizq)/4
df_idpart_centizq_1991 <- df_idpart_centizq_1991_17

df_idpart_centizq_1992_21$porc_idpart_centizq <- (df_idpart_centizq_1992_21$porc_idpart_centizq + df_idpart_centizq_1992_22$porc_idpart_centizq + df_idpart_centizq_1992_23$porc_idpart_centizq)/3
df_idpart_centizq_1992 <- df_idpart_centizq_1992_21

df_idpart_centizq_1993_24$porc_idpart_centizq <- (df_idpart_centizq_1993_24$porc_idpart_centizq + df_idpart_centizq_1993_25$porc_idpart_centizq + df_idpart_centizq_1993_28$porc_idpart_centizq)/3
df_idpart_centizq_1993 <- df_idpart_centizq_1993_24

# 1994-1996

df_idpart_centizq_1994_29 <- df1994_29 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1995_30 <- df1995_30 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1995_31 <- df1995_31 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1996_32 <- df1996_32 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1996_33 <- df1996_33 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

# Proms
df_idpart_centizq_1995_31[nrow(df_idpart_centizq_1995_31)+1,] <- NA
df_idpart_centizq_1996_32[nrow(df_idpart_centizq_1996_32)+1,] <- NA
df_idpart_centizq_1996_33[nrow(df_idpart_centizq_1996_33)+1,] <- NA

df_idpart_centizq_1994 <- df_idpart_centizq_1994_29

df_idpart_centizq_1995_30$porc_idpart_centizq <- (df_idpart_centizq_1995_30$porc_idpart_centizq + df_idpart_centizq_1995_31$porc_idpart_centizq)/2
df_idpart_centizq_1995 <- df_idpart_centizq_1995_30

df_idpart_centizq_1996_32$porc_idpart_centizq <- (df_idpart_centizq_1996_32$porc_idpart_centizq + df_idpart_centizq_1996_33$porc_idpart_centizq)/2
df_idpart_centizq_1996 <- df_idpart_centizq_1996_32

# 1997 - 1999

df_idpart_centizq_1997_34 <- df1997_34 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1997_35 <- df1997_35 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1998_36 <- df1998_36 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1999_37 <- df1999_37 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_1999_38 <- df1999_38 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

# Prom

df_idpart_centizq_1997_34$porc_idpart_centizq <- (df_idpart_centizq_1997_34$porc_idpart_centizq + df_idpart_centizq_1997_35$porc_idpart_centizq)/2
df_idpart_centizq_1997 <- df_idpart_centizq_1997_34

df_idpart_centizq_1998 <- df_idpart_centizq_1998_36

df_idpart_centizq_1999_37$porc_idpart_centizq <- (df_idpart_centizq_1999_37$porc_idpart_centizq + df_idpart_centizq_1999_38$porc_idpart_centizq)/2
df_idpart_centizq_1999 <- df_idpart_centizq_1999_37



#2000 - 2002

df_idpart_centizq_2000_39 <- df2000_39 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2000_40 <- df2000_40 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2001_41 <- df2001_41 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2001_42 <- df2001_42 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2002_43 <- df2002_43 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2002_44 <- df2002_44 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

# Proms

df_idpart_centizq_2000_39$porc_idpart_centizq <- (df_idpart_centizq_2000_39$porc_idpart_centizq + df_idpart_centizq_2000_40$porc_idpart_centizq)/2
df_idpart_centizq_2000 <- df_idpart_centizq_2000_39

df_idpart_centizq_2001_41$porc_idpart_centizq <- (df_idpart_centizq_2001_41$porc_idpart_centizq + df_idpart_centizq_2001_42$porc_idpart_centizq)/2
df_idpart_centizq_2001 <- df_idpart_centizq_2001_41

df_idpart_centizq_2002_43$porc_idpart_centizq <- (df_idpart_centizq_2002_43$porc_idpart_centizq + df_idpart_centizq_2002_44$porc_idpart_centizq)/2
df_idpart_centizq_2002 <- df_idpart_centizq_2002_43

# 2003 - 2005

df_idpart_centizq_2003_45 <- df2003_45 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2003_46 <- df2003_46 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2004_47 <- df2004_47 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2004_48 <- df2004_48 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2005_49 <- df2005_49 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2005_50 <- df2005_50 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2005_51 <- df2005_51 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

# Proms

df_idpart_centizq_2003_45$porc_idpart_centizq <- (df_idpart_centizq_2003_45$porc_idpart_centizq + df_idpart_centizq_2003_46$porc_idpart_centizq)/2
df_idpart_centizq_2003 <- df_idpart_centizq_2003_45

df_idpart_centizq_2004_47$porc_idpart_centizq <- (df_idpart_centizq_2004_47$porc_idpart_centizq + df_idpart_centizq_2004_48$porc_idpart_centizq)/2
df_idpart_centizq_2004 <- df_idpart_centizq_2004_47

df_idpart_centizq_2005_49$porc_idpart_centizq <- (df_idpart_centizq_2005_49$porc_idpart_centizq + df_idpart_centizq_2005_50$porc_idpart_centizq + df_idpart_centizq_2005_51$porc_idpart_centizq)/3
df_idpart_centizq_2005 <- df_idpart_centizq_2005_49

# 2006-2008

df_idpart_centizq_2006_52 <- df2006_52 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2006_54 <- df2006_54 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2007_55 <- df2007_55 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2007_56 <- df2007_56 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2008_57 <- df2008_57 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2008_58 <- df2008_58 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

#Prom

df_idpart_centizq_2006_52$porc_idpart_centizq <- (df_idpart_centizq_2006_52$porc_idpart_centizq + df_idpart_centizq_2006_54$porc_idpart_centizq)/2
df_idpart_centizq_2006 <- df_idpart_centizq_2006_52

df_idpart_centizq_2007_55$porc_idpart_centizq <- (df_idpart_centizq_2007_55$porc_idpart_centizq + df_idpart_centizq_2007_56$porc_idpart_centizq)/2
df_idpart_centizq_2007 <- df_idpart_centizq_2007_55

df_idpart_centizq_2008_57$porc_idpart_centizq <- (df_idpart_centizq_2008_57$porc_idpart_centizq + df_idpart_centizq_2008_58$porc_idpart_centizq)/2
df_idpart_centizq_2008 <- df_idpart_centizq_2008_57

# 2009 - 2011

df_idpart_centizq_2009_59 <- df2009_59 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2009_60 <- df2009_60 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2009_61 <- df2009_61 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2010_62 <- df2010_62 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2010_63 <- df2010_63 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2011_64 <- df2011_64 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2011_65 <- df2011_65 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

#Prom

df_idpart_centizq_2009_59$porc_idpart_centizq <- (df_idpart_centizq_2009_59$porc_idpart_centizq + df_idpart_centizq_2009_60$porc_idpart_centizq + df_idpart_centizq_2009_61$porc_idpart_centizq)/3
df_idpart_centizq_2009 <- df_idpart_centizq_2009_59


df_idpart_centizq_2010_62$porc_idpart_centizq <- (df_idpart_centizq_2010_62$porc_idpart_centizq + df_idpart_centizq_2010_63$porc_idpart_centizq)/2
df_idpart_centizq_2010 <- df_idpart_centizq_2010_62


df_idpart_centizq_2011_64$porc_idpart_centizq <- (df_idpart_centizq_2011_64$porc_idpart_centizq + df_idpart_centizq_2011_65$porc_idpart_centizq)/2
df_idpart_centizq_2011 <- df_idpart_centizq_2011_64

# 2012 - 2014

df_idpart_centizq_2012_66 <- df2012_66 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2012_67 <- df2012_67 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2012_68 <- df2012_68 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2013_69 <- df2013_69 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2013_70 <- df2013_70 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2014_71 <- df2014_71 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2014_72 <- df2014_72 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

# Prom

df_idpart_centizq_2012_66$porc_idpart_centizq <- (df_idpart_centizq_2012_66$porc_idpart_centizq + df_idpart_centizq_2012_67$porc_idpart_centizq + df_idpart_centizq_2012_68$porc_idpart_centizq)/3
df_idpart_centizq_2012 <- df_idpart_centizq_2012_66

df_idpart_centizq_2013_69$porc_idpart_centizq <- (df_idpart_centizq_2013_69$porc_idpart_centizq + df_idpart_centizq_2013_70$porc_idpart_centizq)/2
df_idpart_centizq_2013 <- df_idpart_centizq_2013_69

df_idpart_centizq_2014_71$porc_idpart_centizq <- (df_idpart_centizq_2014_71$porc_idpart_centizq + df_idpart_centizq_2014_72$porc_idpart_centizq)/2
df_idpart_centizq_2014 <- df_idpart_centizq_2014_71

# 2015 - 2017

df_idpart_centizq_2015_73 <- df2015_73 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2015_74 <- df2015_74 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2015_75 <- df2015_75 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

#df_idpart_centizq_2016_76 <- df2016_76 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
#length(which(id_part == "Centro-Izquierda concertación"))
#                                               /(length(which(id_part == "Centro-Izquierda concertación"))+
#                                                  length(which(id_part == "Izquierda extraconcertación"))+
#                                                 length(which(id_part == "Ninguno"))+
#                                                length(which(id_part == "Derecha"))))

df_idpart_centizq_2016_77 <- df2016_77 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2016_78 <- df2016_78 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

#df_idpart_centizq_2017_79 <- df2017_79 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
#                                                  length(which(id_part == "Centro-Izquierda concertación"))
#                                               /(length(which(id_part == "Centro-Izquierda concertación"))+
#                                                  length(which(id_part == "Izquierda extraconcertación"))+
#                                                 length(which(id_part == "Ninguno"))+
#                                                length(which(id_part == "Derecha"))))

#df_idpart_centizq_2017_80 <- df2017_80 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
#                                                  length(which(id_part == "Centro-Izquierda concertación"))
#                                               /(length(which(id_part == "Centro-Izquierda concertación"))+
#                                                  length(which(id_part == "Izquierda extraconcertación"))+
#                                                 length(which(id_part == "Ninguno"))+
#                                                length(which(id_part == "Derecha"))))

df_idpart_centizq_2017_81 <- df2017_81 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

# Proms

#df_idpart_centizq__$porc_idpart_centizq <- (df_idpart_centizq__$porc_idpart_centizq + df_idpart_centizq__$porc_idpart_centizq + df_idpart_centizq__$porc_idpart_centizq)/3
#df_idpart_centizq_ <- df_idpart_centizq__

df_idpart_centizq_2015_73$porc_idpart_centizq <- (df_idpart_centizq_2015_73$porc_idpart_centizq + df_idpart_centizq_2015_74$porc_idpart_centizq + df_idpart_centizq_2015_75$porc_idpart_centizq)/3
df_idpart_centizq_2015 <- df_idpart_centizq_2015_73

df_idpart_centizq_2016_77$porc_idpart_centizq <- (df_idpart_centizq_2016_77$porc_idpart_centizq + df_idpart_centizq_2016_78$porc_idpart_centizq)/2
df_idpart_centizq_2016 <- df_idpart_centizq_2016_77

df_idpart_centizq_2017 <- df_idpart_centizq_2017_81

# 2018 - 2019

df_idpart_centizq_2018_82 <- df2018_82 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2019_83 <- df2019_83 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

df_idpart_centizq_2019_84 <- df2019_84 %>%group_by(year, edad)%>%summarise(porc_idpart_centizq = 
                                                                            length(which(id_part == "Centro-Izquierda concertación"))
                                                                          /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                              length(which(id_part == "Izquierda extraconcertación"))+
                                                                              length(which(id_part == "Ninguno"))+
                                                                              length(which(id_part == "Derecha"))))

# prom

df_idpart_centizq_2018 <- df_idpart_centizq_2018_82

df_idpart_centizq_2019_83$porc_idpart_centizq <- (df_idpart_centizq_2019_83$porc_idpart_centizq + df_idpart_centizq_2019_84$porc_idpart_centizq)/2
df_idpart_centizq_2019 <- df_idpart_centizq_2019_83

# Crear base

df_idpart_centizq <- do.call("rbind", list(df_idpart_centizq_1990,
                                           df_idpart_centizq_1991, 
                                           df_idpart_centizq_1992, 
                                           df_idpart_centizq_1993, 
                                           df_idpart_centizq_1994, 
                                           df_idpart_centizq_1995, 
                                           df_idpart_centizq_1996, 
                                           df_idpart_centizq_1997, 
                                           df_idpart_centizq_1998, 
                                           df_idpart_centizq_1999, 
                                           df_idpart_centizq_2000, 
                                           df_idpart_centizq_2001, 
                                           df_idpart_centizq_2002, 
                                           df_idpart_centizq_2003, 
                                           df_idpart_centizq_2004, 
                                           df_idpart_centizq_2005, 
                                           df_idpart_centizq_2006, 
                                           df_idpart_centizq_2007, 
                                           df_idpart_centizq_2008, 
                                           df_idpart_centizq_2009, 
                                           df_idpart_centizq_2010, 
                                           df_idpart_centizq_2011, 
                                           df_idpart_centizq_2012, 
                                           df_idpart_centizq_2013, 
                                           df_idpart_centizq_2014, 
                                           df_idpart_centizq_2015, 
                                           df_idpart_centizq_2016, 
                                           df_idpart_centizq_2017, 
                                           df_idpart_centizq_2018, 
                                           df_idpart_centizq_2019))

# Save data base

# Save database
#save(df_idpart_centizq, file = "output/Idpart/Centro-Izquierda-Concertacion/Centro-Izquierda-Concertacion-Edad/CEP-Idpart-Centro-Izquierda-Concertacion_Edad.RData")
#write.csv(df_idpart_centizq, "output/Idpart/Centro-Izquierda-Concertacion/Centro-Izquierda-Concertacion-Edad/CEP-Idpart-Centro-Izquierda-Concertacion_Edad.csv")
write.xlsx(df_idpart_centizq, "output/Idpart/Centro-Izquierda-Concertacion/Centro-Izquierda-Concertacion-Edad/CEP-Idpart-Centro-Izquierda-Concertacion_Edad.xlsx")



# Ninguno

# 1990

df_idpart_nin_1990_14 <- df1990_14 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1990_15 <- df1990_15 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1990_16 <- df1990_16 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Prom

df_idpart_nin_1990_14$porc_idpart_nin <- (df_idpart_nin_1990_14$porc_idpart_nin + df_idpart_nin_1990_15$porc_idpart_nin + df_idpart_nin_1990_16$porc_idpart_nin)/3
df_idpart_nin_1990 <- df_idpart_nin_1990_14

# 1991-1993 

df_idpart_nin_1991_17 <- df1991_17 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1991_18 <- df1991_18 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1991_19 <- df1991_19 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1991_20 <- df1991_20 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1992_21 <- df1992_21 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1992_22 <- df1992_22 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1992_23 <- df1992_23 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1993_24 <- df1993_24 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1993_25 <- df1993_25 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))


df_idpart_nin_1993_28 <- df1993_28 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Proms
df_idpart_nin_1991_18 <- df_idpart_nin_1991_18[-5,]

df_idpart_nin_1991_17$porc_idpart_nin <- (df_idpart_nin_1991_17$porc_idpart_nin + df_idpart_nin_1991_18$porc_idpart_nin + df_idpart_nin_1991_19$porc_idpart_nin + df_idpart_nin_1991_20$porc_idpart_nin)/4
df_idpart_nin_1991 <- df_idpart_nin_1991_17

df_idpart_nin_1992_21$porc_idpart_nin <- (df_idpart_nin_1992_21$porc_idpart_nin + df_idpart_nin_1992_22$porc_idpart_nin + df_idpart_nin_1992_23$porc_idpart_nin)/3
df_idpart_nin_1992 <- df_idpart_nin_1992_21

df_idpart_nin_1993_24$porc_idpart_nin <- (df_idpart_nin_1993_24$porc_idpart_nin + df_idpart_nin_1993_25$porc_idpart_nin + df_idpart_nin_1993_28$porc_idpart_nin)/3
df_idpart_nin_1993 <- df_idpart_nin_1993_24

# 1994-1996

df_idpart_nin_1994_29 <- df1994_29 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1995_30 <- df1995_30 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1995_31 <- df1995_31 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1996_32 <- df1996_32 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1996_33 <- df1996_33 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Proms
df_idpart_nin_1995_31[nrow(df_idpart_nin_1995_31)+1,] <- NA
df_idpart_nin_1996_32[nrow(df_idpart_nin_1996_32)+1,] <- NA
df_idpart_nin_1996_33[nrow(df_idpart_nin_1996_33)+1,] <- NA

df_idpart_nin_1994 <- df_idpart_nin_1994_29

df_idpart_nin_1995_30$porc_idpart_nin <- (df_idpart_nin_1995_30$porc_idpart_nin + df_idpart_nin_1995_31$porc_idpart_nin)/2
df_idpart_nin_1995 <- df_idpart_nin_1995_30

df_idpart_nin_1996_32$porc_idpart_nin <- (df_idpart_nin_1996_32$porc_idpart_nin + df_idpart_nin_1996_33$porc_idpart_nin)/2
df_idpart_nin_1996 <- df_idpart_nin_1996_32

# 1997 - 1999

df_idpart_nin_1997_34 <- df1997_34 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1997_35 <- df1997_35 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1998_36 <- df1998_36 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1999_37 <- df1999_37 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_1999_38 <- df1999_38 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Prom

df_idpart_nin_1997_34$porc_idpart_nin <- (df_idpart_nin_1997_34$porc_idpart_nin + df_idpart_nin_1997_35$porc_idpart_nin)/2
df_idpart_nin_1997 <- df_idpart_nin_1997_34

df_idpart_nin_1998 <- df_idpart_nin_1998_36

df_idpart_nin_1999_37$porc_idpart_nin <- (df_idpart_nin_1999_37$porc_idpart_nin + df_idpart_nin_1999_38$porc_idpart_nin)/2
df_idpart_nin_1999 <- df_idpart_nin_1999_37



#2000 - 2002

df_idpart_nin_2000_39 <- df2000_39 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2000_40 <- df2000_40 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2001_41 <- df2001_41 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2001_42 <- df2001_42 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2002_43 <- df2002_43 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2002_44 <- df2002_44 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Proms

df_idpart_nin_2000_39$porc_idpart_nin <- (df_idpart_nin_2000_39$porc_idpart_nin + df_idpart_nin_2000_40$porc_idpart_nin)/2
df_idpart_nin_2000 <- df_idpart_nin_2000_39

df_idpart_nin_2001_41$porc_idpart_nin <- (df_idpart_nin_2001_41$porc_idpart_nin + df_idpart_nin_2001_42$porc_idpart_nin)/2
df_idpart_nin_2001 <- df_idpart_nin_2001_41

df_idpart_nin_2002_43$porc_idpart_nin <- (df_idpart_nin_2002_43$porc_idpart_nin + df_idpart_nin_2002_44$porc_idpart_nin)/2
df_idpart_nin_2002 <- df_idpart_nin_2002_43

# 2003 - 2005

df_idpart_nin_2003_45 <- df2003_45 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2003_46 <- df2003_46 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2004_47 <- df2004_47 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2004_48 <- df2004_48 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2005_49 <- df2005_49 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2005_50 <- df2005_50 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2005_51 <- df2005_51 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Proms

df_idpart_nin_2003_45$porc_idpart_nin <- (df_idpart_nin_2003_45$porc_idpart_nin + df_idpart_nin_2003_46$porc_idpart_nin)/2
df_idpart_nin_2003 <- df_idpart_nin_2003_45

df_idpart_nin_2004_47$porc_idpart_nin <- (df_idpart_nin_2004_47$porc_idpart_nin + df_idpart_nin_2004_48$porc_idpart_nin)/2
df_idpart_nin_2004 <- df_idpart_nin_2004_47

df_idpart_nin_2005_49$porc_idpart_nin <- (df_idpart_nin_2005_49$porc_idpart_nin + df_idpart_nin_2005_50$porc_idpart_nin + df_idpart_nin_2005_51$porc_idpart_nin)/3
df_idpart_nin_2005 <- df_idpart_nin_2005_49

# 2006-2008

df_idpart_nin_2006_52 <- df2006_52 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2006_54 <- df2006_54 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2007_55 <- df2007_55 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2007_56 <- df2007_56 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2008_57 <- df2008_57 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2008_58 <- df2008_58 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

#Prom

df_idpart_nin_2006_52$porc_idpart_nin <- (df_idpart_nin_2006_52$porc_idpart_nin + df_idpart_nin_2006_54$porc_idpart_nin)/2
df_idpart_nin_2006 <- df_idpart_nin_2006_52

df_idpart_nin_2007_55$porc_idpart_nin <- (df_idpart_nin_2007_55$porc_idpart_nin + df_idpart_nin_2007_56$porc_idpart_nin)/2
df_idpart_nin_2007 <- df_idpart_nin_2007_55

df_idpart_nin_2008_57$porc_idpart_nin <- (df_idpart_nin_2008_57$porc_idpart_nin + df_idpart_nin_2008_58$porc_idpart_nin)/2
df_idpart_nin_2008 <- df_idpart_nin_2008_57

# 2009 - 2011

df_idpart_nin_2009_59 <- df2009_59 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2009_60 <- df2009_60 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2009_61 <- df2009_61 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2010_62 <- df2010_62 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2010_63 <- df2010_63 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2011_64 <- df2011_64 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2011_65 <- df2011_65 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

#Prom

df_idpart_nin_2009_59$porc_idpart_nin <- (df_idpart_nin_2009_59$porc_idpart_nin + df_idpart_nin_2009_60$porc_idpart_nin + df_idpart_nin_2009_61$porc_idpart_nin)/3
df_idpart_nin_2009 <- df_idpart_nin_2009_59


df_idpart_nin_2010_62$porc_idpart_nin <- (df_idpart_nin_2010_62$porc_idpart_nin + df_idpart_nin_2010_63$porc_idpart_nin)/2
df_idpart_nin_2010 <- df_idpart_nin_2010_62


df_idpart_nin_2011_64$porc_idpart_nin <- (df_idpart_nin_2011_64$porc_idpart_nin + df_idpart_nin_2011_65$porc_idpart_nin)/2
df_idpart_nin_2011 <- df_idpart_nin_2011_64

# 2012 - 2014

df_idpart_nin_2012_66 <- df2012_66 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2012_67 <- df2012_67 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2012_68 <- df2012_68 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2013_69 <- df2013_69 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2013_70 <- df2013_70 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2014_71 <- df2014_71 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2014_72 <- df2014_72 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Prom

df_idpart_nin_2012_66$porc_idpart_nin <- (df_idpart_nin_2012_66$porc_idpart_nin + df_idpart_nin_2012_67$porc_idpart_nin + df_idpart_nin_2012_68$porc_idpart_nin)/3
df_idpart_nin_2012 <- df_idpart_nin_2012_66

df_idpart_nin_2013_69$porc_idpart_nin <- (df_idpart_nin_2013_69$porc_idpart_nin + df_idpart_nin_2013_70$porc_idpart_nin)/2
df_idpart_nin_2013 <- df_idpart_nin_2013_69

df_idpart_nin_2014_71$porc_idpart_nin <- (df_idpart_nin_2014_71$porc_idpart_nin + df_idpart_nin_2014_72$porc_idpart_nin)/2
df_idpart_nin_2014 <- df_idpart_nin_2014_71

# 2015 - 2017

df_idpart_nin_2015_73 <- df2015_73 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2015_74 <- df2015_74 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2015_75 <- df2015_75 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

#df_idpart_nin_2016_76 <- df2016_76 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
#length(which(id_part == "Ninguno"))
#                                               /(length(which(id_part == "Centro-Izquierda concertación"))+
#                                                  length(which(id_part == "Izquierda extraconcertación"))+
#                                                 length(which(id_part == "Ninguno"))+
#                                                length(which(id_part == "Derecha"))))

df_idpart_nin_2016_77 <- df2016_77 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2016_78 <- df2016_78 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

#df_idpart_nin_2017_79 <- df2017_79 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
#                                                  length(which(id_part == "Ninguno"))
#                                               /(length(which(id_part == "Centro-Izquierda concertación"))+
#                                                  length(which(id_part == "Izquierda extraconcertación"))+
#                                                 length(which(id_part == "Ninguno"))+
#                                                length(which(id_part == "Derecha"))))

#df_idpart_nin_2017_80 <- df2017_80 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
#                                                  length(which(id_part == "Ninguno"))
#                                               /(length(which(id_part == "Centro-Izquierda concertación"))+
#                                                  length(which(id_part == "Izquierda extraconcertación"))+
#                                                 length(which(id_part == "Ninguno"))+
#                                                length(which(id_part == "Derecha"))))

df_idpart_nin_2017_81 <- df2017_81 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# Proms

#df_idpart_nin__$porc_idpart_nin <- (df_idpart_nin__$porc_idpart_nin + df_idpart_nin__$porc_idpart_nin + df_idpart_nin__$porc_idpart_nin)/3
#df_idpart_nin_ <- df_idpart_nin__

df_idpart_nin_2015_73$porc_idpart_nin <- (df_idpart_nin_2015_73$porc_idpart_nin + df_idpart_nin_2015_74$porc_idpart_nin + df_idpart_nin_2015_75$porc_idpart_nin)/3
df_idpart_nin_2015 <- df_idpart_nin_2015_73

df_idpart_nin_2016_77$porc_idpart_nin <- (df_idpart_nin_2016_77$porc_idpart_nin + df_idpart_nin_2016_78$porc_idpart_nin)/2
df_idpart_nin_2016 <- df_idpart_nin_2016_77

df_idpart_nin_2017 <- df_idpart_nin_2017_81

# 2018 - 2019

df_idpart_nin_2018_82 <- df2018_82 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2019_83 <- df2019_83 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

df_idpart_nin_2019_84 <- df2019_84 %>%group_by(year, edad)%>%summarise(porc_idpart_nin = 
                                                                        length(which(id_part == "Ninguno"))
                                                                      /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                          length(which(id_part == "Izquierda extraconcertación"))+
                                                                          length(which(id_part == "Ninguno"))+
                                                                          length(which(id_part == "Derecha"))))

# prom

df_idpart_nin_2018 <- df_idpart_nin_2018_82

df_idpart_nin_2019_83$porc_idpart_nin <- (df_idpart_nin_2019_83$porc_idpart_nin + df_idpart_nin_2019_84$porc_idpart_nin)/2
df_idpart_nin_2019 <- df_idpart_nin_2019_83

# Crear base

df_idpart_nin <- do.call("rbind", list(df_idpart_nin_1990,
                                       df_idpart_nin_1991, 
                                       df_idpart_nin_1992, 
                                       df_idpart_nin_1993, 
                                       df_idpart_nin_1994, 
                                       df_idpart_nin_1995, 
                                       df_idpart_nin_1996, 
                                       df_idpart_nin_1997, 
                                       df_idpart_nin_1998, 
                                       df_idpart_nin_1999, 
                                       df_idpart_nin_2000, 
                                       df_idpart_nin_2001, 
                                       df_idpart_nin_2002, 
                                       df_idpart_nin_2003, 
                                       df_idpart_nin_2004, 
                                       df_idpart_nin_2005, 
                                       df_idpart_nin_2006, 
                                       df_idpart_nin_2007, 
                                       df_idpart_nin_2008, 
                                       df_idpart_nin_2009, 
                                       df_idpart_nin_2010, 
                                       df_idpart_nin_2011, 
                                       df_idpart_nin_2012, 
                                       df_idpart_nin_2013, 
                                       df_idpart_nin_2014, 
                                       df_idpart_nin_2015, 
                                       df_idpart_nin_2016, 
                                       df_idpart_nin_2017, 
                                       df_idpart_nin_2018, 
                                       df_idpart_nin_2019))

# Save data base

# Save database
#save(df_idpart_nin, file = "output/Idpart/Ninguno/Ninguno-Edad/CEP-Idpart-Ninguno_Edad.RData")
#write.csv(df_idpart_nin, "output/Idpart/Ninguno/Ninguno-Edad/CEP-Idpart-Ninguno_Edad.csv")
write.xlsx(df_idpart_nin, "output/Idpart/Ninguno/Ninguno-Edad/CEP-Idpart-Ninguno_Edad.xlsx")

# Izquierda extraconcertación

# 1990

df_idpart_izqext_1990_14 <- df1990_14 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1990_15 <- df1990_15 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1990_16 <- df1990_16 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

# Prom

df_idpart_izqext_1990_14$porc_idpart_izqext <- (df_idpart_izqext_1990_14$porc_idpart_izqext + df_idpart_izqext_1990_15$porc_idpart_izqext + df_idpart_izqext_1990_16$porc_idpart_izqext)/3
df_idpart_izqext_1990 <- df_idpart_izqext_1990_14

# 1991-1993 

df_idpart_izqext_1991_17 <- df1991_17 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1991_18 <- df1991_18 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1991_19 <- df1991_19 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1991_20 <- df1991_20 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1992_21 <- df1992_21 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1992_22 <- df1992_22 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1992_23 <- df1992_23 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1993_24 <- df1993_24 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1993_25 <- df1993_25 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))


df_idpart_izqext_1993_28 <- df1993_28 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

# Proms
df_idpart_izqext_1991_18 <- df_idpart_izqext_1991_18[-5,]

df_idpart_izqext_1991_17$porc_idpart_izqext <- (df_idpart_izqext_1991_17$porc_idpart_izqext + df_idpart_izqext_1991_18$porc_idpart_izqext + df_idpart_izqext_1991_19$porc_idpart_izqext + df_idpart_izqext_1991_20$porc_idpart_izqext)/4
df_idpart_izqext_1991 <- df_idpart_izqext_1991_17

df_idpart_izqext_1992_21$porc_idpart_izqext <- (df_idpart_izqext_1992_21$porc_idpart_izqext + df_idpart_izqext_1992_22$porc_idpart_izqext + df_idpart_izqext_1992_23$porc_idpart_izqext)/3
df_idpart_izqext_1992 <- df_idpart_izqext_1992_21

df_idpart_izqext_1993_24$porc_idpart_izqext <- (df_idpart_izqext_1993_24$porc_idpart_izqext + df_idpart_izqext_1993_25$porc_idpart_izqext + df_idpart_izqext_1993_28$porc_idpart_izqext)/3
df_idpart_izqext_1993 <- df_idpart_izqext_1993_24

# 1994-1996

df_idpart_izqext_1994_29 <- df1994_29 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1995_30 <- df1995_30 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1995_31 <- df1995_31 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1996_32 <- df1996_32 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1996_33 <- df1996_33 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

# Proms
df_idpart_izqext_1995_31[nrow(df_idpart_izqext_1995_31)+1,] <- NA
df_idpart_izqext_1996_32[nrow(df_idpart_izqext_1996_32)+1,] <- NA
df_idpart_izqext_1996_33[nrow(df_idpart_izqext_1996_33)+1,] <- NA

df_idpart_izqext_1994 <- df_idpart_izqext_1994_29

df_idpart_izqext_1995_30$porc_idpart_izqext <- (df_idpart_izqext_1995_30$porc_idpart_izqext + df_idpart_izqext_1995_31$porc_idpart_izqext)/2
df_idpart_izqext_1995 <- df_idpart_izqext_1995_30

df_idpart_izqext_1996_32$porc_idpart_izqext <- (df_idpart_izqext_1996_32$porc_idpart_izqext + df_idpart_izqext_1996_33$porc_idpart_izqext)/2
df_idpart_izqext_1996 <- df_idpart_izqext_1996_32

# 1997 - 1999

df_idpart_izqext_1997_34 <- df1997_34 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1997_35 <- df1997_35 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1998_36 <- df1998_36 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1999_37 <- df1999_37 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_1999_38 <- df1999_38 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

# Prom

df_idpart_izqext_1997_34$porc_idpart_izqext <- (df_idpart_izqext_1997_34$porc_idpart_izqext + df_idpart_izqext_1997_35$porc_idpart_izqext)/2
df_idpart_izqext_1997 <- df_idpart_izqext_1997_34

df_idpart_izqext_1998 <- df_idpart_izqext_1998_36

df_idpart_izqext_1999_37$porc_idpart_izqext <- (df_idpart_izqext_1999_37$porc_idpart_izqext + df_idpart_izqext_1999_38$porc_idpart_izqext)/2
df_idpart_izqext_1999 <- df_idpart_izqext_1999_37



#2000 - 2002

df_idpart_izqext_2000_39 <- df2000_39 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2000_40 <- df2000_40 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2001_41 <- df2001_41 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2001_42 <- df2001_42 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2002_43 <- df2002_43 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2002_44 <- df2002_44 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

# Proms

df_idpart_izqext_2000_39$porc_idpart_izqext <- (df_idpart_izqext_2000_39$porc_idpart_izqext + df_idpart_izqext_2000_40$porc_idpart_izqext)/2
df_idpart_izqext_2000 <- df_idpart_izqext_2000_39

df_idpart_izqext_2001_41$porc_idpart_izqext <- (df_idpart_izqext_2001_41$porc_idpart_izqext + df_idpart_izqext_2001_42$porc_idpart_izqext)/2
df_idpart_izqext_2001 <- df_idpart_izqext_2001_41

df_idpart_izqext_2002_43$porc_idpart_izqext <- (df_idpart_izqext_2002_43$porc_idpart_izqext + df_idpart_izqext_2002_44$porc_idpart_izqext)/2
df_idpart_izqext_2002 <- df_idpart_izqext_2002_43

# 2003 - 2005

df_idpart_izqext_2003_45 <- df2003_45 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2003_46 <- df2003_46 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2004_47 <- df2004_47 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2004_48 <- df2004_48 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2005_49 <- df2005_49 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2005_50 <- df2005_50 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2005_51 <- df2005_51 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

# Proms

df_idpart_izqext_2003_45$porc_idpart_izqext <- (df_idpart_izqext_2003_45$porc_idpart_izqext + df_idpart_izqext_2003_46$porc_idpart_izqext)/2
df_idpart_izqext_2003 <- df_idpart_izqext_2003_45

df_idpart_izqext_2004_47$porc_idpart_izqext <- (df_idpart_izqext_2004_47$porc_idpart_izqext + df_idpart_izqext_2004_48$porc_idpart_izqext)/2
df_idpart_izqext_2004 <- df_idpart_izqext_2004_47

df_idpart_izqext_2005_49$porc_idpart_izqext <- (df_idpart_izqext_2005_49$porc_idpart_izqext + df_idpart_izqext_2005_50$porc_idpart_izqext + df_idpart_izqext_2005_51$porc_idpart_izqext)/3
df_idpart_izqext_2005 <- df_idpart_izqext_2005_49

# 2006-2008

df_idpart_izqext_2006_52 <- df2006_52 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2006_54 <- df2006_54 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2007_55 <- df2007_55 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2007_56 <- df2007_56 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2008_57 <- df2008_57 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2008_58 <- df2008_58 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

#Prom

df_idpart_izqext_2006_52$porc_idpart_izqext <- (df_idpart_izqext_2006_52$porc_idpart_izqext + df_idpart_izqext_2006_54$porc_idpart_izqext)/2
df_idpart_izqext_2006 <- df_idpart_izqext_2006_52

df_idpart_izqext_2007_55$porc_idpart_izqext <- (df_idpart_izqext_2007_55$porc_idpart_izqext + df_idpart_izqext_2007_56$porc_idpart_izqext)/2
df_idpart_izqext_2007 <- df_idpart_izqext_2007_55

df_idpart_izqext_2008_57$porc_idpart_izqext <- (df_idpart_izqext_2008_57$porc_idpart_izqext + df_idpart_izqext_2008_58$porc_idpart_izqext)/2
df_idpart_izqext_2008 <- df_idpart_izqext_2008_57

# 2009 - 2011

df_idpart_izqext_2009_59 <- df2009_59 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2009_60 <- df2009_60 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2009_61 <- df2009_61 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2010_62 <- df2010_62 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2010_63 <- df2010_63 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2011_64 <- df2011_64 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2011_65 <- df2011_65 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

#Prom

df_idpart_izqext_2009_59$porc_idpart_izqext <- (df_idpart_izqext_2009_59$porc_idpart_izqext + df_idpart_izqext_2009_60$porc_idpart_izqext + df_idpart_izqext_2009_61$porc_idpart_izqext)/3
df_idpart_izqext_2009 <- df_idpart_izqext_2009_59


df_idpart_izqext_2010_62$porc_idpart_izqext <- (df_idpart_izqext_2010_62$porc_idpart_izqext + df_idpart_izqext_2010_63$porc_idpart_izqext)/2
df_idpart_izqext_2010 <- df_idpart_izqext_2010_62


df_idpart_izqext_2011_64$porc_idpart_izqext <- (df_idpart_izqext_2011_64$porc_idpart_izqext + df_idpart_izqext_2011_65$porc_idpart_izqext)/2
df_idpart_izqext_2011 <- df_idpart_izqext_2011_64

# 2012 - 2014

df_idpart_izqext_2012_66 <- df2012_66 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2012_67 <- df2012_67 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2012_68 <- df2012_68 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2013_69 <- df2013_69 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2013_70 <- df2013_70 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2014_71 <- df2014_71 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2014_72 <- df2014_72 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

# Prom

df_idpart_izqext_2012_66$porc_idpart_izqext <- (df_idpart_izqext_2012_66$porc_idpart_izqext + df_idpart_izqext_2012_67$porc_idpart_izqext + df_idpart_izqext_2012_68$porc_idpart_izqext)/3
df_idpart_izqext_2012 <- df_idpart_izqext_2012_66

df_idpart_izqext_2013_69$porc_idpart_izqext <- (df_idpart_izqext_2013_69$porc_idpart_izqext + df_idpart_izqext_2013_70$porc_idpart_izqext)/2
df_idpart_izqext_2013 <- df_idpart_izqext_2013_69

df_idpart_izqext_2014_71$porc_idpart_izqext <- (df_idpart_izqext_2014_71$porc_idpart_izqext + df_idpart_izqext_2014_72$porc_idpart_izqext)/2
df_idpart_izqext_2014 <- df_idpart_izqext_2014_71

# 2015 - 2017

df_idpart_izqext_2015_73 <- df2015_73 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2015_74 <- df2015_74 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2015_75 <- df2015_75 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

#df_idpart_izqext_2016_76 <- df2016_76 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
#length(which(id_part == "Izquierda extraconcertación"))
#                                               /(length(which(id_part == "Centro-Izquierda concertación"))+
#                                                  length(which(id_part == "Izquierda extraconcertación"))+
#                                                 length(which(id_part == "Ninguno"))+
#                                                length(which(id_part == "Derecha"))))

df_idpart_izqext_2016_77 <- df2016_77 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2016_78 <- df2016_78 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

#df_idpart_izqext_2017_79 <- df2017_79 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
#                                                  length(which(id_part == "Izquierda extraconcertación"))
#                                               /(length(which(id_part == "Centro-Izquierda concertación"))+
#                                                  length(which(id_part == "Izquierda extraconcertación"))+
#                                                 length(which(id_part == "Ninguno"))+
#                                                length(which(id_part == "Derecha"))))

#df_idpart_izqext_2017_80 <- df2017_80 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
#                                                  length(which(id_part == "Izquierda extraconcertación"))
#                                               /(length(which(id_part == "Centro-Izquierda concertación"))+
#                                                  length(which(id_part == "Izquierda extraconcertación"))+
#                                                 length(which(id_part == "Ninguno"))+
#                                                length(which(id_part == "Derecha"))))

df_idpart_izqext_2017_81 <- df2017_81 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

# Proms

#df_idpart_izqext__$porc_idpart_izqext <- (df_idpart_izqext__$porc_idpart_izqext + df_idpart_izqext__$porc_idpart_izqext + df_idpart_izqext__$porc_idpart_izqext)/3
#df_idpart_izqext_ <- df_idpart_izqext__

df_idpart_izqext_2015_73$porc_idpart_izqext <- (df_idpart_izqext_2015_73$porc_idpart_izqext + df_idpart_izqext_2015_74$porc_idpart_izqext + df_idpart_izqext_2015_75$porc_idpart_izqext)/3
df_idpart_izqext_2015 <- df_idpart_izqext_2015_73

df_idpart_izqext_2016_77$porc_idpart_izqext <- (df_idpart_izqext_2016_77$porc_idpart_izqext + df_idpart_izqext_2016_78$porc_idpart_izqext)/2
df_idpart_izqext_2016 <- df_idpart_izqext_2016_77

df_idpart_izqext_2017 <- df_idpart_izqext_2017_81

# 2018 - 2019

df_idpart_izqext_2018_82 <- df2018_82 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2019_83 <- df2019_83 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

df_idpart_izqext_2019_84 <- df2019_84 %>%group_by(year, edad)%>%summarise(porc_idpart_izqext = 
                                                                           length(which(id_part == "Izquierda extraconcertación"))
                                                                         /(length(which(id_part == "Centro-Izquierda concertación"))+
                                                                             length(which(id_part == "Izquierda extraconcertación"))+
                                                                             length(which(id_part == "Ninguno"))+
                                                                             length(which(id_part == "Derecha"))))

# prom

df_idpart_izqext_2018 <- df_idpart_izqext_2018_82

df_idpart_izqext_2019_83$porc_idpart_izqext <- (df_idpart_izqext_2019_83$porc_idpart_izqext + df_idpart_izqext_2019_84$porc_idpart_izqext)/2
df_idpart_izqext_2019 <- df_idpart_izqext_2019_83

# Crear base

df_idpart_izqext <- do.call("rbind", list(df_idpart_izqext_1990,
                                          df_idpart_izqext_1991, 
                                          df_idpart_izqext_1992, 
                                          df_idpart_izqext_1993, 
                                          df_idpart_izqext_1994, 
                                          df_idpart_izqext_1995, 
                                          df_idpart_izqext_1996, 
                                          df_idpart_izqext_1997, 
                                          df_idpart_izqext_1998, 
                                          df_idpart_izqext_1999, 
                                          df_idpart_izqext_2000, 
                                          df_idpart_izqext_2001, 
                                          df_idpart_izqext_2002, 
                                          df_idpart_izqext_2003, 
                                          df_idpart_izqext_2004, 
                                          df_idpart_izqext_2005, 
                                          df_idpart_izqext_2006, 
                                          df_idpart_izqext_2007, 
                                          df_idpart_izqext_2008, 
                                          df_idpart_izqext_2009, 
                                          df_idpart_izqext_2010, 
                                          df_idpart_izqext_2011, 
                                          df_idpart_izqext_2012, 
                                          df_idpart_izqext_2013, 
                                          df_idpart_izqext_2014, 
                                          df_idpart_izqext_2015, 
                                          df_idpart_izqext_2016, 
                                          df_idpart_izqext_2017, 
                                          df_idpart_izqext_2018, 
                                          df_idpart_izqext_2019))

# Save data base

# Save database
#save(df_idpart_izqext, file = "output/Idpart/Izquierda-extraconcertacion/Izquierda-extraparlamentaria-Edad/CEP-Idpart-Izquierda-extraconcertacion_Edad_Edad.RData")
#write.csv(df_idpart_izqext, "output/Idpart/Izquierda-extraconcertacion/Izquierda-extraparlamentaria-Edad/CEP-Idpart-Izquierda-extraconcertacion_Edad.csv")
write.xlsx(df_idpart_izqext, "output/Idpart/Izquierda-extraconcertacion/Izquierda-extraparlamentaria-Edad/CEP-Idpart-Izquierda-extraconcertacion_Edad.xlsx")
