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

load("input/data/bd2012_2014_69.RData") #2013
load("input/data/bd2012_2014_71.RData") #2014

load("input/data/bd2015_2017_79.RData") #2017

load("input/data/bd2018_2019_84.RData") #2019

# Re asignar nombres
bd1991_17 <- bd1991_1993_17
bd1991_18 <- bd1991_1993_18
bd1992_23 <- bd1991_1993_23

bd1998_36 <-bd1997_1999_36

bd2002_44 <-bd2000_2002_44

bd


#---- Etapa 3: Procesamiento de datos----
#---- 3.1 Merge bases de datos ----


