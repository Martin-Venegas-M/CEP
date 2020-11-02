# ---- Etapa 0: Información del documento ----
# Título: Documento de procesamiento para bases de datos CEP
# Autor: Ojeda, P & Venegas, M
# Fecha: 29 - 10 - 2020

# ---- Etapa 1: Cargar paquetes ----
library(pacman)

pacman::p_load(tidyverse, summarytools, ggplot2, sjmisc, stargazer, openxlsx, readxl, sjlabelled, car)

# ---- Etapa 2: Cargar bases de datos ---

# Confianza en instituciones
## Mucha confianza

mci <- readxl::read_excel("output/Confianza-Mucha-Conf/CEP-Mucha-Conf-Inst.xlsx")
mcp <- readxl::read_excel("output/Confianza-Mucha-Conf/CEP-Mucha-Conf-Inst.xlsx")

mci_edad <- readxl::read_excel("output/Confianza-Mucha-Conf/Mucha-Conf-Edad/CEP-Mucha-Conf-Inst-Edad.xlsx")
mcp_edad <- readxl::read_excel("output/Confianza-Mucha-Conf/Mucha-Conf-Edad/CEP-Mucha-Conf-Per-Edad.xlsx")

mci_nse <- readxl::read_excel("output/Confianza-Mucha-Conf/Mucha-Conf-NSE/CEP-Mucha-Conf-Inst-NSE.xlsx")
mcp_nse <- readxl::read_excel("output/Confianza-Mucha-Conf/Mucha-Conf-NSE/CEP-Mucha-Conf-Per-NSE.xlsx")

mci_esc <- readxl::read_excel("output/Confianza-Mucha-Conf/Mucha-Conf-Escolaridad/CEP-Mucha-Conf-Inst-Escolaridad.xlsx")
mcp_esc <- readxl::read_excel("output/Confianza-Mucha-Conf/Mucha-Conf-Escolaridad/CEP-Mucha-Conf-Per-Escolaridad.xlsx")

mci_sexo <- readxl::read_excel("output/Confianza-Mucha-Conf/Mucha-Conf-Sexo/CEP-Mucha-Conf-Inst-Sexo.xlsx")
mcp_sexo <- readxl::read_excel("output/Confianza-Mucha-Conf/Mucha-Conf-Sexo/CEP-Mucha-Conf-Per-Sexo.xlsx")

# Identificacion partidaria

# Derecha
ip_der <- readxl::read_excel("output/Idpart/Derecha/CEP-Idpart-Derecha.xlsx")


ip_der_edad <- readxl::read_excel("output/Idpart/Derecha/Derecha-Edad/CEP-Idpart-Derecha_Edad.xlsx")
ip_der_esc <- readxl::read_excel("output/Idpart/Derecha/Derecha-Escolaridad/CEP-Idpart-Derecha_Escolaridad.xlsx")
ip_der_nse <- readxl::read_excel("output/Idpart/Derecha/Derecha-NSE/CEP-Idpart-Derecha_NSE.xlsx")
ip_der_sexo <- readxl::read_excel("output/Idpart/Derecha/Derecha-Sexo/CEP-Idpart-Derecha_Sexo.xlsx")

# Centro Izquierda Concertacion
ip_centizq <- readxl::read_excel("output/Idpart/Centro-Izquierda-Concertacion/CEP-Idpart-Centro-Izquierda-Concertacion.xlsx")

ip_centizq_edad <- readxl::read_excel("output/Idpart/Centro-Izquierda-Concertacion/Centro-Izquierda-Concertacion-Edad/CEP-Idpart-Centro-Izquierda-Concertacion_Edad.xlsx")
ip_centizq_esc <- readxl::read_excel("output/Idpart/Centro-Izquierda-Concertacion/Centro-Izquierda-Concertacion-Escolaridad/CEP-Idpart-Centro-Izquierda-Concertacion_Escolaridad.xlsx")
ip_centizq_nse <- readxl::read_excel("output/Idpart/Centro-Izquierda-Concertacion/Centro-Izquierda-Concertacion-NSE/CEP-Idpart-Centro-Izquierda-Concertacion_NSE.xlsx")
ip_centizq_sexo <- readxl::read_excel("output/Idpart/Centro-Izquierda-Concertacion/Centro-Izquierda-Concertacion-Sexo/CEP-Idpart-Centro-Izquierda-Concertacion_Sexo.xlsx")

# Izquierda-extraconcertacion
ip_izqext <- readxl::read_excel("output/Idpart/Izquierda-extraconcertacion/CEP-Idpart-Izquierda-extraconcertacion.xlsx")


ip_izqext_edad <- readxl::read_excel("output/Idpart/Izquierda-extraconcertacion/Izquierda-extraparlamentaria-Edad/CEP-Idpart-Izquierda-extraconcertacion_Edad.xlsx")
ip_izqext_esc <- readxl::read_excel("output/Idpart/Izquierda-extraconcertacion/Izquierda-extraparlamentaria-Escolaridad/CEP-Idpart-Izquierda-extraconcertacion_Escolaridad.xlsx")
ip_izqext_nse <- readxl::read_excel("output/Idpart/Izquierda-extraconcertacion/Izquierda-extraparlamentaria-NSE/CEP-Idpart-Izquierda-extraconcertacion_NSE.xlsx")
ip_izqext_sexo <- readxl::read_excel("output/Idpart/Izquierda-extraconcertacion/Izquierda-extraparlamentaria-Sexo/CEP-Idpart-Izquierda-extraconcertacion_Sexo.xlsx")

# Ninguno
ip_nin <- readxl::read_excel("output/Idpart/Ninguno/CEP-Idpart-Ninguno.xlsx")


ip_nin_edad <- readxl::read_excel("output/Idpart/Ninguno/Ninguno-Edad/CEP-Idpart-Ninguno_Edad.xlsx")
ip_nin_esc <- readxl::read_excel("output/Idpart/Ninguno/Ninguno-Escolaridad/CEP-Idpart-Ninguno_Escolaridad.xlsx")
ip_nin_nse <- readxl::read_excel("output/Idpart/Ninguno/Ninguno-NSE/CEP-Idpart-Ninguno_NSE.xlsx")
ip_nin_sexo <- readxl::read_excel("output/Idpart/Ninguno/Ninguno-Sexo/CEP-Idpart-Ninguno_Sexo.xlsx")

# Posicion politica

# Derecha
pp_der <- readxl::read_excel("output/Pospol/Derecha/CEP-pospol-Derecha.xlsx")

pp_der_edad <- readxl::read_excel("output/Pospol/Derecha/Derecha-Edad/CEP-pospol-Derecha-Edad.xlsx")
pp_der_esc <- readxl::read_excel("output/Pospol/Derecha/Derecha-Escolaridad/CEP-pospol-Derecha-Escolaridad.xlsx")
pp_der_nse <- readxl::read_excel("output/Pospol/Derecha/Derecha-NSE/CEP-pospol-Derecha-NSE.xlsx")
pp_der_sexo <- readxl::read_excel("output/Pospol/Derecha/Derecha-Sexo/CEP-pospol-Derecha-Sexo.xlsx")

# Centro-Derecha
pp_centder<- readxl::read_excel("output/Pospol/Centro-Derecha/CEP-pospol-Centro-Derecha.xlsx")

pp_centder_edad <- readxl::read_excel("output/Pospol/Centro-Derecha/Centro-Derecha-Edad/CEP-pospol-Centro-Derecha-Edad.xlsx")
pp_centder_esc <- readxl::read_excel("output/Pospol/Centro-Derecha/Centro-Derecha-Escolaridad/CEP-pospol-Centro-Derecha-Escolaridad.xlsx")
pp_centder_nse <- readxl::read_excel("output/Pospol/Centro-Derecha/Centro-Derecha-NSE/CEP-pospol-Centro-Derecha-NSE.xlsx")
pp_centder_sexo <- readxl::read_excel("output/Pospol/Centro-Derecha/Centro-Derecha-Sexo/CEP-pospol-Centro-Derecha-Sexo.xlsx")

# Centro
pp_cent <- readxl::read_excel("output/Pospol/Centro/CEP-pospol-Centro.xlsx")

pp_cent_edad <- readxl::read_excel("output/Pospol/Centro/Centro-Edad/CEP-pospol-Centro-Edad.xlsx")
pp_cent_esc <- readxl::read_excel("output/Pospol/Centro/Centro-Escolaridad/CEP-pospol-Centro-Escolaridad.xlsx")
pp_cent_nse <- readxl::read_excel("output/Pospol/Centro/Centro-NSE/CEP-pospol-Centro-NSE.xlsx")
pp_cent_sexo <- readxl::read_excel("output/Pospol/Centro/Centro-Sexo/CEP-pospol-Centro-Sexo.xlsx")

# Centro-Izquierda
pp_centizq <- readxl::read_excel("output/Pospol/Centro-Izquierda/CEP-pospol-Centro-Izquierda.xlsx")

pp_centizq_edad <- readxl::read_excel("output/Pospol/Centro-Izquierda/Centro-Izquierda-Edad/CEP-pospol-Centro-Izquierda-Edad.xlsx")
pp_centizq_esc <- readxl::read_excel("output/Pospol/Centro-Izquierda/Centro-Izquierda-Escolaridad/CEP-pospol-Centro-Izquierda-Escolaridad.xlsx")
pp_centizq_nse <- readxl::read_excel("output/Pospol/Centro-Izquierda/Centro-Izquierda-NSE/CEP-pospol-Centro-Izquierda-NSE.xlsx")
pp_centizq_sexo <- readxl::read_excel("output/Pospol/Centro-Izquierda/Centro-Izquierda-Sexo/CEP-pospol-Centro-Izquierda-Sexo.xlsx")

# Izquierda
pp_izq<- readxl::read_excel("output/Pospol/Izquierda/CEP-pospol-Izquierda.xlsx")

pp_izq_edad <- readxl::read_excel("output/Pospol/Izquierda/Izquierda-Edad/CEP-pospol-Izquierda-Edad.xlsx")
pp_izq_esc <- readxl::read_excel("output/Pospol/Izquierda/Izquierda-Escolaridad/CEP-pospol-Izquierda-Escolaridad.xlsx")
pp_izq_nse <- readxl::read_excel("output/Pospol/Izquierda/Izquierda-NSE/CEP-pospol-Izquierda-NSE.xlsx")
pp_izq_sexo <- readxl::read_excel("output/Pospol/Izquierda/Izquierda-Sexo/CEP-pospol-Izquierda-Sexo.xlsx")

# Independiente
pp_indep <- readxl::read_excel("output/Pospol/Independiente/CEP-pospol-Independiente.xlsx")

pp_indep_edad <- readxl::read_excel("output/Pospol/Independiente/Independiente-Edad/CEP-pospol-Independiente-Edad.xlsx")
pp_indep_esc <- readxl::read_excel("output/Pospol/Independiente/Independiente-Escolaridad/CEP-pospol-Independiente-Escolaridad.xlsx")
pp_indep_nse <- readxl::read_excel("output/Pospol/Independiente/Independiente-NSE/CEP-pospol-Independiente-NSE.xlsx")
pp_indep_sexo <- readxl::read_excel("output/Pospol/Independiente/Independiente-Sexo/CEP-pospol-Independiente-Sexo.xlsx")

# Ninguna
pp_nin <- readxl::read_excel("output/Pospol/Ninguna/CEP-pospol-Ninguna.xlsx")

pp_nin_edad <- readxl::read_excel("output/Pospol/Ninguna/Ninguna-Edad/CEP-pospol-Ninguna-Edad.xlsx")
pp_nin_esc <- readxl::read_excel("output/Pospol/Ninguna/Ninguna-Escolaridad/CEP-pospol-Ninguna-Escolaridad.xlsx")
pp_nin_nse <- readxl::read_excel("output/Pospol/Ninguna/Ninguna-NSE/CEP-pospol-Ninguna-NSE.xlsx")
pp_nin_sexo <- readxl::read_excel("output/Pospol/Ninguna/Ninguna-Sexo/CEP-pospol-Ninguna-Sexo.xlsx")

# ---- Reordenar dataframes para recodificar ----


# Identificacion partidaria derecha

ip_der <- ip_der %>% pivot_wider(names_from = year, values_from = porc_idpart_der, names_prefix = "D") %>% transmute("1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

ip_der_sexo <- ip_der_sexo[-c(3,6,12),]
ip_der_sexo <- ip_der_sexo %>% pivot_wider(names_from = year, values_from = porc_idpart_der, names_prefix = "D") %>% transmute(sexo, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
ip_der_sexo <- ip_der_sexo[-3,]

ip_der_edad <- ip_der_edad %>% pivot_wider(names_from = year, values_from = porc_idpart_der, names_prefix = "D") %>% transmute(edad, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

ip_der_esc <- ip_der_esc[-c(5,10,15,78),]
ip_der_esc <- ip_der_esc %>% pivot_wider(names_from = year, values_from = porc_idpart_der, names_prefix = "D") %>% transmute(esc, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
ip_der_esc <- ip_der_esc[-c(5,6),]

ip_der_nse <- ip_der_nse %>% pivot_wider(names_from = year, values_from = porc_idpart_der, names_prefix = "D") %>% transmute(nse, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
ip_der_nse <- ip_der_nse[-6,]



# Identificacion partidaria centro izquierda
ip_centizq <- ip_centizq %>% pivot_wider(names_from = year, values_from = porc_idpart_centizq, names_prefix = "D") %>% transmute("1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

ip_centizq_sexo <- ip_centizq_sexo[-c(3,6,12),]
ip_centizq_sexo <- ip_centizq_sexo %>% pivot_wider(names_from = year, values_from = porc_idpart_centizq, names_prefix = "D") %>% transmute(sexo, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
ip_centizq_sexo <- ip_centizq_sexo[-3,]

ip_centizq_edad <- ip_centizq_edad[-28,]
ip_centizq_edad <- ip_centizq_edad %>% pivot_wider(names_from = year, values_from = porc_idpart_centizq, names_prefix = "D") %>% transmute(edad, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

ip_centizq_esc <- ip_centizq_esc[-c(5,10,15,78),]
ip_centizq_esc <- ip_centizq_esc %>% pivot_wider(names_from = year, values_from = porc_idpart_centizq, names_prefix = "D") %>% transmute(esc, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
ip_centizq_esc <- ip_centizq_esc[-c(5,6),]

ip_centizq_nse <- ip_centizq_nse %>% pivot_wider(names_from = year, values_from = porc_idpart_centizq, names_prefix = "D") %>% transmute(nse, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
ip_centizq_nse <- ip_centizq_nse[-6,]

# Identificacion partidaria izquierda extraparlamentaria
ip_izqext <- ip_izqext %>% pivot_wider(names_from = year, values_from = porc_idpart_izqext, names_prefix = "D") %>% transmute("1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

ip_izqext_sexo <- ip_izqext_sexo[-c(3,6,12),]
ip_izqext_sexo <- ip_izqext_sexo %>% pivot_wider(names_from = year, values_from = porc_idpart_izqext, names_prefix = "D") %>% transmute(sexo, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
ip_izqext_sexo <- ip_izqext_sexo[-3,]

ip_izqext_edad <- ip_izqext_edad %>% pivot_wider(names_from = year, values_from = porc_idpart_izqext, names_prefix = "D") %>% transmute(edad, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

ip_izqext_esc <- ip_izqext_esc[-c(5,10,15,78),]
ip_izqext_esc <- ip_izqext_esc %>% pivot_wider(names_from = year, values_from = porc_idpart_izqext, names_prefix = "D") %>% transmute(esc, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
ip_izqext_esc <- ip_izqext_esc[-c(5,6),]

ip_izqext_nse <- ip_izqext_nse %>% pivot_wider(names_from = year, values_from = porc_idpart_izqext, names_prefix = "D") %>% transmute(nse, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
ip_izqext_nse <- ip_izqext_nse[-6,]

# Identificacion partidaria ninguna
ip_nin <- ip_nin %>% pivot_wider(names_from = year, values_from = porc_idpart_nin, names_prefix = "D") %>% transmute("1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

ip_nin_sexo <- ip_nin_sexo[-c(3,6,12),]
ip_nin_sexo <- ip_nin_sexo %>% pivot_wider(names_from = year, values_from = porc_idpart_nin, names_prefix = "D") %>% transmute(sexo, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
ip_nin_sexo <- ip_nin_sexo[-3,]

ip_nin_edad <- ip_nin_edad %>% pivot_wider(names_from = year, values_from = porc_idpart_nin, names_prefix = "D") %>% transmute(edad, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

ip_nin_esc <- ip_nin_esc[-c(5,10,15,78),]
ip_nin_esc <- ip_nin_esc %>% pivot_wider(names_from = year, values_from = porc_idpart_nin, names_prefix = "D") %>% transmute(esc, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
ip_nin_esc <- ip_nin_esc[-c(5,6),]

ip_nin_nse <- ip_nin_nse %>% pivot_wider(names_from = year, values_from = porc_idpart_nin, names_prefix = "D") %>% transmute(nse, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
ip_nin_nse <- ip_nin_nse[-6,]

# Posicion politica

# Posicion politica derecha

pp_der <- pp_der %>% pivot_wider(names_from = year, values_from = porc_pospol_der, names_prefix = "D") %>% transmute("1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_der_sexo <- pp_der_sexo[-c(3,6),]
pp_der_sexo <- pp_der_sexo %>% pivot_wider(names_from = year, values_from = porc_pospol_der, names_prefix = "D") %>% transmute(sexo, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_der_sexo <- pp_der_sexo[-3,]

pp_der_edad <- pp_der_edad %>% pivot_wider(names_from = year, values_from = porc_pospol_der, names_prefix = "D") %>% transmute(edad, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_der_esc <- pp_der_esc[-c(5,10,15,78),]
pp_der_esc <- pp_der_esc %>% pivot_wider(names_from = year, values_from = porc_pospol_der, names_prefix = "D") %>% transmute(esc, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_der_esc<- pp_der_esc[-c(5,6),]

pp_der_nse <- pp_der_nse %>% pivot_wider(names_from = year, values_from = porc_pospol_der, names_prefix = "D") %>% transmute(nse, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_der_nse <- pp_der_nse[-6,]

# Posicion politica centro derecha

pp_centder <- pp_centder %>% pivot_wider(names_from = year, values_from = porc_pospol_centder, names_prefix = "D") %>% transmute("1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_centder_sexo <- pp_centder_sexo[-c(3,6),]
pp_centder_sexo <- pp_centder_sexo %>% pivot_wider(names_from = year, values_from = porc_pospol_centder, names_prefix = "D") %>% transmute(sexo, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_centder_sexo <- pp_centder_sexo[-3,]

pp_centder_edad <- pp_centder_edad %>% pivot_wider(names_from = year, values_from = porc_pospol_centder, names_prefix = "D") %>% transmute(edad, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_centder_esc <- pp_centder_esc[-c(5,10,15,78),]
pp_centder_esc <- pp_centder_esc %>% pivot_wider(names_from = year, values_from = porc_pospol_centder, names_prefix = "D") %>% transmute(esc, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_centder_esc<- pp_centder_esc[-c(5,6),]

pp_centder_nse <- pp_centder_nse %>% pivot_wider(names_from = year, values_from = porc_pospol_centder, names_prefix = "D") %>% transmute(nse, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_centder_nse <- pp_centder_nse[-6,]

# Posicion politica centro

pp_cent <- pp_cent %>% pivot_wider(names_from = year, values_from = porc_pospol_cent, names_prefix = "D") %>% transmute("1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_cent_sexo <- pp_cent_sexo[-c(3,6),]
pp_cent_sexo <- pp_cent_sexo %>% pivot_wider(names_from = year, values_from = porc_pospol_cent, names_prefix = "D") %>% transmute(sexo, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_cent_sexo <- pp_cent_sexo[-3,]

pp_cent_edad <- pp_cent_edad %>% pivot_wider(names_from = year, values_from = porc_pospol_cent, names_prefix = "D") %>% transmute(edad, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_cent_esc <- pp_cent_esc[-c(5,10,15,78),]
pp_cent_esc <- pp_cent_esc %>% pivot_wider(names_from = year, values_from = porc_pospol_cent, names_prefix = "D") %>% transmute(esc, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_cent_esc<- pp_cent_esc[-c(5,6),]

pp_cent_nse <- pp_cent_nse %>% pivot_wider(names_from = year, values_from = porc_pospol_cent, names_prefix = "D") %>% transmute(nse, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_cent_nse <- pp_cent_nse[-6,]

# Posicion politica centro izquierda

pp_centizq <- pp_centizq %>% pivot_wider(names_from = year, values_from = porc_pospol_centizq, names_prefix = "D") %>% transmute("1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_centizq_sexo <- pp_centizq_sexo[-c(3,6),]
pp_centizq_sexo <- pp_centizq_sexo %>% pivot_wider(names_from = year, values_from = porc_pospol_centizq, names_prefix = "D") %>% transmute(sexo, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_centizq_sexo <- pp_centizq_sexo[-3,]

pp_centizq_edad <- pp_centizq_edad %>% pivot_wider(names_from = year, values_from = porc_pospol_centizq, names_prefix = "D") %>% transmute(edad, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_centizq_esc <- pp_centizq_esc[-c(5,10,15,78),]
pp_centizq_esc <- pp_centizq_esc %>% pivot_wider(names_from = year, values_from = porc_pospol_centizq, names_prefix = "D") %>% transmute(esc, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_centizq_esc<- pp_centizq_esc[-c(5,6),]

pp_centizq_nse <- pp_centizq_nse %>% pivot_wider(names_from = year, values_from = porc_pospol_centizq, names_prefix = "D") %>% transmute(nse, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_centizq_nse <- pp_centizq_nse[-6,]

# Posicion politica izquierda

pp_izq <- pp_izq %>% pivot_wider(names_from = year, values_from = porc_pospol_izq, names_prefix = "D") %>% transmute("1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_izq_sexo <- pp_izq_sexo[-c(3,6),]
pp_izq_sexo <- pp_izq_sexo %>% pivot_wider(names_from = year, values_from = porc_pospol_izq, names_prefix = "D") %>% transmute(sexo, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_izq_sexo <- pp_izq_sexo[-3,]

pp_izq_edad <- pp_izq_edad %>% pivot_wider(names_from = year, values_from = porc_pospol_izq, names_prefix = "D") %>% transmute(edad, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_izq_esc <- pp_izq_esc[-c(5,10,15,78),]
pp_izq_esc <- pp_izq_esc %>% pivot_wider(names_from = year, values_from = porc_pospol_izq, names_prefix = "D") %>% transmute(esc, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_izq_esc<- pp_izq_esc[-c(5,6),]

pp_izq_nse <- pp_izq_nse %>% pivot_wider(names_from = year, values_from = porc_pospol_izq, names_prefix = "D") %>% transmute(nse, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_izq_nse <- pp_izq_nse[-6,]

# Posicion politica independientes

pp_indep <- pp_indep %>% pivot_wider(names_from = year, values_from = porc_pospol_indep, names_prefix = "D") %>% transmute("1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_indep_sexo <- pp_indep_sexo[-c(3,6),]
pp_indep_sexo <- pp_indep_sexo %>% pivot_wider(names_from = year, values_from = porc_pospol_indep, names_prefix = "D") %>% transmute(sexo, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_indep_sexo <- pp_indep_sexo[-3,]

pp_indep_edad <- pp_indep_edad %>% pivot_wider(names_from = year, values_from = porc_pospol_indep, names_prefix = "D") %>% transmute(edad, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_indep_esc <- pp_indep_esc[-c(5,10,15,78),]
pp_indep_esc <- pp_indep_esc %>% pivot_wider(names_from = year, values_from = porc_pospol_indep, names_prefix = "D") %>% transmute(esc, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_indep_esc<- pp_indep_esc[-c(5,6),]

pp_indep_nse <- pp_indep_nse %>% pivot_wider(names_from = year, values_from = porc_pospol_indep, names_prefix = "D") %>% transmute(nse, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_indep_nse <- pp_indep_nse[-6,]

# Posicion politica ninguna

pp_nin <- pp_nin %>% pivot_wider(names_from = year, values_from = porc_pospol_nin, names_prefix = "D") %>% transmute("1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_nin_sexo <- pp_nin_sexo[-c(3,6),]
pp_nin_sexo <- pp_nin_sexo %>% pivot_wider(names_from = year, values_from = porc_pospol_nin, names_prefix = "D") %>% transmute(sexo, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_nin_sexo <- pp_nin_sexo[-3,]

pp_nin_edad <- pp_nin_edad %>% pivot_wider(names_from = year, values_from = porc_pospol_nin, names_prefix = "D") %>% transmute(edad, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))

pp_nin_esc <- pp_nin_esc[-c(5,10,15,78),]
pp_nin_esc <- pp_nin_esc %>% pivot_wider(names_from = year, values_from = porc_pospol_nin, names_prefix = "D") %>% transmute(esc, "1990" = D1990, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_nin_esc<- pp_nin_esc[-c(5,6),]

pp_nin_nse <- pp_nin_nse %>% pivot_wider(names_from = year, values_from = porc_pospol_nin, names_prefix = "D") %>% transmute(nse, "1991-1993" = ((D1991+D1992+D1993)/3), "1994-1996" = ((D1994+D1995+D1996)/3), "1997-1999" = ((D1997+D1998+D1999)/3), "2000-2002" = ((D2000+D2001+D2002)/3), "2003-2005" = ((D2003+D2004+D2005)/3), "2006-2008" = ((D2006+D2007+D2008)/3), "2009-2011" = ((D2009+D2010+D2011)/3), "2012-2014" = ((D2012+D2013+D2014)/3), "2015-2017" = ((D2015+D2016+D2017)/3), "2018-2019" = ((D2018+D2019)/2))
pp_nin_nse <- pp_nin_nse[-6,]

# Save datasets

# Mucha confianza
writexl::write_xlsx(mci, "output/CEP-Datasets/V.2CEP-Mucha-Confianza-Inst.xlsx")

writexl::write_xlsx(mci_edad, "output/CEP-Datasets/V.2CEP-Mucha-Confianza-Inst-Edad.xlsx")
writexl::write_xlsx(mci_sexo, "output/CEP-Datasets/v.2CEP-Mucha-Confianza-Inst-Sexo.xlsx")
writexl::write_xlsx(mci_esc, "output/CEP-Datasets/V.2CEP-Mucha-Confianza-Inst-Escolaridad.xlsx")
writexl::write_xlsx(mci_nse, "output/CEP-Datasets/v.2CEP-Mucha-Confianza-Inst-NSE.xlsx")



writexl::write_xlsx(mcp, "output/CEP-Datasets/V.2CEP-Mucha-Confianza-Per.xlsx")

writexl::write_xlsx(mcp_edad, "output/CEP-Datasets/V.2CEP-Mucha-Confianza-Per-Edad.xlsx")
writexl::write_xlsx(mcp_sexo, "output/CEP-Datasets/V.2CEP-Mucha-Confianza-Per-Sexo.xlsx")
writexl::write_xlsx(mcp_esc, "output/CEP-Datasets/V.2CEP-Mucha-Confianza-Per-Escolaridad.xlsx")
writexl::write_xlsx(mcp_nse, "output/CEP-Datasets/V.2CEP-Mucha-Confianza-Per-NSE.xlsx")

# Identificacion paridaria
# Derecha

writexl::write_xlsx(ip_der, "output/CEP-Datasets/V.2CEP-Idpart-Derecha.xlsx")

writexl::write_xlsx(ip_der_edad, "output/CEP-Datasets/V.2CEP-Idpart-Derecha-Edad.xlsx")
writexl::write_xlsx(ip_der_esc, "output/CEP-Datasets/V.2CEP-Idpart-Derecha-Escolaridad.xlsx")
writexl::write_xlsx(ip_der_nse, "output/CEP-Datasets/V.2CEP-Idpart-Derecha-NSE.xlsx")
writexl::write_xlsx(ip_der_sexo, "output/CEP-Datasets/V.2CEP-Idpart-Derecha-Sexo.xlsx")

# Centro izqueirda concerta
writexl::write_xlsx(ip_centizq, "output/CEP-Datasets/V.2CEP-Idpart-Centro-Izquierda-Concertacion.xlsx")

writexl::write_xlsx(ip_centizq_edad, "output/CEP-Datasets/V.2CEP-Idpart-Centro-Izquierda-Concertacion-Edad.xlsx")
writexl::write_xlsx(ip_centizq_esc, "output/CEP-Datasets/V.2CEP-Idpart-Centro-Izquierda-Concertacion-Escolaridad.xlsx")
writexl::write_xlsx(ip_centizq_nse, "output/CEP-Datasets/V.2CEP-Idpart-Centro-Izquierda-Concertacion-NSE.xlsx")
writexl::write_xlsx(ip_centizq_sexo, "output/CEP-Datasets/V.2CEP-Idpart-Centro-Izquierda-Concertacion-Sexo.xlsx")

# Ninguno
writexl::write_xlsx(ip_nin, "output/CEP-Datasets/V.2CEP-Idpart-Ninguno.xlsx")

writexl::write_xlsx(ip_nin_edad, "output/CEP-Datasets/V.2CEP-Idpart-Ninguno-Edad.xlsx")
writexl::write_xlsx(ip_nin_esc, "output/CEP-Datasets/V.2CEP-Idpart-Ninguno-Escolaridad.xlsx")
writexl::write_xlsx(ip_nin_nse, "output/CEP-Datasets/V.2CEP-Idpart-Ninguno-NSE.xlsx")
writexl::write_xlsx(ip_nin_sexo, "output/CEP-Datasets/V.2CEP-Idpart-Ninguno-Sexo.xlsx")

# Izquierda extraconcertacion
writexl::write_xlsx(ip_nin, "output/CEP-Datasets/V.2CEP-Idpart-Izquierda-Extraconcertacion.xlsx")

writexl::write_xlsx(ip_nin_edad, "output/CEP-Datasets/V.2CEP-Idpart-Izquierda-Extraconcertacion-Edad.xlsx")
writexl::write_xlsx(ip_nin_esc, "output/CEP-Datasets/V.2CEP-Idpart-Izquierda-Extraconcertacion-Escolaridad.xlsx")
writexl::write_xlsx(ip_nin_nse, "output/CEP-Datasets/V.2CEP-Idpart-Izquierda-Extraconcertacion-NSE.xlsx")
writexl::write_xlsx(ip_nin_sexo, "output/CEP-Datasets/V.2CEP-Idpart-Izquierda-Extraconcertacion-Sexo.xlsx")

# Posicion politica

# Derecha

writexl::write_xlsx(pp_der, "output/CEP-Datasets/V.2CEP-Pospol-Derecha.xlsx")

writexl::write_xlsx(pp_der_edad, "output/CEP-Datasets/V.2CEP-Pospol-Derecha-Edad.xlsx")
writexl::write_xlsx(pp_der_esc, "output/CEP-Datasets/V.2CEP-Pospol-Derecha-Escolaridad.xlsx")
writexl::write_xlsx(pp_der_nse, "output/CEP-Datasets/V.2CEP-Pospol-Derecha-NSE.xlsx")
writexl::write_xlsx(pp_der_sexo, "output/CEP-Datasets/V.2CEP-Pospol-Derecha-Sexo.xlsx")

# Centro derecha

writexl::write_xlsx(pp_centder, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Derecha.xlsx")

writexl::write_xlsx(pp_centder_edad, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Derecha-Edad.xlsx")
writexl::write_xlsx(pp_centder_esc, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Derecha-Escolaridad.xlsx")
writexl::write_xlsx(pp_centder_nse, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Derecha-NSE.xlsx")
writexl::write_xlsx(pp_centder_sexo, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Derecha-Sexo.xlsx")

# Centro

writexl::write_xlsx(pp_cent, "output/CEP-Datasets/V.2CEP-Pospol-Centro.xlsx")

writexl::write_xlsx(pp_cent_edad, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Edad.xlsx")
writexl::write_xlsx(pp_cent_esc, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Escolaridad.xlsx")
writexl::write_xlsx(pp_cent_nse, "output/CEP-Datasets/V.2CEP-Pospol-Centro-NSE.xlsx")
writexl::write_xlsx(pp_cent_sexo, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Sexo.xlsx")

# Centro izquierda

writexl::write_xlsx(pp_centizq, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Izquierda.xlsx")

writexl::write_xlsx(pp_centizq_edad, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Izquierda-Edad.xlsx")
writexl::write_xlsx(pp_centizq_esc, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Izquierda-Escolaridad.xlsx")
writexl::write_xlsx(pp_centizq_nse, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Izquierda-NSE.xlsx")
writexl::write_xlsx(pp_centizq_sexo, "output/CEP-Datasets/V.2CEP-Pospol-Centro-Izquierda-Sexo.xlsx")

# Izquierda

writexl::write_xlsx(pp_izq, "output/CEP-Datasets/V.2CEP-Pospol-Izquierda.xlsx")

writexl::write_xlsx(pp_izq_edad, "output/CEP-Datasets/V.2CEP-Pospol-Izquierda-Edad.xlsx")
writexl::write_xlsx(pp_izq_esc, "output/CEP-Datasets/V.2CEP-Pospol-Izquierda-Escolaridad.xlsx")
writexl::write_xlsx(pp_izq_nse, "output/CEP-Datasets/V.2CEP-Pospol-Izquierda-NSE.xlsx")
writexl::write_xlsx(pp_izq_sexo, "output/CEP-Datasets/V.2CEP-Pospol-Izquierda-Sexo.xlsx")

# Independiente

writexl::write_xlsx(pp_indep, "output/CEP-Datasets/V.2CEP-Pospol-Independiente.xlsx")

writexl::write_xlsx(pp_indep_edad, "output/CEP-Datasets/V.2CEP-Pospol-Independiente-Edad.xlsx")
writexl::write_xlsx(pp_indep_esc, "output/CEP-Datasets/V.2CEP-Pospol-Independiente-Escolaridad.xlsx")
writexl::write_xlsx(pp_indep_nse, "output/CEP-Datasets/V.2CEP-Pospol-Independiente-NSE.xlsx")
writexl::write_xlsx(pp_indep_sexo, "output/CEP-Datasets/V.2CEP-Pospol-Independiente-Sexo.xlsx")

# Ninguna

writexl::write_xlsx(pp_nin, "output/CEP-Datasets/V.2CEP-Pospol-Ninguna.xlsx")

writexl::write_xlsx(pp_nin_edad, "output/CEP-Datasets/V.2CEP-Pospol-Ninguna-Edad.xlsx")
writexl::write_xlsx(pp_nin_esc, "output/CEP-Datasets/V.2CEP-Pospol-Ninguna-Escolaridad.xlsx")
writexl::write_xlsx(pp_nin_nse, "output/CEP-Datasets/V.2CEP-Pospol-Ninguna-NSE.xlsx")
writexl::write_xlsx(pp_nin_sexo, "output/CEP-Datasets/V.2CEP-Pospol-Ninguna-Sexo.xlsx")



