library(tidyverse)

# Importação dos dados ---------------------------------------------------------

POA <- read.csv("POA.csv", sep = ";")

# SEXO -------------------------------------------------------------------------

POA <- POA%>%
  add_column(Dummy = 1)

SEXO_POA_CBO <- POA%>%
  group_by(GrupoCBO, Sexo.Trabalhador)%>%
  summarise(TotalSexo = sum(Dummy))


## Total do Grupo --------------------------------------------------------------
SEXO_POA_CBO%>%
  group_by(GrupoCBO)%>%
  summarise(sum(TotalSexo))

## Colocar total depois do ~ ---------------------------------------------------  

SEXO_POA_CBO <- SEXO_POA_CBO%>%
  mutate(Total = case_when(Grupo == 11 ~ "",
                           Grupo == 12 ~ "",
                           Grupo == 21 ~ "",
                           Grupo == 22 ~ "",  
                           Grupo == 31 ~ "",
                           Grupo == 32 ~ "",  
                           Grupo == 41 ~ "",  
                           Grupo == 42 ~ ""))

## Calcular percentual ---------------------------------------------------------
SEXO_POA_CBO <- SEXO_POA_CBO%>%
  mutate(Percentual = (TotalSexo/Total)*100)

# ESCOLARIDADE -----------------------------------------------------------------

POA_ESCOLARIDADE <- POA%>%
  mutate(Escolaridade = case_when(Escolaridade.após.2005 %in% c(1,2,3,4) ~ "Fundamental Incompleto",
                                  Escolaridade.após.2005 %in% c(5, 6) ~ "Fundamental completo",
                                  Escolaridade.após.2005 %in% c(7, 8) ~ "Médio completo",
                                  Escolaridade.após.2005 %in% c(9, 10, 11) ~ "Superior completo +"))
## Total escolaridade ----------------------------------------------------------
POA <- POA%>%
  group_by(GrupoCBO, Escolaridade)%>%
  summarise(TotalEscolaridade = sum(Dummy))

## Total do Grupo --------------------------------------------------------------

POA%>%group_by(GrupoCBO)%>%
  summarise(Total = sum(TotalEscolaridade))%>%
  print(n = 40)

## Colocar total depois do ~ ---------------------------------------------------  

POA_ESCOLARIDADE <- POA_ESCOLARIDADE%>%
  mutate(Total = case_when(Grupo == 11 ~ "",
                           Grupo == 12 ~ "",
                           Grupo == 21 ~ "",
                           Grupo == 22 ~ "",  
                           Grupo == 31 ~ "",
                           Grupo == 32 ~ "",  
                           Grupo == 41 ~ "",  
                           Grupo == 42 ~ ""))

## Calcular percentual ---------------------------------------------------------

POA_ESCOLARIDADE <- POA_ESCOLARIDADE%>%
  mutate(Percentual = (TotalEscolaridade/Total)*100)

# Idade ------------------------------------------------------------------------

IDADE_POA_CBO <- POA%>%
  group_by(GrupoCBO)%>%
  summarise(mean(Idade))

