library(tidyverse)
# Importação dos dados ---------------------------------------------------------

POA <- read.csv("RAIS_VINC_PUB_SUL.txt", sep = ";")

## Filtrar bases por município  

POA <- RAIS%>%
  filter(Município == 431490)

rm(RAIS)

## Vínculos empregativos ativos 

POA <- POA%>%
  filter(Ind.Atividade.Ano == 1)

## Criar grupos por CNAE e CBO -------------------------------------------------

POA <- POA%>%
  mutate(GrupoCNAE = case_when(CNAE.2.0.Classe %in% c(47890, 91015, 91023, 91031, 93212, 93298, 94936)~11,
                               CNAE.2.0.Classe %in% c(18300, 32205, 47563, 47890, 47890, 59201, 74200, 82300, 90019, 90027, 90035, 93298)~12,
                               CNAE.2.0.Classe %in% c(18113, 18211, 18229, 46478, 47610, 58115, 58123, 58131, 58212, 58221, 58239, 63194, 63917)~21,
                               CNAE.2.0.Classe %in% c(26400, 26701, 26809, 47628, 59111, 59120, 59138, 59146, 60101, 60217, 60225, 61418, 61434, 77225)~22,
                               CNAE.2.0.Classe %in% c(14118, 14126, 14142, 14215, 14223, 15211, 15319, 15327, 15335, 15394, 32116, 32124, 32400, 46168,
                                                      46427, 46435, 47814, 47822, 47831, 47857, 71111, 74102, 77233)~31,
                               CNAE.2.0.Classe %in% c(18130, 73114, 73122, 73190, 73203)~32,
                               CNAE.2.0.Classe %in% c(72100, 72207, 82300, 85317, 85325, 85333, 85929, 85937)~41,
                               CNAE.2.0.Classe %in% c(26108, 26213, 26221, 46516, 47512, 62015, 62023, 62031, 62040, 63119)~42))

POA$GrupoCNAE[is.na(POA$GrupoCNAE)] <- 99

## Descrição Grupo -------------------------------------------------------------

Grupo <- c(11, 12, 21, 22, 31, 32, 41, 42)
Descricao <- c("Patrimônio e Culturas Tradicionais", "Artes Visuais e Performáticas", 
               "Publicação, editoração e mídia", "Audiovisual", "Arquitetura, Design e Moda", 
               "Publicidade e pesquisa de mercado", "Ensino e pesquisa", "TI e software, pesquisa e desenvolvimento")

Descricao <- data.frame(Grupo, Descricao)

Descricao$Grupo <- as.numeric(gsub(",","",Descricao$Grupo,fixed=TRUE))

POA <- left_join(POA, Descricao, by = "Grupo")


POA$Descricao[is.na(POA$Descricao)] <- "Outros"

## Criar Série ----------------------------------------------------------------- 
POA_VINC <- rbind(POA2009, POA2010, POA2011, POA2012, POA2013, POA2014, 
                  POA2015, POA2016, POA2017, POA2018, POA2019)

rm(POA2009, POA2010, POA2011, POA2012, POA2013, 
   POA2014, POA2015, POA2016, POA2017, POA2018, POA2019)
