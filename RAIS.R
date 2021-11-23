library(tidyverse)

# Importação dos dados da RAIS -------------------------------------------------
RAIS <- read.csv2("RAIS_VINC_PUB_SUL.txt", sep = ";")

## Filtrar bases por município  
POA <- RAIS%>%
  filter(Município == 431490)

rm(RAIS)

## Vinculos empregativos ativos 
POA <- POA%>%
  filter(Vínculo.Ativo.31.12 == 1)

## Add coluna ano na base
POA <- POA%>%
  add_column(Ano = 2019)

## Criar CBO de 4 digitos ------------------------------------------------------
POA$CBO.Ocupação.2002 <- as.numeric(POA$CBO.Ocupação.2002)

POA <- POA%>%
  mutate(Ocupacao.4.digitos = round(CBO.Ocupação.2002/100))


## Criar grupos por CNAE e CBO -------------------------------------------------
POA <- POA%>%
  mutate(GrupoCBO = case_when(Ocupacao.4 %in% c(1311, 2612, 2613, 2711, 3250, 3711, 3712, 3761, 3762, 7911, 8401)~11,
                           Ocupacao.4 %in% c(2349, 2618, 2623, 2624, 2625, 2626, 2627, 2628, 3524, 3741, 3742, 7421, 7523, 7524, 7664, 9152)~12,
                           CBO.Ocupação.2002 %in% c(376310, 516130, 715515)~12, 
                           Ocupacao.4 %in% c(2534, 2611, 2614, 2616, 2615, 3713, 7606, 7661, 7662, 7663, 7686, 7687)~21,
                           Ocupacao.4 %in% c(2617, 2619, 2621, 2622, 3721, 3731, 3742, 3743, 3744)~22,
                           CBO.Ocupação.2002 %in% c(376315, 376320)~22,
                           Ocupacao.4 %in% c(2141, 2629, 3184, 3188, 3751, 3764, 7510, 7511, 7531, 7533, 7711, 7751)~31,
                           Ocupacao.4 %in% c(1233, 1423, 2531, 4241)~32,
                           Ocupacao.4 %in% c(1237, 1313, 1426, 2030, 2031, 2032, 2033, 2034, 2035, 2343, 2346, 2347, 2511, 2514, 3951)~41,
                           CBO.Ocupação.2002 %in% c(234120)~41,
                           Ocupacao.4 %in% c(1236, 1425, 2122, 2124, 3171)~42))

POA$GrupoCBO[is.na(POA$GrupoCBO)] <- 99

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


# # Total vinc -------------------------------------------------------------------

# POA <- read.csv2("POA2017.csv", sep = ",")

POA <- POA%>%
  add_column(Dummy = 1)

CBO <- POA%>%
  group_by(Grupo, Ano, Descricao)%>%
  summarise(Total = sum(Dummy))


CBO2009 <- CBO[-9, ]

CBO <- rbind(CBO2009, CBO2010, CBO2011, CBO2012, CBO2013, CBO2014, CBO2015, CBO2016, CBO2017, CBO2018, CBO2019)
rm(CBO2009, CBO2010, CBO2011, CBO2012, CBO2013, CBO2014, CBO2015, CBO2016, CBO2017, CBO2018, CBO2019)



ggplot(CBO, mapping = aes(x = Ano, y =  Total, shape = Descricao))+
  geom_line(linetype = "dashed", size = 1)+
  geom_point(size = 5)+
  theme_bw()+
  labs(y = NULL, x = NULL, shape = NULL)+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = "."), expand = c(0, 0), limits = c(0, 14000), breaks = seq(from = 0, to = 14000, by = 2500))+
  scale_x_continuous(breaks = seq(from = 2009, to = 2019, by = 1))+
  theme(legend.position="bottom",
        legend.text = element_text(size = 19),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_shape_manual(values = c(0, 1, 17, 15, 7, 2 , 6, 19))

## SEXO ------------------------------------------------------------------------
POA <- read.csv2("POA2019.csv", sep = ",")

POA <- POA%>%
  add_column(Dummy = 1)

SEXO_POA_CBO <- POA%>%
  group_by(Grupo, Sexo.Trabalhador)%>%
  summarise(TotalSexo = sum(Dummy))

SEXO_POA_CBO<- na.omit(SEXO_POA_CBO)

SEXO_POA_CBO%>%
  group_by(Grupo)%>%
  summarise(sum(TotalSexo))

SEXO_POA_CBO <- SEXO_POA_CBO%>%
  mutate(Total = case_when(Grupo == 11 ~ 1804,
                           Grupo == 12 ~ 1416,
                           Grupo == 21 ~ 2429,
                           Grupo == 22 ~ 1405,  
                           Grupo == 31 ~ 1334,
                           Grupo == 32 ~ 8005,  
                           Grupo == 41 ~ 2346,  
                           Grupo == 42 ~ 12009))

SEXO_POA_CBO <- SEXO_POA_CBO%>%
  mutate(Percentual = (TotalSexo/Total)*100)

## ESCOLARIDADE ----------------------------------------------------------------


POA <- read.csv2("POA2019.csv", sep = ",")

POA <- POA%>%
  filter(Vínculo.Ativo.31.12 == 1)

POA <- POA%>%
  add_column(Dummy = 1)

POA <- POA%>%
  mutate(Escolaridade = case_when(Escolaridade.após.2005 %in% c(1,2,3,4) ~ "Fundamental Incompleto",
                                  Escolaridade.após.2005 %in% c(5, 6) ~ "Fundamental completo",
                                  Escolaridade.após.2005 %in% c(7, 8) ~ "Médio completo",
                                  Escolaridade.após.2005 %in% c(9, 10, 11) ~ "Superior completo +"))

POA <- POA%>%
  group_by(Grupo, Escolaridade)%>%
  summarise(TotalEscolaridade = sum(Dummy))

POA2019 <- POA%>%add_column(.after = "Grupo", Ano = 2019)

POA%>%group_by(Grupo)%>%
  summarise(Total = sum(TotalEscolaridade))%>%
  print(n = 40)

POA <- POA%>%
  mutate(Total = case_when(Grupo == 11 ~ 1804,
                           Grupo == 12 ~ 1416,
                           Grupo == 21 ~ 2429,
                           Grupo == 22 ~ 1405,  
                           Grupo == 31 ~ 1334,
                           Grupo == 32 ~ 8005,  
                           Grupo == 41 ~ 2346,  
                           Grupo == 42 ~ 12009))

ESCOLARIDADE_POA_CBOs <- POA%>%
  mutate(Percentual = (TotalEscolaridade/Total)*100)

## idade -----------------------------------------------------------------------

SalarioMedio <- POA%>%
  group_by(Grupo)%>%
  summarise(mean(Vl.Remun.Dezembro.Nom))

## Massa salarial --------------------------------------------------------------
POA <- read.csv2("POA2019.csv", sep = ",")

POA$Vl.Remun.Dezembro.Nom <- as.numeric(gsub(",",".",POA$Vl.Remun.Dezembro.Nom,fixed=TRUE))

POA$Vl.Remun.Dezembro.Nom <- as.numeric(POA$Vl.Remun.Dezembro.Nom)

Salario <- POA%>%
  group_by(Grupo, Ano)%>%
  summarise(Salario = sum(Vl.Remun.Dezembro.Nom))

Salario2019 <- Salario[-9, ]

rm(POA, Salario)

Salario <- rbind(Salario2009,Salario2010, Salario2011, Salario2012, Salario2013, Salario2014,
   Salario2015, Salario2016, Salario2017, Salario2018, Salario2019)

rm(Salario2009,Salario2010, Salario2011, Salario2012, Salario2013, Salario2014,
   Salario2015, Salario2016, Salario2017, Salario2018, Salario2019)

Salario <- left_join(Salario, IPCA, by = "Ano")

Salario <- Salario%>%
  mutate(SalarioDezIPCA = Salario*IPCAam)

Salario <- left_join(Salario, Descricao, by = "Grupo")

## ggplot ----------------------------------------------------------------------

ggplot(Salario, mapping = aes(x = Ano, y =  SalarioDezIPCA, shape = Descricao))+
  geom_line(linetype = "dashed", size = 1)+
  geom_point(size = 5)+
  theme_bw()+
  labs(y = NULL, x = NULL, shape = NULL)+
  scale_y_continuous(labels = function(x) paste("R$", format(x, scientific = FALSE, big.mark = ".")))+
  scale_x_continuous(breaks = seq(from = 2009, to = 2019, by = 1))+
  theme(legend.position="bottom",
        legend.text = element_text(size = 17),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_shape_manual(values = c(0, 1, 17, 15, 7, 2 , 6, 19))

## CNAE E CBO ------------------------------------------------------------------

POA <- read.csv2("POA2019.csv", sep = ",")

POA <- POA%>%
  add_column(Dummy = 1)

CNAE <- POA%>%group_by(GrupoCNAE, GrupoCBO)%>%summarise(TotalCBO = sum(Dummy))

CNAE2 <- CNAE%>%filter(GrupoCBO %in% c(11, 12, 21, 22, 31, 32, 41, 42))

CNAE2 <- CNAE2%>%
  mutate(Total = case_when(GrupoCNAE == 11 ~ 270,
                           GrupoCNAE == 12 ~ 368,
                           GrupoCNAE == 21 ~ 1412,
                           GrupoCNAE == 22 ~ 1369,  
                           GrupoCNAE == 31 ~ 551,
                           GrupoCNAE == 32 ~ 1121,  
                           GrupoCNAE == 41 ~ 1707,  
                           GrupoCNAE == 42 ~ 8161,
                           GrupoCNAE == 99 ~ 18395))

CNAE2 <- CNAE2%>%
  mutate(Percentual = (TotalCBO/Total)*100)

CNAE2$DescricaoCNAE[is.na(CNAE2$DescricaoCNAE)] <- "Outros"

CNAE2 <- left_join(CNAE2, Descricao, by= "Grupo")

CNAE2 <- rename(CNAE2, DescricaoCBO = Descricao)

CNAE2$DescricaoCNAE[is.na(CNAE2$DescricaoCNAE)] <- "Outros"

ggplot(CNAE2, aes(x = reorder(DescricaoCBO, -Percentual), y = Percentual, fill = reorder(DescricaoCBO, -Percentual)))+
  geom_col(position = "dodge", colour="black")+
  geom_text(aes(label=paste(format(round(Percentual, digits = 2), nsmall = 2, big.mark = "."), "%"), fill = DescricaoCBO), size = 5, position = position_dodge(width = .8), vjust = -0.2)+
  facet_wrap(~DescricaoCNAE,  ncol=2)+
  theme_bw()+
  scale_fill_brewer(palette = "Greys")+
  scale_y_continuous(labels = function(x) paste(x, "%"), limits = c(0, 100))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text = element_text(size = 23),
        axis.text.y = element_text(size = 20),
        strip.text = element_text(size = 20))+
  labs(y = NULL, x = NULL, fill = NULL)
