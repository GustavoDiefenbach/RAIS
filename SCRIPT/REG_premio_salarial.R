library(dplyr)
library(tibble)
library(plm)
library(ggplot2)


## Importação das bases --------------------------------------------------------
POA2019 <- read.csv2("POA2019.csv", sep = ";")

## Filtro vínculos ativos ------------------------------------------------------
POA2019 <- POA2019%>%
  filter(Vínculo.Ativo.31.12 == 1)

## ADD ano ---------------------------------------------------------------------
POA2019 <- POA2019%>%
  add_column(Ano = 2019)

## Faixa de escolaridade -------------------------------------------------------

POA2019 <- POA2019%>%
  mutate(Escolaridade = case_when(Escolaridade.após.2005 %in% c(1,2,3,4) ~ "Fundamental Incompleto",
                                  Escolaridade.após.2005 %in% c(5, 6) ~ "Fundamental completo",
                                  Escolaridade.após.2005 %in% c(7, 8) ~ "Médio completo",
                                  Escolaridade.após.2005 %in% c(9, 10, 11) ~ "Superior completo +"))

## Faixa Etária ----------------------------------------------------------------

POA2019 <- POA2019%>%
  mutate(Faixa.Etária = case_when(Idade %in% c(18, 19, 21, 22, 23) ~ "18-23",
                                  Idade %in% c(24, 25, 26, 27, 28) ~ "24-28",
                                  Idade %in% c(29, 30, 31, 32, 33) ~ "29-33",
                                  Idade %in% c(34, 35, 36, 37, 38) ~ "34 - 38",
                                  Idade %in% c(39, 40, 41, 42, 43) ~ "39 - 43",
                                  Idade %in% c(44, 45, 46, 47, 48) ~ "44 - 48",
                                  Idade %in% c(49, 50, 51, 52, 53) ~ "49 - 53",
                                  Idade %in% c(54, 55, 56, 57, 58) ~ "54 - 58", 
                                  Idade %in% c(59, 60, 61, 62, 63) ~ "59 - 63",
                                  Idade %in% c(64, 65, 66, 67, 68) ~ "64 - 68",
                                  Idade %in% c(69, 70, 71, 72, 73) ~ "69 - 73"))

PAINEL_POA <- rbind(POA2009, POA2014, POA2019)

PAINEL_POA$GrupoCNAE[is.na(PAINEL_POA$GrupoCNAE)] = 99
PAINEL_POA$AreaCNAE[is.na(PAINEL_POA$AreaCNAE)] = 99
PAINEL_POA$GrupoCBOs[is.na(PAINEL_POA$GrupoCBOs)] = 99
PAINEL_POA$AreaCBOs[is.na(PAINEL_POA$AreaCBOs)] = 99

## Dummy tamanho empresa -------------------------------------------------------

PAINEL_POA$D_ZERO <- ifelse(PAINEL_POA$Tamanho.Estabelecimento == "1", 1, 0) 
PAINEL_POA$D_ATE_4 <- ifelse(PAINEL_POA$Tamanho.Estabelecimento == "2", 1, 0) 
PAINEL_POA$D_5_A_9 <- ifelse(PAINEL_POA$Tamanho.Estabelecimento == "3", 1, 0) 
PAINEL_POA$D_10_A_19 <- ifelse(PAINEL_POA$Tamanho.Estabelecimento == "4", 1, 0) 
PAINEL_POA$D_20_A_49 <- ifelse(PAINEL_POA$Tamanho.Estabelecimento == "5", 1, 0) 
PAINEL_POA$D_50_A_99 <- ifelse(PAINEL_POA$Tamanho.Estabelecimento == "6", 1, 0) 
PAINEL_POA$D_100_A_249 <- ifelse(PAINEL_POA$Tamanho.Estabelecimento == "7", 1, 0) 
PAINEL_POA$D_250_A_499 <- ifelse(PAINEL_POA$Tamanho.Estabelecimento == "8", 1, 0) 
PAINEL_POA$D_500_A_999 <- ifelse(PAINEL_POA$Tamanho.Estabelecimento == "9", 1, 0) 
PAINEL_POA$D_1000_OU_MAIS <- ifelse(PAINEL_POA$Tamanho.Estabelecimento == "10", 1, 0) 

## Dummy grupos CNAES ----------------------------------------------------------
PAINEL_POA$D_Grupo_11 <- ifelse(PAINEL_POA$GrupoCNAE == "11", 1, 0) 
PAINEL_POA$D_Grupo_12 <- ifelse(PAINEL_POA$GrupoCNAE == "12", 1, 0) 
PAINEL_POA$D_Grupo_21 <- ifelse(PAINEL_POA$GrupoCNAE == "21", 1, 0) 
PAINEL_POA$D_Grupo_22 <- ifelse(PAINEL_POA$GrupoCNAE == "22", 1, 0) 
PAINEL_POA$D_Grupo_31 <- ifelse(PAINEL_POA$GrupoCNAE == "31", 1, 0) 
PAINEL_POA$D_Grupo_32 <- ifelse(PAINEL_POA$GrupoCNAE == "32", 1, 0) 
PAINEL_POA$D_Grupo_41 <- ifelse(PAINEL_POA$GrupoCNAE == "41", 1, 0) 
PAINEL_POA$D_Grupo_42 <- ifelse(PAINEL_POA$GrupoCNAE == "42", 1, 0) 

## Sexo descrição --------------------------------------------------------------
PAINEL_POA <- PAINEL_POA%>%
  mutate(Sexo = case_when(Sexo.Trabalhador == 1 ~ "Masculino",
                          Sexo.Trabalhador == 2 ~ "Feminino"))


## Grupos dos indivduos por escolaridade, sexo e faixa etária ------------------
PAINEL_POA$Grupo <- paste(PAINEL_POA$Sexo, PAINEL_POA$Faixa.Etária, PAINEL_POA$Escolaridade)


## Valor da remuneração de dezembro corrigido pelo IPCA ------------------------
PAINEL_POA <- PAINEL_POA%>%
  mutate(Vl.Remun.Dezembro.IPCA = Vl.Remun.Dezembro.Nom*IPCAacum)


## Cálculo da média ------------------------------------------------------------ 
PAINEL_MEDIA <- PAINEL_POA%>%
  group_by(Ano, Grupo)%>%
  summarise(D_Grupo_11 = mean(D_Grupo_11), D_Grupo_12 = mean(D_Grupo_12), D_Grupo_21 = mean(D_Grupo_21), D_Grupo_22 = mean(D_Grupo_22), D_Grupo_31 = mean(D_Grupo_31),D_Grupo_32 = mean(D_Grupo_32),
            D_Grupo_41 = mean(D_Grupo_41),D_Grupo_42 = mean(D_Grupo_42), Qtd.Hora.Contr = mean(Qtd.Hora.Contr), Ind.Portador.Defic = mean(Ind.Portador.Defic), Vl.Remun.Dezembro.IPCA = mean(Vl.Remun.Dezembro.IPCA), 
            Tempo.Emprego = mean(Tempo.Emprego), D_ZERO = mean(D_ZERO),D_ATE_4= mean(D_ATE_4), D_5_A_9 = mean(D_5_A_9), D_10_A_19 = mean(D_10_A_19), D_20_A_49 = mean(D_20_A_49), D_50_A_99 = mean(D_50_A_99), 
            D_100_A_249 = mean(D_100_A_249), D_250_A_499 = mean(D_250_A_499), D_500_A_999 = mean(D_500_A_999), D_1000_OU_MAIS = mean(D_1000_OU_MAIS))

## Dummy Ano -------------------------------------------------------------------
PAINEL_MEDIA$D_2019 <- ifelse(PAINEL_MEDIA$Ano == "2019", 1, 0) 
PAINEL_MEDIA$D_2014 <- ifelse(PAINEL_MEDIA$Ano == "2014", 1, 0) 
PAINEL_MEDIA$D_2009 <- ifelse(PAINEL_MEDIA$Ano == "2009", 1, 0)

## Filtro valor zerado ---------------------------------------------------------
PAINEL_MEDIA <- PAINEL_MEDIA%>%
  filter(Vl.Remun.Dezembro.IPCA != 0)

## Log valor remuneração dezembro ----------------------------------------------
PAINEL_MEDIA$log.Vl.Remun.Dezembro.IPCA <- log(PAINEL_MEDIA$Vl.Remun.Dezembro.IPCA)

##------------------------------------------------------------------------------
PAINEL_MEDIA$D_Grupo_11 <- 100*(PAINEL_MEDIA$D_Grupo_11)
PAINEL_MEDIA$D_Grupo_12 <- 100*(PAINEL_MEDIA$D_Grupo_12)
PAINEL_MEDIA$D_Grupo_21 <- 100*(PAINEL_MEDIA$D_Grupo_21)
PAINEL_MEDIA$D_Grupo_22 <- 100*(PAINEL_MEDIA$D_Grupo_22)
PAINEL_MEDIA$D_Grupo_31 <- 100*(PAINEL_MEDIA$D_Grupo_31)
PAINEL_MEDIA$D_Grupo_32 <- 100*(PAINEL_MEDIA$D_Grupo_32)
PAINEL_MEDIA$D_Grupo_41 <- 100*(PAINEL_MEDIA$D_Grupo_41)
PAINEL_MEDIA$D_Grupo_42 <- 100*(PAINEL_MEDIA$D_Grupo_42)
PAINEL_MEDIA$D_ZERO <- 100*(PAINEL_MEDIA$D_ZERO)
PAINEL_MEDIA$D_ATE_4 <- 100*(PAINEL_MEDIA$D_ATE_4)
PAINEL_MEDIA$D_5_A_9 <- 100*(PAINEL_MEDIA$D_5_A_9)
PAINEL_MEDIA$D_10_A_19 <- 100*(PAINEL_MEDIA$D_10_A_19)
PAINEL_MEDIA$D_20_A_49 <- 100*(PAINEL_MEDIA$D_20_A_49)
PAINEL_MEDIA$D_50_A_99 <- 100*(PAINEL_MEDIA$D_50_A_99)
PAINEL_MEDIA$D_100_A_249 <- 100*(PAINEL_MEDIA$D_100_A_249)
PAINEL_MEDIA$D_250_A_499 <- 100*(PAINEL_MEDIA$D_250_A_499)
PAINEL_MEDIA$D_500_A_999 <- 100*(PAINEL_MEDIA$D_500_A_999)
PAINEL_MEDIA$D_1000_OU_MAIS <- 100*(PAINEL_MEDIA$D_1000_OU_MAIS)

## Regressão -------------------------------------------------------------------


plm <-  plm(log.Vl.Remun.Dezembro.IPCA ~ D_Grupo_11+ D_Grupo_12+ D_Grupo_21+ D_Grupo_22+ D_Grupo_31+ D_Grupo_32+	D_Grupo_41+	D_Grupo_42+
                      Ind.Portador.Defic+Tempo.Emprego+D_ZERO+D_ATE_4+D_5_A_9+D_10_A_19+	D_20_A_49+	D_50_A_99+	D_100_A_249+
                      D_250_A_499+D_500_A_999+D_1000_OU_MAIS+D_2019+D_2014+D_2009, 
                    data = PAINEL_MEDIA,
                    index = c("Grupo", "Ano"), 
                    model = "within")
summary(plm)


## Data frame com os valores dos betas estimados por setor ---------------------
DadosEstimados <- data.frame(Descricao = c("Patrimônio e Culturas Tradicionais", "Artes Visuais e Performáticas", 
                                           "Publicação, editoração e mídia", "Audiovisual", "Arquitetura, Design e Moda", 
                                           "Publicidade e pesquisa de mercado", "Ensino e pesquisa", "TI e software, pesquisa e desenvolvimento"),
                             Estimacao = c(-0.00616656, -0.04460186, 0.03961090, -0.01578818, 0.02723167, 
                                           -0.05154788, 0.02146924, -0.00235401),
                             ExpEstimacao = c(exp(-0.00616656), exp(-0.04460186), exp(0.03961090), exp(-0.01578818), exp(0.02723167), 
                                                                                           exp( -0.05154788), exp(0.02146924), exp(-0.00235401)))

DadosEstimados$ExpEstimacao.100 <- 100*(DadosEstimados$ExpEstimacao)

## Gráfico do premio salarial --------------------------------------------------

ggplot(DadosEstimados, aes(y =(ExpEstimacao.100)-100, x = reorder(Descricao, -(ExpEstimacao.100)-100)))+
  geom_col()+
  geom_text(aes(label=paste(format(round((ExpEstimacao.100)-100, digits = 2), nsmall = 2, big.mark = "."),"%")), size = 12, position = position_dodge(width = .8), vjust = -0.2, hjust = 0.4)+
  theme_bw()+
  labs(x= NULL, y = NULL)+
  theme(axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25))+
  scale_y_continuous(labels = function(x) paste(format(x, big.mark = "."),"%"))+
scale_x_discrete(labels=c("Publicação, \n editoração e\n mídia", "Arquitetura,\n Design e \n Moda", "Ensino \n e pesquisa", "TI e software, \n pesquisa e \n desenvolvimento",
                          "Patrimônio  \n e Culturas Tradicionais", "Audiovisual", "Artes Visuais \n e Performáticas", "Publicidade e \n pesquisa de mercado"))
  




