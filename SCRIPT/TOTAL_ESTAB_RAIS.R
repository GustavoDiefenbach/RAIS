library(tidyverse)

# Importação dos dados ---------------------------------------------------------

POA <- read.csv("POA.csv", sep = ";")

## Criar coluna com valor 1 ----------------------------------------------------

POA <- POA%>%
  add_column(Dummy = 1)

POA_ESTAB <- POA%>% 
  group_by(GrupoCNAE)%>%
  summarise(TotalCNAE = sum(Dummy))

## Gráfico Nº de empresas ------------------------------------------------------

ggplot(POA_ESTAB, mapping = aes(x = Ano, y =  TotalCNAE, shape = Descricao))+
  geom_line(linetype = "dashed",  size = 1)+
  geom_point(size = 5)+
  theme_bw()+
  labs(y = NULL, x = NULL, shape = NULL)+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = "."), expand = c(0, 0), limits = c(0, 7000), breaks = seq(from = 0, to = 7000, by = 2000))+  
  scale_x_continuous(breaks = seq(from = 2009, to = 2019, by = 1))+
  theme(legend.position="bottom",
        legend.text = element_text(size = 17),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_shape_manual(values = c(0, 1, 17, 15, 7, 2 , 6, 19))

# Tamanho  ---------------------------------------------------------------------

## Soma das CNAEs -------------------------------------------------------------- 
POA_ESTAB <- POA_ESTAB%>% 
  group_by(GrupoCNAE, Tamanho.Estabelecimento)%>%
  summarise(TotalCNAE = sum(Dummy))

#Descrição estabelecimentos ----------------------------------------------------
Tamanho.Estabelecimento <- c(1,2, 3, 4, 5, 6, 7, 8, 9, 10)
DescricaoTamanho.Tamanho <- c("ZERO", "ATE 4", "DE 5 A 9", "DE 10 A 19", "DE 20 A 49", "DE 50 A 99", 
                              "DE 100 A 249", "DE 250 A 499", "DE 500 A 999", "1000 OU MAIS")

DescricaoTamanho <- data.frame(DescricaoTamanho.Tamanho, Tamanho.Estabelecimento)

POA_ESTAB <- left_join(POA_ESTAB, DescricaoTamanho, by = "Tamanho.Estabelecimento")

# Soma do total e percentual ---------------------------------------------------
## Soma do total ---------------------------------------------------------------
Dist%>%
  group_by(DescricaoTamanho.Tamanho, GrupoCNAE)
summarise(sum(Dummy))

## Colocar total depois do ~ ---------------------------------------------------  

POA_ESTAB <- POA_ESTAB%>%
  mutate(Total = case_when(Grupo == 11 ~ "",
                           Grupo == 12 ~ "",
                           Grupo == 21 ~ "",
                           Grupo == 22 ~ "",
                           Grupo == 31 ~ "",
                           Grupo == 32 ~ "",
                           Grupo == 41 ~ "",
                           Grupo == 42 ~ ""))

POA_ESTAB <- POA_ESTAB%>%
  mutate(Percentual = (TotalCNAE/Total)*100)

## Plotar gráfico --------------------------------------------------------------

ggplot(POA_ESTAB, aes(x = Descricao, y = Percentual, fill = DescricaoTamanho.Tamanho))+
  geom_col(position = "dodge")+
  geom_text(aes(label=paste(format(round(Percentual, digits = 2), nsmall = 2, big.mark = "."), "%"), fill = DescricaoTamanho.Tamanho), size = 3, position = position_dodge(width = .8), vjust = -0.8)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 70, hjust = 1))+
  labs(y = NULL, x = NULL, fill = NULL)+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))


ggplot(POA_ESTAB, aes(x = reorder(DescricaoTamanho.Tamanho, -Percentual), y = Percentual, fill = DescricaoTamanho.Tamanho ))+
  geom_col(position = "dodge")+
  geom_text(aes(label=paste(format(round(Percentual, digits = 2), nsmall = 2, big.mark = "."), "%"), fill = DescricaoTamanho.Tamanho), size = 5, position = position_dodge(width = .8), vjust = -0.2)+
  facet_wrap(~Descricao,  ncol=2)+
  theme_bw()+
  scale_fill_manual(breaks = c("ZERO", "ATE 4", "DE 5 A 9", "DE 10 A 19", "DE 20 A 49", "DE 50 A 99", 
                               "DE 100 A 249", "DE 250 A 499", "DE 500 A 999", "1000 OU MAIS"),
                    values = c("grey5", "grey20", "grey27", "grey36", "grey43", "grey50", "grey65", "grey75", "grey84", "grey91"))+
  scale_y_continuous(labels = function(x) paste(x, "%"), limits = c(0, 100))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text = element_text(size = 23),
        axis.text.y = element_text(size = 20),
        strip.text = element_text(size = 25))+
  labs(y = NULL, x = NULL, fill = NULL)



