library(tidyverse)

# Importação dos dados ---------------------------------------------------------

POA <- read.csv("POA.csv", sep = ";")

## Criar coluna com valor 1 ----------------------------------------------------
POA <- POA%>%
  add_column(Dummy = 1)

## Agrupar e somar o total de CNAEs e CBOs -------------------------------------

CNAE <- POA%>%
  group_by(GrupoCNAE)%>%
  summarise(TotalCNAE = sum(Dummy))

CBO <- POA%>%
  group_by(GrupoCBO, Ano)%>%
  summarise(TotalCBOs = sum(Dummy))


TotalPOA <- left_join(CBO, CNAE, by = "Grupo")

### Gráfico CNAE total - SÉRIE HISTÓRICA ---------------------------------------

ggplot(POA_VINC, mapping = aes(x = Ano, y =  TotalCNAE, shape =Descricao))+
  geom_line(linetype = "dashed", size = 1)+
  geom_point(size = 5)+
  theme_bw()+
  labs(y = NULL, x = NULL, shape = NULL)+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = "."), expand = c(0, 0), limits = c(0, 18000), breaks = seq(from = 0, to = 18000, by = 2500))+
  scale_x_continuous(breaks = seq(from = 2009, to = 2019, by = 1))+
  theme(legend.position="bottom",
        legend.text = element_text(size = 19),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_shape_manual(values = c(0, 1, 17, 15, 7, 2 , 6, 19))

### Gráfico CBOs total - SÉRIE HISTÓRICA ---------------------------------------

ggplot(POA_VINC, mapping = aes(x = Ano, y =  TotalCBOs, shape = Descricao))+
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



# Percentual de CNAEs ----------------------------------------------------------
## Agrupar bases e filtrar -----------------------------------------------------
POA2019 <- read.csv("POA2019.csv", sep = ";")
POA2009 <- read.csv("POA2009.csv", sep = ";")

Dist <- left_join(POA2009, POA2019, by = "Descricao") 

### Criar coluna com valor 1 ---------------------------------------------------
Dist <- Dist%>%
  add_column(Dummy = 1)

## Total do ano ----------------------------------------------------------------

Dist%>%
  group_by(Ano, GrupoCNAE)
  summarise(sum(Dummy))
  
## Colocar total depois do ~ ---------------------------------------------------  

  Dist <- Dist%>%
  mutate(SomaCBOs = case_when(Ano == 2009 ~ "",
                              Ano == 2019 ~ ""))

  Dist <- Dist%>%
  mutate(SomaCNAE = case_when(Ano == 2009 ~ "",
                              Ano == 2019 ~ ""))

## Calcular percentual ---------------------------------------------------------
Dist <- Dist%>%
  group_by(Grupo)%>%
  mutate(PercentualCBO = (TotalCBOs/SomaCBOs)*100, PercentualCNAE = (TotalCNAE/SomaCNAE)*100)

Dist$Ano <- as.character(gsub(",","",Dist$Ano,fixed=TRUE))

ggplot(POA_VINC, mapping = aes(x = Ano, y =  TotalCNAE, shape =Descricao))+
  geom_line(linetype = "dashed", size = 1)+
  geom_point(size = 5)+
  theme_bw()+
  labs(y = NULL, x = NULL, shape = NULL)+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = "."), expand = c(0, 0), limits = c(0, 18000), breaks = seq(from = 0, to = 18000, by = 2500))+
  scale_x_continuous(breaks = seq(from = 2009, to = 2019, by = 1))+
  theme(legend.position="bottom",
        legend.text = element_text(size = 19),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))+
  scale_shape_manual(values = c(0, 1, 17, 15, 7, 2 , 6, 19))

dist <- POA_VINC%>%
  filter(Ano %in% c(2009, 2019))

dist <- dist%>%
  mutate(SomaCBOs = case_when(Ano == 2009 ~ 27146,
                              Ano == 2019 ~ 35446))

dist <- dist%>%
  mutate(SomaCNAE = case_when(Ano == 2009 ~ 64971,
                              Ano == 2019 ~ 60792))

dist <- dist%>%
  group_by(Grupo)%>%
  mutate(PercentualCBO = (TotalCBOs/SomaCBOs)*100, PercentualCNAE = (TotalCNAE/SomaCNAE)*100)

dist$Ano <- as.character(gsub(",","",dist$Ano,fixed=TRUE))

# Gráfico distribuição CNAE ----------------------------------------------------

ggplot(dist, aes(x = reorder(Descricao, - PercentualCNAE), y = PercentualCNAE))+
  geom_col(aes(fill = Ano), position = "dodge")+
  geom_text(aes(label=paste(format(round(PercentualCNAE, digits = 2), nsmall = 2, big.mark = "."),"%"), fill = Ano), size = 8, position = position_dodge(width = .8), vjust = -0.10, hjust = 0.465)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 23),
        legend.text = element_text(size = 30),
        axis.text.y = element_text(size = 30))+
  labs(y = NULL, x = NULL, fill = NULL)+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  scale_x_discrete(labels = c("Arquitetura, \n Design e \n Moda", "Ensino \n e pesquisa", "TI e software, \n pesquisa e \n desenvolvimento", "Publicidade e \n pesquisa de \n mercado", 
                              "Patrimônio e \n Culturas Tradicionais",  "Audiovisual", "Publicação, \n editoração  \n e mídia", "Artes Visuais e  \n Performáticas"))+
  scale_fill_manual(values=c("grey70", "grey30"))

# Gráfico distribuição CBOs ----------------------------------------------------

ggplot(dist, aes(x = reorder(Descricao, - PercentualCBO), y = PercentualCBO))+
  geom_col(aes(fill = Ano), position = "dodge")+
  geom_text(aes(label=paste(format(round(PercentualCBO, digits = 2), nsmall = 2, big.mark = "."),"%"), fill = Ano),  size = 8, position = position_dodge(width = .8), vjust = -0.10, hjust = 0.465)+
  theme_bw()+
  theme(axis.text.x = element_text(size = 23),
        legend.text = element_text(size = 30),
        axis.text.y = element_text(size = 30))+
  labs(y = NULL, x = NULL, fill = NULL)+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+
  scale_x_discrete(labels = c("TI e software, \n pesquisa e \n desenvolvimento", "Publicidade e \n pesquisa de \n mercado", "Artes Visuais e  \n Performáticas", "Publicação, \n editoração  \n e mídia", "Ensino \n e pesquisa", "Arquitetura, \n Design e \n Moda",  
                              "Patrimônio e \n Culturas Tradicionais",  "Audiovisual"))+
  scale_fill_manual(values=c("grey70", "grey30"))

