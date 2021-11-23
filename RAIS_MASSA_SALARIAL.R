library(tidyverse)

# Importação dos dados ---------------------------------------------------------

POA <- read.csv("POA.csv", sep = ";")

IPCA <- read.csv("IPCA.csv", sep = ";")

## Valor da remuneração de dezembro a valor presente ---------------------------

POA$Vl.Remun.Dezembro.Nom <- as.numeric(gsub(",",".",POA$Vl.Remun.Dezembro.Nom,fixed=TRUE))

POA <- POA%>%
  mutate(Vl.Remun.Dezembro.real = Vl.Remun.Dezembro.Nom*IPCAam)


## Agrupar e somar a massa salarial --------------------------------------------
SalarioCNAE <- POA%>%
  group_by(Grupo = GrupoCNAE)%>%
  summarise(SalarioDezCNAE = sum(Vl.Remun.Dezembro.real))

SalarioCBOs <- POA%>%
  group_by(Grupo = GrupoCBO, Ano)%>%
  summarise(SalarioDezCBOs = sum(Vl.Remun.Dezembro.real))

## Juntar as bases salarial ----------------------------------------------------

Salario<- left_join(SalarioCBOs, SalarioCNAE, by = "Grupo")

### Gráfico massa salarial CNAE ------------------------------------------------

ggplot(salario, mapping = aes(x = Ano, y =  SalarioDezCNAE, shape = Descricao))+
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


### Gráfico massa salarial CBOs ------------------------------------------------

ggplot(salario, mapping = aes(x = Ano, y =  SalarioDezCBOs, shape = Descricao))+
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

