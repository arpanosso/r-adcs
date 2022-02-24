# fatorial-duplo-dbc
# LAMERS (1981) – Neste trabalho foram utilizados 3
# espaçamentos (25, 50 e 75 cm) e 3 densidades de plantas 
# por metro linear (15, 30 e 45 planta por metro linear) 
# para o estudo de produção de sementes de Crotalaria juncea L
# O experimento foi em DBC com 3 repetições e a variável
# resposta foi a produção de massa verde em t ha-1. 
# Os dados encontram-se no arquivo crotalaria.txt.
library(nortest)
library(tidyverse)
library(lawstat)
library(ggpubr)
library(gghighlight)
library(skimr)
library(ExpDes.pt)
library(tibble)

# Entrada de dados
dados <- read.table("data/crotalaria.txt",h=TRUE)

# Vislumbre do data set
glimpse(dados)

# Resumo estatístico
skim(dados)

# Criar os tratamentos
dens <- dados %>% 
  pull(DENS) %>% 
  as.factor()

esp <- dados %>% 
  pull(ESP) %>% 
  as.factor()

trat <- paste0(esp,"-", dens) %>% 
  as.factor()

# Definir o modelos de experimentos
y <- dados %>% 
  pull(Y) %>% 
  as.numeric()

bloco <- dados %>% 
  pull(BLOCO) %>% 
  as.factor()

mod <- aov(y ~ trat + bloco)
anova(mod)

# Análise de Normalidade dos Resíduos -------------------------------------
theme_set(theme_classic())
rs <- rstudent(mod) # erros estudentizados

## Histograma para o resíduos
rs %>% 
  tibble::tibble() %>% 
  ggplot(aes(x=rs)) + 
  geom_histogram(bins = 12, color="black", fill= "lightgray")

## QQ plot dos resíduos
rs %>% 
  tibble::tibble() %>% 
  ggplot(aes(sample = rs))+
  stat_qq(color="red")+
  stat_qq_line(color="blue")

## Passando em todos os 4 testes de normalidade
shapiro.test(rs)
lillie.test(rs)
ad.test(rs)
cvm.test(rs)

## Estudo dos outliers
yp <- predict(mod)
tibble(yp, rs) %>% 
  ggplot(aes(x=yp, y=rs)) +
  geom_point(color="red",size=3)+
  geom_hline(yintercept = c(-3, 3), color="red", linetype=2)+
  gghighlight(rs > 3 | rs < -3,
              unhighlighted_params = list(
                shape = 23,
                color = "black",
                fill = "lightgray",
                size=2
              ))

# Análise da homogeneidade das variâncias ---------------------------------
tibble(trat, y) %>% 
  ggplot(aes(x=trat, y=y, fill=trat)) +
  geom_boxplot()

## teste de Bartlett
bartlett.test(y,trat)
levene.test(y,trat) ## Brown and Forsythe
levene.test(y,trat,location = "mean") ## teste de Levene

## Definir o modelo com os efeitos simples, principal e 
## a interação
mod <- aov(y ~ bloco + esp + dens + esp:dens)
anova(mod)

## Observar a interação Esp dentro Dens
dados %>% 
  group_by(ESP, DENS) %>% 
  summarise(prod = mean(Y, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.factor(ESP), 
             y=prod, 
             fill = as.factor(DENS)))+
  geom_col(position="dodge",color="black")


## Observar a interação Dens dentro Esp
dados %>% 
  group_by(ESP, DENS) %>% 
  summarise(prod = mean(Y, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.factor(DENS), 
             y=prod, 
             fill = as.factor(ESP)))+
  geom_col(position="dodge",color="black")


## Rodar a análise com o ExpDes.pt
fat2.dbc(esp,dens,bloco,y,quali=c(TRUE, TRUE),
         fac.names = c("Espaçamento","Densidade"))

tapply(dados$Y,dados[1:2], mean)



