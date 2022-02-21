# Exercício ítem 10.4
## Leitura do Banco de dados
URL <- "https://raw.githubusercontent.com/arpanosso/r_data_science_fcav/master/dados/BroomBarnFarm.txt"
dados <- read.table(URL, h=TRUE)

## Carregar os pacotes 
library(tidyverse)
library(nortest)
library(agricolae)
library(corrplot)
library(skimr)
source("R/minhas-funcoes.R")

## Calcular a média, a mediana, o desvio padrão, 
## o 1º (Q1) e o 3º (Q3) quartis, os coeficientes de 
## assimetria, de curtosis, de variação (CV), as observações 
## máxima e mínima das 3 variáveis do conjunto de dados.
glimpse(dados)
skim(dados)
apply(dados,2,est_descritiva) %>% round(3)

## Refazer essa análise com os dados transformados na escala 
## logarítmica.
## 1
apply(log(dados), 2, est_descritiva) %>% round(3)

## 2
dados <- read.table(URL, h=TRUE)
dados <- dados %>% 
  mutate(
    K_log = log(K),
    pH_log = log(pH),
    P_log = log(P)
  ) 
apply(dados, 2, est_descritiva) %>% round(3)

## 3
dados_log <- log(dados)
apply(dados_log, 2, est_descritiva) %>% round(3)

## Construa os gráficos Boxplot, histogramas 
# e da função de distribuição acumulada empírica 
# para as 3 variáveis. Discuta o que você observa.
ecdf_ph <- dados %>%  # acumulada empírica 
  ggplot(aes(x=pH))+
  stat_ecdf(geom="line")

ecdf_k <- dados %>%  # acumulada empírica 
  ggplot(aes(x=K))+
  stat_ecdf(geom="line")

ecdf_P <- dados %>%  # acumulada empírica 
  ggplot(aes(x=P))+
  stat_ecdf(geom="line")

hist_ph <- dados %>% 
  ggplot(aes(x=pH)) +
  geom_histogram(bins=12, color="black", 
                 fill = "orange")

hist_k <- dados %>% 
  ggplot(aes(x=K)) +
  geom_histogram(bins=12, color="black", 
                 fill = "lightgreen")

hist_P <- dados %>% 
  ggplot(aes(x=P)) +
  geom_histogram(bins=12, color="black", 
                 fill = "lightblue")

box_P <- dados %>% 
  ggplot(aes(x = P)) +
  geom_boxplot( fill="lightblue") +
  coord_cartesian(ylim=c(-1,1))

box_k <- dados %>% 
  ggplot(aes(x = K)) +
  geom_boxplot( fill="lightgreen") +
  coord_cartesian(ylim=c(-1,1))

box_ph <- dados %>% 
  ggplot(aes(x = pH)) +
  geom_boxplot( fill="orange") +
  coord_cartesian(ylim=c(-1,1))

library(patchwork)
ecdf_P + ecdf_ph + ecdf_k + 
 hist_P + hist_ph + hist_k +
box_P + box_ph + box_k

## reconstruir para as variáveis na escala
## logarítmica
dados <- read.table(URL, h=TRUE)
dados_log <- log(dados)
ecdf_ph <- dados_log %>%  # acumulada empírica 
  ggplot(aes(x=pH))+
  stat_ecdf(geom="line")

ecdf_k <- dados_log %>%  # acumulada empírica 
  ggplot(aes(x=K))+
  stat_ecdf(geom="line")

ecdf_P <- dados_log %>%  # acumulada empírica 
  ggplot(aes(x=P))+
  stat_ecdf(geom="line")

hist_ph <- dados_log %>% 
  ggplot(aes(x=pH)) +
  geom_histogram(bins=12, color="black", 
                 fill = "orange")

hist_k <- dados_log %>% 
  ggplot(aes(x=K)) +
  geom_histogram(bins=12, color="black", 
                 fill = "lightgreen")

hist_P <- dados_log %>% 
  ggplot(aes(x=P)) +
  geom_histogram(bins=12, color="black", 
                 fill = "lightblue")

box_P <- dados_log %>% 
  ggplot(aes(x = P)) +
  geom_boxplot( fill="lightblue") +
  coord_cartesian(ylim=c(-1,1))

box_k <- dados_log %>% 
  ggplot(aes(x = K)) +
  geom_boxplot( fill="lightgreen") +
  coord_cartesian(ylim=c(-1,1))

box_ph <- dados_log %>% 
  ggplot(aes(x = pH)) +
  geom_boxplot( fill="orange") +
  coord_cartesian(ylim=c(-1,1))

ecdf_P + ecdf_ph + ecdf_k + 
  hist_P + hist_ph + hist_k +
  box_P + box_ph + box_k














