# fatorial-triplo
# Obtenha e interprete o quadro da análise de variância de um
# experimento em DBC onde estudou-se o efeito da adubação NPK
# (presença e ausência) na produção da cultura do cafeeiro em kg por
# parcela. Dados encontram-se no arquivo cafeeiro.txt
library(nortest)
library(tidyverse)
library(lawstat)
library(ggpubr)
library(gghighlight)
library(skimr)
library(ExpDes.pt)
library(tibble)

# Entrada de dados
dados <- read.table("data/cafeeiro.txt",h=TRUE)

# Vislumbre do data set
glimpse(dados)

# Resumo estatístico
skim(dados)

# Criar os tratamentos
N <- dados %>% pull(N) %>% as.factor()
P <- dados %>% pull(P) %>% as.factor()
K <- dados %>% pull(K) %>% as.factor()

trat <- paste0(N,"-", P, "-",K) %>% 
  as.factor()

# Definir o modelos de experimentos
y <- dados %>% pull(Y) %>% as.numeric()
bloco <- dados %>% pull(BLOCO) %>% as.factor()
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
mod <- aov(y ~ bloco + N*P*K)
anova(mod)

## Observar a interação N dentro K
dados %>% 
  group_by(N, K) %>% 
  summarise(prod = mean(Y, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.factor(K), 
             y=prod, 
             fill = as.factor(N)))+
  geom_col(position="dodge",color="black")


## Observar a interação K dentro N
dados %>% 
  group_by(N, K) %>% 
  summarise(prod = mean(Y, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.factor(N), 
             y=prod, 
             fill = as.factor(K)))+
  geom_col(position="dodge",color="black")

## Utilizando o Pacote ExpDes.pt
fat3.dbc(N,P,K,bloco,y,
         quali=c(TRUE, TRUE, TRUE),
         fac.names = c("N","P","K"))



