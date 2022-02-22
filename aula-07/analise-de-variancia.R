# Delimeamento Inteiramente Casualizado
##
library(tidyverse)
theme_set(theme_bw())

## Carregar o geomorfologia
geomorfologia <- read.table("data/geomorfologia.txt", h=TRUE)
glimpse(geomorfologia)

## Fazer uma análise de variância para Argila em função 
## das superfícies utilizando um modelo em delineamento
## inteiramente casualizado.
## preciso de objeto FATOR (factor)
sup <- geomorfologia %>% 
  pull(SUP) %>% 
  as.factor()

is.factor(sup)
levels(sup)

## preciso da variável aleatória
argila <- geomorfologia %>% 
  pull(ARGILA)
is.numeric(argila)

## Construir o modelo da análise de variância (ANOVA)
mod <- aov(argila ~ sup)

## quadro clássico da ANOVA
anova(mod)

# Teste Diagnósticos ------------------------------------------------------
## Normalidade dos erros, os erros aleatórios, devem ter
## distribuição normal
rs <- rstudent(mod) # erros estudentizados

## Histograma para o resíduos
rs %>% 
  tibble::tibble() %>% 
  ggplot(aes(x=rs)) + 
  geom_histogram(bins = 12, color="black", fill= "lightgray")

## Passando em todos os 4 testes de normalidade
shapiro.test(rs)
nortest::lillie.test(rs)
nortest::ad.test(rs)
nortest::cvm.test(rs)
## Levando em consideração o alpah de 1% de probabilidade,
## não rejeitamos H0 dos testes de normalidade, e concluímos
## que os resíduos têm distribuição normal.


## Homogeneidade das variâncias (Homocedasticidade)
## Boxplot para o teor de argila por superfície
geomorfologia %>% 
  ggplot(aes(x=SUP, y=ARGILA, fill=SUP)) +
  geom_boxplot()

## teste de Bartlett
bartlett.test(argila,sup)
## Rejeitamos H0 ao nível de 1% de probabiliade (p-value <0.01)
## e concluímos que os resíduos não são homocedásticos, eles são
## heterocedásticos

## teste de Levene e de Brown and Forsythe
library(lawstat)
levene.test(argila,sup) ## Brown and Forsythe
levene.test(argila,sup,location = "mean") ## teste de Levene
## Rejeitamos H0 ao nível de 1% de probabiliade (p-value <0.01)
## e concluímos que os resíduos não são homocedásticos, eles são
## heterocedásticos

## Uma das possíveis soluções seria a transformação dos dados
## para realizar essa transformação vamos utilizar a abordagem
## de box and cox.
# Uma breve explicação
## se lambda == 1 ~ homocedásticos
## se lambda != 1 mas igual a zero (0) uso log(y)
## se lambda != 1 e diferente de zero (0) y ^(lambda)
library(MASS)
BoxCox <- boxcox(mod)

# Buscando o valor de lambda, ou seja o maior valor da quantidade
# log da verossimilhança
BoxCox %>% 
  data.frame() %>% 
  arrange(desc(y)) %>% 
  slice(1)

# Transformação da argila -------------------------------------------------
argila_t <- argila^0.6262626
mod_t <- aov(argila_t ~ sup)
anova(mod_t)
## Normalidade dos erros, os erros aleatórios, devem ter
## distribuição normal
rs <- rstudent(mod_t) # erros estudentizados

## Histograma para o resíduos
rs %>% 
  tibble::tibble() %>% 
  ggplot(aes(x=rs)) + 
  geom_histogram(bins = 12, color="black", fill= "lightgray")

## Passando em todos os 4 testes de normalidade
shapiro.test(rs)
nortest::lillie.test(rs)
nortest::ad.test(rs)
nortest::cvm.test(rs)

## Homogeneidade das variâncias (Homocedasticidade)
## Boxplot para o teor de argila por superfície
geomorfologia %>% 
  ggplot(aes(x=SUP, y=ARGILA^0.6262626, fill=SUP)) +
  geom_boxplot()

## teste de Bartlett
bartlett.test(argila_t,sup)
## Rejeitamos H0 ao nível de 1% de probabiliade (p-value <0.01)
## e concluímos que os resíduos não são homocedásticos, eles são
## heterocedásticos

## teste de Levene e de Brown and Forsythe
library(lawstat)
levene.test(argila_t,sup) ## Brown and Forsythe
levene.test(argila_t,sup,location = "mean") ## teste de Levene
## Rejeitamos H0 ao nível de 1% de probabiliade (p-value <0.01)
## e concluímos que os resíduos não são homocedásticos, eles são
## heterocedásticos













