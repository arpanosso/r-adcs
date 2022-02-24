# desdobramento-gl-dic
# Considere os dados do trabalho "Ação de microrganismos 
# do solo sobre o desenvolvimento de mudas de Eucalyptus
# grandis Hill Ex Maiden em tubetes" (KOHARI, 1990), 
# realizado no delineamento inteiramente casualizado 
# com 5 tratamentos e 5 repetições, os dados encontram-se 
# no arquivo eucalyptus.txt.
#Os tratamentos foram:
#1 – ST – Solo rizosférico de Pastalum notatum (grama batatis)
#2 – AC – Cultura de Azotobacter chroococcum
#3 – AP – Cultura de Azotobacter pastali
#4 – PM – Cultura de Azotobacter pastali morta
#5 – Testemunha

# Carregando os pacotes
library(nortest)
library(tidymodels)
library(lawstat)
library(ggpubr)
library(gghighlight)
library(skimr)
library(ExpDes.pt)
library(tibble)

## entrando com o banco de dados
dados <- read.table("data/eucalyptus.txt", h=TRUE)

## vislumbre
glimpse(dados)

## Definição do modelos
trat <- dados %>% 
  pull(TRAT) %>% 
  as.factor()

y <- dados %>% 
  pull(Y) %>% 
  as.numeric()

### DIAGNÓSTICO
# definindo o modelo de dic
mod <- aov(y ~ trat)
anova(mod)

# normalidade dos erros
rs <- rstudent(mod)
shapiro.test(rs)

# homogeneidade das variâncias
bartlett.test(y, trat)
###
dic(trat, y)

## definir a matriz de contrastes para a comparação
coef_1 <- c(2, 2, 2, -3, -3) 
coef_2 <- c(2, -1, -1, 0, 0)
coef_3 <- c(0, 1, -1, 0, 0)
coef_4 <- c(0, 0, 0, 1, -1)

## crair a matriz
matriz_contrastes <- cbind(coef_1, coef_2, coef_3, coef_4)
matriz_contrastes

## observar a matriz de contrastes do tratamento
contrasts(trat)

## substituir a matriz de contrastes nos tratamentos
contrasts(trat) <- matriz_contrastes
contrasts(trat)

## criar o modelos da análise de variância com o novo contraste
mod_contraste <- aov(y ~ trat)
summary(mod_contraste,
        split = list(
          trat = list(
            "(ST+AC+AP) vs (PM + T)" = 1,
            "ST vs (AC+AP)" = 2,
            "AC vs AP" = 3,
            "PM vs T"= 4
          )
        ))






