# analise-regressao-dic
# Para apresentação do método dos polinômios ortogonais 
# no delineamento inteiramente casualizados, vamos considerar 
# os dados do arquivo regP2O5.txt, referentes a um experimento 
# com 5 níveis de P2O5 e 6 repetições, onde a variável 
# resposta (Y) é produção de cana-de-açúcar (t/ha).
# Carregando os pacotes
library(nortest)
library(tidymodels)
library(lawstat)
library(ggpubr)
library(gghighlight)
library(skimr)
library(ExpDes.pt)
library(tibble)

## Leitura do banco de dados
dados <- read.table("data/regP2O5.txt", h=TRUE)

## vislumbre
glimpse(dados)

trat <- dados %>% 
  pull(Trat)
trat %>% unique()

## Até 3 tratamentos Tukey, não usa regressão
## Mais que 3 tratamentos, você até o polinômio referente
## ao grau de liberdade de tratamento
## 5 tratamentos (4 graude de liberdade)
Linear <- trat
Quadratico <- trat^2
Cubico <- trat^3
Ef_4o <- trat^4

## retirar a variavel resposta
y <- dados %>% 
  pull(Y) %>% 
  as.numeric()

## Passo I - Testar o efeito LINEAR
mod_linear <- aov(y ~ Linear)
summary.lm(mod_linear)

## Passo II - Testar o efeito LINEAR e QUADRÁTICO
mod_quadratico <- aov(y ~ Linear + Quadratico)
summary.lm(mod_quadratico)

## Passo III - Testar o efeito LINEAR e QUADRÁTICO e CUBICO
mod_cubico <- aov(y ~ Linear + Quadratico + Cubico)
summary.lm(mod_cubico)

## PAsso IV - Testa todos os efeitos
mod_total <- aov(y ~ Linear + Quadratico + Cubico + Ef_4o)
summary.lm(mod_total)

## Utilizando o ExpDes.pt
dic(trat, y, quali = FALSE)

## extrair os coefficientes a análise
a <- mod_quadratico$coefficients[1] ## linear c
b <- mod_quadratico$coefficients[2] ## quadratico b
c <- mod_quadratico$coefficients[3] ## cubico a

## Construir o Gráfico
theme_set(theme_minimal())
formula <- y ~ poly(x,2,raw=TRUE)
dados %>% 
  group_by(Trat) %>% 
  summarise(prod = mean(Y)) %>% 
  ggplot(aes(x=Trat, y=prod)) +
  geom_point(size=3, color="blue") +
  geom_smooth(color="red", method="lm", formula=formula) +
  stat_regline_equation(aes(
    label =  paste(..eq.label.., 
                   ..rr.label.., 
                   sep = "*plain(\",\")~~")),
    formula=formula) 


## Fazer algumas alterações
Ymax = - (b^2-4*c*a)/(4*c) # calculando o Y máximo
Xmax = -b/2/c # calculando o X máximo
dados %>% 
  group_by(Trat) %>% 
  summarise(prod = mean(Y)) %>% 
  ggplot(aes(x=Trat, y=prod)) +
  geom_point(size=3, color="blue") +
  geom_smooth(color="red", method="lm", formula=formula,
              se=FALSE, linetype=2) +
  annotate(geom="text",x=100, y=75, 
           label = "y = a + bX+ cx² \n R² = 0.97 \n P < 0.001",
           size=6) +
  geom_vline(xintercept = Xmax, linetype=2) +
  geom_hline(yintercept = Ymax, linetype=2) 

















