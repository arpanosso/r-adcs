# analise-racoes-dic
## analisar o banco de dados de acordo com um delineamento
## inteiramente casualizado, quanto aos diagnósticos da ANOVA.
library(tidyverse)
theme_set(theme_bw())

## Carregar o geomorfologia
racoes <- read.table("data/racoes.txt", h=TRUE)
glimpse(racoes)

## preciso de objeto FATOR (factor)
trat <- racoes %>% 
  pull(racoes) %>% 
  as.factor()

is.factor(trat)
levels(trat)

## preciso da variável aleatória
peso <- racoes %>% 
  pull(a_peso)
is.numeric(peso)

## Construir o modelo da análise de variância (ANOVA)
mod <- aov(peso ~ trat)

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

## QQ plot dos resíduos
rs %>% 
  tibble::tibble() %>% 
  ggplot(aes(sample = rs))+
  stat_qq(color="red")+
  stat_qq_line(color="blue")
  

## Passando em todos os 4 testes de normalidade
shapiro.test(rs)
nortest::lillie.test(rs)
nortest::ad.test(rs)
nortest::cvm.test(rs)
## Levando em consideração o alpha de 5% não rejeitamos H0
## concluímos que os resíduos tem distribuição normal

## Estudo dos outliers
## Quando temos os resíduos estudentizados (padronizados) 
## com distribuição normal, ou próximo à normal, 95% dos 
## resíduos encontram-se entre os valores de -3 e 3, portanto,
## toda a observação que estiver acima de 3 ou abaixo de -3
## pode ser considerado um outlier. Para esse diagnóstico vamos
## construir um gráfico de dispersão entre os resíduos e os
## valores de y (variável resposta) preditos pelo modelo da
## ANOVA.
library(gghighlight)
yp <- predict(mod)
tibble::tibble(yp, rs) %>% 
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

tibble::tibble(trat, peso, yp, rs) %>% 
  arrange(rs) %>% 
  View()
  
## não foram observados outliers/valores discrepantes na análise
## dos resíduos estudentizados.

## Homogeneidade das variâncias (Homocedasticidade)
## Boxplot para o teor de argila por superfície
racoes %>% 
  ggplot(aes(x=racoes, y=a_peso, fill=racoes)) +
  geom_boxplot()

## teste de Bartlett
bartlett.test(peso,trat)
## Não rejeitamos H0 ao nível de 5% de probabiliade 
## (p-value > 0.05) e concluímos que os resíduos são 
## homocedásticos

## teste de Levene e de Brown and Forsythe
library(lawstat)
levene.test(peso,trat) ## Brown and Forsythe
levene.test(peso,trat,location = "mean") ## teste de Levene
## Não rejeitamos H0 ao nível de 5% de probabilidade 
## (P > 0.05).

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

# Análise de variância ----------------------------------------------------
library(ExpDes.pt)
??"delineamento inteiramente"
dic(trat, peso, quali=TRUE, sigF = 0.05, sigT=0.05,
    mcomp = "tukey")
## Conclusão para o Teste F:
## Rejeitamos H0 ao nível de 5% de probabilidade, e
## concluímos que as rações tem efeito no ganho de peso
## do animais.
## cv calculado no BRAÇO
100*sqrt(68.75)/mean(peso)

## Para o teste de comparações múltiplas:
## médias seguidas pela mesma letra não diferem entre
## si pelo teste de Tukey ao nível de 5% de significância.





