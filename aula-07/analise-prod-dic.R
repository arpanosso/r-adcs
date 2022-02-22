# analise-prod-dic
## analisar o banco de dados de acordo com um delineamento
## inteiramente casualizado, quanto aos diagnósticos da ANOVA.
library(tidyverse)
theme_set(theme_bw())

## Carregar o geomorfologia
dados <- read.table("data/prod.txt", h=TRUE)
glimpse(dados)

## preciso de objeto FATOR (factor)
trat <- dados %>% 
  pull(Trat) %>% 
  as.factor()
is.factor(trat)
levels(trat)

## preciso da variável aleatória
prod <- dados %>% 
  pull(Y)
is.numeric(prod)

## Construir o modelo da análise de variância (ANOVA)
mod <- aov(prod ~ trat)

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
## Levando em consideração o alpha de 1%  rejeitamos H0
## concluímos que os resíduos não tem distribuição normal

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
rep <- dados %>% pull(Rep)
tibble::tibble(trat, prod, rep, yp, rs) %>% 
  arrange(rs) %>% 
  View()

## não foram observados outliers/valores discrepantes na análise
## dos resíduos estudentizados.

## Homogeneidade das variâncias (Homocedasticidade)
## Boxplot para o teor de argila por superfície
tibble::tibble(trat, prod) %>% 
  ggplot(aes(x=trat, y=prod, fill=trat)) +
  geom_boxplot()

## teste de Bartlett
bartlett.test(prod,trat)
## rejeitamos H0 ao nível de 1% de probabiliade 
## (p-value > 0.05) e concluímos que os resíduos não são 
## homocedásticos

## teste de Levene e de Brown and Forsythe
library(lawstat)
levene.test(prod,trat) ## Brown and Forsythe
levene.test(prod,trat,location = "mean") ## teste de Levene
##  rejeitamos H0 ao nível de 1% de probabilidade 
## (P < 0.01).

## Estudo da HETEROCEDASTICIDADE
## REGULAR: existe uma relação entre a média e a 
## variância dos tratamentos, nesse caso, é observado
## significância na regressão linear entre log_var e 
## log_media, e a trasnformação indicada é a:
## y^(1-b/2) b é o coeficiente angular do ajuste de 
## regressão
library(ggpubr)
dados_aux <- dados %>% 
  group_by(Trat) %>% 
  summarise(
    media = mean(Y, na.rm=TRUE),
    variancia = var(Y,na.rm=TRUE),
    log_media = log(media),
    log_var = log(variancia)
  )
dados_aux %>% 
  ggplot(aes(x=log_media, y=log_var)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_regline_equation(aes(
    label =  paste(..eq.label.., 
                   ..rr.label.., 
                   sep = "*plain(\",\")~~")))

# Análise de Regressão
log_var <- dados_aux %>% pull(log_var)
log_media <- dados_aux %>% pull(log_media)
reg <- lm(log_var ~ log_media)
summary.lm(reg)
## Observando o valor de b, da análise de regessão, 
## rejeitamos H0 ao nível de 1% de probabilidade pelo teste
## t-Student, e concluímos que o valor do coeficiente 
## angular da reta de regressão é diferente de zero. 
## Heterocedasticidade é REGULAR, então podemos realizar a 
## transformação dos dados
beta <- reg$coefficients[2]

# Transformação de Bartellet ----------------------------------------------
dados <- dados %>% 
  mutate(
    Y_t = Y^(1-beta/2)
  )
## preciso da variável aleatória
prod_t <- dados %>% 
  pull(Y_t)
is.numeric(prod_t)

## Construir o modelo da análise de variância (ANOVA)
mod <- aov(prod_t ~ trat)

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
## Levando em consideração o alpha de 1%  rejeitamos H0
## concluímos que os resíduos não tem distribuição normal

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

## Homogeneidade das variâncias (Homocedasticidade)
## Boxplot para o teor de argila por superfície
tibble::tibble(trat, prod_t) %>% 
  ggplot(aes(x=trat, y=prod_t, fill=trat)) +
  geom_boxplot()

## teste de Bartlett
bartlett.test(prod_t,trat)


## teste de Levene e de Brown and Forsythe
library(lawstat)
levene.test(prod_t,trat) ## Brown and Forsythe
levene.test(prod_t,trat,location = "mean") ## teste de Levene


# Análise de variância ----------------------------------------------------
library(ExpDes.pt)
dic(trat, prod_t, quali=TRUE, sigF = 0.05, sigT=0.05,
    mcomp = "tukey")

# Transformação de BoxCox -------------------------------------------------
# modelo antigo
library(MASS)
mod <- aov(dados$Y ~ as.factor(dados$Trat))
BoxCox <- MASS::boxcox(mod)
## Critério
### se lambda não difere de 1 => Homocedasticidade
### se lambda difere de 1 (transformação nos dados)
### se lambda difere de zero y_t = (y^lambda-1)/lambda
### se lambda não difere de zero y_t =log(y)

BoxCox %>% 
  data.frame() %>% 
  arrange(desc(y)) %>% 
  slice(1)
lambda <- 0.1818182

prod_t2 <- (prod^(lambda)-1)/lambda
## Construir o modelo da análise de variância (ANOVA)
mod <- aov(prod_t2 ~ trat)

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
## Levando em consideração o alpha de 1%  rejeitamos H0
## concluímos que os resíduos não tem distribuição normal

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

## Homogeneidade das variâncias (Homocedasticidade)
## Boxplot para o teor de argila por superfície
tibble::tibble(trat, prod_t2) %>% 
  ggplot(aes(x=trat, y=prod_t2, fill=trat)) +
  geom_boxplot()

## teste de Bartlett
bartlett.test(prod_t2,trat)

# Análise de variância ----------------------------------------------------
library(ExpDes.pt)
dic(trat, prod_t2, quali=TRUE, sigF = 0.05, sigT=0.05,
    mcomp = "tukey")