# analise-dbc-parcela-perdida
# Para exemplificar a análise de variância de um experimento 
# em blocos casualizados, com 2 parcelas perdidas, vamos 
# considerar os dados disponíveis no arquivo sorgoPP.txt, 
# adaptados do trabalho "Comparação de métodos para patologia 
# de sementes de sorgo Sorghum bicolo (L.) Moench", 
# onde estudou-se a porcentagem de incidência de Alternaria 
# tenuis sp. nas sementes de 7 cultivares de sorgo. 
# Os dados obtidos foram transformados ArcoSeno((p/100)^0.5)
# Carregando os pacotes
library(nortest)
library(tidymodels)
library(lawstat)
library(ggpubr)
library(gghighlight)
library(skimr)
library(ExpDes.pt)
library(tibble)

# Entrada de dados
dados <- read.table("data/sorgoPP.txt",h=TRUE)

# Vislumbre do data set
glimpse(dados)

# Resumo estatístico
skim(dados)

## Estimar a parcela perdida
# PASSO I - criar um modelo contendo as parcelas perdidas
trat <- dados %>% 
  pull(Cultivar) %>% 
  as.factor()

bloco <- dados %>% 
  pull(Bloco) %>% 
  as.factor()

y <- dados %>% 
  pull(Y) %>% 
  as.numeric()

mod_parcela_perdida <- aov(y ~ trat + bloco)

# PASSO II - Fazer a estimativa das parcelas
ye <- predict(mod_parcela_perdida, newdata = dados)
tibble(y, ye) %>%  View()

# PASSO III - substituir o valor em Y
dados_aux <- dados %>% 
  mutate(
    Ye = ye,
    Y = ifelse(is.na(Y),Ye,Y)
  )

# PASSO 4 devifir o novo modelo
y <- dados_aux %>% 
  pull(Y) %>% 
  as.numeric()
mod <- aov(y ~trat + bloco)
anova(mod)

## Entrar com a rotina de diagnóstico...

## Usando o ExpDes.pt
dbc(trat, bloco, y, quali=TRUE, mcomp="tukey")













