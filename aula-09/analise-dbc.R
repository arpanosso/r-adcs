# analise-dbc
# Um exemplo de DBC realizado por ARF (1981), estudou-se
# o efeito de inseticidas no controle de tripes (Enneothips flavens
# Moulton, 1941) na cultura do amendoim. Foram utilizados 7
# tratamentos e 4 blocos, com os blocos controlando diferenças de
# infestação da cultura. Os resultados de produção de vagens em kg
# ha-1 encontra-se no arquivo inseticidas.txt.

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
dados <- read.table("data/inseticidas.txt",h=TRUE)

# Vislumbre do data set
glimpse(dados)

# Resumo estatístico
skim(dados)

# Extrair os fatores trat e bloco
trat <- dados %>% 
  pull(TRAT) %>% 
  as.factor()

bloco <- dados %>% 
  pull(BL) %>% 
  as.factor()

# Estair a variável resposta Y
y <- dados %>% 
  pull(Y) %>% 
  as.numeric()

# Criando o modelo da ANOVA
mod <- aov(y ~ trat + bloco)
anova(mod)


# Análise de Normalidade dos Resíduos -------------------------------------



# Análise da homogeneidade das variâncias ---------------------------------











