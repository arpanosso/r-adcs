#parcelas-subdivididas
# Supor um experimento com três adubos A, B e C em seis blocos
# casualizados, sendo cada parcela subdividida de duas subparcelas. Em uma
# determinada fase do experimento, as subparcelas dentro de cada parcela,
# passaram a receber, por sorteio, um dos dois tipos de suplementos 
# minerais M e P. A variável dependente é a produção no final do
# experimento.
library(nortest)
library(tidyverse)
library(lawstat)
library(ggpubr)
library(gghighlight)
library(skimr)
library(ExpDes.pt)
library(tibble)

# Entrada de dados
dados <- read.table("data/subdividida.txt",h=TRUE)

# Vislumbre do data set
glimpse(dados)

# Resumo estatístico
skim(dados)

# Criar os tratamentos
adubo <- dados %>% pull(adubo) %>% as.factor()
suplemento <- dados %>% pull(suplemento) %>%  as.factor()
bloco <- dados %>%  pull(bloco) %>% as.factor()
y <- dados %>% pull(prod) %>% as.numeric()
mod <- aov(y ~ bloco + adubo + Error(bloco/adubo)+ suplemento +
             adubo:suplemento)
summary(mod)

# EspDes.pt
psub2.dbc(adubo, suplemento, bloco, y,
          quali=c(TRUE, TRUE),
          fac.names=c("AD","SM"))
