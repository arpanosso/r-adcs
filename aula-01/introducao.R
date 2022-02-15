# Operações Básicas
2*4
5-6
10/3
4+4
sqrt(225)
8^(1/3)
8**(1/3)
exp(2)
exp(1)
log(10)
round(exp(2.302585),2)
log(125, 3)
log(1000, 10)
log10(1000)
log2(8)

# Média
altura <- c(1.73, 1.75, 1.78, 1.63)
altura

# Média por diferentes funções
soma <- sum(altura)
n <- length(altura)
media <- soma/n
media

# Média usando a função do R
mean(altura)

# Vamos criar a NOSSA função de Média
minha_media <- function(x){ # protótipo da função
  soma <- sum(x) 
  n <- length(x)
  media <- soma/n # o retorno da função é sempre na última linha
  return(paste0("Minha média é: ", round(media,2) ))
  # paste0("Minha média é: ", media)
}
minha_media(altura)

# Pacotes no R - coleção de funções, exemplos e documentação
(.packages()) # apresentou os pacotes carregados no ambiente
(.packages(all.available = TRUE)) # todos pacotes do seu PC
MASS::boxcox

# Carregando um pacote
library("MASS")
require("MASS")
(.packages())
boxcox # faz análise de homocedasticidade

# Instalação de Pacotes
install.packages("agricolae") #instalando o pacote
library(agricolae) # carregamos o pacote no ambiente
kurtosis(altura) # usamos uma função do pacote {agricolae}
install.packages("tidyverse")

# Moda
# Criar uma função que conta os números que se repetem
idades <- c(27, 25, 25, 25, 24, 28)
tabela_freq <- table(idades) # pegando a tab de frequências
vetor_de_ordem <- order(tabela_freq, decreasing = TRUE) # odenando a tabela de maneira decrescente
tabela_freq_ordenada <- tabela_freq[vetor_de_ordem] # salvando a tabela ordenada
names(tabela_freq_ordenada[1]) # mostrando a primeira posição

minha_moda <- function(x){
  tabela_freq <- table(x)
  vetor_de_ordem <- order(tabela_freq, decreasing = TRUE)
  tabela_freq_ordenada <- tabela_freq[vetor_de_ordem]
  valor_base <- tabela_freq_ordenada[1]
  n_classes <- length(tabela_freq_ordenada)
  if(sum(valor_base == tabela_freq_ordenada) == n_classes){
    return("Banco de dados sem Moda")
  }else{
    names(tabela_freq_ordenada[1])
  }
}
minha_moda(idades)
minha_moda(altura)

dados_trimodal <- c(3,4,5,2,3,3,4,4,5,5,7,8,10)
table(dados_trimodal)
minha_moda(dados_trimodal) # 3 4 5

# Lógica no R
altura > 1.8
altura <= 1.75
altura == 1.63 # um igual é equivalente à atribuição <- 2 iguais é o operador relacional "igual a"
altura != 1.63 # != é o operador "diferente de"
sum(altura <= 1.78)

# Mediana
median(altura)

# Resumo estatístico
summary(altura)
























