library(tibble)
library(tidyverse)
# Teorema Central do Limite (teorema do limite central)
num <- 1:9
freq <- c(240, 100, 400, 150, 980, 438, 560, 1000, 320)
x <- rep(num, freq)
table(x)

# X é a variável da população
# Vamos buscar Parâmetros
E <- mean(x)
VAR <- var(x)

# Ditribuição de frequência
hist_pop <- tibble(x) %>% 
  ggplot(aes(x=x)) +
  geom_histogram(color="black", fill="aquamarine4") +
  geom_vline(xintercept = E, color="red", linetype=2)

# vamos repetir o processo acima k vezes, salvar o valor da média
# e criar o gráfico de distribuição
n = 5
k = 1000
medias <- 0
for( i in 1:k){
  medias[i] <- mean( sample(x,n,replace=TRUE) )
  }
hist_amostra_5 <- tibble(medias) %>% 
  ggplot(aes(x=medias)) +
  geom_histogram(color="black", fill="orange") +
  geom_vline(xintercept = E, color="red", linetype=2) +
  coord_cartesian(xlim=c(0,9))


n = 50
k = 1000
medias <- 0
for( i in 1:k){
  medias[i] <- mean( sample(x,n,replace=TRUE) )
}
hist_amostra_50 <- tibble(medias) %>% 
  ggplot(aes(x=medias)) +
  geom_histogram(color="black", fill="blue") +
  geom_vline(xintercept = E, color="red", linetype=2) +
  coord_cartesian(xlim=c(0,9))


library(patchwork)
hist_pop / hist_amostra_5 / hist_amostra_50












