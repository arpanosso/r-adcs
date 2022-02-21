# Análise exploratória dos dados
# ler os dados de geomorfologia
geomorfologia <- read.table("data/geomorfologia.txt",
                            header = TRUE)

# Carregar o pacote Tidyverse
library(tidyverse)
glimpse(geomorfologia)

# função mutate do dplyr.
geomorfologia <- geomorfologia %>% 
  mutate(
    AREIA = AMG + AG+ AM+ AF+ AMF
  )

# realocar colunas
geomorfologia <- geomorfologia %>% 
  relocate(SUP, Solo, Amostra, X, AREIA)

geomorfologia %>% 
  relocate(AREIA, .after = ARGILA) %>% 
  View()


# Classificar a textura do solo
# craiar a variável TEXTURA
# Arenosa Argila < 15
# Média Argila entre 15 e 35
# Argilosa para Argila entre 35 e 60
# Muito argilosa para Argila maior que 60
# Caso contrário "Sem classificação"
# função case_when()
geomorfologia <- geomorfologia %>% 
  mutate(
    TEXTURA = case_when(
      ARGILA < 15 ~ "Arenosa",
      ARGILA <=35 ~ "Média",
      ARGILA <= 60 ~ "Argilosa",
      ARGILA > 60 ~ "Muito argilosa",
      TRUE ~ "Sem classificação"
    )
  )

## Estatistica descritiva por SUP
geomorfologia %>% 
  group_by(SUP) %>% 
  summarise(contagem = n()) %>% 
  mutate(
    contagem_p = contagem / sum(contagem),
    contagem_p_acumulada = cumsum(contagem_p)
  )


## Estatistica descritiva por Solo
geomorfologia %>% 
  group_by(Solo) %>% 
  summarise(contagem = n()) %>% 
  mutate(
    contagem_p = contagem / sum(contagem),
    contagem_p_acumulada = cumsum(contagem_p)
  ) %>% 
  mutate(
    Solo = fct_reorder(Solo, contagem)
  ) %>% 
  ggplot(aes(y=Solo, x= contagem_p, fill=Solo)) +
  scale_fill_viridis_d() +
  geom_col(color="black") +
  theme_bw()

# Calcular a média o desvio padrão e o CV, para Argila Silte
# e Areia por solo.
source("R/minhas-funcoes.R")
geomorfologia %>% 
  group_by(Solo) %>% 
  summarise(
    media_argila = mean(ARGILA,na.rm=TRUE),
    media_areia = mean(AREIA,na.rm=TRUE),
    media_silte = mean(SILTE,na.rm=TRUE),
    desvio_argila = sd(ARGILA,na.rm=TRUE),
    desvio_areia = sd(AREIA,na.rm=TRUE),
    devio_silte = sd(SILTE,na.rm=TRUE),
    cv_argila = meu_cv(ARGILA),
    cv_areia = meu_cv(AREIA),
    cv_silte = meu_cv(SILTE)
  ) %>% View()


# Construir os histogramas para Argila Silte e Areia
hist_argila <- geomorfologia %>% 
  ggplot(aes(x=ARGILA, y = ..density..)) +
  geom_histogram(bins=12, color="black", fill="orange") +
  geom_density(fill="blue", alpha = 0.1, color="red")

hist_areia <- geomorfologia %>% 
  ggplot(aes(x=AREIA, y = ..density..)) +
  geom_histogram(bins=12, color="black", fill="gray") +
  geom_density(fill="blue", alpha = 0.1, color="red")

hist_silte <- geomorfologia %>% 
  ggplot(aes(x=SILTE, y = ..density..)) +
  geom_histogram(bins=12, color="black", fill="lightgreen")+
  geom_density(fill="blue", alpha = 0.1, color="red")

library(patchwork)
# hist_argila | hist_areia | hist_silte
hist_argila / hist_areia / hist_silte


## Somente para o Teor de Argila
## Construir um histograma por SUP
geomorfologia %>% 
  ggplot(aes(x=ARGILA, y = ..density.., fill=SUP)) +
  geom_histogram(bins=12, color="black") +
  geom_density(fill="blue", alpha = 0.1, color="red") +
  facet_wrap(~SUP, nrow = 3) +
  labs(x="Teor de Argila", y = "Densidade de frequência",
       fill = "Superfície geomórfica") +
  theme_bw() +
  theme(legend.position="top")

## Usando o fiter
geomorfologia %>% 
  filter(SUP == "II") %>% 
  ggplot(aes(x=ARGILA, y = ..density..)) +
  geom_histogram(bins=5, color="black", fill="pink") +
  geom_density(fill="blue", alpha = 0.1, color="red")

## Boxplot
geomorfologia %>% 
  ggplot(aes(y=ARGILA, x=SUP, fill=SUP)) +
  geom_boxplot()

## Boxplot de cada solo, por sua superfície individual
geomorfologia %>% 
  ggplot(aes(x=ARGILA, y=Solo, fill=Solo)) +
  geom_boxplot() +
  facet_wrap(~SUP, scales="free")

# Violino
geomorfologia %>% 
  ggplot(aes(x=SUP, y=ARGILA, fill=SUP))+
  geom_violin(trim = FALSE)


geomorfologia %>% 
  ggplot(aes(x=SUP, y=ARGILA))+
  geom_violin(trim = FALSE) +
  geom_dotplot(binaxis = "y", dotsize = 1, 
               stackdir = "center")

# Boxplot e violin plot
geomorfologia %>% 
  ggplot(aes(x=SUP, y=ARGILA, fill=SUP))+
  geom_violin(trim = FALSE, fill = "lightgray",
              width=1.5) +
  geom_boxplot(width=.15) +
  theme_classic()


# Testes de Normalidade ---------------------------------------------------
# Variável Argila
library(nortest) # vários testes de normalidade
geomorfologia %>% 
  ggplot(aes(x=ARGILA, y = ..density..))+
  geom_histogram(bins=12, color="black", fill="lightgray")

# Shapiro and Wilks
geomorfologia %>% 
  pull(ARGILA) %>% 
  shapiro.test()

# Kolmogorov Smirnov - recomendado para n > 2000 observações
geomorfologia %>% 
  pull(ARGILA) %>% 
  lillie.test()

# Anderson Darlin 
geomorfologia %>% 
  pull(ARGILA) %>% 
  ad.test()

# Cramer-von Mises
geomorfologia %>% 
  pull(ARGILA) %>% 
  cvm.test()


# Normalidade para SUP I --------------------------------------------------
geomorfologia %>% 
  filter(SUP == "I") %>% 
  ggplot(aes(x=ARGILA, y = ..density..))+
  geom_histogram(bins=7, color="black", fill="lightgray")

# Shapiro and Wilks
geomorfologia %>% 
  filter(SUP == "I") %>% 
  pull(ARGILA) %>% 
  shapiro.test()

# Kolmogorov Smirnov - recomendado para n > 2000 observações
geomorfologia %>% 
  filter(SUP == "I") %>% 
  pull(ARGILA) %>% 
  lillie.test()

# Anderson Darlin 
geomorfologia %>% 
  filter(SUP == "I") %>% 
  pull(ARGILA) %>% 
  ad.test()

# Cramer-von Mises
geomorfologia %>% 
  filter(SUP == "I") %>% 
  pull(ARGILA) %>% 
  cvm.test()


# Normalidade para SUP II ----------------------------------------------
geomorfologia %>% 
  filter(SUP == "II") %>% 
  ggplot(aes(x=ARGILA, y = ..density..))+
  geom_histogram(bins=7, color="black", fill="lightgray")

# Shapiro and Wilks
geomorfologia %>% 
  filter(SUP == "II") %>% 
  pull(ARGILA) %>% 
  shapiro.test()

# Kolmogorov Smirnov - recomendado para n > 2000 observações
geomorfologia %>% 
  filter(SUP == "II") %>% 
  pull(ARGILA) %>% 
  lillie.test()

# Anderson Darlin 
geomorfologia %>% 
  filter(SUP == "II") %>% 
  pull(ARGILA) %>% 
  ad.test()

# Cramer-von Mises
geomorfologia %>% 
  filter(SUP == "II") %>% 
  pull(ARGILA) %>% 
  cvm.test()


# QQ PLOT PARA O TEOR DE AREIA --------------------------------------------
geomorfologia %>% 
  filter(SUP == "II") %>% 
  ggplot(aes(x=AREIA, y = ..density..))+
  geom_histogram(bins=7, color="black", fill="lightgray")

geomorfologia %>% 
  ggplot(aes(sample = (AREIA))) +
  stat_qq(color="red") +
  stat_qq_line(color="blue") +
  facet_wrap(~SUP)


## QQ plot para argila
geomorfologia %>% 
  ggplot(aes(sample = (ARGILA))) +
  stat_qq(color="red") +
  stat_qq_line(color="blue") +
  facet_wrap(~SUP)










