# analise-varias-variaveis
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
geomorfologia <- read.table("data/geomorfologia.txt",h=TRUE)

# Vislumbre do data set
glimpse(geomorfologia)

# Resumo estatístico
skim(geomorfologia)

# Vetor com o nome das variáveis a serem analisadas
variaveis <- geomorfologia %>% 
  select(AMG:V) %>% 
  names()

theme_set(theme_bw())
for(i in seq_along(variaveis)){
  print("========================================")
  print(variaveis[i])
  print("========================================")
  
  sup <- geomorfologia %>% 
    pull(SUP) %>% 
    as.factor()
  
  y <- geomorfologia %>% 
    pull(variaveis[i]) %>% 
    as.numeric()
  
  # Definindo o modelo
  mod <- aov(y ~ sup)
  
  # Teste Diagnósticos   
  ## Normalidade dos erros, os erros aleatórios, devem ter
  ## distribuição normal
  rs <- rstudent(mod) # erros estudentizados
  
  ## Histograma para o resíduos
  hist_residuos <- rs %>% 
    tibble::tibble() %>% 
    ggplot(aes(x=rs)) + 
    geom_histogram(bins = 12, color="black", fill= "lightgray") +
    labs(title = paste0("Histograma dos resíduos - ",variaveis[i]))
  print(hist_residuos)
  
  ## QQ plot dos resíduos
  qq_plot_residuos <- rs %>% 
    tibble() %>% 
    ggplot(aes(sample = rs))+
    stat_qq(color="red")+
    stat_qq_line(color="blue")+
    labs(title = paste0("QQ plot dos resíduos - ",variaveis[i]))
  print(qq_plot_residuos)
  
  ## Passando em todos os 4 testes de normalidade
  print("testes de normalidade dos resíduos")
  print(shapiro.test(rs))
  print(lillie.test(rs))
  print(ad.test(rs))
  print(cvm.test(rs))
  ## concluímos que os resíduos não tem distribuição normal
  
  ## Estudo dos outliers
  yp <- predict(mod)
  
  plot_dos_residuos <- tibble(yp, rs) %>% 
    ggplot(aes(x=yp, y=rs)) +
    geom_point(color="red",size=3)+
    geom_hline(yintercept = c(-3, 3), color="red", linetype=2)+
    gghighlight(rs > 3 | rs < -3,
                unhighlighted_params = list(
                  shape = 23,
                  color = "black",
                  fill = "lightgray",
                  size=2
                )) +
    labs(title = paste0("Estudo de outliers - ",variaveis[i]))
  print(plot_dos_residuos)
  
  
  ## Boxplot para o teor de argila por superfície
  boxplots_trats <- tibble(sup, y) %>% 
    ggplot(aes(x=sup, y=y, fill=sup)) +
    geom_boxplot() +
    labs(title = paste0("Boxplot - ",variaveis[i]))
  print(boxplots_trats)
  
  ## teste de Bartlett
  print(bartlett.test(y,sup))
  print(levene.test(y,sup)) ## Brown and Forsythe
  print(levene.test(y,sup,location = "mean")) ## teste de Levene
  
  # Análise de variância 
  dic(sup, y, quali=TRUE, mcomp = "tukey", sigT=0.05, sigF = 0.05)
  cat("\n")
}



