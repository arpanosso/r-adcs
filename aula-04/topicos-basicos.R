# Tópicos Basicos
## Carregar o banco de dados goemorfologia
geomorfologia <- read.table("data/geomorfologia.txt", h=TRUE)

## Extrair somente as variáveis numéricas de geomorfologia
geomorfo_num <- geomorfologia[5:22]
head(geomorfo_num)

## Chamar as funções previamente criadas
source("R/minhas-funcoes.R")
apply(geomorfo_num, 2, meu_cv)
apply(geomorfo_num, 2, meu_erro_padrao)

# Rodar a estatítica descritiva
saida <- apply(geomorfo_num, 2, est_descritiva)

# Salvar em excel
library(writexl)
estatistica <- row.names(saida)
saida <- as.data.frame(saida)
saida <- cbind(estatistica, saida)
write_xlsx(saida,"aula-04/est_descritiva.xlsx")

## Estatística por superfície
tapply(geomorfo_num$ARGILA, 
       geomorfologia$Solo, 
       est_descritiva)




