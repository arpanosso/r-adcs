# Leiam o banco de dados geomorfologia
geomorfologia <- read.table("data/geomorfologia.txt",h=TRUE)

# Estrutura do banco de dados
str(geomorfologia)
dplyr::glimpse(geomorfologia)

# Quantos tipos de solo (Solo) e 
# quantas superfícies (SUP) temos no banco de dados
unique(geomorfologia$SUP)
unique(geomorfologia$Solo)

# Como caracter: Filtrar somente solos "R" - "Neossolo Quartizarênico"
filtro <- geomorfologia$Solo == "R"
geomorfologia[5]
geomorfologia["AMG"]

geomorfologia[5:9]
geomorfologia[c("AMG","AG","AM","AF","AMF")]

# Vamos buscar as colunas de areia com a primeira linha
geomorfologia[1, 5:9]
# vamos omitir a linha
geomorfologia[, 5:9]
geomorfologia[8:12, 5:9] # da 5 até a 9
geomorfologia[8:12, c(5,9)] # somente a 5 e a 9

# Flitro dos Neossolos (R)
geomorfologia[filtro,]


# Filtrar apenas solos da SUP igual a II
filtro <- geomorfologia$SUP == "II"
geomorfologia[filtro,]

# como duas colunas: Flitrar os "LVp"s somente na 
# SUP igual a "II"
filtro_1 <- geomorfologia$SUP == "II"
filtro_2 <- geomorfologia$Solo == "LVp"
geomorfologia[filtro_1 & filtro_2,]

# Filtrar apenas os solos LV, PV2, R, PV5
filtro_1 <- geomorfologia$Solo == "LV"
filtro_2 <- geomorfologia$Solo == "PV2"
filtro_3 <- geomorfologia$Solo == "R"
filtro_4 <- geomorfologia$Solo == "PV5"
geomorfologia[filtro_1|filtro_2|filtro_3|filtro_4,]

# Usando o operador %in%
solos <- c("LV", "PV2", "R", "PV5")
filtro <- geomorfologia$Solo %in% solos
geomorfologia[filtro,]

# caracter e depois numérico: Todos da SUP igual a "I" com
# teor de argila maior que 15
filtro_1 <- geomorfologia$SUP == "I"
filtro_2 <- geomorfologia$ARGILA > 15
geomorfologia[filtro_1 & filtro_2,]

# Vamos calcular para todas as variáveis numéricas
# a média, variância, desvio padrão.
str(geomorfologia)
length(geomorfologia) # retorno o número de COL do data.frame
geomorfo_num <- geomorfologia[5:22]
head(geomorfo_num)

# Calcular a média para cada coluna
dim(geomorfo_num) # as dimensões do data.frame
apply(geomorfo_num, 2, mean)

# Variância
apply(geomorfo_num, 2, var)

# Desvio Padrão
apply(geomorfo_num, 2, sd)

# Criar a função que retorna o CV
meu_cv <- function(x){
  100*sd(x)/mean(x)
}

# Coeficiente de Variação
apply(geomorfo_num, 2, meu_cv)


# Calcular a Média por superfície I, II e III
tapply(geomorfologia$AMG, # variável
       geomorfologia$SUP, # coluna categórica
       mean)              # a função

tapply(geomorfologia$AMG, # variável
       geomorfologia[c("SUP","Solo")], # coluna categórica
       mean)  

summary(geomorfo_num)

# Criar a função para o cálculo do erro padrão da média
meu_erro_padrao <- function(x){
  sd(x)/sqrt(length(x))
}

# Erro padrão da Média
round( apply(geomorfo_num, 2, meu_erro_padrao), 3)
