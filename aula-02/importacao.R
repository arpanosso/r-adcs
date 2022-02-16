# Importação de Dados
## Caminho Absoluto (Não RECOMENDADO)
caminho <- "C:\\Users\\Usuario\\Downloads\\geomorfologia.txt"
geomorfologia <- read.table(caminho, h=TRUE)

## Caminho Relativo (pasta data)
getwd() # busco o diretório de trabalho
setwd("C://R//r-adcs") # definir o diretório de trabalho
geomorfologia <- read.table("data/geomorfologia.txt", h=TRUE)

## Dados a partir da área de transferência
# Copiar a coluna no Excel
# Colar em um objeto no console utilizando o Scan
# Terminar com um ENTER
x

# Tabela do Excel na área de transferência
geral <- read.table("clipboard", h=TRUE)
geral

# Ler a partir do EXCEL
library(readxl)
geral <- read_excel("data/geral.xlsx")

# Lendo o geomorfologia.xlsx (EXCEL)
geomorfologia <- read_excel("data/geomorfologia.xlsx")

# Carregando o MetaPacote (pacote de pacotes...)
library(tidyverse)
glimpse(geomorfologia)
geomorfologia$argila_p <- geomorfologia$ARGILA/100
geomorfologia$silte_p <- geomorfologia$SILTE/100

# Criar a variável Ca_Mg = Ca + Mg
geomorfologia$Ca_Mg <- geomorfologia$Ca + geomorfologia$Mg

# e criar a variável Log_P = log(P) 
geomorfologia$Log_P <- log(geomorfologia$P)
glimpse(geomorfologia)

# Exportar e Excel
library(writexl)
write_xlsx(geomorfologia, "data/geomorfologia_new.xlsx")

# Exportar um arquivo geomorfologia padronizado, 
# todas as variáveis com média zero e variância unitária
library(vegan)
geo_std <- decostand(geomorfologia[-c(1,2,3,4)], method = "standardize",
                     na.rm = TRUE)
typeof(geo_std)
summary(geo_std)

# utilizando o scale
geo_std_2 <- scale(geomorfologia[-c(1,2,3,4)])
summary(geo_std_2)
typeof(geo_std_2)


# Melhor forma de guardar um banco de Dados é em RDS
library(readr)
write_rds(geomorfologia,"data/geomorfologia.rds")
geo_tib <- read_rds("data/geomorfologia.rds")

# Importação direto da Internet
URL <- "https://raw.githubusercontent.com/arpanosso/r_data_science_fcav/master/dados/geomorfologia.txt"
geomorfologia <- read.table(URL, h = TRUE)
head(geomorfologia) # mostra os 6 primeiros registros
tail(geomorfologia) # mostra os 6 últimos registros


























