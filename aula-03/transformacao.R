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

# como duas colunas

# caracter e depois numérico











