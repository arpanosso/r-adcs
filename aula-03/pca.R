# Análise de componentes Principais
# Ler o banco de dados de Geomorfologia
geomorfologia <- read.table("data/geomorfologia.txt",h=TRUE)

# Filtrar somente as variáveis numéricas desse banco de dados
geomorfo_num <- geomorfologia[5:22]

# Criar a matriz de correlação linear entre as variáveis
matriz_corr <- cor(geomorfo_num, use ="complete.obs")

# Visualização pelo corrplot
library(corrplot)
corrplot(matriz_corr)
corrplot(matriz_corr, method = "ellipse")
corrplot(matriz_corr, method = "color")
corrplot.mixed(matriz_corr,upper = "ellipse",
               lower.col = "black")

## Criar a variável AREIA = AMG+AG+AM+AF+AMF
geomorfo_num$AREIA <- geomorfo_num$AMG + geomorfo_num$AG +
  geomorfo_num$AM + geomorfo_num$AF + geomorfo_num$AMF
## Retirar AF_AG, SB, T, V
geomorfo_num <- geomorfo_num[c("SILTE","ARGILA","AREIA",
                               "P","pH","K","Ca","Mg","H_Al")]
matriz_corr <- cor(geomorfo_num, use ="complete.obs")
corrplot.mixed(matriz_corr,upper = "ellipse",
               lower.col = "black")

# padronizar o banco de dados
library(vegan)
geomorfo_pad <- decostand(geomorfo_num,
                          method = "standardize",
                          na.rm=TRUE)
# matriz de correlação não se altera
matriz_corr <- cor(geomorfo_pad, use ="complete.obs")
corrplot.mixed(matriz_corr,upper = "ellipse",
               lower.col = "black")


# Análise de Componentes Principais
pca <- prcomp(geomorfo_pad)
summary(pca)

# Extraindo os autovalores
eig <- pca$sdev^2
eig

# Variância Explicada
ve<-eig/sum(eig)
ve

# Variância Explicada acumulada
cumsum(ve)

# Estudar a contribuição de cada variável na componente
# principal
mc <- cor(geomorfo_pad, pca$x)
corrplot(mc)

# Screeplot
screeplot(pca)

#Biplot
da <- geomorfo_pad
grupos <- geomorfologia$Solo
pc1V<-cor(da,pca$x)[,1]/sd(cor(da,pca$x)[,1])
pc2V<-cor(da,pca$x)[,2]/sd(cor(da,pca$x)[,2])
pc1c<-pca$x[,1]/sd(pca$x[,1])
pc2c<-pca$x[,2]/sd(pca$x[,2])
nv<-ncol(da) # número de variáveis utilizadas na análise
plot(pc1V,pc2V,
     xlim=c(min(-3,pc1V,pc1c),
            max(pc1V,pc1c,5)),
     ylim=c(min(pc2V,pc2c),
            max(pc2V,pc2c)),pch="",las=1,
     xlab=paste("PC1 (",round(100*ve[1],2),"%)",sep=""),
     ylab=paste("PC2 (",round(100*ve[2],2),"%)",sep=""),
     font.lab=2)
abline(v=0,h=0)
arrows(rep(0,nv),rep(0,nv),pc1V*.90,pc2V*.90,lwd=1.5,length=.1)
text(pc1V,pc2V,names(pc1V),font=4)
lv<-as.factor(grupos);nlv<-levels(lv)

# Adicionando os identificadores dos acesso partir de uma variável categórica
for(i in 1:length(nlv)){
  ff<-lv==nlv[i]  
  # points(pc1c[ff],pc2c[ff],cex=1.3,col=i,pch=i+15) 
  # df$Municípios[ff]
  text(pc1c[ff],pc2c[ff],grupos[ff],cex=.8,col=i)}






