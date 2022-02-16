# Visualização de dados
# Correlação / análise de regressão
## Entrar com o banco de dados geomorfologia
geomorfologia <- read.table("data/geomorfologia.txt",
                            h=TRUE)

## Extrair o x e o y
x <- geomorfologia$AG
y <- geomorfologia$ARGILA

## Construir o Gráfico
plot(x,y,
     xlab = "Areia Grossa",
     ylab = "Teor de Argila",
     main = "Gráfico de Dispersão",
     xlim = c(0,30),
     cex.lab = 1.2,
     cex = 1.4,
     axes = FALSE,
     col = "black",
     pch = 21,
     bg = "gray")
axis(1,seq(0,30,3))
axis(2,las = 1)
box()

# Correlação
cor(x,y)
cor.test(x,y)

# Teste de Normalidade
shapiro.test(x)
hist(x)

log_x <- log(x)
shapiro.test(log_x)
hist(log_x)

shapiro.test(y)
hist(y)

# Salvar em 300 dpis ?
png("plot_AG_ARGILA.png")
plot(x,y)
dev.off()

# Análise de Regressão
plot(x,y,xlab="Areia Grossa", ylab="Argila")

# Craindo o modelo
mod_reg <- lm(y ~ x)
mod_reg

# Resumo da análise de regresão
summary.lm(mod_reg)

# No ajuste linear, o r = sqrt(r²)
sqrt(0.1054)

# Uma breve vizualisação das pressuposições
plot(mod_reg)
rs <- rstudent(mod_reg)
hist(rs)
shapiro.test(rs)

# Passando a resta ajustada na nuvém de pontos
plot(x,y)
abline(mod_reg)

#### Repetir a análise para a superfície "I"
## Extrair o x e o y
filtro <- geomorfologia$SUP == "I"

x <- geomorfologia[filtro, "AG"]
y <- geomorfologia[filtro, "ARGILA"]

## Construir o Gráfico
plot(x,y,
     xlab = "Areia Grossa",
     ylab = "Teor de Argila",
     main = "Gráfico de Dispersão (SUP I)",
     xlim = c(3,6),
     cex.lab = 1.2,
     cex = 1.4,
     axes = FALSE,
     col = "black",
     pch = 21,
     bg = "gray")
axis(1,seq(0,6,.5))
axis(2,las = 1)
box()

# Correlação
cor(x,y)
cor.test(x,y)

# Teste de Normalidade
shapiro.test(x)
hist(x)

shapiro.test(y)
hist(y)

# Análise de Regressão
plot(x,y,xlab="Areia Grossa", ylab="Argila")

# Craindo o modelo
mod_reg <- lm(y ~ x)
mod_reg

# Resumo da análise de regresão
summary.lm(mod_reg)

# No ajuste linear, o r = sqrt(r²)
sqrt(0.1054)

# Uma breve vizualisação das pressuposições
plot(mod_reg)
rs <- rstudent(mod_reg)
hist(rs)
shapiro.test(rs)

# Passando a resta ajustada na nuvém de pontos
plot(x,y)
abline(mod_reg)



#### Repetir a análise para a superfície "II"
## Extrair o x e o y
filtro <- geomorfologia$SUP == "II"

x <- geomorfologia[filtro, "AG"]
y <- geomorfologia[filtro, "ARGILA"]

## Construir o Gráfico
plot(x,y,
     xlab = "Areia Grossa",
     ylab = "Teor de Argila",
     main = "Gráfico de Dispersão (SUP II)",
     xlim = c(3,6),
     cex.lab = 1.2,
     cex = 1.4,
     axes = FALSE,
     col = "black",
     pch = 21,
     bg = "gray")
axis(1,seq(0,6,.5))
axis(2,las = 1)
box()

# Correlação
cor(x,y)
cor.test(x,y)

# Teste de Normalidade
shapiro.test(x)
hist(x)

shapiro.test(y)
hist(y)

# Análise de Regressão
plot(x,y,xlab="Areia Grossa", ylab="Argila")

# Craindo o modelo
mod_reg <- lm(y ~ x)
mod_reg

# Resumo da análise de regresão
summary.lm(mod_reg)

# Uma breve vizualisação das pressuposições
plot(mod_reg)
rs <- rstudent(mod_reg)
hist(rs)
shapiro.test(rs)

# Passando a resta ajustada na nuvém de pontos
plot(x,y)
abline(mod_reg)

# Estudo de outliers
rs <- rstudent(mod_reg)
yp <- predict(mod_reg)

plot(yp, rs,
     ylim = c(-5,5))
abline(h=c(-3,3), col="red", lty=2)










