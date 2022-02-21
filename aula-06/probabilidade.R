# Distribuições de Probabilidade
### Distribuição BINOMIAL
n = 5
p = 0.75
x = 0:n
PX <- dbinom(x, n, p)
barplot(PX, names=x,
        xlab="X",
        ylab="P(X)")

# Ditribuição NORMAL
m = 5
dp = .25
curve(dnorm(x,m,dp), 3, 7, ylim=c(0,3))
abline(v=m,col="blue")

# Encontrando o valor de a
qnorm(.025, m, dp)

# Encontrando o valor de b
qnorm(0.025 + 0.95, m , dp)

# Normal Padronizada
curve(dnorm(x), -3, 3)


# Distribuição F
gl_num <- 7
gl_den <- 7

curve(df(x,gl_num,gl_den), 0, 6)
qf(0.9, gl_num, gl_den)
qf(0.95, gl_num, gl_den)
qf(0.99, gl_num, gl_den)

# Teste de Variâncias
GF <- c(1.63, 1.58, 1.72, 1.71, 1.65)
GM <- c(1.78, 1.81, 1.67, 1.68, 1.76)

curve(df(x,4,4),0,7)
Fobs <- var(GM)/var(GF)
abline(v=Fobs, col="blue")

Fc <- qf(0.95,4,4)
abline(v=Fc, col="red")

var.test(GM,GF)


# Teste de Médias para essas populações
mean(GM)
mean(GF)
t.test(GM,GF, alternative = "t", var.equal = TRUE)

curve(dt(x,8),-3,3)
qt(0.025,8)
qt(0.975,8)








