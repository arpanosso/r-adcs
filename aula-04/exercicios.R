# Crie uma script/função que peça dois números e 
# imprima o maior deles.
x <- 8
y <- 78
meu_maior <- function(a, b){
  if(a == b){
    "Números são iguais"
  }else{
    if(a > b ){
      paste0("Maior número: ",a)
    }else{
      paste0("Maior número: ",b)
    }
  }
}
meu_maior(x,y) # maior número: 78
meu_maior(3,3)

# Utilizando o scan
meu_maior_scan <- function(){
  a <- readline("Digite o valor de a > ")
  a <- as.numeric(a)
  b <- readline("Digite o valor de b > ")
  b <- as.numeric(b)
  if(a == b){
    "Números são iguais"
  }else{
    if(a > b ){
      paste0("Maior número: ",a)
    }else{
      paste0("Maior número: ",b)
    }
  }
}
meu_maior_scan()

# Crie um script/função que peça um número e informe 
# se o número é inteiro ou decimal.
inteiro_decimal <- function(num){
  inteiro = trunc(num)
  if(num == inteiro){
    print("Inteiro")
  }else{
    print("Decimal")
  }
}
inteiro_decimal(4.2) # decimal
inteiro_decimal(8) # inteiro

# Crie um script/função que leia um número de 1 a 7 e 
# exiba o dia correspondente da semana. 
# (1- Domingo, 2- Segunda, …, 7-Sábado.). 
# Caso o usuário digitar um número diferente o 
# programa deve escrever Valor inválido.
dia_da_semana <- function(x){
  dplyr::case_when(
    x == 1 ~ "Domingo",
    x == 2 ~ "Segunda-feira",
    x == 3 ~ "Terça-feira",
    x == 4 ~ "Quarta-feira",
    x == 5 ~ "Quinta-feira",
    x == 6 ~ "Sexta-feira",
    x == 7 ~ "Sábado",
    TRUE ~ "Valor inválido"
  )
}
dia_da_semana(2) # Segunda-feira
dia_da_semana(7) # Sábado
dia_da_semana(18) # Valor inválido
 
# Crie um script/função que verifique se uma 
# letra digitada é vogal ou consoante.
eh_vogal <- function(letra){
  vogais <- c("a","e","i","o","u","A","E","I","O","U")
  if(letra %in% vogais){
    print("Vogal")
  }else{
    print("Consoante")
  }
}
eh_vogal("a") # Vogal
eh_vogal("E") # Vogal
eh_vogal("b") # Consoante

# Crie um script/função que leia três números 
# e mostre o maior e o menor deles.
amplitude <- function(x){
  menor <- min(x)
  maior <- max(x)
  return(c(menor, maior))
}
amplitude( c(4,3,7,1,10) )# 1   10
range(c(4,3,7,1,10))

# Elabore um algoritmo que, classifique 
# um número X, fornecido pelo usuário, em par ou ímpar, 
# utilize o operador % para o cálculo do resto da divisão.
24 %% 5 # resto da divisão
24 %/% 5 # inteiro da divisão

par_impar <- function(x){
  if( (x %% 2) == 0) {
    print("par")
  }else{
    print("impar")
  }
}
par_impar(5) # impar
par_impar(44) # par


# Faça um programa que imprima uma frase 
# n vezes na tela do computador, 
# n deve ser um número fornecido pelo usuário.
i <- 1
while(n >= i){
  if(i == 1){
    n <- readline("Digite número maior que 1 > ")
    n <- as.numeric(n)
    print(paste0(i," - O R domina o mundo"))
  }else{
    print(paste0(i," - O R domina o mundo"))
  }
  i = i+1
}

# Faça um programa que imprima 
# na tela os números de 1 a 20.
1:20
20:1

# Faça um programa para obter as sequências 
# de 0 a 25 com passo igual a 2.
seq(0,25,02)


# Exercício da Soma S
S <- 0
for(i in 1:10){
  if(i %% 2 == 0){
    S = S -i/i^2
  }else{
    S = S +i/i^2
  }
}
S

i <- 1:10
j <- rep(c(1,-1),5)
sum(j*i/i^2)

# Construa um algoritmo que verifique se o número 
# fornecido pelo usuário (inteiro maior que 1) é 
# primo ou não (números primos são os números naturais 
# que têm apenas dois divisores o 1 e ele mesmo, 
# exemplo (2, 3, 5, 7, 11, 13, 17…).
eh.primo <- function(x){
  sum(x %% 1:x == 0) == 2
}

eh.primo(17) # TRUE
eh.primo(6) # FALSE
eh.primo(37)
eh.primo(2)









