#Jogo Natal Cenário Simplificado

#Número de participantes, a probabilidade
#não depende de n, vai definir apenas k máximo.
n = 30

#Matriz que armazenará os valores de K,
#e suas probabilidades
M <- matrix(0, ncol=2,nrow=n-1)
colnames(M) <- c("K", "Prob")

#Meu número, 2 <= k <= n
for (k in 2:n){
  #Corridas
  r <- 10000
  
  #Número de corridas em que roubar é melhor
  f <- 0
  
  for (i in 1:r){
    #Array dos presentes
    pre <- sample(1:n, n, replace=FALSE)
    #Se o presente roubado for maior que
    #o escolhido da pilha
    if (max(pre[1:k-1]) > pre[k]){
      f <- f +1
    }
  }
  M[k-1,1] <- k
  M[k-1,2] <- f/r
}

M