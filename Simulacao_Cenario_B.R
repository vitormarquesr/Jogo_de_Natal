#Jogo Natal Cenário Completo

#Nesse cenário, as probabilidades
#dependem de n, o programa simulará
#de 3 até nmax.
nmax = 30

#Matriz que armazenará os resultados
M <- matrix(0, ncol=3,nrow=(2+nmax-1)*(nmax-2)/2)
colnames(M) <- c("N", "K", "Prob")

#Contador das linhas
y <- 0 

for (n in 3:nmax){
  #Seu número
  for (k in 2:n){
    #O número de corridas será 1000,
    #pois um número maior demorará muito tempo.
    #Será necessário sacrificar um pouco de precisão.
    r <- 1000
    
    #Número de corridas em que roubar é melhor
    f <- 0
    for (i in 1:r){
      #Sorteio dos presentes
      pre <- sample(1:n, n, replace=FALSE)
      
      #1 - Caso em que mantém o presente
      pre2 <- pre
      if (k < n){
        #Percorrendo o turno dos outros jogadores
        for (t in (k+1):n){
          #O jogador da vez irá roubar o melhor presente
          #até o momento. O jogador que teve o presente roubado
          #irá pegar o presente que teria ido para o jogador da
          #vez. Podemos usar esse truque para facilitar a 
          #programação sem perda de generalidade.
          i_max = which(pre2[1:t-1]==max(pre2[1:t-1]))
          v_max = max(pre2[1:t-1])
          pre2[i_max] = pre2[t]
          pre2[t] = v_max
        }
      }
      #2 - Caso em que troca pelo n-k+1 melhor presente se possível,
      #se não for possível troca pelo menor.
      
      if (n-k+1 >= k){
        #2.1 - Caso em que troca pelo menor,
        #pode haver roubos.
        v <- min(pre[1:k-1])
        i_min = which(pre[1:k-1]==v)
        pre[i_min] = pre[k]
        pre[k] = v
        #Percorrendo o turno dos outros jogadores
        for (t in (k+1):n){
          i_max = which(pre[1:t-1]==max(pre[1:t-1]))
          v_max = max(pre[1:t-1])
          pre[i_max] = pre[t]
          pre[t] = v_max
        }
        #Valor final que ficará
        v <- pre[k]
        
      }else{
        #2.2 - Caso em que trocará pelo (n-k+1) melhor,
        #não há a possibilidade de roubos
        v <- sort(pre[1:k-1], decreasing=TRUE)[n-k+1]
      }
      #Se o presente depois que rouba for
      #melhor do que fica, incrementa f.
      if (v > pre2[k]){
        f <- f + 1
      }
    }
    y <- y + 1
    M[y,1] <- n
    M[y,2] <- k
    M[y,3] <- f/r
  }
}

