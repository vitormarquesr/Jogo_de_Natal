Análise Jogo de Natal
================

# Pré-Requisitos

``` r
library(tidyverse)
```

# Enunciado

Uma família decidiu realizar um jogo de natal. As regras são a seguinte:

-   Os presentes comprados por cada um são colocados na roda e embalados
    para que ninguém consiga ver o conteúdo.  
-   Cada participante recebe um número inteiro distinto, representando
    sua vez de jogar.
-   Em sua vez, o jogador tem duas opções, pegar um presente da pilha e
    revelar seu conteúdo, ou roubar o presente de um jogador anterior.
    Se um jogador tiver seu presente roubado, ele obrigatoriamente tem
    que pegar um outro presente da pilha.  
-   O jogo continua até que o jogador com o último número tenha
    jogado.  
    Suponha que n pessoas participam do jogo e seu número é k.

**Condições: n &gt; 3, 2 &lt;= k &lt;= n**

Considerando dois cenários:

## 1 - Cenário simplificado

Considere nesse cenário que o presente que você roubar e pegar da pilha
ficará com você até o final do jogo, ou seja, ninguém o roubará de você.
Seu objetivo é obter o melhor presente possível. Sua estratégia será
roubar o melhor presente até sua vez, ou seja, o melhor presente que
saiu nas últimas (k - 1) jogadas.

**Calcule a probabilidade de você obter um presente melhor com essa
estratégia ao invés de pegar um presente da pilha.**

### 1.1 - Simulação de Monte Carlo

``` r
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
```

### 1.2 - Análise

``` r
M %>% as_tibble %>%
  ggplot(aes(x=K, y=Prob))+
  geom_point()+
  geom_hline(yintercept = 0.5, colour="red")+
  ylim(c(0,1))
```

![](Analise_files/figure-gfm/simplificado%20analise-1.png)<!-- -->

As probabilidades não dependem de n. É possível observar que a
probabilidade de obter um presente melhor tomando de outro jogador é
sempre maior ou igual à 50%, indicando que é uma estratégia boa. Para
k=1, a probabilidade é extamante igual a 50%, resultado que pode ser
provado analiticamente. Já para k&gt;1, a probabilidade é sempre maior
que 50%, crescendo com o aumento de k.

------------------------------------------------------------------------

## 2 - Cenário Completo

Considere que nesse cenário os participantes podem roubar o presente de
você. A estratégia deles será sempre roubar o melhor presente até o
momento de suas jogadas. Consciente da estratégia dos outros jogadores
você reformulou a sua, você não roubará o melhor presente. Sua
estratégia será roubar o melhor presente que você tenha a certeza de que
ninguém o roubará, se isso não for possível, você roubará o pior
presente até o momento.

Por exemplo:

-   Suponha n = 10 e k = 7. É a sua vez, já jogaram 6 jogadores e ainda
    há 3 para jogar. Se você roubar o melhor presente até o momento, um
    dos 3 jogadores poderá roubar de você, o mesmo para o 2º melhor e o
    3º melhor. Porém roubando o 4º melhor é garantido que você o manterá
    até o final, pois os outros 3 jogadores roubarão até no máximo o 3º
    melhor.
-   Suponha agora que n = 10 e k = 3. É a sua vez, já jogaram 2
    jogadores e ainda há 7 para jogar. Você não pode usar a estratégia
    do exemplo anterior, tendo em vista isso, você roubará o pior
    presente até o momento, no caso o 2º.

**Calcule a probabilidade de você obter um presente melhor com essa
estratégia ao invés de pegar um presente da pilha.**

## 2.1 - Simulação de Monte Carlo

``` r
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
```

## 2.2 - Análise

``` r
M %>% as_tibble() %>%
  filter(N %in% c(10,20,30)) %>%
  mutate(N=factor(N)) %>%
  ggplot(aes(x=K, y=Prob, colour=N, shape=N))+
  geom_point()+
  geom_hline(yintercept = 0.5,colour="red")+
  ylim(c(0,1))
```

![](Analise_files/figure-gfm/completo%20analise-1.png)<!-- -->

Nesse caso as probabilidades dependem também de n. Para um n, as
probabilidades decrescem de 1 até n-k+1, quando a estratégia é trocar
pelo pior. A partir de n-k+1 as probabilidades começam a crescer, pois é
possível tomar o melhor presente que não pode ser tirado de você.
Aumentando o k essa probabilidade aumenta, pois há cada vez menos
jogadores para jogar, logo é possível pegar presentes cada vez melhores.
