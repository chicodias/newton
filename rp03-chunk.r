install.packages("pracma") 
install.packages("spuRs")

library(spuRs)

>>ftn=function(x) log(x)-x+2
>>bisection(ftn,3,4,10^-5)
  
####Método de Newton  
>>ftn2 <- function(x) {
    fx <- log(x)-x+2
    dfx <- 1/x - 1
    return(c(fx, dfx))
  } 
  
>>newtonraphson(ftn2,3.3,10^-5)

####Função que exibe a solução graficamente
>>newtonraphson_show(ftn2,3.3,10^-5)

###Métodos de Bissecção e Newton-Raphson no \\spuRs

>>library(pracma)

>>f <- function(x) log(x)-x+2 

####Método de Newton
>>newton(f, 1.0) 

###Método das Secantes
>>secant(f, 0.9, 1)
  

##Newton-Rapson em problemas envolvendo funções
 ````{r} 
 f <- quote(sin(x)-x^2/16)        # expressão da função 
 fx0 <- function(x){ eval(f) }    # função 
 curve(fx0, -10, 10); abline(h=0) # gráfico da função 
  
 f1 <- D(f,"x")                   # expressão da derivada 
 fx1 <- function(x){ eval(f1) }   # função 
 par(new=TRUE); curve(fx1, -10, 10, col=2, axes=FALSE) 
 ```` 

### Trajetória até a obtenção da raíz

  i <- 1       # valor inicial para o passo 
  dif <- 100   # valor inicial para a diferença entre duas avaliações 
  tol <- 10^-9 # diferência mínima entre duas avaliações (tolerância) 
  dif <- 100   # número máximo de passos 
  x <- 10      # valor inicial para a raiz 
   
  while(i<100 & dif>tol){ 
    x[i+1] <- x[i]-fx0(x[i])/fx1(x[i]) 
    dif <- abs(x[i+1]-x[i]) 
    i <- i+1 
  } 
   
  curve(fx0, -10, 10) 
  for(j in 2:i){ 
    arrows(x[j-1], fx0(x[j-1]), x[j], fx0(x[j]), length=0.1, col=3) 
    Sys.sleep(0.5) 
  } 
  abline(v=x[i], h=fx0(x[i]), col=2) 


### Pontos de máximo de uma função com newton-raphson

  f <- quote(x*(2+0.5*x)^(-4))     # expressão da função, queremos obter MÁXIMO 
  fx0 <- function(x){ eval(f) }    # função 
  curve(fx0, -100, 10) 
   
  f1 <- D(f,"x")                   # expressão da derivada, obteremos o zero 
  fx1 <- function(x){ eval(f1) }   # função 
  curve(fx1, -10, 10, col=2)         # gráfico 
   
  f2 <- D(f1, "x") 
  fx2 <- function(x){ eval(f2) }   # função 
   