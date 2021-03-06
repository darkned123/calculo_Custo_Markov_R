######################################
#Compile e execute .CalculoCusto()   #
#Rafael Rani                         #
#Matheus Coimbra                     #
######################################
.CalculoCusto<-function() {
  require(markovchain)
  require(diagram)
  
  
  #Define os estados da matriz
  #P-> Produ��o
  #I-> Inspe��o
  #D-> Despacho
  #R-> Refugo
  Estados <- c("P", "I", "D", "R")
  
  byRow <- TRUE
  
  #Cria��o da matriz
  transMatrix <- matrix(data = c( 
    0,0.99,0,0.01,
    0,0,0.98,0.02,
    0,0,1,0,
    0,0,0,1
  ), byrow = byRow, nrow = 4,
  dimnames = list(Estados, Estados))
  
  #Cria-se um obejto da classe markovchain
  #states = lista os estados de cada probabilidade de transi��o definida
  #byrow= um elemento l�gico, indicando se as probabilidades de transi�ai s�o mostradas por linha ou coluna
  #transitionMatrix= probabilidades da matriz de transi��o
  #name = nome
  DWmc <- new("markovchain", states = Estados, byrow = byRow,
              transitionMatrix = transMatrix, name = "Matriz Producao")
  
  DWmc
  
  #fun��o para visualizar os estados
  plotmat(transMatrix,pos = c(1,3), 
          lwd = 1, box.lwd = 2, 
          cex.txt = 0.8, 
          box.size = 0.1, 
          box.type = "circle", 
          box.prop = 0.5,
          box.col = "light yellow",
          arr.length=.1,
          arr.width=.1,
          self.cex = .4,
          self.shifty = -.01,
          self.shiftx = .13,
          main = "")
  
  # Determina estados transientes
  transientStates(DWmc) 
  
  
  # determina estados absorventes
  absorbingStates(DWmc) 
  
  
  #A forma canonica da matriz de transi��o � particionada na matriz identidade I,
  #uma matriz de 0's, a matriz   Q , cont�m probabilidades de transi��o para os estados transientes e 
  #uma matriz R, contendo as probabilidades de transi��o dos estados absorventes.
  # ENCONTRA MATRIZ Q
  getRQ <- function(M,type="Q"){
    if(length(absorbingStates(M)) == 0) stop("Nao e matriz absorvente")
    tm <- M@transitionMatrix
    d <- diag(tm)
    m <- max(which(d == 1))
    n <- length(d)
    ifelse(type=="Q",
           A <- tm[(m+1):n,(m+1):n],
           A <- tm[(m+1):n,1:m])
    return(A)
  }
  
  #  DWmc Forma Canonica
  P <- canonicForm(DWmc)
  P
  
  Q <- getRQ(P)
  
  
  #finalmente, a inversa de uma matriz n�o singular quadrada
  #Encontra Matriz Fundamental
  #(I - N)-1
  I <- diag(dim(Q)[2])
  N <- solve(I - Q)
  cat("\nEncontra Matriz Fundamental (I - N)-1\n")
  print(N)
  
  
  # Calculo do tempo de absor��o
  c <- rep(1,dim(N)[2])
  u <- N %*% c
  u
  
  #(I - N) -1. A
  #Probabilidade de absor��o dos estados absorventes
  R <- getRQ(P,Q)
  B <- N %*% R
  cat("\nProbabilidade de absor��o dos estados absorventes\n")
  print(B)
  
  #para mostrar apenas 4 digitos
  options(digits = 4)
  
  #F�RMULA DE CUSTO: C = (1/pa)*(Mp + np*P + ni*I + D - Pr*R)
  #Pa - probabilidade de absor��o para o despacho;
  #Mp - custo com mat�ria prima;
  #Np - n�mero esperado de vezes que passa produ��o;
  #P - custo de produ��o;
  #nI - n�mero esperado de vezes que passa pela inspe��o;
  #I - custo com inspe��o;
  #D - custo com despacho;
  #PR - probabilidade de ser absorvido para o estado refugo;
  #R - custo com refugo.
  
  #CUSTO CASA 01
  C <- (1/B[2,1])*(35+(N[1,1]*30) + (N[2,2]*1) + 1 - B[2,2]*20)
  cat("\nCasa 01: R$",C)
  
  #CUSTO CASA 02 (muda apenas valor do custo de produ��o apenas)
  C1 <- (1/B[2,1])*(35+(N[1,1]*33) + (N[2,2]*1) + 1 - B[2,2]*20)
  cat("\nCasa 02: R$",C1)
  
  
  cat("\n\nPressione qualquer tecla Caso deseja botar os valores Custo com materia-prima, de produ��o e refugo de forma dinamica.\n") 
  cat("Caso Contr�rio, digite 'sair'.\n") 
  escolha <- readLines(n = 1) 
  
  if(escolha == "sair"){
    cat("\nObrigado!")
  }else{
    
    cat("\nInforme a quantidade de custo de producoes diferentes\n")
    qtdProdcao <- readLines(n = 1) 
    
    for(i in 1:qtdProdcao){
    cat("\n\nCasa ",i)
    cat("\nInforme o valor do custo com mat�ria-prima\n")
    mp <- readLines(n = 1) 
    
    cat("Informe o valor do custo de producao\n")
    p <- readLines(n = 1) 
    
    cat("Informe o valor do custo com refugo\n")
    r <- readLines(n = 1) 
    
    options(digits = 4)
    
    
    C <- (1/B[2,1])*(as.double(mp)+(N[1,1]*as.double(p)) + (N[2,2]*1) + 1 - B[2,2]*as.double(r))
    cat("\nCasa ",i)
    cat(" R$",C)}
  }
  
}
