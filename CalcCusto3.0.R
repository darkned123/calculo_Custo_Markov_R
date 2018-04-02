######################################
#Compile e execute .CalculoCusto()   #
#Rafael Rani                         #
#Matheus Coimbra                     #
######################################
.CalculoCusto<-function() {
  require(markovchain)
  require(diagram)
  
  
  #Define os estados da matriz
  #P-> Produção
  #I-> Inspeção
  #D-> Despacho
  #R-> Refugo
  Estados <- c("P", "I", "D", "R")
  
  byRow <- TRUE
  
  #Criação da matriz
  transMatrix <- matrix(data = c( 
    0,0.99,0,0.01,
    0,0,0.98,0.02,
    0,0,1,0,
    0,0,0,1
  ), byrow = byRow, nrow = 4,
  dimnames = list(Estados, Estados))
  
  #Cria-se um obejto da classe markovchain
  #states = lista os estados de cada probabilidade de transição definida
  #byrow= um elemento lógico, indicando se as probabilidades de transiçai são mostradas por linha ou coluna
  #transitionMatrix= probabilidades da matriz de transição
  #name = nome
  DWmc <- new("markovchain", states = Estados, byrow = byRow,
              transitionMatrix = transMatrix, name = "Matriz Producao")
  
  DWmc
  
  #função para visualizar os estados
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
  
  
  #A forma canonica da matriz de transição é particionada na matriz identidade I,
  #uma matriz de 0's, a matriz   Q , contém probabilidades de transição para os estados transientes e 
  #uma matriz R, contendo as probabilidades de transição dos estados absorventes.
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
  
  
  #finalmente, a inversa de uma matriz não singular quadrada
  #Encontra Matriz Fundamental
  #(I - N)-1
  I <- diag(dim(Q)[2])
  N <- solve(I - Q)
  cat("\nEncontra Matriz Fundamental (I - N)-1\n")
  print(N)
  
  
  # Calculo do tempo de absorção
  c <- rep(1,dim(N)[2])
  u <- N %*% c
  u
  
  #(I - N) -1. A
  #Probabilidade de absorção dos estados absorventes
  R <- getRQ(P,Q)
  B <- N %*% R
  cat("\nProbabilidade de absorção dos estados absorventes\n")
  print(B)
  
  #para mostrar apenas 4 digitos
  options(digits = 4)
  
  #FÓRMULA DE CUSTO: C = (1/pa)*(Mp + np*P + ni*I + D - Pr*R)
  #Pa - probabilidade de absorção para o despacho;
  #Mp - custo com matéria prima;
  #Np - número esperado de vezes que passa produção;
  #P - custo de produção;
  #nI - número esperado de vezes que passa pela inspeção;
  #I - custo com inspeção;
  #D - custo com despacho;
  #PR - probabilidade de ser absorvido para o estado refugo;
  #R - custo com refugo.
  
  #CUSTO CASA 01
  C <- (1/B[2,1])*(35+(N[1,1]*30) + (N[2,2]*1) + 1 - B[2,2]*20)
  cat("\nCasa 01: R$",C)
  
  #CUSTO CASA 02 (muda apenas valor do custo de produção apenas)
  C1 <- (1/B[2,1])*(35+(N[1,1]*33) + (N[2,2]*1) + 1 - B[2,2]*20)
  cat("\nCasa 02: R$",C1)
  
  
  cat("\n\nPressione qualquer tecla Caso deseja botar os valores Custo com materia-prima, de produção e refugo de forma dinamica.\n") 
  cat("Caso Contrário, digite 'sair'.\n") 
  escolha <- readLines(n = 1) 
  
  if(escolha == "sair"){
    cat("\nObrigado!")
  }else{
    
    cat("\nInforme a quantidade de custo de producoes diferentes\n")
    qtdProdcao <- readLines(n = 1) 
    
    for(i in 1:qtdProdcao){
    cat("\n\nCasa ",i)
    cat("\nInforme o valor do custo com matéria-prima\n")
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
