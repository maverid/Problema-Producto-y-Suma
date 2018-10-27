## 
# Dr.Producto sabe el producto de los dos número P = x * y
# Dr.Suma sabe la suma de los dos número S=x+y 
# Dr.Producto = "No lo sé" 
# Dr.Suma = "Sabía que no lo sabrías"
# Dr.Producto = "Entonces, ya lo sé"
# Dr.Suma = "Entonces, ya lo sé también"
##

# Como Dr.Producto no lo sabe x,y no son números primos (ambos)
# Como Dr.Suma sabe que los posibles factores de la suma no son números primos.
library("numbers")
ij <- matrix(1:50, 50, 50)
ji <- matrix(1:50, 50, 50, byrow= TRUE)
SUMA <- ij + ji
PRODUCTO <- ij * ji

## sP son todos los posibles valores Suma de elementos que no son números primos.
sumaPrimos <- unique(as.vector (SUMA[isPrime(1:50), isPrime(1:50)]))
sP <- setdiff(c(1:100), sumaPrimos)
sP <- sP[sP>3] #quitamos los valores 1,2 y 3.

#Esta función devuelve en una matriz todos los pares de factores del valor dado
obtenerFactores <- function(valor){
  raiz <- trunc(sqrt(valor))
  for(prov in 2:raiz){
    if(valor%%prov==0){
      if(exists("mtrprod")){
        mtrprod <- rbind(mtrprod, c(prov, valor/prov))
      }else{
        mtrprod <- matrix(c(prov, valor/prov), ncol=2)
      }
    }
  }
  mtrprod <- cbind(mtrprod, apply(mtrprod,1,sum))
  mtrprod <- cbind(mtrprod, (mtrprod[,3] %in% sP))
  colnames(mtrprod) <- c("prod1", "prod2","suma","sP")
  mtrprod
}

##Esta función devuelve en una matriz todos los sumandos posibles de un valor
obtenerSumando <- function(valor){
  medio <- valor / 2
  mtrx <- matrix(c(2:medio,valor-2:medio),ncol=2)
  mtrx <- cbind(mtrx, apply(mtrx,1,prod), c(0))
  colnames(mtrx) <- c("valor1", "valor2", "producto","numPrimos")
  mtrx
}
listaSumandosSp <- lapply(sP, obtenerSumando)


cuantos1Primos <- c(NULL)
for(n in 1:length(listaSumandosSp)){
  for (m in 1:length(listaSumandosSp[[n]][,3])){
    sumaPrim <- sum(obtenerFactores(listaSumandosSp[[n]][,3][m])[,4])
    listaSumandosSp[[n]][m,4] <- sumaPrim
  }
  cuantos1Primos <- c(cuantos1Primos, length(which(listaSumandosSp[[n]][,4] %in% 1)))
}
solucion <- listaSumandosSp[which(cuantos1Primos %in% 1)]
##Con eso obtenemos la solución