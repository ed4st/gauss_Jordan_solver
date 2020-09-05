#punto 1

#Programe una función en r que reciba de entrada por parte del usuario el
#tamaño de una matriz y las entradas de la misma en orden de izquierda a derecha,
#de arriba a abajo. Luego, debe ir preguntando que operación elemental debe hacerse,
# y una vez que el usuario indique precisamente cuál, diciendo exactamente que renglones
#y números estarán involucrados, debe mostrarle al usuario la matriz resultante.  Cuando
#el usuario llegue a una forma escalonada, deberá decirle al usuario, mostrarle la matriz
#final y terminar el programa. Usted debe especificar al usuario cómo y en que orden debe
#introducir los valores y hacer las validaciones correspondientes.

cargarMatriz = function(){
  nS <- readline("Ingrese el tamaño de la matriz:")
  n <- strtoi(nS)
  v = c()
  print("Ingrese la matriz por filas, separando las entradas con comas (,)")
  for (i in 1:n) {
    vS = readline(paste("Fila ", i, ": ", sep = ""))
    v = c(v,strtoi(strsplit(vS, ",")[[1]]))
  }
  matriz = matrix(v, nrow = n, byrow = T)
  return(matriz)
}


elemental_1 = function(matriz, i, j){
  aux = matriz[i,]
  matriz[i,] = matriz[j,]
  matriz[j,] = aux
  return(matriz)
}

elemental_2 = function(matriz, i, a){
  matriz[i,] = a*matriz[i,] 
  return(matriz)
}

elemental_3 = function(matriz, i, j, b){
  matriz[i,] = matriz[i,] + b*matriz[j,]
  return(matriz)
}


m = matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, byrow = T)
elemental_3(m,3,1,-7)


cargarMatriz()

rm(list = ls())
