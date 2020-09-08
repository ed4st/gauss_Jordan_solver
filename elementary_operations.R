#punto 1

#Programe una función en r que reciba de entrada por parte del usuario el
#tamaño de una matriz y las entradas de la misma en orden de izquierda a derecha,
#de arriba a abajo. Luego, debe ir preguntando que operación elemental debe hacerse,
# y una vez que el usuario indique precisamente cuál, diciendo exactamente que renglones
#y números estarán involucrados, debe mostrarle al usuario la matriz resultante.  Cuando
#el usuario llegue a una forma escalonada, deberá decirle al usuario, mostrarle la matriz
#final y terminar el programa. Usted debe especificar al usuario cómo y en que orden debe
#introducir los valores y hacer las validaciones correspondientes.

loadMatrix = function(){
  s = readline("Enter the size of the mxn matrix separated by coma (,):")
  sNum = strtoi(strsplit(s,",")[[1]]) #here we split and parse 
                                      #the input data to integer
  m = sNum[1]
  n = sNum[2]
  print(m)
  print(n)
  v = c()
  print("Enter the data by rows, separated by coma (,)")
  for (i in 1:m) {
    vS = readline(paste("Row ", i, ": ", sep = ""))
    v = c(v,strtoi(strsplit(vS, ",")[[1]])) #here we split the row data
                                            #and parse the input data to integer
  }
  #matrix creation by using the matrix data structure
  mat = matrix(v, nrow = m, byrow = T)
  return(mat)
}

#following matrix computes the type 1 elemental row operation
elemental_1 = function(m, i, j){
  aux = m[i,]
  m[i,] = m[j,]
  m[j,] = aux
  return(m)
}

#following matrix computes the type 2 elemental row operation
elemental_2 = function(m, i, a){
  m[i,] = a*m[i,] 
  return(m)
}

#following matrix computes the type 3 elemental row operation
elemental_3 = function(m, i, j, b){
  m[i,] = m[i,] + b*m[j,]
  return(m)
}


m = matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, byrow = T)
elemental_3(m,3,1,-7)


loadMatrix()

rm(list = ls())
