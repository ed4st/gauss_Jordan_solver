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

#following function performs the type 1 elementary row operation
elementary_1 = function(m, i, j){
  aux = m[i,]
  m[i,] = m[j,]
  m[j,] = aux
  return(m)
}

#following function performs the type 2 elementary row operation
elementary_2 = function(m, i, a){
  m[i,] = a*m[i,] 
  return(m)
}

#following function performs the type 3 elementary row operation
elementary_3 = function(m, i, j, b){
  m[i,] = m[i,] + b*m[j,]
  return(m)
}

#following function returns the question: 
#Is the matrix m in Echelon form?
is_echelon_form = function(mat){
  #number of rows
  m = dim(mat)[1]
  #number of columns
  n = dim(mat)[2]
  #following arrays save the (i,j) position of first nonzero entry
  #in backward column
  pivots_i = c()
  pivots_j = c()
  for (j in 1:n){
    if(!is_zero(mat[,j])){
      pivots_j = c(pivots_j, j)
      pivots_i = c(pivots_i, rowpivot(mat[,j]))
    }
  }
  #veryfing that row pivot positions are increasing
  if(length(pivots_i) > 0){
    for (index in 1:max(pivots_i)) {
      if(!(index %in% pivots_i[1:match(max(pivots_i),pivots_i)])){
        return(FALSE)
      }
    }
    return(!is.unsorted(pivots_i[1:match(max(pivots_i),pivots_i)]))
  }
  if(is.null(pivots_i)){
    return(TRUE)
  }
  return(FALSE)
}
m = matrix(c(0,0,0,0,0,0,0,0,0,0,0,0), nrow = 3, byrow = T)
m
echelon_form(m)

is_almost_increasing = function(pivots_i){
  
}

#following function verifies if a vector is the zero vector
is_zero = function(c){
  for(num in c){
    if(num != 0){
      return(FALSE)
    }
  }
  return(TRUE)
}


#following function returns recursively the index
#where the pivot is located in a nonzero column
#cl is a column
rowpivot = function (cl){
  len = length(cl)
  if(cl[len] != 0 | len == 0){
    return(len)
  }
  else{
    return(rowpivot(cl[1:(len-1)]))
  }
}


m = matrix(c(1,2,3,4,0,6,0,8,0,0,0,0), nrow = 3, byrow = T)
m
echelon_form(m)

dim(m)[1]
elemental_3(m,3,1,-7)


loadMatrix()

rm(list = ls())
