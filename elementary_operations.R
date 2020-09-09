
#----------------------1----------------------------------------------
loadMatrix = function(){
  s = readline("Enter the size of the mxn matrix separated by coma (,):")
  sNum = strtoi(strsplit(s,",")[[1]]) #here we split and parse 
                                      #the input data to integer
  m = sNum[1]
  n = sNum[2]
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




main_function = function(){
  m = loadMatrix()
  repeat{
    
    print("1. Intercambiar filas.")
    print("2. Multiplicar una fila por un escalar distinto de 0.")
    print("3. Remplazar una fila por un múltiplo escalar de otra fila.")
    print("4. Salir")
    entry = strtoi(readline("¿Qué operación quieres realizar?:"))
    if(entry == 4){
      break
    }
    else if(entry == 1){
      s = readline("ingresa las filas a intercambiar, separado por coma:")
      sInt = strtoi(strsplit(s,",")[[1]])
      i = sInt[1]
      j = sInt[2]
      print("¡operación realizada!")
      m = elementary_1(m,i,j)
      prmatrix(m)
      if(is_echelon_form(m)){
        print("La matriz se redujo a forma escalonada:")
        prmatrix(m)
        break
      }
    }
    else if(entry == 2){
      s = readline("ingresa la fila  y el escalar, separado por coma:")
      sInt = strtoi(strsplit(s,",")[[1]])
      i = sInt[1]
      a = sInt[2]
      print("¡operación realizada!")
      m = elementary_2(m,i,a)
      prmatrix(m)
      if(is_echelon_form(m)){
        print("La matriz se redujo a forma escalonada:")
        prmatrix(m)
        break
      }
    }

    if(entry == 3){
      s = readline("ingresa la fila a la que sumarás,
                   la fila que sumarás y el múltiplo escalar de esta última,
                   separado por coma:")
      sInt = strtoi(strsplit(s,",")[[1]])
      i = sInt[1]
      j = sInt[2]
      a = sInt[3]
      print("¡operación realizada!")
      m = elementary_3(m,i,j,a)
      prmatrix(m)
      if(is_echelon_form(m)){
        print("La matriz se redujo a forma escalonada:")
        prmatrix(m)
        break
      }
    }
  }
  
  
}

main_function()
