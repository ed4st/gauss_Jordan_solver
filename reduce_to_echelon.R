#-----------------------------------2---------------------------------------
#following function verifies if a vector is the zero vector
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


is_zero = function(c){
  for(num in c){
    if(num != 0){
      return(FALSE)
    }
  }
  return(TRUE)
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


first_nonzero_index = function(row){
  return(min(which(row != 0)))
}
#The following function converts the matrix to a row echelon form

reduce_to_echelon = function(m){
  lead = 1
  nr = dim(m)[1]
  nc = dim(m)[2]
  
  while( lead < nr){
    for (r in (lead + 1):nr) {
       a = -m[r,first_nonzero_index(m[r,])]/m[lead,first_nonzero_index(m[lead,])]
       m = elementary_3(m, r, lead,a)
    }
    lead = lead + 1
  }
  return(m)
}

main_function2 = function(){
  m = loadMatrix()
  prmatrix(reduce_to_echelon(m))
}

main_function2()

