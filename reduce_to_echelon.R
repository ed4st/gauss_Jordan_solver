#following function verifies if a vector is the zero vector
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

#The following function converts the matrix to a row echelon form

reduce_to_echelon = function(m){
  nr = dim(m)[1]
  nc = dim(m)[2]
  for(r in 1:nr){
    #verifying if a row is 0
    if(is_zero(m[r,])){
      
    }
    
  }
}