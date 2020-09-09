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


m = matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3, byrow = T)
m
m = elementary_3(m, 2, 1 , -4)
elementary_3(m, 3, 1 , -7)
reduce_to_echelon(m)
