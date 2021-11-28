generateMatrix <- function(x = matrix()) {
  if ((dim(x)[1]==dim(x)[2])&det(x)!=0){
  print("inverse matrix exists")
  
  inv <- NULL
  
  setMatrix <- function(y)  x <<- y; inv <<- NULL
  getMatrix <- function() x
  setInvMatrix <- function(invM) inv <<- invM
  getInvMatrix <- function() inv
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
  }
  
  else{
    print("inverse matrix does not exist")
  }
}


cacheInverseMatrix <- function(x, ...) {
  
  invM <- x$getInvMatrix()
  
  if(!is.null(invM)) return(invM)
  
  matr <- x$getMatrix()
  invM <- solve(matr)
  x$setInvMatrix(invM)
  invM
}
