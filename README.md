## Together, these functions create a cache for the inverse of a matrix, so that 
## when computing the inverse of the matrix, if it's already in the cache, it 
## will be retrieved instead of recalculated. 

## This function makes a matrix object that can cache its inverse. 

makeCacheMatrix<- function(x = matrix()){
  inv<- NULL
  set<- function(y){
    x<<- y
    inv<<- NULL
  }
  get<- function() {x}
  setInverse<- function(inverse) {inv<<- inverse}
  getInverse<- function() {inv}
  list(set=set, get=get, setInverse= setInverse, getInverse=getInverse)
}

## This function computes the inverse of the matrix returned by the
## previous function, such that if the inverse has already been calculated,
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve<- function(x, ...){
  inv<- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<- x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
}








