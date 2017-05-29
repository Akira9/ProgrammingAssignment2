## These functions speed up matrix inversion by caching previous inversion results. 
## Caching relies on the <<- operator which allows variable assignment in the parent frame

## makeCacheMatrix creates an object to store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  setM<-function(mat){
    x<<-mat
    inv<-NULL
  }
  getM<-function() x
  setInv<-function(y) inv<<-y
  getInv<-function() inv
  list(set=setM, get=getM, setInverse=setInv, getInverse=getInv)
}

## cacheSolve returns the cached inverse of the matrix if it has already been stored.
## If it hasn't, the inverse is calculated for the first time and cached. 

cacheSolve <- function(x, ...) {
  inverse<-x$getInverse()
  if(!is.null(inverse)){
    message("Getting cached inverse of the matrix")
    return (inverse)
  }
  data<-x$get()
  inverse<-solve(data)
  x$setInverse(inverse)
  inverse
}
