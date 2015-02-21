## These functions help return the inverse of a matrix faster. 
## By caching the inverse on its first calculation, time-consuming repetition is avoided. 

## makeCacheMatrix creates a list of methods to set and return the matrix and its inverse
## The inverse is NULL until calculated and cached.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(mat){
    x<<-mat
    inv<-NULL
    
  }
  get<-function() x
  setInv<-function(y) inv<<-y
  getInv<-function() inv
  list(set=set, get=get, setInverse=setInv, getInverse=getInv)
}


## cacheSolve returns the cached inverse of the matrix if there is one.
## If not, the inverse is calculated for the first time and cached for reuse. 

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
