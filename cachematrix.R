## Put comments here that give an overall description of what your
## functions do
##the combination of these functions caches the inverse of a matrix as opposed to computing it repeatedly

 

## Write a short comment describing this function
##makeCacheMatrix Function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<-y
    inv<<- NULL
  }
  get<- function()x
  setinverse<- function(inverse) inv<<- inverse
  getinverse<- function(inv)
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
##cachesolve function computes the inverse of the special matrix returned by makeCacheMatrix- if the inverse was already calculated, it retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
}