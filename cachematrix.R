## programming asignment for Coursera course "R programming"
## by Konstantin Kononenko (Moscow, Russia)
## date: 27.09.2015
## version: 02

## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly 
## (there are also alternatives to matrix inversion that we will not discuss here). 
## The assignment is to write a pair of functions that cache the inverse of a matrix.

##  the function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # in current scope
  cached_matrix <- NULL
  
  # setter
  set <- function(y) {
    x <<-  y
    # in 'different' scope
    cached_matrix <<-  NULL
  }
  
  # getter
  get <- function() x
  
  # store result of inverse into cache
  setInverse <- function(solve) cached_matrix <<- solve
  
  # get the cached result
  getInverse <- function() cached_matrix
  
  # mapping
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(y, ...) {
  # get the cached result
  # using getInverse() method of y obj
  inversed_matrix <- y$getInverse()
  # if cache is there, return cached result and tell this
  if(!is.null(inversed_matrix)) {
    message("there is result stored in cache. Getting it for you")
    return (inversed_matrix)
  } 
  # if there is no cached result
  # make inverse matrix and store it in the cache
  x <- y$get()
  inversed_matrix <- solve(x)
  y$setInverse(inversed_matrix)
  
  inversed_matrix
  
}
