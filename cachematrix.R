rm(list=ls())
setwd("~/Documents/AAAS_2013_2014/R_Stats/courserastuff/R Programming/ProgrammingAssignment2/")


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function( m = matrix() ) {
  i <- NULL #initialize matrix
  #set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ##get and return the matrix
  get <- function() {
    m
  }
  
  #set the inverse of the matrix
  setinverse <- function(inverse) {
    i <<- inverse
  }
  
  #get and return the inverse of the matrix
  getinverse <- function() {
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## return inverse of x
  m <- x$getInverse()
  
  ## return inverse if it already exists....don't recalculate
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## get matrix from x$get
  data <- x$get()
  
  ## calculate inverse
  m <- solve(data) %*% data
  
  ## set newly calculated inverse to an object and return matrix
  x$setInverse(m)
  
  m
}