## Considering that matrix inversion is quite a costly computation, 
## there may be some benefit to caching the inverse of a matrix. 
## This saves you from having to compute it over and over again.

## In my code below, I will demonstrate how to write a pair of functions
## that cache the inverse of a matrix.

## The makeCacheMatrix function does the following four actions:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse
## 4. gets the value of the inverse

makeCacheMatrix <- function(x = matrix()){
  
  m <- NULL
  
  ## 1. set the value of the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  ## 2. get the value of the matrix
  get <- function() x
  
  ## 3. set the value of the inverse
  setinverse <- function(solve) m <<- solve
  
  ## 4. get the value of the inverse
  getinverse <- function() m
  
  ## return a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by the 
## makeCacheMatrix function above.

## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve function should retrieve the inverse from the cache.
## The computation will be skipped in this instance.

## If the inverse has not already been calculated, 
## then the cacheSolve function calculates the inverse of the matrix.
## The value of the matrix will be set in the cache.

cacheSolve <- function(x, ...){
  
  ## Retrieve the inverse from the cache
  m <- x$getinverse()
  
  ## if we have something (m is not null), return a message
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## if the inverse has not already been calculated, output a matrix that is the inverse of 'x'
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
