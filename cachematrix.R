## Task : Function below creates a special "matrix" object that can cache its inverse.
## Author: Ash Pandey
## Date: 29/01/2021

## below function takes the argument matrix and assuming matrix we supply is Invertible
makeCacheMatrix <- function(x = matrix()) {
  invTemp <- NULL
  ## Setting value of the matrix using another function
  set <- function(y){
    x <<- y
    invTemp <<- NULL
  }
  ## Getting the value of the Matrix
  get <- function(){
    x
  }
  ## Setting the value of the Inverse
  setInverse <- function(inverse){
    invTemp <<- inverse
  } 
  ## Getting the value of the Inverse
  getInverse <- function(){
    invTemp 
  }
  ## Creating a list 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Below function computes the inverse of the matrix 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invTemp <- x$getInverse()
  ## Check if the inverse has been calculated
  if(!is.null(invTemp)){
    message("this is cached data")
    return(invTemp)
  }
  
  mat <- x$get()
  ## Calculate Inverse of the matrix 
  invTemp <- solve(mat,...)
  ## Set value of Inverse in the Cache
  x$setInverse(invTemp)
  invTemp
}
