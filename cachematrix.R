##Author: Francisco J. Álvarez Montero
##In order to test this code follow the following steps:
##1.Create an invertible matrix: m <- matrix(c(0, 2, 1, 0), nrow = 2, ncol = 2, byrow = TRUE)
##2.Create a list through the 'makeCacheMatrix' function and pass the created matrix as an argument: lst<-makeCacheMatrix(m)
##3.call the 'cacheSolve' function: cacheSolve(m)
##4.call again the 'cacheSolve' function: cacheSolve(m)

## This function takes a matrix as an argument.
## It creates and returns a list of functions: set,get,setinverse,getinverse
makeCacheMatrix <- function(x = matrix()) {
     
     mtrx <- NULL
     ##Set of functions to be returned as a list     
     set <- function(y=matrix()) {
          x <<- y
          mtrx <<- NULL
     }
     get <- function() x
     setinverse <- function(invrs=matrix()) mtrx <<- invrs
     getinverse <- function() mtrx
     ##Create and return a list with the precedent functions
     list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

## This function takes a list as an argument and computes the 
## inverse of the matrix which is passed as argument to function 'makeCacheMatrix'.
cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inversematrix <- x$getinverse()
     if(!is.null(inversematrix)) {
          message("getting cached data")
          return(inversematrix)
     }
     originalmatrix <- x$get()
     ##The 'solve' function calculates the inverse of a matrix
     inversematrix <- solve(originalmatrix, ...)
     x$setinverse(inversematrix)
     inversematrix     
}
