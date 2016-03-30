## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  
  i <- NULL
  #function (stored as setmatrix) to set the matrix 
  setmatrix <- function(y) 
    {
    x <<- y
    i <<- NULL
    }
  
  #function (stored as getmatrix) to get the matrix 
  getmatrix <- function() x
  
  #function (stored as setinverse) to invert the matrix 
  setinverse <- function(solve) i <<- solve
  
  #function(stored as getinverse) to get the inverted matrix
  getinverse <- function() i
  
  #making a list to store the new function content 
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


#The cacheSolve takes makeCacheMatrix as an input and either returns the cached inversion 
#of the matrix or performs the inversion of the matrix (using makeCacheMatrix functions) 
#if the inverted matrix is not available or the matrix has changed 

cacheSolve <- function(x, ...) 
  {
  
  #passes inverted matrix that is the inverse of original function [x]   
  
  i <- x$getinverse()
  
  #checking the inverted matrix has been cached or not,if its cached then use the cached version 
    if(!is.null(i)) 
      {
      message("Getting the inverted matrix")
      return(i)
      }
  
  #if no inverted matrix found in cache, return the matrix to invert
  matrix_data <- x$getmatrix()
  
  #invertion and setting inversion
      i <- solve(matrix_data)
      x$setinverse(i)
      i
  
}
