## Put comments here that give an overall description of what your
## functions do
## These functions are designed to cache the inverse of a matrix. 
## The `makeCacheMatrix` function creates a special "matrix" object that can store 
## a matrix and its inverse. The `cacheSolve` function computes the inverse of the 
## matrix, caching the result so that if the inverse is requested again, it can 
## be retrieved without recomputing it.

## Write a short comment describing this function

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It includes functions to set and get the matrix, as well as to set and get the
## inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the variable that will store the inverse
  
  set <- function(y) {
    x <<- y  # Assign the input matrix to the variable x in the parent environment
    inv <<- NULL  # Clear the cached inverse when the matrix is updated
  }
  
  get <- function() x  # Return the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
  
  getInverse <- function() inv  # Return the cached inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  # Return a list of functions that can set/get the matrix and set/get the inverse
}
## Write a short comment describing this function

## cacheSolve computes the inverse of the special "matrix" created by 
## makeCacheMatrix. If the inverse has already been calculated and 
## the matrix has not changed, then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if the inverse is already cached
  if (!is.null(inv)) {  # If the inverse is cached, return it
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()  # Otherwise, get the matrix from the special object
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse for future use
  inv  # Return the computed inverse
}
