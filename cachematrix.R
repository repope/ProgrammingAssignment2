## makeCacheMatrix accepts a matrix and returns a list of getters and setters
## to store/reset the matrix and its inverse matrix.

## cacheSolve accepts a wrapped matrix from makeCacheMatrix and returns
## the inverse of it, caching the value for faster returns on subsequent calls.

## returns some getters and setters to 
## get/set a matrix and it's inverse (via solve fn)
makeCacheMatrix <- function(x = matrix()) {

  #init the var for caching the inverse result
  inverse <- NULL
  
  #return matrix passed
  getMatrix <- function() x
  #call to reset function with new matrix, clearing previously cached inverse, if any
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #return the inverse, possibly null if not cached yet
  getInverse <- function() inverse
  
  #set the value of the inversed matrix
  setInverse <- function(inv) inverse <<- inv
  
  #return the functions
  list(getMatrix = getMatrix
       ,setMatrix = setMatrix
       , getInverse = getInverse
       , setInverse = setInverse)
}


## returns the inverse (via solve) of a square matrix. assumes matrix is squre.
## Will cache and return the value on subsequent calls
cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  inv = x$getInverse()
  #returned the cached value if present
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #calculate and cache inverse before returning it
  mat <- x$getMatrix()
  inv <- solve(mat)
  x$setInverse(inv)
  
  inv
}
