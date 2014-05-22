## Functions makeCacheMatrix and cacheSolve calculate
## and cache the inverse of a matrix. 
##
## Sample Usage:
##  > mat <- matrix(1:4, nrow = 2, ncol = 2)
##  > cm  <- makeCacheMatrix(mat)
##  > inv <- cacheSolve(cm)
##  > mat %*% inv
##        [,1] [,2]
## [1,]    1    0
## [2,]    0    1


## makeCacheMatrix creates a list of functions that can store a
## matrix, and calculate and cache its inverse

makeCacheMatrix <- function(m = matrix()) {
  inverse <- NULL            # Creates a variable to hold the inverse matrix
  
  set <- function(newx) {    # Sets the matrix and clears the inverse
    m <<- newx
    inverse <<- NULL
  }
  
  get <- function() {        # Retrieves the cached matrix
    m
  }
  
  setinverse <- function(newinverse) {
    inverse <<- newinverse
  }
  
  getinverse <- function() {
    inverse
  }
  
  list(set = set,            # Return the list of functions
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve uses the function list created by function makeCacheMatrix
## to return a given inverted matrix. 
## If the inverse is already cached, then the cached value is used; otherwise
## the inverse is computed and cached.

cacheSolve <- function(cm, ...) {       # cm is the CacheMatrix object
  
  inverse <- cm$getinverse()
  if(!is.null(inverse)) {          
    message("getting cached data")
    return(inverse)
  }
  
  data <- cm$get()
  newinverse <- solve(data, ...)
  cm$setinverse(newinverse)             # cache the inverse before returning
  newinverse
}
