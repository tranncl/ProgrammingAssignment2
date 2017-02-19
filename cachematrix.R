## The function makeCacheMatrix creates a special matrix object 
## This function returns a list containing functions to
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseofx <- NULL               # Local object
  set <- function(y) {
    x <<- y            # Assign to an object in a different environment
    inverseofx <<- NULL   # Initialize the object inverseofx in a different environment
  }
  get <- function() x   # Print the input matrix object
  setinverse<- function(inverse) inverseofx <<-inverse  # Set the inverse of the matrix
  getinverse <- function() inverseofx    # Get the inverse matrix object
  list(set = set, get = get,     # Return a list of functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve returns the inverse of a matrix 'x' created with
## the makeCacheMatrix function.
## If the cached inverse matrix exists then cacheSolve will retrieve it.
## If the cached inverse matrix does not exist (NULL) the cacheSolve will compute the inverse matrix, 
## cache, and return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseofx <- x$getinverse()   # Get the cached inverse matrix
  if (!is.null(inverseofx)) {    # if the cached inverse matrix exists then return it
    message("getting cached data")
    return(inverseofx)
  } else {   # if the cached inverse matrix does not exist
    inverseofx <- solve(x$get())   # Compute the inverse of the input matrix
    x$setinverse(inverseofx)   # Cache the inverse matrix
    return(inverseofx)   # return the inverse matrix
  }
}
