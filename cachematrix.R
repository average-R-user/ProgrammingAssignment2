# Define the special matrix functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # Function to set the matrix
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to set the inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  # Function to get the inverse of the matrix
  getInverse <- function() {
    inv
  }
  
  # Return a list of functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(cache, ...) {
  inv <- cache$getInverse()
  
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  data <- cache$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  cache$setInverse(inv)
  
  inv
}