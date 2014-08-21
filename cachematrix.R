## Assignment: Caching the Inverse of a Matrix

## Creates a special 'matrix' object that can cache its inverse.
## Assumes that 'x' is always invertible.

makeCacheMatrix <- function(x = matrix()) {

  # Store the Matrix Inverse
  ivr <- NULL
  
  # Setter for matrix
  set <- function(y) {
    x <<- y
    ivr <<- NULL
  }
  
  # Getter for the data
  get <- function() x

  # Setter for solve
  setsolve <- function(slv) ivr <<- slv

  # Getter for solve
  getsolve <- function() ivr
  
  # List of methods 
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix
## above.
## Return a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {

  # Try get cached data
  ivr <- x$getsolve()

  # Is the cached data available ?
  if(!is.null(ivr)) {
    message("getting cached data")
    return(ivr)
  }
  
  # Get matrix data  
  data <- x$get()
  
  # Calculate inverse matrix
  ivr <- solve(data, ...)
  
  # Cache inverse matrix
  x$setsolve(ivr)
  
  # Return inverse matrix
  ivr
}

