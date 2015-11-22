##
## This file has two functions that create a matrix object, solve, and cache/retrieve result.
##

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## set the solution to NULL for any new object or changed object
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## return the matrix
  get <- function() x
  ## solve the matrix
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  ## list function available for the object
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  ## does the solution already exist?
  if(!is.null(m)) {
    ## Yes, return the cached solution
    message("getting cached data")
    return(m)
  }
  ## No, solve the matrix
  data <- x$get()
  print("solving matrix")
  m <- solve(data)
  ## cache the solution
  x$setsolve(m)
  m
}
