#Katherine Glover
#July 23, 2015
#coursera.com Assignment #2

## Fxn makeCacheMatrix accepts and stores a matrix. 
## "setinverse" and "getinverse" are also defined,
## but do not run/compute until called.


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function () x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve first checks to see if "m" already has stored values; 
## will skip re-computation if the matrix inverse already exists and retrieve it.
## If no previously-stored values for m, it gets the orig matrix 
## and calls $getinverse to compute the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

