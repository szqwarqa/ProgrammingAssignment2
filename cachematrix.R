## Computing the inverse of a square matrix can be
## done with the solve function in R

## Creating a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Inverting the matrix in case it haven't been inverted before

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    return(m)
  }
  compute <- x$get()
  m <- solve(compute, ...)
  x$setinverse(m)
  m
}