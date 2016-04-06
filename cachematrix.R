#MakeCacheMatrix makes a list of functions used to invert and store matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(toinvert) {inverse <- solve(toinvert) %*% toinvert
  m <<- inverse}
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculates inverse of matrix unless it can find the cached inverse. In that case If will return the cached inverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m))  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) %*% data
  x$setinverse(m)
  m
}
