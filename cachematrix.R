#makeCacheMatrix is a function that creates a list of functions, that can be used to invert a matrix.

#Instructions:
#Use a square matrix as input: for example "x <- matrix(stats::rnorm(16), 4, 4)".
#Run MakeCacheMatrix() and assign it to an object. For example: "d <- MakeCacheMatrix".
#Run "d$setinverse(x)" to invert the matrix and assign the inverted matrix to object m which is subsequently returned.
#Inversion ca be reviewed with "d$getinverse()".
#Repeat the above without inversion to create list of functions in an environment that contains empty object m, and assign
#the list to another object.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(toinvert) {inverse <- solve(toinvert)
  m <<- inverse}
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Calculates inverse of matrix unless it can find the cached inverse. In that case If will return the cached inverse.

#Instructions:
#First run cacheSolve(d), since inverted matrix was created in list d, and getinverse() does not return null, function
#will print "getting cached data", and return m. If one runs list z from first function example getinverse() will return null
# and function proceeds to perform inverse, and finally return inverse matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m))  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
