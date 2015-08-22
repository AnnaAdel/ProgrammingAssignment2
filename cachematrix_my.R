#makeCacheMatrix is a function that creates a special "matrix" object
#that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #set the value of the inverse
  #get the value of the inverse
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#function returns the inverse of the matrix
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  #function checks to see if the mean has already been calculated
  if(!is.null(m)) {
    # If so, it gets the mean from the cache and skips the computation
    message("getting cached data")
    return(m)
  }
  #Otherwise, it calculates the inverse 
  data <- x$get()
  m <- solve(data, ...)
  #and sets the inverse matrix in the cache via the setInverse function
  x$setInverse(m)
  m
}
r = rnorm(1:9)
matr = matrix(r, nrow=3, ncol=3)
matr
test <- makeCacheMatrix(matr)
cacheSolve(test)
