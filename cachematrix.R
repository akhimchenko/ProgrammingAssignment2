## Sorry, no comments here

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() {x}
  setInverse <- function(imatrix) i <<- imatrix
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}

# uncomment lines below to check the functions
#m <- matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), nrow = 3, ncol = 3)
#cachedMatrix <- makeCacheMatrix(m)
#cacheSolve(cachedMatrix)
#cacheSolve(cachedMatrix) # second call shall return cached data