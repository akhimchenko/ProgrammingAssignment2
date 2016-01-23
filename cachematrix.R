## A pair of functions to store inverted matrix

## Object that stores matrix and its inversion
## Returns a list of functions
makeCacheMatrix <- function(x = matrix()) {
  # init inverted with empty value
  i <- NULL
  # matrix setter function
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # matrix getter function
  get <- function() {x}
  # inverted matrix setter function
  setInverse <- function(imatrix) i <<- imatrix
  # inverted matrix getter function
  getInverse <- function() i
  # return a functions list
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Object that computes (if necessary) and saves inverted matrix
## Returns inverted matrix
cacheSolve <- function(x, ...) {
  # get inverted value for the given matrix
  i <- x$getInverse()
  # Found? Return it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # Not found? Calculate and store
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  # And return, of course
  i
}

# code to check if it works
#m <- matrix(c(1, 2, 3, 0, 1, 4, 5, 6, 0), nrow = 3, ncol = 3)
#cachedMatrix <- makeCacheMatrix(m)
#cacheSolve(cachedMatrix)
#cacheSolve(cachedMatrix) # second call shall return cached data