## Put comments here that give an overall description of what your
## functions do

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
  ## Initializes to null
  s <- NULL
  ## Set the value of the matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## Set the value of the matrix
  get <- function() x
  ## Set the value of the inverted matrix
  setSolve <- function(solve) s <<- solve
  ## Set the value of the inverted matrix
  getSolve <- function() s
  ## List of functions
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}
