## makeCacheMatrix creates a list containing functions
## sets the value of the vector
## gets the value of the vector
## sets the value of the mean
## gets the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## The following calculates the mean of makeCacheMatrix output
## created before. First, checks mean value.
## It calculates the mean of the data and sets the mean value.

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...) ## Return a matrix that is the inverse of 'x'
  x$setmatrix(m)
  m
}