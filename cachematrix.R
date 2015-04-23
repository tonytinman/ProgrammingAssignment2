## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes a value 'x' which, for the purpose of this assignment,
## is expected to be a square matrix that can be inversed. If 'x' is not specified
## an empty matrix is set as the default.
## The function returns a list of functions which provide methods of setting and 
## retrieving the matrix values, and caching and retrieving the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set = set,get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## cacheSolve takes the output of the makeCacheMatrix function and returns
## the inverse of the matrix set in makeCacheMatrix()
## If a cached value for the inverse can be found this value is returned
## otherwise the inverse is calculated.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInv(i)
  i
}
