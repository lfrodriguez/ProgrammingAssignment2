## Put comments here that give an overall description of what your
## functions do

## This function is going to help us set the matrix as a list, and
## stores a function to: 

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  
  ## 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  ## 2. Get the value of the matrix
  get <- function() x
  
  ## 3. set hte value of the inverse of the matrix
  setInverse <- function(solve) inverse_matrix <<- solve
  
  ## 4. Get the value of the inverse of the matrix
  getInverse <- function() inverse_matrix
  
  ## below returns our list 
  list(
    set = set, 
    get = get,
    setInverse = setInverse,
    getInverse = getInverse )
}

## This function is going to give us the inverse of the matrix and store it.
## If we already have the value of the inverse of the matrix, 
## the cache will reply "getting cached answer", and print the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getInverse()
  
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
 
   data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setInverse(inverse_matrix)
  inverse_matrix
}
