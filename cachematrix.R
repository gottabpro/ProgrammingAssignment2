## Write a short comment describing this function

## makeCacheMatrix creates a list of 4 containing a function to
## set the value of the matrix, get the value of the matrix, 
## set the value of the inverse, get the value of the inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse_mat) inv <<- inverse_mat
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve checks if inverse has not already been calculated. 
## if so, it retrieves the cached object
## else it calculated the inverse and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
