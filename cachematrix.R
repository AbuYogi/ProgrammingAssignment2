## These functions take a matrix, and provide its inverse while saving computing time by caching the inverse 
## and using that cached inverse matrix to avoid rerunning the solve function.

## This function takes a matrix and returns a list of functions 1)a function that allows you to manually set
## the relevant matrix after the function has already been called 2) return the original matrix passed as input
## 3) set the inverse based on input 4) return the stored inverse as output

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function receives the list created by makeCacheMatrix, and either gives the inverse stored in that list
## or calculates that inverse if it hasn't been calculated already.

cacheSolve <- function(a, ...) {
  n <- a$getinverse()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- a$get()
  n <- solve(data, ...)   ##this is doing the actual matrix inversion
  a$setinverse(n)
  n
}
