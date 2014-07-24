## makeCacheMatrix ... will hold a matrix in cache
## in combination with cacheSolve it will hold the inverted matrix as well
## what is really returned by this function, is a list of functions 
## that allow you to access the cached values

## usage: mtx <- makeCacheMatrix()
## mtx$set( matrix(c(2,5,-4,5),2,2) )

makeCacheMatrix <- function(x = matrix()) {
  #x to hold matrix
  #inv to hold inverted matrix
  inv <- NULL
  setinverse <- function(invmtx) inv <<- invmtx
  getinverse <- function() inv
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This will take in one of the objects created by makeCacheMatrix and
## will determine if the inverse has been 'solved' yet and if not it will
## 'solve' and cache the results and show them, otherwise it will just
## pull the cached inverse and show it
## usage looks like this: cacheSolve(mtx)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
