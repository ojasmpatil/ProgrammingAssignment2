## Here we have two functions, which save computation time while 
## computing inverse a matrix by checking if there is an already
## computed inverse in the cache

## The makeCacheMatrix function converts a matrix into a "special" matrix 
## which is a list of set matrix , get matrix, set inverse and get inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## the cacheSolve function computes the inverse of the "special" matrix returned
## by the makeCacheMatrix function. Here if the inverse exists in cache then it 
## uses that value or else computes a new inverse using the solve function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
