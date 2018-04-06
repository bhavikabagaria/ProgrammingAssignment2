## makeCacheMatrix makes a list with a function that sets and gets the value of the matrix
## as well as sets and gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve computes the inverse matrix to X and returns it. It checks if the 
## matrix inverse has already been computed and is in the cache. If so, it returns 
## the inverse, and if not it computes the inverse, sets the inverse in the cache 
## and then returns it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
        inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    inv
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
