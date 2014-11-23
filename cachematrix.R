## The first function:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse matrix
## 4. gets the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setminverse = setinverse,
       getinverse = getinverse)
}


## The next function returns an inverse of a matrix, which is cached or if there's no calculated
## it calculates the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getineverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
