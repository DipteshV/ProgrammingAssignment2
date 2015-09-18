

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  d <- NULL
  set <- function(v) {
    x <<- v
    d <<- NULL
  }
  get <- function() x 
  setinverse <- function(inverse) d <<- inverse
  getinverse <- function() d
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Return a matrix that is the inverse of 'x'

## The following function calculates the inverse of the special "matrix" created with the function above. However, it first checks to
## see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it
## calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  d <- x$getinverse()
  if(!is.null(d)){
    message("getting cached data")
    return(d)
  }
  data <- x$get()
  d <- solve(data,...)
  x$setinverse(d)
  d
}
