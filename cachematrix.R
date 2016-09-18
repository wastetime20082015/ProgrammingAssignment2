# R Programming Week 3 Assignment 2 Linfeng Wei
## Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly
##The function is to cache the inverse of a matrix

## To create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  local_in <- NULL
  set <- function(y) {
    x <<- y
    local_in <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) local_in <<- solve
  getinverse <- function() local_in
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##To compute the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  local_in <- x$getinverse()
  if(!is.null(local_in)) {
    message("getting cached data.")
    return(local_in)
  }
  data <- x$get()
  local_in <- solve(data)
  x$setinverse(local_in)
  local_in
}
