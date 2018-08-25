## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## it creates a list of functions that do the following
## 1. sets value of a matrix
## 2. gets value of a matrix
## 3. sets the inverse of a matrix 
## 4. gets the value of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  y = matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
  set <- function (z) {
    x <<- z
    y <<- matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
  }
  get <- function() x
  setinverse <- function (a= matrix()) y <<- a
  getinverse <- function () y
  list(set=set, get = get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
## this function checks whether inverse has been computed or not. if it is not computed then inverse is calculated

cacheSolve <- function(x, ...) {
  y <- x$getinverse()
  if(!is.na(y)) {
    message("getting cached data")
    return (y)
  }
  data <- x$get()
  y <- solve(data, ...)
  x$setinverse(y)
  y
}
