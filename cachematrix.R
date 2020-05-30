## This function is used to cache the inverse of a square matrix rather than computing it repeatedly.

## The first function 'makeCacheMatrix creates a special matrix which
## 1. sets the value of matrix
## 2. gets the value of matrix
## 3. sets the value of inverse
## 4. gets the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  a <- NULL
  set <- function(y) {
    x <<- y
    a <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) a <<- inverse
  getInverse <- function()a
  list(set=set,get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The following function calculates the inverse of the special matrix which is created in the above
## fuction. It also checks if the inverse has already been created.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  a <- x$getInverse()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  mtrx <- x$get()
  a <- solve(mtrx,...)
  x$setInverse(a)
  a
}
