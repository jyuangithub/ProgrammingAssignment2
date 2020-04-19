## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL #clear the inverse matrix
  }
  get <- function() x #return matrix itself
  setInverse <- function(inverse) inv <<- inverse #set inverse matrix
  getInverse <- function() inv #return the inverse matrix
  
  #all functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) { #return cached data if exists
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...) #calculate inverse matrix. Note: assumption here is that the matrix is inversible.
  x$setInverse(inv) #set inverse value
  inv #return inverse value
}

