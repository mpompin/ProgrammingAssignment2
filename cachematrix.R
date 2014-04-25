## makeCacheMatrix creates a special "matrix"
## which is a list containing functions 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {   ##set the matrix
  x <<- y
  m <<- NULL
  }
  get <- function() x    ##get the matrix
  setInverse <- function(solve)  ##set the inverse
  m <<- solve
  getInverse <- function()       ##get the inverse
  m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## The following function calculates the inverse of the special "matrix" 
## created with the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()  #query the x matrix's cache
  if(!is.null(m)) {    #if there is a cache
    message("getting cached data")
    return(m)          #just return the cache, no computation needed
  }
  data <- x$get()      #if there's no cache
  m <- solve(data, ...) #compute the inverse here
  x$setInverse(m)       #save the result back to x's cache
  m                     #return the result
}
