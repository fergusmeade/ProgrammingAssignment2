## Matrix inversion can be computationally expensive and it would be advantageous
## to avoid repeatedly re-computing the result by caching results and returning
## a result if requested once more.

## First create a special matrix where the input is a variable of type matrix
## This object will consist of four functions contained in a list
    #1. set the Matrix
    #2. get the Matrix
    #3. set the inverse of the Matrix
    #4. get the inverser of the Matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initially set to null
  set <- function(y) { #set the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x #get the matrix
  
  setinverse <- function(inverse) inv <<- inverse #manually set the inverse
  
  getinverse <- function() inv #get the inverse
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Then use the cacheSolve function to compute the inverse and cache the result.
## Running cacheSolve again on the same special matrix, will cause the pre-computed
## result to be returned, thus avoiding the need for recomputation.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() #check if its been computed yet
  if(!is.null(inv)) { #if it has been
    message("Getting cached data") #supply an informative message
    return(inv) #return the precomputed inverse
  }
  #if it hasn't been computed yet
  data <- x$get() #get the matrix 
  inv <- solve(data, ...) #find the inverse
  x$setinverse(inv) #cache this result in the object
  inv #return this new result
}
