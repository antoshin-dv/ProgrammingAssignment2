## Calculation of inverse matrix is an expensive operation. 
## It may therefore be useful for caching the inverse matrix rather than compute it repeatedly.
## The following functions allow cache the inverse of a matrix.

## Function stores the matrix and its inverse (it has been calculated)
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL    # Inverse matrix
    
    ## Set new original matrix
    set <- function(y)
    {
        x <<- y
        inverse <<- NULL
    }
  
    ## Get original matrix
    get <- function() x
  
    ## Set inverse matrix for "x"
    setInverse <- function(solv) inverse <<- solv
    
    ## Get inverse matrix
    getInverse <- function() inverse
  
    ## Access to function set/get
    list(set = set, get = get,
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Calculation the inverse matrix of the special "Matrix" created with function makeCacheMatrix
## The function checks to see if the inverse matrix has been calculated.
## If yes, it gets inverse matrix from the cache and displays a message "getting cached data"
## Otherwise, it calculates the inverse matrix and store it in makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
  
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
