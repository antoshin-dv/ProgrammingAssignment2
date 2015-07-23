## Put comments here that give an overall description of what your
## functions do

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
  
    ## Set matrix inverse "x"
    setInverse <- function(solv) inverse <<- solv
    
    ## Get inverse matrix
    getInverse <- function() inverse
  
    ## Access to function set/get
    list(set = set, get = get,
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Calculation of the inverse matrix
## If the matrix X has been calculated inverse matrix
## then return the matrix and displays a message "getting cached data"
## else calculate inverse matrix and store it in makeCacheMatrix
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
