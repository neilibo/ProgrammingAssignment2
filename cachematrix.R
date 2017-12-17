## The following functions will cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
    {
        i <- matrix(numeric(0), 0,0) 
        ## set the value of the matrix
        set <- function(y) 
            {
                x <<- y
                i <<- NULL
            }
        ## get the value of the matrix
        get <- function() x
        ## set the vale of the inverse
        setinv <- function(inv) i <<- inv
        ## get the vale of the mean
        getinv <- function() i
        list(set = set ,get = get, setinv = setinv ,getinv = getinv)
        
    }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
    { ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) 
            {
                message("getting cached data")
                return(i)
            }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
    }