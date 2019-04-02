## Put comments here that give an overall description of what your
## functions do

## This function builds the functions needed to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) 
{
    x_inv = NULL
    set = function(y)
    {
        x <<- y
        x_inv <<- NULL
    }
    get = function() x
    setinv = function(inv) x_inv <<- inv
    getinv = function() x_inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function tries to retrieve the inverse of the matrix from the cache
## If the inverse is not in the cache, it is computed and stored in the cache
## for future calls.

cacheSolve <- function(x, ...) 
{
    x_inv = x$getinv()
    if(!is.null(x_inv))
    {
        message("getting cached data")
        return(x_inv)
    }
    data = x$get()
    x_inv = solve(data, ...)
    x$setinv(x_inv)
    x_inv
}



