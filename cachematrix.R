## Bethg86

## makeCacheMatrix stores the given matrix and defines
## functions that can be performed on the matrix.
## set() replaces the stored matrix with the new given matrix and 
## clears any stored value of the inverse.
## get() returns the stored matrix.
## setinverse() caches the given value as the inverse of the matrix.
## getinverse() returns the current stored value of the inverse of 
## the matrix or NULL if no value has been stored.
makeCacheMatrix <- function(x = matrix()) 
{
    minv <- NULL
    set <- function(y)
    {
        x <<- y
        minv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) minv <<- inverse
    getinverse <- function() minv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns a matrix that is the inverse 
## of the original matrix stored in 'x'.
## If the inverse is already cached within 'x', it is
## retrieved and returned.  Otherwise, the inverse is
## calculated and cached within 'x', then returned.
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse))
    {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
