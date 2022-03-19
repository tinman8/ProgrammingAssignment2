## The functions defined within create the ability to cache an inverse matrix.
## There are two functions defined: the first creates a special matrix object
## that caches an inverse matrix, and the second is a function to calculate the 
## inverse of a matrix, using the special matrix object.

## function to create the special matrix object that can store the inverse 
## matrix in memory.

makeCacheMatrix <- function(x = matrix()) {
    inversed_matrix <- NULL
    
    set <- function(y) {
        x <<- y  # cache the input matrix
        inversed_matrix <<- NULL  ## if the input matrix changes, this prevents
                                  ## the cache from being outputted when solving
    }
    
    get <- function () x

    setinverse <- function(inversed) { 
        inversed_matrix <<- inversed  # cache the inverse matrix
    }
    
    getinverse <- function() inversed_matrix
    
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## Calculates the inverse of a matrix using the special matrix object created 
## with the 'makeCacheMatrix' function defined above. If the matrix has already 
## been calculated, it will return the cached version.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()

    if (!is.null(m)) {
        message("getting cached data")
        return (m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
