## R programming assignment 2 - Lexical Scoping
##
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    a <- NULL   
    set <- function(y) {
        x <<- y
        a <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) a <<- inverse
    getInverse <- function() a
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    a <- x$getInverse()
    if(!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    data <- x$get()
    a <- solve(data)  
    x$setInverse(a)
    a
}
