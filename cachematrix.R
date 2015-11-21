## Put comments here that give an overall description of what your
## functions do
## The first function makes the cachematrix in preparation for the second function.
## The first function goes as to first make sure that the local variable is null to start with so that it can be 
## used to store changes, in this case, the matrix.
## The second function finds the inverse of the matrix created as a result of the first function. 
## The second function checks for whether the inverse has been created. If not, the function goes to calculate the inverse matrix of
## the input

## Write a short comment describing this function

makeCache <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
