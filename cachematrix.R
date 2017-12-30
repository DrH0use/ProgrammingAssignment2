## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##
## The function returns a list of functions to handle the matrix
## that is required to be inversed. The list of functions contains
## typical getter/setter functions to assign the original matrix
## plus getter/setter functions to assign (and calculate) the
## reversed matrix

makeCacheMatrix <- function(x = matrix()) {
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL
        }
        get <- function() x
        setinversed <- function(solve) mat <<- solve
        getinversed <- function() mat
        
        list(set = set, get = get,
             setinversed = setinversed,
             getinversed = getinversed)
}


## Write a short comment describing this function
## This function will calculate the inverse matrix
## only if it was not calculated previously.
## The function returns either way the inversed 
## matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getinversed()
        if(!is.null(mat)) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data, ...)
        x$setinversed(mat)
        mat
}
