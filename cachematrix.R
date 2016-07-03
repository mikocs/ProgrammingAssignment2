## Functions managing cached inverse of matrices
## we assume that the matrix passed to the function is inversable

## 'makeCacheMatrix' creates a list of functions to handle cacheing of matrix
## inverses, using the solve method.

## 's' is the cached invcerse for the matrix
## 'set' function resets the list and the cache
## 'get' function provides access to the original matrix
## 'setsolve' function sets the 's' variable to contain the inverted matrix
## 'getsolve' function returns the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## 'cacheSolve' is a function that returns the inverse of a matrix provided 
## in the parameters.
## if the function has not yet been calculated on the matrix, the inversion is
## calculated using the 'solve' function.
## if the function was already called on the matrix before, a cached version is 
## returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## call the solved inverse from the list object provided
        s <- x$getsolve()
        ## if the value returned is not null, the 'setsolve' method of the 
        ## list has already been called. in this case the data we have in the 
        ## 's' variable is the value we are looking for.
        ## return it and end process.
        
        if (!is.null(s)) {
                message('getting cached data')
                return (s)
        }
        
        ## if the value returned from the list was null, assign the original 
        ## matrix to the 'data' variable
        data <- x$get()
        ## invert the matrix using the 'solve' function
        s <- solve(data, ...)
        ## set the inverted matrix in the list to be re-usable in cache
        x$setsolve(s)
        ## return the inverted matrix
        s
}
