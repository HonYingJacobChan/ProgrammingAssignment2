## This is the second programming assignment -
##
## Modified from makeVector and cachemean (for vectors),
## "makeCacheMatrix" and "cacheSolve" are for invertible square matrices
##
## "makeCacheMatrix" creates a special 'vector', which is really a list containing a function to
## 1. set the value of the given matrix
## 2. get the value of the given matrix
## 3. set the value of the inverse of given matrix
## 4. get the value of the inverse of given matrix
##
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

##
## "cacheSolve" calculates the mean of the special 'vector' created with makeCacheMatrix. 
## However, it first checks to see if the inverse of given matrix has already been calculated.
## If so, it gets the mean from the cache and skips the computation.
## Otherwise, calculates the inverse of the given matrix
## and sets the inverse of the given matrix in the cache via the setsolve function.
##
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
