## functions that together cache the inverse of an invertible matrix:

makeCacheMatrix <- function(x = matrix()) { # begin making a function that uses a matrix as input
    inv <- NULL # inv is a new element that will be the inverse matrix
    set <- function (y) { # setup to change the vector
        x <<- y # substitute the inputted vector (x) with a new vector (y)
        inv <<- NULL # delete any old inverse matrices in the parent environment from past runs of this function
    }
    get <- function () x # this returns the original input matrix (x)
    setInverse <- function(solve) inv <<- solve # store the inverse function into makeCacheMatrix
    getInverse <- function () inv # store the get function( displays the inverse) into makeCacheMatrix
    list(set = set, get = get, # store all four functions above (set, get, setInverse, getInverse) into a list so that when I use makeCacheMatrix, the input matrix (x) will be treated by all four functions
         setInverse = setInverse,
         getInverse = getInverse)
}


## this function creates an inverse matrix for the above output

cacheSolve <- function(x) {
    inv <- x$getInverse() # load the inverse matrix if it's already been made and stroed in cache
    if(!is.null(inv)) { # check if the inverse already exists and stored in cache
        message ("getting cached data") # display a message saying the inverse was stored in cache and being pulled from memory
        return(inv)
    }
    data <- x$get() # in case the inverse wasn't in cache, here I load the data as the original input matrix
    inv <- solve(data) # store the inverse matrix in "inv"
    x$setInverse(inv) # save the inverse matrix to cache for future storage
    inv # display inverse matrix
}
