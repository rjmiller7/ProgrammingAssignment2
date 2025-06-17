## Put comments here that give an overall description of what your
## functions do

## This function returns a list of four functions which can be used to interact with the matrix and its cached inverse. The value is set, then is retrieved by get, and setinverse and getinverse set and retrieve the cached inverse respectively.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        set <- function(a) {
                x <<- a 
                inv <<- NULL
        }

        get <- function() x

        setinverse <- function(inverse) inv <- inverse

        getinverse <- function() inv

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function first checks if the inverse is already using x$getinverse(), if it's not NULL, the cached inverse is returned. If the inverse isn't cached, it retrieves the matrix using x$get() and the computed inverse from solve(data, ...) is cached using x$setinverse(inv).

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!isnull(inv)) {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()
        inv <- solve(data, ...)

        x$setinverse(inv)

        inv
}
