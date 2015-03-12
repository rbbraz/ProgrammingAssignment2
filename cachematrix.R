## Declares two functions that can be used to create a matrix with cache functionality, and use it cache matrix inversion operations.

## Creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve #sets the cached version of the result
        getsolve <- function() m #gets the cached version of the result
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x', getting the cached version if available
cacheSolve <- function(x, ...) {        	
		m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m) #returns cached version if it exists..
        }
        data <- x$get()
        m <- solve(data, ...) #gets the inverse of the matrix
        x$setsolve(m) #sets cache since none existed until now
        m	
}
