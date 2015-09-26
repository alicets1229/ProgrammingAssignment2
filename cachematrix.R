## These two functions cache the inverse of the given matrix because it is
## a costly computation to invert a matrix.

## This function creates a list that sets the value of the matrix, gets the
## value, then sets and gets the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		INV <<- NULL 
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse,
	getinverse = getinverse)
}


## This function first checks the cache to see if the inverse of the matrix
## has already been computed, if so it skips re-computing it and tells the
## user before returning the inverse. If not, it computes the inverse and 
## sets it in the cache.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("Retrieving cached data.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
