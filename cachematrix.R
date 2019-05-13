## Cache inverse of a matrix to avoid computing repeatedly
## Functions makeCacheMatrix and cacheSolve below do this to avoid costly repeated computation

## makeCacheMatrix function will set the value of the matrix, get the value of the matrix, set the value of inverse of the matrix, and get the value of inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function below now will produce the matrix inverse. If the matrix inverse has already been computed, it will produce this result rather than re computing. Otherwise, the inverse is still computed, and the value set in the cache

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("retrieving cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
