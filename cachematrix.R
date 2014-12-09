## A pair of functions that cache the inverse of an invertible matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	matr_inv <- NULL
	set <- function(y) {
		if(!identical(y, get())) {
			x <<- y
			matr_inv <<- NULL
		}	
	}
	get <- function() x
	setinv <- function(solve) matr_inv <<- solve
	getinv <- function() matr_inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	matr_inv <- x$getinv()
	if(!is.null(matr_inv)) {
		message("getting cached data")
		return(matr_inv)
	}
	data <- x$get()
	matr_inv <- solve(data, ...)
	x$setinv(matr_inv)
	matr_inv
}
