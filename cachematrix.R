## A pair of functions that cache the inverse of an invertible matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	# initialize the inverse matrix
	matr_inv <- NULL

	# function to set a new matrix if it is different from the old matrix
	set <- function(y) {
		if(!identical(y, get())) {
			x <<- y
			matr_inv <<- NULL
		}	
	}

	# function to get the matrix
	get <- function() x

	# function to set the inverse matrix
	setinv <- function(solve) matr_inv <<- solve

	# function to get the inverse matrix
	getinv <- function() matr_inv

	# return a list of the functions
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'

	# get the inverse matrix
	matr_inv <- x$getinv()

	# if the inverse matrix is in the cache....get it from the cache
	if(!is.null(matr_inv)) {
		message("getting cached data")
		return(matr_inv)
	}

	# if the inverse matrix is not in the cache....compute and return the inverse
	data <- x$get()
	matr_inv <- solve(data, ...)
	x$setinv(matr_inv)
	matr_inv
}
