## The following functions together compute the invert of an invertible square
## matrix. Once the invert is calc'ed, the result is cached. So when the
## invert is needed at any later time, no computation is needed.

## Function makeCacheMatrix(x) creates an object with the caching capability
## of invert of matrix, together with accesser functions

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL
        }
	get <- function() x
	setinvert <- function(invert) m <<- invert
	getinvert <- function() m
	list(set = set, get = get,
		setinvert = setinvert,
		getinvert = getinvert)
}


## Function cacheSolve(x) checks into object x, see if its the invert is ready.
## It is returned as result if so, otherwise, the actual computation occurs and
## cahced, then result is returned.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinvert()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinvert(m)
	m
}
