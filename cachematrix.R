## makeCacheMatrix make and cache the matrix which is a invertible square.
## cacheSolve calculate the inverse of the matrix

## If the matrix is not invertible or not a square, it will notice you with a simple message.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		if(is.matrix(y) == FALSE) {
			print("Please input matrix.")
		} else if(ncol(y) != nrow(y)) {
			print("nrow and ncol are different.")
			print("Please use a square matrix.")
		} else if(det(y) == 0) {
			print("The determinant of matrix is 0.")
			print("Please use an invertible matrix.")
		} else {
			x <<- y
			m <<- NULL
		}
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}


## It just calculate the inverse of matrix and cache it in the makeCacheMatrix func.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
