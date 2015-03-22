## The following functions will cache the inverse matrix 
## so that we can use it again if we need it.



## The following function - makeCacheMatrix function - creates a special "matrix",
## which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of an inverse matrix
## 4. get the value of an inverse matrix


makeCacheMatrix <- function(x = matrix()) {
	inverted <- NULL

	set <- function(y) {
		x <<- y
		inverted <<- NULL	
	}
	
	get <- function() x
	setinverse <- function(inverse) inverted <<- inverse
	getinverse <- function() inverted
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The following function - cacheSolve function - calculates 
## the inverse matrix of the special "matrix" created with the above function.
## It first checks if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips computation.
## Otherwise , it calculates the inverse matrix of the data and sets 
## the inverse matrix in the cache via setinverse function.

cacheSolve <- function(x, ...) {
	inverted <- x$getinverse()
	if(!is.null(inverted)) {
		message("getting cached data")
		return(inverted)
	} 
	data <- x$get()
	inverted <- solve(data, ...)
	x$setinverse(inverted)
	inverted
}
