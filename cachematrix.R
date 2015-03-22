##The following functions will cache the inverse matrix so that we can use it again if we need it.



## The following function - makeCacheMatrix function - creates a special "matrix", which is really a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of an inverse matrix
## 4. get the value of an inverse matrix


makeCacheMatrix <- function(x = matrix()) {
	i <- NULL

    set <- function(y) {
         x <<- y
         i <<- NULL
     }
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## The following function - cacheSolve function - calculates the inverse matrix of the special "matrix" created with the above function.
## It first checks if the inverse matrix has already been calculated. If so, it gets the inverse matrix from the cache and skips computation.
## Otherwise , it calculates the inverse matrix of the data and sets the inverse matrix in the cache via setinverse function.

cacheSolve <- function(x, ...) {
        	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	} 
	data <- x$get()
	i <- solve(data)
	x$setinverse(i)
	i
}
