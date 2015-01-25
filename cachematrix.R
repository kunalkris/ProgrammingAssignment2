## The following pair of functions cache the inverse of a matrix

## The function makeCacheMatrix creates a special "matrix", which is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL 
	
	set <- function(y) 
	{ 
		x <<- y 
		inv <<- NULL 
	} 
	
	get <- function() 
	{
		x
	}

	setinv <- function(inverse)
	{
		inv <<- inverse
	}

	getinv <- function()
	{
		inv
	}

	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inverted <- x$getinv()
	if(!is.null(inverted)) 
	{
		message("getting cached data")
		return(inverted)
	}
	inputMatrix <- x$get()
	inverted <- solve(inputMatrix, ...)
	x$setinv(inverted)
	inverted
}