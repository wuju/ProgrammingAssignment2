## Put comments here that give an overall description of what your
## functions do

## creates a special matrix object
## and returns the following list of functions
## set: sets and stores the matrix object
## get: gets the matrix object
## setinverse: computes the inverse of the matrix and caches it
## getinverse: gets the cached inverse

makeCacheMatrix <- function(x = matrix()) {
	## initialize cache of inverse
	inv <- NULL
	
	## set the matrix and clear the cache of inverse
	set <- function(y) {
		x   <<- y
		inv <<- NULL
	}
	
	## return the matrix
	get <- function() x
	
	## cache the inverse
	setinverse <- function(inverse) inv <<- inverse
	
	## return the cached inverse
	getinverse <- function() inv
	
	## return the list of functions
	list(set = set, get = get,
		 setinverse = setinverse, getinverse = getinverse)
}


## returns the inverse of the matrix returned by makeCacheMatrix
## if the inverse has previously been calculated,
## it returns the cached

cacheSolve <- function(x, ...) {
	
	## check if there's a cached inverse
	inv <- x$getinverse()
	
	## if inv holds a value, that means there's a cached version
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	
	## if inv was NULL, then get the value of the matrix
	data <- x$get()
	
	## compute the inverse
	inv <- solve(data, ...)
	
	## cache the inverse
	x$setinverse(inv)
    
    ## Return a matrix that is the inverse of 'x'
    inv
}
