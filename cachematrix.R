## The purpose of makeCacheMatrix function is to store a martix and a cached  
## value of the inverse of the matrix.
## It returns a list of the following functions:
## 1. setMatrix      set the value of a matrix
## 2. getMatrix      get the value of a matrix
## 3. setInverse     set the cached value (inverse of the matrix)
## 4. getInverse     get the cached value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
	## hold the cached value or NULL if nothing is cached
	cache <- NULL
	## store a matrix
	setMatrix <- function(y){
		x <<- y
		## since the matrix is assigned a new value, refresh cache
		cache <<- NULL
	}
	## return the stored matrix
	getMatrix <- function() {
		x
	}
	## cache the given argument
	setInverse <- function(solve) {
		cache <<- solve
	}
	## get the cached value
	getInverse <- function() {
		cache
	}
	## return a list, each named element being a function
	list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## The purpose of the cacheSolve function is to calculate the inverse of a 
## "special" matrix created using above function.

cacheSolve <- function(x, ...) {
	## get the cached value
	inverse <- x$getInverse()
	## if a cached value exists return it
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	## otherwise get the matrix, calculate the inverse and store it in the cache
	matrix <- x$getMatrix()
	inverse <- solve(matrix, ...)
	x$setInverse(inverse)
	## return the desired inverse
	inverse
}
