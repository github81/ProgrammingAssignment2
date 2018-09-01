## Put comments here that give an overall description of what your
## functions do

## this function defines four functions (set, get, setinverse and getinverse) and returns them as a list.
## set - initializes the inv_mat variable, caches matrix in the memory
## get - returns the cached matrix
## setinverse - caches inverse matrix in the memory
## getinverse - returns inverse matrix cached in the memory


makeCacheMatrix <- function(x = matrix()) {
	inv_mat <- NULL
	set <- function(y) {
		x <<- y
		inv_mat <<- NULL
	}	
	get <- function() x
	setinverse <- function(mat) inv_mat <<- mat
	getinverse <- function() inv_mat
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## this function returns the inverse of a matrix (variable inv_mat)
## if the inv_mat is NULL, it calculates the inverse of a matrix (using solve() function) and caches it.
## if the inv_mat is not NULL, it returns the cached inverse of a matrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv_mat <- x$getinverse()
	if (!is.null(inv_mat)) {
		message("getting cached data")
		return(inv_mat)
	}
	mat <- x$get()
	inv_mat <- solve(mat)
	x$setinverse(inv_mat)
	inv_mat
}
