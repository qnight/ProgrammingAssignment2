## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly 

## makeCacheMatrix:  creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
    set <- function(y) {
		x <<- y
        m <<- NULL
	}
    get <- function() x
    setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	message("processing new data")
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m        
## Return a matrix that is the inverse of 'x'
}

# Example:
# mymatrix <- matrix(c(1,1,0,1,0,1,0,1,0),nrow = 3, ncol = 3) # normal matrix
# mt <- makeCacheMatrix(mymatrix) # making it 'special'
# cacheSolve(mt) # new
# cacheSolve(mt) # cached