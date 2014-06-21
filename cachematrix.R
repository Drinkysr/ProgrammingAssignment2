
## Functions to evaluate the inverse of a matrix
## And cache the result
## If the inverse of that particular matrix has been
## found before, the function will obtain the inverse
## from the cache instead of calling the potentially
## resource-intensive solve function

#makeCacheMatrix takes a matrix as a argument and 
#returns a list of 4 functions: set() caches the matrix
## get() returns the matrix, setinverse caches the 
## matrix inverse, and getinverse() retrieves it



makeCacheMatrix <- function(x = matrix()) {
        
        
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
##cacheSolve uses calls to the function list to 
## determine whether a given matrix's inverse has been
## cached. If not, the inverse is returned via a call to 
## solve(). If so, the inverse is returned from the cache
cacheSolve <- function(x, ...) {
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

##testing
#z<-matrix(c(1,2,3,4),2,2)
# v<-makeCacheMatrix()
# v$set(z)
# v$get()
# cacheSolve(v) #stores in cache
# cacheSolve(v) #gets from cache