## Save actual matrix and return inverse of matrix from cache, 
## (if calculation is already done) or first calculate inverse,cache it and then
## return

## This function caches the actual and inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
        
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x ## get actual matrix
        setinverse <- function(inverse) invs <<- inverse
        getinverse <- function() invs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function calculates inverse of matrix and cache it,
## if cache is already present then it returns it without calculation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinverse() ## get inverse of matrix if it is cached
        if(!is.null(invs)) { ## return cached inverse of matrix
                message("getting cached data")
                return(invs)
        }
        data <- x$get() ##get actual matrix
        inv <- solve(data) ##get inverse of matrix
        x$setinverse(inv) ##set inverse of matrix
        inv
}