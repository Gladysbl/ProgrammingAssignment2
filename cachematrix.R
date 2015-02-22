## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        xinv <- NULL 
        
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        ## Get the value of the matrix
        get <- function() x 
        
        ## Set the valueof the inverse of the matrix
        setInv <- function(inv) xinv <<- inv 
        
        ## Get the value of the inverse of the matriz
        getInv <- function() xinv 
        
        ## Return our list
        list(set = set, get = get,
                setInv = setInv,
                getInv = getInv)
}

## Return the inverse of the matrix 
cacheSolve <- function(x, ...) {
        
        ##Return a matrix that is the inverse of "x"
        m <- x$getInv() 
        if(!is.null(m)) { 
                message("getting cached data")
                return(m) 
        }
        data <- x$get() ## Get it
        m <- solve(data)## Solve it
        x$setInv(m)     ## Set it
        m               ## Return it
}
