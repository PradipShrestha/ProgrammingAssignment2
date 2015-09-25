## Put comments here that give an overall description of what your
## functions do

## This function creates the cache of inverse of matrix.
## @x: input matrix
## return: list of functions set, get, setinverse, getinverse
makeCacheMatrix <- function(x = matrix()) {
        m = NULL
        
        ## Sets function - Set the matrix
        ## @y: input matrix
        set = function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## Gets function - Gets the matrix
        get = function()
                x
        
        ## Sets the inverse of matrix
        ## @i: input inverse matrix
        setinverse = function(i)
                m <<- i
        
        ## Gets the inverse of matrix
        ## return: the inverse matrix
        getinverse = function()
                m
        
        list(
                set = set, get = get, setinverse = setinverse, getinverse = getinverse
        )
}


## This function returns the inverse of matrix
## @x: output of makeCacheMatrix
## return: the inverse of matrix
cacheSolve <- function(x, ...) {
        # First try to get the value from cache
        m = x$getinverse()
        
        # If we already have value in cache
        if (!is.null(m)) {
                # Use it and avoid costly calculation
                message("getting cached data")
                return(m)
        }
        
        # If we do not have value in cache,
        # Calculate it.
        mat.data = x$get()
        m = solve(mat.data, ...)
        
        # Store value in cache
        x$setinverse(m)
        
        m
}
