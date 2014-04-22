# The function makeCacheMatrix caches the inverse of a matrix
# The function cacheSolve computes the inverse of a matrix with the 
# solve R function. The assumption is that the matrix is always 
# invertible

# This function creates a special "matrix" which is a list 
# containing a function to
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse
# 4 get the value of the inverse
makeCacheMatrix <- function(x = matrix()) 
{
    # set the matrix to a null matrix
    mat <- NULL
    
    # define the set function
    set <- function(y)
    {
        # assign in cache to x the matrix y
        x <<- y
        
        # set mat to NULL
        mat <<- NULL
    }
    
    # get the value of the matrix x
    get <- function() x
    
    # Copy the inverse if it has already been computed
    setinverse <- function() mat 
    
    # get the value of the inverse and compute it if it has not yet been
    # computed
    getinverse <- function() 
    {
        if (is.null(mat)) mat <<- solve(x)
        mat
    }
        
    
    # store as a list the information
    list(set, get, setinverse = setinverse, getinverse = getinverse)
}

# This function calculates the inverse of the matrix created
# in the function makeCacheMatrix but before doing so, it 
# checks whether the inverse has been computed before. If it
# has already being computed, it just retrieve it from the
# cache and does not compute its inverse
cacheSolve <- function(x, ...)
{
    # See whether there is an inverse already computed
    mat <- x$getinverse()
    
    # if mat is not NULL, print a message indicating you are
    # retrieving the cached inverse
    if (!is.null(mat))
    {
        message("Getting the cached inverse")
        return(mat)
    }
    
    # If you are here, you need to compute the inverse as it
    # is not cached. Assign x to data
    data <- x$get()
    
    # compute the inverse of the matrix with the solve function
    mat <- solve(data)
    
    # Set the value of the inverse in cache
    x$setinverse(mat)
    mat
}