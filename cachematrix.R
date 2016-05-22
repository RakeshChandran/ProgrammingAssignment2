# makeCacheMatrix and cacheSolve functions help in reducing computation time by caching the inverse of a matrix. 
# When calling the matrix inversal function, it searches if the inverse has already been computed. 
# If yes, then the cached output is returned thereby saving time. Else the inverse of the matrix is calculated.


# 'makeCacheMatrix' takes a matrix as input and it is a list which has functions for performing the following,
# 1. set(): to input the value of the matrix
# 2. get(): for printing the value of the stored matrix
# 3. setinverse(): to store the value of the calculated matrix inverse
# 4. getinverse(): to print the value of the calculated matrix inverse

makeCacheMatrix <- function(x = matrix()) 
{
        m <- NULL
        set <- function(y) 
        {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse,getinverse = getinverse)
}

# 'cacheSolve' takes a matrix as input and calculates the inverse of the matrix. It checks if the calculation has already been 
# performed. If yes, it retreives the value of the matrix inverse from the cache. If no, the calculation is performed and 
# the output is printed.

cacheSolve <- function(x = matrix(), ...) 
{
  	  m <- x$getinverse()
        if(!is.null(m)) 
	  {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
