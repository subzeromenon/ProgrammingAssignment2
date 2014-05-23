


## No computation takes place at this level. makeCacheMatrix is purely for saving 
## and retrieving the list in which the data entered is saved.
makeCacheMatrix <- function(x = matrix()) 
{   #Initializing the var i as Null in the current environment
    i <- NULL
    set <- function(y) 
    {
        x <<- y
        i <<- NULL
        #Overrides the value of i in the env. of makeCacheMatrix
    }
    get <- function() x
    #Function which sets the inverse value in the cache value, i.e. again overrides the parent
    #environment
    setInverse <- function(inverse) i <<- inverse
    #Function that retrieves the value from the cache. 
    getInverse <- function() i
    #Function which stores the values in the list
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Calculates the inverse of the matrix and furthermore invokes the nested functions
## in the function makeCacheMatrix()    

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) 
    {   # Checks to see if the value is already available in the cache
        # or global envir.
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    #Retrieves the matrix, and calculates the inverse
    i <- solve(data)
    x$setInverse(i)
    i
    ## Return a matrix that is the inverse of 'x'
}

##The effect of caching was rather evident if we use matrices of order 1000x1000.
## Took 20 to 30 seconds to calculate first time, and took less than 2 secs to 
##retrieve from the cache.
