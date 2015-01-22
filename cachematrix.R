## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix constructs a CacheMatrix Object.
## CacheMatrix can store a matrix and the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
    ## construction, set inv to NULL
    inv <- NULL
    ## set a new matrix
    set <- function(y) {
        ## save the matrix
        x <<- y
        ## clear the cached inverse
        inv <<- NULL
        message("set was called")
    }
    ## get the matrix
    get <- function() x
    ## set the inverse of the matrix, just save it
    ## maybe we should at least check the dimensions ...
    setinv <- function(inv) inv <<- inv
    ## simply return the value of inv
    getinv <- function() inv
    ## create the 'interface', i.e. a list of publicly available functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve takes a CacheMatrix Object and returns the inverse of it data
cacheSolve <- function(x, ...) {
    ## get the 'inverse' member of x    
    i <- x$getinv()
    if(!is.null(i)) {
        ## the has allready been calculated, 
        message("getting cached data")
        ## we're done
        return(i)
    }
    ## the cache was empty, so we have to calculate the inverse
    ## from the data stored in x
    i <- NULL
    i <- solve(x$get(), ...)
    ## now store the result in the cache, for future use
    x$setinv(i)
    ## and don't forget to return the result
    i
}

# runTest is a helper function to verify the above set of functions.
# it creates an instance of 'cacheMatrix', 
# then gets the result two times to see if caching works
# then it constructs a new 'cacheMatrix' object, which then is
# inverted again. 
# finally the inverse of the inverse is tested against the first matrix.
# these should have identical values.
runTest <- function(){
    f <- makeCacheMatrix(matrix(c(1, 5, 4, 2, 3, 9, 8, 7, 6),3,3))
    ## this one is not invertable:
    ## f <- makeCacheMatrix(matrix(1:9,3,3))
    print(f)
    print('=======================')
    print(f$get())
    print('=======================')
    print(f$getinv())
    print('=======================')
    print(cacheSolve(f))
    print('=======================')
    f1 <- makeCacheMatrix(cacheSolve(f))
    print(f1$get())
    print('=======================')
    f2 <- cacheSolve(f1)
    print(f2)
    print('=======================')
    if (all.equal(f$get(),f2)){
        print ("seems OK")
    } else { 
        print ("we have a problem ...")
    }
}