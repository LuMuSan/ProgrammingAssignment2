## makeCacheMatrix constructs a CacheMatrix Object.
## CacheMatrix can store a matrix and the inverse of it.

## I started out copying the cacheVector example, modifing it to my needs.

makeCacheMatrix <- function(x = matrix()) {
    ## construction, set inv to NULL
    inv <- NULL
    set <- function(y) {
        ## set the main member of this object
        ## save the matrix
        x <<- y
        ## clear the cached inverse
        inv <<- NULL
        message("set was called")
    }
    get <- function() x
        ## return the matrix, that's all
    
    setinv <- function(inv) {
        ## set the inverse of the matrix, just save it
        ## this allows to set any matrix in the cache. I don't like it.
        ## maybe one should at least check the dimensions ...
        ## or much better put the caching logic in here and make it private.
        ## actually this function doesn't make any sense to me.
        inv <<- inv
    }
    getinv <- function(...) {
        ## getinv check wether the matrix has allready been inverted
        ## if so, simply return the value of inv
        ## otherwise calculate inv using the solve function, store the 
        ## result for further calls, and then return it.
        if(!is.null(inv)) {
            ## the has allready been calculated, 
            message("getting cached data")
            ## we're done
            return(inv)
        }
        inv <<- solve(x, ...)
        inv
    }
    ## create the 'interface', i.e. a list of publicly available functions
    list(set = set, 
         get = get,
         getinv = getinv)
}


## cacheSolve takes a CacheMatrix Object and returns the inverse of it data
cacheSolve <- function(x, ...) {
    ## get the 'inverse' member of x    
    i <- x$getinv(...)
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
    print('=======================')
    ## this one is not invertable:
    f$set(matrix(1:9,3,3))
    print("setting non invertible matrix, and try to solve it ...")
    print(cacheSolve(f))
    print('=======================')
    
}