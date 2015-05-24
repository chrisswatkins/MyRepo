## These functions illustrate how functions can be used to capture 
## state in R programs


## This function creates a special "matrix" object that prodices a list of 3 functions.

makeCacheMatrix <- function(x = matrix()) {

        invmat <- NULL                             ## initialize inverse variable
        datastore <- NULL                          ## initialize input data
        
        setcachedata <- function(z) datastore <<- z
        getcachedata <- function() datastore 
        get <- function() x                        ## Create function to get input matrix
        setmatrixinv <- function(y) invmat <<- y   ## Create function to set the result of matrix inverse
        getmatrixinv <- function() invmat          ## Create function to get the result of matrix inverse

        list(setcachedata = setcachedata,          ## return liST when makeCacheMatrix is called
             getcachedata = getcachedata,
             get = get, 
             setmatrixinv = setmatrixinv,
             getmatrixinv = getmatrixinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        datastore <- NULL                       ## initialize cached input data

        m <- x$getmatrixinv()                   ## test for cached calculated inverse
        
        if(!is.null(m) || identical(x$getcachedata(), x$get() )  ) {                       ## Check for NULL or changed input
                message("getting cached data")
                return(m)
        }

        data <- x$get()                         ## obtain input data
        x$setcachedata(data)                       ## cache input data
        sol <- solve(data, ...)                 ## solve for inverse
        x$setmatrixinv(sol)                     ## cache inverse 
        sol                                     ## return inverse
}
