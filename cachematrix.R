## To make a function that stores the value of the inverse in cache and retrieves it


## this function defines whether ito calculate the inverse or get the value

makeCacheMatrix <- function(x = matrix()) {
    val <- NULL
    set <- function(y) {
        x <<- y
        val<<-  NULL
    }
    get <- function() x
    setinv <- function(solve) val <<- solve
    getinv <- function() val
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## this function checks if the cache exists and if it does it retrieves the value from there

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    val<-x$getinv()
  
    if (!is.null(val)){
       
        message("getting cached data")
        return(val)
    }
   
    data<-x$get()
    val<-solve(data,...)
    x$setinv(val)
    val
    
}
