# this function is designed to inverse a matrix and cache the
# results in an envrionment outside of this one so that it
# is available
makeCacheMatrix <- function(x=matrix()) {
        s <- NULL                   #sets initial values to Null
        set <- function(y) {
               x<<-y      #resets global val to 0 if new matrix used      
               s<<- NULL  
        }
        get <- function(){x}     #names values for easier testing
        setinverse <- function(solve){s <<- solve}
        getinverse <- function(){s}
        list(set=set, get=get, setinverse=setinverse, 
             getinverse=getinverse)   #creates a list with named values for easier testing
} 


# This function is designed to test whether the inverse matrix
# already exists in the global envrionment, and if it does,
# retrieve those values

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)){
                message("getting cached matrix")
                return(s)
        }
        data <- x$get()
        s<- solve(data)
        x$setinverse(s)
        s
}
