## This function provides basic caching operations, 
## which include initializing, setting matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
            i <- NULL #initialize the matrix inverse
            set <- function(y) {
                ## we set our matrix x with the value of y 
                ## note <<- operator, also we set the inverse to NULL
                x <<- y 
                i <<- NULL
            }
            get <- function() x # we return the matrix x
            setinverse <- function(inverse) i <<- inverse #we set the matrix inverse in this function
            getinverse <- function() i #get the matrix inverse in this function 
            ## return a list that contains the four functionalities 
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
         
}


## CacheSolve will try to inverse a matrix, first it will check if the 
## inverse is available, then no need to calculate. Otherwise, it will 
## calculate the inverse and cache it for further computation if needed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## x here is the object returned from makeCacheMatrix function
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached matrix")
                return(i)
        }
        
        ## No need for else as we return if it is cached
        
        ## In the following 3 steps we get the matrix, 
        ## calculate the index, and cache it back into the matrix
        myMatrix <- x$get() 
        inverse = solve(myMatrix)
        x$setinverse(inverse)
        
        #return the inverse
        inverse
}
