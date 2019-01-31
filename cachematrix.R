## Here we write a pair of functions that cache the inverse of a matrix
## so that when we need it again, it can be looked up in the cache 
## rather than recomputed

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL # we are creating a variable with NULL value where we will 
                    # store the inverse matrix
        set <- function(y) {    #we are defining a function to set the data of 
                #the matrix in the cache
                x <<- y # we are setting the data of matrix x to the parent 
                        # envir  (main environment of function makeCacheMatrix)
                Inv <<- NULL# we are setting the value of Inv to the parent 
                        # envir  (main environment of function makeCacheMatrix)
        }
        get <- function(){ #we are defining a function to call the data of 
                #the matrix that we will need later to calculate the Inv if they 
                #aren't already in the cache
                
                return(x)
        } 
        setInv <- function(x1){
                Inv <<- x1  #we are defining a function to set the passed data
                # of the Inverse Matrix to the parent environment (main 
                # environment of function makeCacheMatrix)
        }
        
        getInv <- function() {#we are defining a function to get the data of 
                # the Inverse matrix variable stored in the cache
                # (which is in the main environment of function makeCacheMatrix)
                return(Inv)
        }
        # The result of our function is a list to be able to access the function.
        # Otherwise x will be a function and we will not be able to 
        # call back the 4 different functions included inside makeCacheMatrix 
        # when we will run the cacheSolve function. 
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        Inv <- x$getInv() # we are calling the Inverse matrix with getInv
        if(!is.null(Inv)) {#if the inverse matrix exists, the existing result
                #of the function should be different than Null
                message("getting cached matrix") # in this case we are returning
                #the message that we are getting the matrix from the cache
                return(Inv) # and we return the existing value which is the 
                #already cached inverse matrix 
        }
        # if the Inv is empty (Null) the inverse matrix doesn't exist. We should
        # find the inverse with solve function
        
        matrix1 <- x$get() # we are storing the values of our matrix to a 
        #matrix variable that we named matrix1
        Inv1 <- solve(matrix1, ...) #we are calculating the inverse and store 
        #it to Inv1
        x$setInv(Inv1)    # we are setting the values of Inv1 to the cache 
        #with setInv function. Now its cached.
        return(Inv1) # and we return the new calculated value which is the 
        # inverse matrix and is also set in the cache with previous function
}
