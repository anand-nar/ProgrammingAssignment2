## Programming Assignment 2: Lexical Scoping 
## Matrix inversion is a costly function and the below pair of functions cache
## inverse of a Matrix.

## This example also demonstrate <<- operation which can be used to assign
## a value to an object in an environment that is differnet from the current
## environment.  

## makeCacheMatrix takes the matrix that has to be inversed and returns a list
## which has the set and get functions to assign and retrieve the inversed matrix.
## The function takes advantage of the scoping rules to preserve the state of the 
## variables inside an R object
## Input  - the matrix to be inversed.
## Output - A list which has the below functions
##         set -> set the matrix and set the inverse to null
##         get -> get the matrix
##         setInv -> set the Inverse
##         getInv -> get the Inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(invM) inv <<- invM
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
     
}

## cacheSolve computes the inverse of the matrix object/list returned by 
## makeCacheMatrix. First it checks if the passed matrix is same as the cached 
## matrix. If it is the sane it checks whether the inverse is available. If 
## available it returns it. Else it calculates the inverse, set the cache inverse 
## value and also return the same to the calling program. If the passed matrix is
## the same as cached matrix, it calculates the inverse and passes it to the 
## calling porgram.
##

cacheSolve <- function(x, y = matrix(), ...) {
        ## Input - list from makeCacheMatrix and the matrix to be inversed.
        ## Return a matrix that is the inverse of 'x'
        # first get the cached matrix
        matDat <- x$get()
        # check if this is equal to the passed one
        if (identical(matDat,y)){
                inv <- x$getInv()
                # if the inverse is already present
                if (!is.null(inv)){
                        # get it from the cache
                        message("getting the inversed matrix from cache")
                        return(inv)
                }
                # otherwise, calculates the inverse
                inv <- solve(matDat, ...)
                # sets the value of the inverse in the cache via the setinv function.
                x$setInv(inv)
                return(inv)
        } 
        # otherwise just calculate the inverse of the passed matric and return it. Don't set the inverse.
        inv <- solve(y, ...)
        return(inv)
}

# Function to test.
# first time call. It takes some time as the inverse is created. The next 2 times are faster as the inverse is got from the cache
#The the matrix is changed. The next call takes almost the same time as first as the inverse is created.
#The next call also takes almost the same time as previus one because when the object are not identical the inverse is not set
#after the swap the inverse is got from the cache.

#[1] "First time Start"
#[1] "First time End, time taken = 1.06287527084351"
#[1] "Second time Start"
#getting the inversed matrix from cache
#[1] "Second time End, time taken = 0.000204324722290039"
#[1] "Third time Start"
#getting the inversed matrix from cache
#[1] "Third time End, time taken = 0.000139713287353516"
#[1] "After change first time Start"
#[1] "After change End, time taken = 1.01118421554565"
#[1] "After change second time Start"
#[1] "After change second time End, time taken = 1.01877665519714"
#[1] "After swap first time Start"
#getting the inversed matrix from cache
#[1] "After Swap End, time taken = 0.000242471694946289"

test <- function(mat){
        ## @mat: an invertible matrix
        
        temp = makeCacheMatrix(mat)
        tempmat <- mat
        
        print("First time Start")
        start.time <- Sys.time()
        cacheSolve(temp, mat)
        dur <- Sys.time() - start.time
        print(paste("First time End, time taken =",dur))
        
        print("Second time Start")
        start.time <- Sys.time()
        cacheSolve(temp,mat)
        dur <- Sys.time() - start.time
        print(paste("Second time End, time taken =",dur))
        
        print("Third time Start")
        start.time <- Sys.time()
        cacheSolve(temp, mat)
        dur <- Sys.time() - start.time
        print(paste("Third time End, time taken =",dur))
        
        #change the matrix
        mat[1,1] <- 5
        
        print("After change first time Start")
        start.time <- Sys.time()
        cacheSolve(temp, mat)
        dur <- Sys.time() - start.time
        print(paste("After change End, time taken =",dur))
        
        print("After change second time Start")
        start.time <- Sys.time()
        cacheSolve(temp, mat)
        dur <- Sys.time() - start.time
        print(paste("After change second time End, time taken =",dur))
        
        #swap the matrix from the temp
        mat <- tempmat
        
        print("After swap first time Start")
        start.time <- Sys.time()
        cacheSolve(temp,mat)
        dur <- Sys.time() - start.time
        print(paste("After Swap End, time taken =",dur))
        
}

