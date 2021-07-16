
## set matrix properties, get matrix properties after they have been set

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        #set matrix values, inverse values
        set <- function(y){
                x <<- y
                I <- NULL
        }
        #returns the matrix     
        get <- function() x
        
        #returns inverse of matrix
        getinverse <- function() I
        
        #set inverse of matrix
        setinverse <- function(inverse) I <<- inverse
        
        list(set = set, get = get,
        setinverse = setinverse, getinverse = getinverse)

}


## returns inverse of x, computing and caching if doesn't exist

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinverse()
        if(!is.null(I)){
                message('getting cached data')
                return(I)
        }
        #inverse has not been computed yet
        matrix <- x$get()
        I <- solve(matrix, ...)
        x$setinverse(I)
        I
}
