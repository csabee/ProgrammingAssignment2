## Matrix cache creation for calculating inverse operations. 
## Basically, matrix inversion is an expensive operation, it should be a 
## great benefit to store the once calculated inverse, and store it in 
## memory cache, instead of recalculating it, every time, the solve() function 
## is called.
##
## usage: create cacheable matrix: 
##      my_matrix <- makeCacheMatrix(matrix(1:4), 2, 2)
## 
## reading the matrix contents:
##      my_matrix$get()
## modifying the matrix:
##      my_matrix$set(matrix(c(2,2,1,4),2,2))
## reading the inverse stored in the cache:
##      my_matrix$getInverse()
##          This function will give NULL, if the inverse has not been calculated
##          before. Else, it will give the inverse.
## setting the inverse of the matrix:
##      my_matrix$setInverse(solve(my_matrix$get()))
##
##
makeCacheMatrix <- function(mymatrix = matrix()) {
    #Safety check: quadratic matrix was determined
    if(dim(mymatrix)[1] != dim(mymatrix)[2]) stop("Matrix is not quadratic.")
    
    det <- determinant( mymatrix )$modulus[1]
    
    #Safety check: the determinant of the matrix is not 0
    if( det == 0 ) stop("Matrix is singular (determinant=0).")
    
    inv <- NULL
    set <- function(y) {
        mymatrix <<- y
        inv <<- NULL
    }
    get <- function() mymatrix
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Calculate the inverse of a matrix. 
## The function will look for values stored in the cache, and return with them.
## If they don't exist, it will calculate the inverse of the matrix (if it is 
## possible), and store it in cache.
##
## Usage: 
##      my_matrix <- makeCacheMatrix(matrix(1:4), 2, 2)
##      cacheSolve(my_matrix)
##
##
cacheSolve <- function(mymatrix){
    inv <- mymatrix$getInverse()
    
    #Cache check
    if(!is.null(inv)){
        message("Providing matrix inverse from cache.")
        return(inv)
    }
    
    data <- mymatrix$get()
    inv <- solve(data)
    mymatrix$setInverse(inv)
    inv
}