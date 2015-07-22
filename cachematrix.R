#Functions which demonstrate ability caching data requiring much time to compute. File consists of two functions with the detailed description given below. 

##Functions should be used as follows. Firstly, the matrix mast be created, for example:
#> matrixData = matrix(2,2,2)
#> matrixData[1] = 1
#then the function makeCacheMatrix is called and returned value if assigned to a variable
#> matrixCache = makeCacheMatrix(matrixData)
#finally, obtained variable is given as an argument to the function cacheSolve()
#> cacheSolve(matrixCache)
#if the inverted is not cached it's computed and cached
#      [,1] [,2]
#[1,]   -1  1.0
#[2,]    1 -0.5

#otherwise, the inverted matrix is read from cache and the message informs about this
#Inverse matrix is already computed. Getting from cache
#     [,1] [,2]
#[1,]   -1  1.0
#[2,]    1 -0.5


## Write a short comment describing this function
#The function makeCacheMatrix creates an object which contains 4 functions to
# set and get the matrix 
# set and get the inverted matrix 
#The assignment requires only two last function but for consistency we implemented all four. 
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	setMatrix <- function(y){
		x <<- y
		inv <<- NULL
	}
	
	getMatrix <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv 
	list(setMatrix = setMatrix, getMatrix = getMatrix, 
		setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
#The value computes the inversion of the object created by means of the function "makeCacheMatrix". If the inverted matrix was already computed its value is taken from cache otherwise, function solve is used.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
		if (!is.null(inv))
			{message("Inverse matrix is already computed. Getting from cache")
			return(inv)
		}
		matrixToInvert <- x$getMatrix()
		inv <- solve(matrixToInvert,...)
		x$setInverse(inv)
		inv
		
}
