## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(Matrix_From_Global = matrix()) {
	
	Matrix_Inverse_To_Return <- NULL # Initialize the Matrix variable that needs to be inversed in Parent Environment to be returned 
	
	# setMatrix Function would set the Global Matrix
	setMatrix <- function (Matrix) {
		Matrix_From_Global <<- Matrix           # Just an Assignment of the Local Variable Matrix to Parent Variable Matrix.		
		Matrix_Inverse_To_Return <<- NULL     # 
	}
	getMatrix <- function () Matrix_From_Global
	setInverse <- function(solve) Matrix_Inverse_To_Return <<- solve
	getInverse <- function() Matrix_Inverse_To_Return
	
	list(getMatrix = getMatrix, setMatrix = setMatrix, setInverse = setInverse, getInverse = getInverse) # Returns the list for the makeCacheMatrix function
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Matrix_Inverse_To_Return <- x$getInverse
        if(!is.null(Matrix_Inverse_To_Return)) {
                message("getting cached data")
                return(Matrix_Inverse_To_Return)
        }
        data <- x$getMatrix
        Matrix_Inverse_To_Return <- solve(data, ...)
        x$setInverse(Matrix_Inverse_To_Return)
        Matrix_Inverse_To_Return 
}
