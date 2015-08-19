##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.

##  1. The makeCacheMatrix function would take the matrix as an argument return a list of matrix function.

makeCacheMatrix <- function(x = matrix()) {
	
	Matrix_Inverse_To_Return <- NULL			# Initialize the Matrix variable to NULL Value so that NULL value can be evaluated in cacheSolve function
							
	setMatrix <- function (Matrix) {			# setMatrix() Function would set the Global matrix
		x <<- Matrix          					# Just an Assignment of the Local Variable Matrix to Parent matrix Variable .		
		Matrix_Inverse_To_Return <<- NULL    	 
	}
	getMatrix <- function () x											# getMatrix() function would just return the matrix 	
	setInverse <- function(solve) Matrix_Inverse_To_Return <<- solve	# setInverse() function would cache or set the inverse matrix
	getInverse <- function() Matrix_Inverse_To_Return					# getInverse() function would read the inverse matrix 
	
	list(getMatrix = getMatrix, setMatrix = setMatrix, setInverse = setInverse, getInverse = getInverse) # Returns the list for the makeCacheMatrix function
}


##  2. The cacheSolve function would take the matrix as an argument created by makeCacheMatrix and would act on the function written in makeCache Matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Matrix_Inverse_To_Return <- x$getInverse()      	# The getInverse() function return value is assigned to a variable Matrix_Inverse_To_Return which would be evaluated in next step
        if(!is.null(Matrix_Inverse_To_Return)) {            	# In this "if" block code Matrix_Inverse_To_Return variable is evaluated to check for NULL value,  
                message("getting cached data")           	# If there is No NULL value then this indicates that the matrix x was inversed,  and the code would read the value from the locale cache
                return(Matrix_Inverse_To_Return)            #  rather than re-computing inverse matrix using Solve function and the cached value is directly returned by the function. 
        }													# # If there is a NULL value then  this indicates that the matrix x was NOT inversed yet, 
        data <- x$getMatrix()                  				# the getMatrix() function is used to read the Matrix in the x matrix object.
        Matrix_Inverse_To_Return <- solve(data, ...)      	# This code computes a Inverse of Matrix using the Solve function and assigns to a Locale variable  Matrix_Inverse_To_Return,  
        x$setInverse(Matrix_Inverse_To_Return)           	# The Matrix_Inverse_To_Return would be the return value for the cacheSolve function.
        Matrix_Inverse_To_Return 
}


##   Validation of the cachematrix.R program in R prompt
##  > source("cachematrix.R")									# Source the function file 
##  > Mat <- matrix (sample(1:27, 9, replace=F), 3,3)			# Initialize a 3 X 3 dimension matrix with sample function that would pick 9 random values between 1 through 27 to Mat variable
##  > print(Mat)    											# Print the Mat variable to display the matrix created and its values
##  > Inverse_Matrix <- cacheSolve(makeCacheMatrix(Mat))		# Use makeCacheMatrix with Mat as an argument to set the Matrix and then use the cacheSolve function to compute the Inverse Matrix
##  > print(Inverse_Matrix)    									# Print the Inverse_Matrix variable to display the inverted matrix values
##  > Back_Mat <- cacheSolve(makeCacheMatrix(Inverse_Matrix))   # Re-run the same function now with inverted matrix values and assign it to a variable Back_Mat
##  > print(Back_Mat)											# Print the Back_Mat variable to display the inverted matrix values and validate if Back_Mat matches the Mat matrix.
