## Create a special R object caching an invertible matrix
## and its inverse

## makeCacheMatrix(args): a function to create the special R object, 
## returning a list of 4functions
## set: set the value of the matrix
## get: get the value of the matrix
## set_inv: set the inverse of the matrix
## get_inv: get the inverse of the matrix

## cacheSolve(args): retrive the inverse of a matrix.
## from the special R object "x", created by makeCacheMatrix(args)

## Author: Yuhui Tong
## email: tongyuhui2009@gmail.com
## purpose: coursera R programming, assignment 2




makeCacheMatrix <- function(mat = matrix()) {
	## Return a list of functions for setting/getting 
	# a matrix/its inverse.
	
	mat_inv <- NULL
	
	## set the value of matrix
	set <- function(input_mat){ 		
		mat <<- input_mat
		mat_inv <<- NULL
	}
	
	## get matrix value 
	get <- function() mat 
	
	## set inverse of matrix
	set_inv <- function(mat_inv_value){
		mat_inv <<- mat_inv_value
	}
	  
	get_inv <- function(){
		## get inverse of mat
		return(mat_inv)
	}
	
	function_list <- list(set = set, get = get,
			set_inv = set_inv, get_inv = get_inv)
	return(function_list)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        mat_inv <- x$get_inv()
        mat <- x$get()

        ## if mat_inv is not NULL (set value already),
        ## get value of mat_inv
        if (!is.null(mat_inv)){
			message("getting cached data")
        	return(mat_inv)
        }
        
        ## if mat_inv is NULL (not yet set), set value
        ## for mat_inv
        mat_data <- x$get()
        mat_inv <- solve(mat_data, ...)
        x$set_inv(mat_inv)
        return(mat_inv)       
}
