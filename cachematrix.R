#The MakeCacheMatrix function accepts a square matrix of numbers
#and builds a list of 4 functions set, get, setmatrix and getmatrix


#get returns the matrix mat_1 stored in the main function
#set changes the matrix mat_1 to input y
#setmatrix builds the inverse of the matrix mat_1
#getmartix retrieves the inverted matrix

#a typical call to this function might look like
#       a <- makeCacheMatrix(matrix(c(3, -7,5,2), nrow = 2, ncol = 2))

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() mat_1
        setmatrix <- function(solve) m <<- solve(x)
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
        
}


#This function will retrieve an inverted matrix, or if it doesn't exist, 
#create its own and return that inverted matrix

#a typical call to this function might look like
#               cacheSolve(a)       where a is a square matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
