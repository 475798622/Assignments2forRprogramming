#用于创建可缓存逆矩阵的特殊“矩阵”对象
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL#把i赋值为NULL
        set <- function(y) {
                x <<- y#set(y)后，x=y,i=null
                i <<- NULL
        }
        get <- function() x#把x赋值给get
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
#用于计算上述makeCacheMatrix返回的特殊“矩阵”的逆矩阵。
#如果已经计算逆矩阵（且尚未更改矩阵），那么cachesolve将检索缓存中的逆矩阵。
cacheSolve <- function(x, ...) {
        i <- x$getinverse()#首先把getinverse赋值给i，如果i不是NULL的话弹出下列提示
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }#出现缓存值
        data <- x$get()#把最开始的numberic向量赋值给data
        i <- solve(data, ...)
        x$setinverse(i)#缓存i
        i
}
