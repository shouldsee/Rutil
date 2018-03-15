#' Broadcasting an array to a wanted shape
#'
#' Extending a given array to match a wanted shape, similar to repmat.
#' @param arr Array to be broadcasted
#' @param dims Desired output dimensions
#'
#' @return array of dimension dim
#' @examples
#'   M = array(1:12,dim=c(1,2,3))
#'   N = broadcast(M,c(4,2,3))
#'
#' @note # Fri Feb  9 14:41:25 2018 ------------------------------
#' @author Feng Geng (shouldsee.gem@gmail.com)
#' @export
broadcast  <- function(arr, dims){
  DIM = dim(arr)
  EXCLUDE = which(DIM==dims) #### find out the margin that will be excluded
  if (length(EXCLUDE)!=0){
    if( !all(DIM[-EXCLUDE]==1) ){
      errmsg = sprintf("All non-singleton dimensions must be equal: Array 1 [%s] , Wanted shape: [%s]",
                       paste(DIM,collapse = ','),
                       paste(dims,collapse = ','))
      stop(errmsg)
    }
    perm <- c(EXCLUDE, seq_along(dims)[-EXCLUDE])
  }else{
    perm <- c(seq_along(dims))
  }

  arr = array(aperm(arr, perm), dims[perm])
  arr = aperm(arr, order(perm)
              ,resize = T)
}
#' Broadcasting an operation on multi-dimensinoal arrays
#'
#' Broadcasting two arrays to the same shape and then apply FUN.
#'
#' @param arrA,arrB Arrays to perfom the operator FUN(arrA,arrB)
#' @param FUN function to be performed (must be vectorised)
#'
#' @return An array of the shape with each dimension being the maximum of dimA,dimB.
#' @examples
#'
#'   M = array(1:12,dim=c(2,2,3))
#'   N = rbind(c(1,2))
#'   O = bsxfun(M,N,'*')
#' @note
#'
#' # Fri Feb  9 14:41:25 2018 ------------------------------
#'
#' @author Feng Geng (shouldsee.gem@gmail.com)
#' @export
#'
bsxfun <-  function(arrA,arrB,FUN = '+',...){
   FUN <- match.fun(FUN)
   arrA <- as.array(arrA)
   arrB <- as.array(arrB)
   dimA <- dim(arrA)
   dimB <- dim(arrB)
   if (identical(dimA,dimB)){
     #### Trivial scenario
     arr = FUN(arrA,arrB, ...)
     return(arr)
   }

   arrL = list(arrA,arrB)
   orient <- order(sapply(list(dimA,dimB),length))
   # orient <- order(sapply(arrL,length))
   arrL = arrL[orient]
   dim1 <- dim(arrL[[1]])
   dim2 <- dim(arrL[[2]])
   dim1 <- c(dim1, rep(1,length(dim2)-length(dim1))) #### Padding smaller array to be of same length
   dim(arrL[[1]])<-dim1;
   nonS = (dim1!=1) & (dim2!=1)

   if( !all(dim1[nonS]== dim2[nonS])){
     errmsg = sprintf("All non-singleton dimensions must be equal: Array 1 [%s] , Array 2 [%s]",
                      paste(dimA,collapse = ','),
                      paste(dimB,collapse = ','))
     stop(errmsg)
   }
   ### Broadcasting to fill singleton dimensions
   # browser()
   dims  <- pmax(dim1,dim2)
   arrL = lapply(arrL,function(x)broadcast(x,dims))
   arrL = arrL[order(orient)]
   arr = FUN(arrL[[1]],arrL[[2]], ...)
   return(arr)
}



if (interactive()){

  M = array(1:12,dim=c(2,2,3))
  N = rbind(c(1,2))
  O = bsxfun(M,N,'*')

  M = array(1:12,dim=c(2,2,3))
  N = rbind(c(1,2))
  O = bsxfun(N,M,'*')

  ## Not run
  # try({
  #   M = array(1:12,dim=c(2,2,3))
  #   N = rbind(c(1,2,3))
  #   O = bsxfun(M,N,'*')
  # })
  ##
}

#
# {
#   library(microbenchmark)
#   a = c(1,0,3,-4)
#   n_col = 4
#
#   # make example more realistic by expanding number of rows of b
#   nsample = 1e3
#   b = pracma::repmat(matrix(c(0,2,1,9,5,-3,7,4),nrow = 2,ncol = n_col), nsample / 2, 1)
#
#   print(microbenchmark(
#     erg_1 = sweep(b, 2, a, '*'),
#     erg_2 = bsxfun(b,  rbind(a), '*'),
#     erg_3 = pracma::bsxfun('*', pracma::repmat(a, nsample, 1), b)
#   ))
#   # Unit: microseconds
#   # expr      min       lq      mean    median       uq       max neval cld
#   # erg_1   94.002  108.327  129.1773  122.7575  142.660   248.796   100  a
#   # erg_2 1152.270 1212.464 1353.5238 1326.7625 1474.444  1979.061   100  b
#   # erg_3 4744.422 4825.741 5323.7169 4882.8810 4997.707 40836.930   100  c
# }
#
#
#
# {
#   erg_1 = sweep(b, 2, a, '*')
#   erg_2 = bsxfun(b,  rbind(a), '*')
#   erg_3 = pracma::bsxfun('*', pracma::repmat(a, nsample, 1), b)
#   stopifnot(identical(erg_1,erg_2,erg_3))
#
# }

#' @export
expand_dims<-function(arr,before){
  DIM <- dim(arr)
  afters = before - 1
  for (after in afters){
    DIM <- append(DIM,1,after)
  }
  dim(arr) <- DIM
  arr
}


#' @source http://r.789695.n4.nabble.com/array-slice-notation-td902486.html
#' @export
arr_slice <- function(A, idx, axis = 1){
  DIM = dim(A)
  NDIM= length(DIM)
  idxlst = as.list(rep(T, NDIM))
  idxlst[[axis]]= idx
  # idxlst = c(1,T,T)
  do.call("[", c(list(A),idxlst))
}

#' A complicated version that slices along multiple axes
arr_slice.list <- function(A, idx, axis = 1
            # ,setVal =
            ){
  DIM = dim(A)
  NDIM= length(DIM)
  idxlst = as.list(rep(T, NDIM))
  if (is.matrix(idx)){
    listbymar <- function(m,MARGIN=2){
      #### Convert an array to list by a margin
      unlist(apply(m,MARGIN,list),recursive = F)
    }
    idx <- listbymar(idx,2)
  }else if(is.vector(idx)){
    idx = list(idx)
  }else{
    stop()
  }
  idxlst[axis]= idx
  # print(idxlst)
  # idxlst = c(1,T,T)
  do.call("[", c(list(A),idxlst) )
}

#' Fix some weird data.frame
#' @source https://stackoverflow.com/questions/49162079/why-does-array-returns-a-data-frame-as-it-is-in-r
#' @export
array2array <-function(arr,callback=identical){
array(callback(unlist(arr)),dim(arr)  )
}

### Example
# {
#   a = do.call(expand.grid,rep(list(0:2),7))
#   is(a)
#   # [1] "data.frame" "list"       "oldClass"   "vector"
#
#   # b = as.array(a)  #### This does not work
#   b = array(a)
#   is(b)
#   # [1] "data.frame" "list"       "oldClass"   "vector"
#   is.array(b)
#   # TRUE
#   is.data.frame(b)
#   c = array2array(b)
#   is(c)
#   ## [1] "matrix"    "array"     "structure" "vector"
# }

#' @source https://stackoverflow.com/questions/4452039/rs-
#' equivalent-to-ind2sub-sub2ind-in-matlab/4455502
#' @export
sub2ind <- function(r,c,m=NULL,mat=NULL){
  if (is.null(m)){
    if (is.null(mat)){
      stop()
    }
    m <- nrow(mat)
  }
  ind = (c-1)*m + r
}
#### Example
# A = matrix(1:9,3,3)
# rc = cbind(c(1,3),2:1)
# r = rc[,1]
# c = rc[,2]
#
# A[r,c]
# A[sub2ind(mat=A,r,c)]

