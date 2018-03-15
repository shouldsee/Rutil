#' @export
partial <- function(f, ...) {
  'Source: https://stackoverflow.com/questions/32173901/how-to-efficiently-partially-apply-a-function-in-r  by josliber '
  l <- list(...)
  function(...) {
    do.call(f, c(l, list(...)))
  }
}

#' @export
is.installed <- function(mypkg,...){
  'Source: https://stackoverflow.com/questions/9341635/check-for-installed-packages-before-running-install-packages'
  is.element(mypkg, installed.packages(...)[,1])
}


#' @export
#'
install.packages.lazy <- function(PKG0,...){
  PKG = Filter(Negate(is.installed),PKG0)
  if (length(PKG)!=0){
    install.packages(PKG,...)
  }else{
    cat('All packages requirements are met:',paste0(sprintf('"%s"',PKG0),collapse = ','),'\n')
  }
}

#' Multinomial coefficient
#' @description Multinomial coefficient
#' @param N a vector specifies the choice. For example N=c(5,1) specifies choose(6,1)
#' @return The scalar multinomial coefficient
#' @export
ndchoose <- function(N,log = F){
  Ln = length(N)
  Sn = sum(N)
  P = dmultinom(N,size=Sn,prob=rep(1,Ln),log=log)
  if (log){
    P = P + Sn*log(Ln)
  }else{
    P = P * Ln^Sn
  }
}

#' Unlist the "list" columns of a dataframe
#' Unlist the "list" columns of a dataframe, often resulted from as.data.frame(x) if x is a string array
#' @export
unlist.df <- function(df){
  for (j in 1:ncol(df)){
    if(is(df[,j],'list')){
      df[,j] = unlist(df[,j])
    }
  }
  df
}

#' Return matrix after generating the index
upper.tri.get <- function(mat,...){
  mat[upper.tri(mat,...)]
}


#' Obtain equation string from a linear model object
#' @export
lm2eqn <- function(mdl,add.rsq=F){
  # mdl$rsq <- summary(mdl)$r.squared
  with(summary(mdl),
       {
         coef <- round(coefficients,4)
         rsq <- round(r.squared,4)
         if (!add.rsq){
           bquote(
             y==.(coef[1])+ .(coef[2])*x
           )
         }else{
           bquote(
             y==.(coef[1])+ .(coef[2])*x~~','~~R^2==.(rsq)
           )
         }
       }
  )
}

# Invert a matrix with optional normalisation
#' @export
mat.invert <- function(mat,post =identity,invertor = MASS::ginv,as.norm=T){
  iv <- invertor(mat)
  if(as.norm){
    sdg<-sqrt(diag(iv))
    iv <- iv/outer(sdg,sdg,'*')
  }
  post(iv)
}


