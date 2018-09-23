
script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)
devtools::document()
install.packages('.',repos=NULL)
