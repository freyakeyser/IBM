setwd("C:/Users/keyserf/Documents/Data/salmonIBM/runFiles/parameters/")
require(Rcpp)

main <- sourceCpp("C:/Users/keyserf/Documents/Data/salmonIBM/main.cpp") 
                  
sourceCpp(
  //[[Rcpp::export]]
  'R CMD SHLIB -Wno-write-strings main.cpp')


?evalCpp
evalCpp("1+1", showOutput=TRUE)

writeLines(strsplit(Sys.getenv("PATH"), ";")[[1L]])

install.packages("devtools")
require(devtools)

?sourceCpp
