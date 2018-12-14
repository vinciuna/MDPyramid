# https://rstudio.cloud/project/152401
rm(list = ls())
gc()

pacman::p_load(tidyverse, data.table, stringr, magrittr,glue,gdata)

txt <- read.delim("https://raw.githubusercontent.com/vinciuna/MDPyramid/master/binaryTree_test.txt", stringsAsFactors = F, header = F)
n <- nrow(txt)
dt <- matrix(-Inf, nrow = n, ncol = n)
for (i in 1:n) {
  dt[i,1:i] <- as.numeric(unlist(strsplit(txt[i,], " ")))
}

#----------------------------------------------------------------------------
#data cleansing
dt_clean <- function(x,p) {
  if (p==1){
    x[!(seq(x)%%2 == x%%2)] <- 0} else
    {x[(seq(x)%%2 == x%%2)] <- 0}
  x[x==0] <- -Inf
  x
}

#as.integer customized
as.integer0 <- function(x){
  x <- as.integer(x)
  if (length(x)==0 ) x=-Inf
  else x=x
  x
}

path.sum <- function(dat) {
  #max_s_p <- NULL
  #browser()
  for (ii in nrow(dat):2) {
    path[[ii-1]] <<- as.list(1:(ncol(dat)-1))
    for (jj in 1:(ncol(dat)-1)) {
      max_n <- max(dat[ii,jj:(jj + 1)])
      #hich(dat[ii,jj:(jj + 1)]==max_)
      max_s <- max_n + dat[ii-1, jj]
      dat[ii-1,jj] <- max_s
      #max_s_p <- c(max_s_p, dat[ii-1, jj])
      path[[ii-1]][[jj]] <<- c(max_n,max_s)
    }
    dat[ii,] <- NA
  }
  return( max(dat, na.rm = TRUE))
}

#----------------------------------------------------------------------------
#data setup
  if (n==1) {
    answer <- dt[1,1]
  }  else{
    path <- as.list(1:(nrow(dt)-1))
    par <- dt[[1,1]]%%2
    dt <- as.data.frame(dt) %>%  mutate_all(~dt_clean(.,par))
    answer <- path.sum(dt)
  }
  if (answer==-Inf) {
    cat("  path not exists...\n")
  } else {
    cat("  sum= ",path[[1]][[1]][[2]],"\n",sep="")
    sum_ <- path[[1]][[1]][2]
    n_ <- path[[1]][[1]][1]
    id_ <- 1
    if (length(path)>1){
      cat( "    path: ",sum_-n_,"+",sep="")
      for (i in 2:(n-1) ) {
        sum_ <- map(path[[i]],2) %>% unlist()
        id_ <- which(sum_==n_)
        if (length(id_)>1) {
          id_ <- id_[1]
        }
        n_ <- map(path[[i]],1) %>% unlist() %>% .[id_]
        cat( sum_[id_]-n_ , "+",sep="")
        if (i==(n-1)) {
          cat( n_,"\n")
          }
      }
    } else {
      cat( "    path: ",sum_-n_,"+",n_,sep="")
      }
  }


