# https://rstudio.cloud/project/152401
rm(list = ls())
gc()

pacman::p_load(tidyverse, data.table, stringr, magrittr,glue,gdata)

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
set.seed(123)
#data setup
tests <- 100
output_file_csv <- "binominal_trees"
write(paste0("tests=",tests,"begin",sep=" "),file = output_file_csv,append=F)
for (t in seq(tests)) {
  cat("##  test=",t,":\n")
  write(paste0("test=",t,":\n",sep=" "),file = output_file_csv,append=T)
  n <- sample(1:50,1,replace = T)
  cat("  n=",n,"\n")
  write(paste0("  n=",n,"\n",sep=" "),file = output_file_csv,append=T)
  dt <- matrix(-Inf, ncol = n, nrow = n)
  if (n==1) {answer <- dt[1,1]}
  else{
    for (smpl in seq(n)) {
      dt[smpl,1:smpl] <- c(sample(-99:999,smpl,replace = F))
    }

    path <- as.list(1:(nrow(dt)-1))
    par <- dt[[1,1]]%%2
    dt <- as.data.frame(dt) %>%  mutate_all(~dt_clean(.,par))
    write.fwf(dt,output_file_csv,append = T,sep="\t")
    answer <- path.sum(dt)
  }
  if (answer==-Inf) {
    cat("  path not found...\n")
    write("  path not found...\n" ,file = output_file_csv,append=T)
  } else {
    cat("  sum= ",path[[1]][[1]][[2]],"\n",sep="")
    write(paste0("  sum= ",path[[1]][[1]][[2]],"\n",sep=""),file = output_file_csv,append=T)
    sum_ <- path[[1]][[1]][2]
    n_ <- path[[1]][[1]][1]
    id_ <- 1
    if (length(path)>1){
      cat( "    path: ",sum_-n_,"+",sep="")
      write(paste0("    path: ",sum_-n_,"+",sep=""),file = output_file_csv,append=T)
      for (i in 2:(n-1) ) {
        sum_ <- map(path[[i]],2) %>% unlist()
        id_ <- which(sum_==n_)
        if (length(id_)>1) {
          id_ <- id_[1]
        }
        n_ <- map(path[[i]],1) %>% unlist() %>% .[id_]
        cat( sum_[id_]-n_ , "+",sep="")
        write(paste0( sum_[id_]-n_ , "+",sep=""),file = output_file_csv,append=T)
        if (i==(n-1)) {
          cat( n_,"\n")
          write(paste0( n_,"\n"),file = output_file_csv,append=T)
        }
      }
    } else {
      cat( "    path: ",sum_-n_,"+",n_,sep="")
      write(paste0("    path: ",sum_-n_,"+",n_,sep=""),file = output_file_csv,append=T)
    }
  }
}

