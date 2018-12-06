# https://rstudio.cloud/project/152401
rm(list = ls())
gc()

pacman::p_load(tidyverse, data.table, stringr, magrittr,glue,data.tree,treemap)

file <- "https://raw.githubusercontent.com/vinciuna/MDPyramid/master/binaryTree_test.txt"
dt <- readLines(con = file)
dt
max <- dt %>% map_int(.,~str_count(.,"\\s+")) %>% max()
dt %<>% map_chr(., ~str_c(., str_c(rep(" 0",max-str_count(.,"\\s")),collapse = ""),sep=""))
dt <- str_split(dt,"\\s")
dt <- pmap_dfc(dt,rbind) %>% mutate_all(.,as.integer)

#----------------------------------------------------------------------------
#data cleansing
dt_clean <- function(x,p) {
  if (p==1){
    x[!(seq(x)%%2 == x%%2)] <- 0} else
    {x[(seq(x)%%2 == x%%2)] <- 0}
  x[x==0] <- -Inf
  x
}
#binary tree elements grouping
split2 <- function(v, seg.length, seg.over) {
  if (length(v)< seg.length){return(v) }
  else{
    starts = seq(1, length(v)-1, by=seg.length-seg.over)
    ends = starts + seg.length - 1
    ends[ends > length(v)] = length(v)
    lapply(1:length(starts), function(i) v[starts[i]:ends[i]])
  }
}
#summing vectors
fadd <- function(v1,v2){
  sums=v1+v2
  max=max(sums)
  maxid=which(sums==max)
  adds=list(v1[v1!=-Inf], v2[v2!=-Inf])
  if (max!=-Inf){
    list(max=max,
         maxid=maxid,
         sums=sums[sums!=-Inf],
         adds=adds)
  } else return(-Inf)
}
#as.integer customized
as.integer0 <- function(x)
{
  x <- as.integer(x)
  if (length(x)==0 ) x=-Inf
  else x=x
  x
}
#----------------------------------------------------------------------------

#data setup
par <- dt[[1,1]]%%2
dt %<>%  mutate_all(~dt_clean(.,par))
dt

n <- nrow(dt)
x2 <- dt[n,] %>% as.numeric(.)
lc <- seq(n)
path <- as.list(lc); p_ <- NULL

for (i in (n-1):1 ) {
  x2 %<>% split2(.,2,1)
  lc %<>% split2(.,2,1)
  x1 <- dt[i,] %>% as.numeric(.)
  path[[i]] <- as.list(x=1:length(x2))
  for (j in 1:length(x2) ){
    add_ <- fadd(x1[j],x2[[j]])
    if (is.list(add_)) {
      lc[[j]] <- j+add_$maxid-1
      path[[i]][[j]] <- list(set_=j,
                             nodes_= c(x2[[j]][[add_$maxid]],x1[j]),
                             sum_=add_$max )
      x2[[j]] <- add_$max
    } else {
      lc[[j]] <- -Inf
      x2[[j]] <- -Inf
    }
  }
  x2 %<>% unlist()
}


cat("sum=",path[[1]][[1]]$sum_)
sum.l <- path[[1]][[1]]$nodes_[1]
p_[1] <- path[[1]][[1]]$nodes_[2]

for (i in 2:(n-1) ) {
  sums <- map(path[[i]],"sum_") %>% map(as.integer0) %>% unlist()
  j <- which(sums==sum.l)
  p_[i] <- map(path[[i]],"nodes_") %>% .[[j]] %>% .[2]
  #cat(p_,"+")
  sum.l <- path[[i]][[j]]$nodes_[1]
}
p_[n] <- map(path[[i]],'nodes_') %>% .[[j]] %>% .[1]

glue("path: {paste0(p_,collapse = ' + ')}")

