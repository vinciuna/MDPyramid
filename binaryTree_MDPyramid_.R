# https://rstudio.cloud/project/152401
rm(list = ls())
gc()

pacman::p_load(tidyverse, data.table, stringr, magrittr,glue,data.tree,treemap)

file <- "https://raw.githubusercontent.com/vinciuna/MDPyramid/master/binaryTree_test.txt"
dt <- readLines(con = file)
max <- dt %>% map_int(.,~str_count(.,"\\s+")) %>% max()
dt %<>% map_chr(., ~str_c(., str_c(rep(" 0",max-str_count(.,"\\s")),collapse = ""),sep=""))
dt <- str_split(dt,"\\s")
dt <- pmap_dfc(dt,rbind) %>% mutate_all(.,as.integer)
dt

#----------------------------------------------------------------------------
#data cleansing
dt_clean <- function(x,p) {
  if (p==1){
  x[!(seq(x)%%2 == x%%2)] <- 0} else
  {x[(seq(x)%%2 == x%%2)] <- 0}
  x[x==0] <- -Inf
  x
}
#summing vectors
vsum <- function(jj){
  #if (v1)
  v1 <- xs[ii]; v2 <- c(x2[jj],x2[jj+1])
  sums=v1+v2
  max=max(sums)
  adds=list(v1, v2)
  #browser()
  if (max!=-Inf){
    list(max=max,
         sums=sums,
         adds=adds)
  } else return(-Inf)

}
#----------------------------------------------------------------------------

#data setup
par <- dt[[1,1]]%%2
dt %<>%  mutate_all(~dt_clean(.,par))
dt
n <- nrow(dt)
x1 <- dt[1,] %>% as.numeric(.)
xs <-x1
path <- as.list(1:n)
lc1 <- which(x1!=-Inf)
path[[1]] <- rep(x1[lc1],2)

#loop over nodes
for (i in 2:n) {
  x2 <- dt[i,] %>% as.numeric(.)
  lc2 <- which(x2!=-Inf)
  lc2 <- lc2[lc2 %in% c(lc1,max(lc1)+1)]
  ii <- 1
  for (j in lc1) {
    rez <- vsum(j)
    kmax <- which(rez$sums==rez$max)
    if( (max(xs[ii])<rez$max)&(length(lc1)>1) ) {
      xs[ii] <- rez$max
      lc0[ii] <- j+kmax-1
      path[[i]][[ii]] <- rez$adds[[2]][kmax]
    } else {
      xs <- rez$sums
      lc0 <- lc2
      path[[i]] <- rez$adds[[2]]
      }
    ii <- ii+1
  }

  x1 <- x2
  lc1 <- lc0
}

glue("path1: MAX sum={map(path,1) %>% unlist() %>% sum()} \t adds={map(path,1) %>% unlist() %>% paste0(.,collapse = '+')}
     path2: MAX sum={map(path,2) %>% unlist() %>% sum()} \t adds={map(path,2) %>% unlist() %>% paste0(.,collapse = '+')}")

pathTree <- data.frame(path=c(map(path,1) %>% unlist()))
pathTree$pathString <- paste("root",
                             pathTree$path,
                            sep = "/")
pathTree <- as.Node(path)
print(pathTree)



print(pathTree)
plot(pathTree)


x <- getURL(, ssl.verifypeer = FALSE)

eval(parse(text = x))
