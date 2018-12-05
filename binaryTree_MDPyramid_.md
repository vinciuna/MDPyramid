MDPyramid
================

MDPyramid Doc
-------------

Simple MDPyramid script in R. Delivers 2 possible routes with maximum sums.

The task to find the maximum sum of the numbers per the given rules below:

1.  You will start from the top and move downwards to an adjacent number as in below.
2.  You are only allowed to walk downwards and diagonally.
3.  You should walk over the numbers as evens and odds subsequently. Suppose that you are on an even number the next number you walk must be odd, or if you are stepping over an odd number the next number must be even. In other words, the final path would be like Odd -&gt; even -&gt; odd -&gt; even …
4.  You must reach to the bottom of the pyramid.

The goal is to find the maximum sum if you walk the path.

Pros: \* This is v01- simple approach, without recursive functions. Code loops over the lines and looking for best 2 sums.
\* As it track for maximum 2 possible routes with maximum sums, it doesn\`t encouters if boths routes will drop \[face not allowed jumps\] due merged way.

Code
----

R code in the document as follows:

``` r
pacman::p_load(tidyverse, data.table, stringr, magrittr,glue,data.tree,treemap)
file <- "https://raw.githubusercontent.com/vinciuna/MDPyramid/master/binaryTree_test.txt"
dt <- readLines(con = file)
dt
```

    ##  [1] "215"                                                        
    ##  [2] "192 124"                                                    
    ##  [3] "117 269 442"                                                
    ##  [4] "218 836 347 235"                                            
    ##  [5] "320 805 522 417 345"                                        
    ##  [6] "229 601 728 835 133 124"                                    
    ##  [7] "248 202 277 433 207 263 257"                                
    ##  [8] "359 464 504 528 516 716 871 182"                            
    ##  [9] "461 441 426 656 863 560 380 171 923"                        
    ## [10] "381 348 573 533 448 632 387 176 975 449"                    
    ## [11] "223 711 445 645 245 543 931 532 937 541 444"                
    ## [12] "330 131 333 928 376 733 017 778 839 168 197 197"            
    ## [13] "131 171 522 137 217 224 291 413 528 520 227 229 928"        
    ## [14] "223 626 034 683 839 052 627 310 713 999 629 817 410 121"    
    ## [15] "924 622 911 233 325 139 721 218 253 223 107 233 230 124 233"

``` r
max <- dt %>% map_int(.,~str_count(.,"\\s+")) %>% max()
dt %<>% map_chr(., ~str_c(., str_c(rep(" 0",max-str_count(.,"\\s")),collapse = ""),sep=""))
dt <- str_split(dt,"\\s")
dt <- pmap_dfc(dt,rbind) %>% mutate_all(.,as.integer)
```

Functions:

``` r
#data cleansing
dt_clean <- function(x) {
  x[!(seq(x)%%2 == x%%2)] <- 0
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
#
```

Data setup: \* cleaning table with impossible

``` r
#data setup
dt %<>%  mutate_all(~dt_clean(.))
dt
```

    ## # A tibble: 15 x 15
    ##       V1    V2    V3    V4    V5    V6    V7    V8    V9   V10   V11   V12
    ##    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ##  1   215  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf
    ##  2   192   124  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf
    ##  3   117   269  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf
    ##  4   218   836  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf
    ##  5  -Inf   805  -Inf   417   345  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf
    ##  6  -Inf  -Inf   728  -Inf  -Inf   124  -Inf  -Inf  -Inf  -Inf  -Inf  -Inf
    ##  7  -Inf  -Inf   277   433   207   263   257  -Inf  -Inf  -Inf  -Inf  -Inf
    ##  8  -Inf   464   504   528   516   716  -Inf   182  -Inf  -Inf  -Inf  -Inf
    ##  9   461   441  -Inf  -Inf   863  -Inf  -Inf   171   923  -Inf  -Inf  -Inf
    ## 10  -Inf   348  -Inf  -Inf   448   632  -Inf   176  -Inf  -Inf  -Inf  -Inf
    ## 11   223   711   445   645   245   543   931  -Inf   937   541  -Inf  -Inf
    ## 12   330  -Inf  -Inf   928   376  -Inf  -Inf   778  -Inf   168  -Inf  -Inf
    ## 13   131   171  -Inf   137   217  -Inf   291   413  -Inf  -Inf   227   229
    ## 14  -Inf   626    34  -Inf  -Inf    52  -Inf   310  -Inf  -Inf  -Inf  -Inf
    ## 15  -Inf  -Inf   911   233   325   139   721  -Inf   253   223   107   233
    ## # ... with 3 more variables: V13 <dbl>, V14 <dbl>, V15 <dbl>

``` r
n <- nrow(dt)
xs <- x1 <- dt[1,] %>% as.numeric(.)
path <- as.list(1:n)
lc1 <- which(x1!=-Inf)
path[[1]] <- rep(x1[lc1],2)
```

Looping over nodes:

``` r
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
```

Rezults:
========

``` r
glue("path1: MAX sum={map(path,1) %>% unlist() %>% sum()} \t adds={map(path,1) %>% unlist() %>% paste0(.,collapse = '+')}
     path2: MAX sum={map(path,2) %>% unlist() %>% sum()} \t adds={map(path,2) %>% unlist() %>% paste0(.,collapse = '+')}")
```

    ## path1: MAX sum=8186   adds=215+192+269+836+805+728+433+528+863+632+931+778+413+310+253
    ## path2: MAX sum=8118   adds=215+124+269+836+805+728+433+528+863+632+931+778+413+310+253
