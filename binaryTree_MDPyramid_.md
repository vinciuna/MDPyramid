MDPyramid
================

# MDPyramid Doc

Simple MDPyramid script in R. Delivers possible route with maximum sums.

The task to find the maximum sum of the numbers per the given rules
below:

1.  You will start from the top and move downwards to an adjacent number
    as in below.  
2.  You are only allowed to walk downwards and diagonally.  
3.  You should walk over the numbers as evens and odds subsequently.
    Suppose that you are on an even number the next number you walk must
    be odd, or if you are stepping over an odd number the next number
    must be even. In other words, the final path would be like Odd -\>
    even -\> odd -\> even …  
4.  You must reach to the bottom of the pyramid.

The goal is to find the maximum sum if you walk the path.

  - This is v03
  - added test with random numbers and random matrix sizes

## Code

R code in the document as follows:

``` r
pacman::p_load(tidyverse, data.table, stringr, magrittr,glue,gdata)
txt <- read.delim("https://raw.githubusercontent.com/vinciuna/MDPyramid/master/binaryTree_test.txt", stringsAsFactors = F, header = F)
n <- nrow(txt)
dt <- matrix(-Inf, nrow = n, ncol = n)
for (i in 1:n) {
  dt[i,1:i] <- as.numeric(unlist(strsplit(txt[i,], " ")))
}
```

### Functions:

``` r
#data cleansing
dt_clean <- function(x,p) {
  if (p==1){
    x[!(seq(x)%%2 == x%%2)] <- 0} else
    {x[(seq(x)%%2 == x%%2)] <- 0}
  x[x==0] <- -Inf
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
```

### Data setup and call functions

``` r
#data setup
  if (n==1) {
    answer <- dt[1,1]
  }  else{
    path <- as.list(1:(nrow(dt)-1))
    par <- dt[[1,1]]%%2
    dt <- as.data.frame(dt) %>%  mutate_all(~dt_clean(.,par))
    answer <- path.sum(dt)
  }
```

### Rezults:

``` r
  if (answer==-Inf) {
    cat("  path not found...\n")
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
```

    ##   sum= 8186
    ##     path: 215+192+269+836+805+728+433+528+863+632+931+778+413+310+253

## Testing pack:

``` r
set.seed(123)
tests <- 200
output_file_csv <- "binominal_trees"
write(paste0("tests=",tests,"begin",sep=" "),file = output_file_csv,append=F)
for (t in seq(tests)) {
  cat("#  test=",t,":\n")
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
      cat( "    path: ",sum_-n_,"+",n_,"\n",sep="")
      write(paste0("    path: ",sum_-n_,"+",n_,"\n",sep=""),file = output_file_csv,append=T)
    }
  }
}
```

    ## #  test= 1 :
    ##   n= 15 
    ##   path not found...
    ## #  test= 2 :
    ##   n= 16 
    ##   path not found...
    ## #  test= 3 :
    ##   n= 24 
    ##   path not found...
    ## #  test= 4 :
    ##   n= 1 
    ##   path not found...
    ## #  test= 5 :
    ##   n= 3 
    ##   sum= 1661
    ##     path: 922+121+618 
    ## #  test= 6 :
    ##   n= 41 
    ##   path not found...
    ## #  test= 7 :
    ##   n= 22 
    ##   path not found...
    ## #  test= 8 :
    ##   n= 17 
    ##   path not found...
    ## #  test= 9 :
    ##   n= 16 
    ##   path not found...
    ## #  test= 10 :
    ##   n= 40 
    ##   path not found...
    ## #  test= 11 :
    ##   n= 21 
    ##   path not found...
    ## #  test= 12 :
    ##   n= 48 
    ##   path not found...
    ## #  test= 13 :
    ##   n= 40 
    ##   path not found...
    ## #  test= 14 :
    ##   n= 13 
    ##   path not found...
    ## #  test= 15 :
    ##   n= 43 
    ##   path not found...
    ## #  test= 16 :
    ##   n= 14 
    ##   path not found...
    ## #  test= 17 :
    ##   n= 44 
    ##   path not found...
    ## #  test= 18 :
    ##   n= 7 
    ##   path not found...
    ## #  test= 19 :
    ##   n= 46 
    ##   path not found...
    ## #  test= 20 :
    ##   n= 36 
    ##   path not found...
    ## #  test= 21 :
    ##   n= 29 
    ##   path not found...
    ## #  test= 22 :
    ##   n= 21 
    ##   path not found...
    ## #  test= 23 :
    ##   n= 40 
    ##   path not found...
    ## #  test= 24 :
    ##   n= 22 
    ##   path not found...
    ## #  test= 25 :
    ##   n= 14 
    ##   path not found...
    ## #  test= 26 :
    ##   n= 47 
    ##   path not found...
    ## #  test= 27 :
    ##   n= 25 
    ##   path not found...
    ## #  test= 28 :
    ##   n= 29 
    ##   path not found...
    ## #  test= 29 :
    ##   n= 49 
    ##   path not found...
    ## #  test= 30 :
    ##   n= 24 
    ##   path not found...
    ## #  test= 31 :
    ##   n= 9 
    ##   path not found...
    ## #  test= 32 :
    ##   n= 33 
    ##   path not found...
    ## #  test= 33 :
    ##   n= 37 
    ##   path not found...
    ## #  test= 34 :
    ##   n= 33 
    ##   path not found...
    ## #  test= 35 :
    ##   n= 44 
    ##   path not found...
    ## #  test= 36 :
    ##   n= 7 
    ##   sum= 3047
    ##     path: 292+595+-30+157+346+945+742 
    ## #  test= 37 :
    ##   n= 44 
    ##   path not found...
    ## #  test= 38 :
    ##   n= 36 
    ##   path not found...
    ## #  test= 39 :
    ##   n= 24 
    ##   path not found...
    ## #  test= 40 :
    ##   n= 7 
    ##   path not found...
    ## #  test= 41 :
    ##   n= 48 
    ##   path not found...
    ## #  test= 42 :
    ##   n= 33 
    ##   path not found...
    ## #  test= 43 :
    ##   n= 17 
    ##   path not found...
    ## #  test= 44 :
    ##   n= 3 
    ##   path not found...
    ## #  test= 45 :
    ##   n= 30 
    ##   path not found...
    ## #  test= 46 :
    ##   n= 40 
    ##   path not found...
    ## #  test= 47 :
    ##   n= 49 
    ##   path not found...
    ## #  test= 48 :
    ##   n= 7 
    ##   sum= 4733
    ##     path: 924+869+404+791+466+943+336 
    ## #  test= 49 :
    ##   n= 46 
    ##   path not found...
    ## #  test= 50 :
    ##   n= 13 
    ##   path not found...
    ## #  test= 51 :
    ##   n= 2 
    ##   path not found...
    ## #  test= 52 :
    ##   n= 2 
    ##   sum= 427
    ##     path: 264+163
    ## #  test= 53 :
    ##   n= 17 
    ##   path not found...
    ## #  test= 54 :
    ##   n= 44 
    ##   path not found...
    ## #  test= 55 :
    ##   n= 21 
    ##   path not found...
    ## #  test= 56 :
    ##   n= 18 
    ##   path not found...
    ## #  test= 57 :
    ##   n= 46 
    ##   path not found...
    ## #  test= 58 :
    ##   n= 38 
    ##   path not found...
    ## #  test= 59 :
    ##   n= 34 
    ##   path not found...
    ## #  test= 60 :
    ##   n= 7 
    ##   path not found...
    ## #  test= 61 :
    ##   n= 38 
    ##   path not found...
    ## #  test= 62 :
    ##   n= 49 
    ##   path not found...
    ## #  test= 63 :
    ##   n= 46 
    ##   path not found...
    ## #  test= 64 :
    ##   n= 33 
    ##   path not found...
    ## #  test= 65 :
    ##   n= 37 
    ##   path not found...
    ## #  test= 66 :
    ##   n= 40 
    ##   path not found...
    ## #  test= 67 :
    ##   n= 41 
    ##   path not found...
    ## #  test= 68 :
    ##   n= 47 
    ##   path not found...
    ## #  test= 69 :
    ##   n= 11 
    ##   sum= 6663
    ##     path: 482+447+332+355+698+773+412+911+778+707+768 
    ## #  test= 70 :
    ##   n= 42 
    ##   path not found...
    ## #  test= 71 :
    ##   n= 1 
    ##   path not found...
    ## #  test= 72 :
    ##   n= 44 
    ##   path not found...
    ## #  test= 73 :
    ##   n= 10 
    ##   path not found...
    ## #  test= 74 :
    ##   n= 36 
    ##   path not found...
    ## #  test= 75 :
    ##   n= 42 
    ##   path not found...
    ## #  test= 76 :
    ##   n= 42 
    ##   path not found...
    ## #  test= 77 :
    ##   n= 10 
    ##   sum= 6271
    ##     path: 961+280+969+490+215+916+557+156+805+922 
    ## #  test= 78 :
    ##   n= 46 
    ##   path not found...
    ## #  test= 79 :
    ##   n= 37 
    ##   path not found...
    ## #  test= 80 :
    ##   n= 47 
    ##   path not found...
    ## #  test= 81 :
    ##   n= 45 
    ##   path not found...
    ## #  test= 82 :
    ##   n= 10 
    ##   path not found...
    ## #  test= 83 :
    ##   n= 4 
    ##   path not found...
    ## #  test= 84 :
    ##   n= 49 
    ##   path not found...
    ## #  test= 85 :
    ##   n= 7 
    ##   sum= 4035
    ##     path: 156+885+788+851+438+625+292 
    ## #  test= 86 :
    ##   n= 20 
    ##   path not found...
    ## #  test= 87 :
    ##   n= 45 
    ##   path not found...
    ## #  test= 88 :
    ##   n= 5 
    ##   path not found...
    ## #  test= 89 :
    ##   n= 16 
    ##   path not found...
    ## #  test= 90 :
    ##   n= 26 
    ##   path not found...
    ## #  test= 91 :
    ##   n= 28 
    ##   path not found...
    ## #  test= 92 :
    ##   n= 3 
    ##   sum= 1542
    ##     path: 165+894+483 
    ## #  test= 93 :
    ##   n= 46 
    ##   path not found...
    ## #  test= 94 :
    ##   n= 8 
    ##   path not found...
    ## #  test= 95 :
    ##   n= 1 
    ##   path not found...
    ## #  test= 96 :
    ##   n= 24 
    ##   path not found...
    ## #  test= 97 :
    ##   n= 18 
    ##   path not found...
    ## #  test= 98 :
    ##   n= 23 
    ##   path not found...
    ## #  test= 99 :
    ##   n= 15 
    ##   path not found...
    ## #  test= 100 :
    ##   n= 5 
    ##   path not found...
    ## #  test= 101 :
    ##   n= 21 
    ##   path not found...
    ## #  test= 102 :
    ##   n= 41 
    ##   path not found...
    ## #  test= 103 :
    ##   n= 40 
    ##   path not found...
    ## #  test= 104 :
    ##   n= 3 
    ##   path not found...
    ## #  test= 105 :
    ##   n= 37 
    ##   path not found...
    ## #  test= 106 :
    ##   n= 43 
    ##   path not found...
    ## #  test= 107 :
    ##   n= 20 
    ##   path not found...
    ## #  test= 108 :
    ##   n= 46 
    ##   path not found...
    ## #  test= 109 :
    ##   n= 8 
    ##   path not found...
    ## #  test= 110 :
    ##   n= 8 
    ##   path not found...
    ## #  test= 111 :
    ##   n= 10 
    ##   path not found...
    ## #  test= 112 :
    ##   n= 6 
    ##   sum= 3797
    ##     path: 714+357+924+91+994+717 
    ## #  test= 113 :
    ##   n= 42 
    ##   path not found...
    ## #  test= 114 :
    ##   n= 3 
    ##   sum= 1870
    ##     path: 711+296+863 
    ## #  test= 115 :
    ##   n= 41 
    ##   path not found...
    ## #  test= 116 :
    ##   n= 14 
    ##   path not found...
    ## #  test= 117 :
    ##   n= 11 
    ##   path not found...
    ## #  test= 118 :
    ##   n= 23 
    ##   path not found...
    ## #  test= 119 :
    ##   n= 32 
    ##   path not found...
    ## #  test= 120 :
    ##   n= 39 
    ##   path not found...
    ## #  test= 121 :
    ##   n= 39 
    ##   path not found...
    ## #  test= 122 :
    ##   n= 10 
    ##   path not found...
    ## #  test= 123 :
    ##   n= 41 
    ##   path not found...
    ## #  test= 124 :
    ##   n= 31 
    ##   path not found...
    ## #  test= 125 :
    ##   n= 2 
    ##   sum= 1165
    ##     path: 425+740
    ## #  test= 126 :
    ##   n= 7 
    ##   path not found...
    ## #  test= 127 :
    ##   n= 33 
    ##   path not found...
    ## #  test= 128 :
    ##   n= 23 
    ##   path not found...
    ## #  test= 129 :
    ##   n= 34 
    ##   path not found...
    ## #  test= 130 :
    ##   n= 42 
    ##   path not found...
    ## #  test= 131 :
    ##   n= 14 
    ##   path not found...
    ## #  test= 132 :
    ##   n= 10 
    ##   path not found...
    ## #  test= 133 :
    ##   n= 5 
    ##   sum= 3176
    ##     path: 358+781+806+299+932 
    ## #  test= 134 :
    ##   n= 36 
    ##   path not found...
    ## #  test= 135 :
    ##   n= 31 
    ##   path not found...
    ## #  test= 136 :
    ##   n= 37 
    ##   path not found...
    ## #  test= 137 :
    ##   n= 25 
    ##   path not found...
    ## #  test= 138 :
    ##   n= 6 
    ##   path not found...
    ## #  test= 139 :
    ##   n= 8 
    ##   sum= 5370
    ##     path: 487+868+895+882+901+864+445+28 
    ## #  test= 140 :
    ##   n= 27 
    ##   path not found...
    ## #  test= 141 :
    ##   n= 21 
    ##   path not found...
    ## #  test= 142 :
    ##   n= 15 
    ##   path not found...
    ## #  test= 143 :
    ##   n= 5 
    ##   path not found...
    ## #  test= 144 :
    ##   n= 50 
    ##   path not found...
    ## #  test= 145 :
    ##   n= 23 
    ##   path not found...
    ## #  test= 146 :
    ##   n= 21 
    ##   path not found...
    ## #  test= 147 :
    ##   n= 42 
    ##   path not found...
    ## #  test= 148 :
    ##   n= 21 
    ##   sum= 10767
    ##     path: 191+362+641+290+-59+620+753+136+319+820+767+516+799+734+613+788+-51+500+429+808+791 
    ## #  test= 149 :
    ##   n= 50 
    ##   path not found...
    ## #  test= 150 :
    ##   n= 38 
    ##   path not found...
    ## #  test= 151 :
    ##   n= 32 
    ##   path not found...
    ## #  test= 152 :
    ##   n= 43 
    ##   path not found...
    ## #  test= 153 :
    ##   n= 22 
    ##   path not found...
    ## #  test= 154 :
    ##   n= 17 
    ##   path not found...
    ## #  test= 155 :
    ##   n= 47 
    ##   path not found...
    ## #  test= 156 :
    ##   n= 7 
    ##   sum= 3191
    ##     path: 12+465+578+873+730+631+-98 
    ## #  test= 157 :
    ##   n= 35 
    ##   path not found...
    ## #  test= 158 :
    ##   n= 45 
    ##   path not found...
    ## #  test= 159 :
    ##   n= 15 
    ##   path not found...
    ## #  test= 160 :
    ##   n= 8 
    ##   path not found...
    ## #  test= 161 :
    ##   n= 28 
    ##   path not found...
    ## #  test= 162 :
    ##   n= 43 
    ##   path not found...
    ## #  test= 163 :
    ##   n= 39 
    ##   path not found...
    ## #  test= 164 :
    ##   n= 6 
    ##   path not found...
    ## #  test= 165 :
    ##   n= 38 
    ##   path not found...
    ## #  test= 166 :
    ##   n= 24 
    ##   path not found...
    ## #  test= 167 :
    ##   n= 11 
    ##   path not found...
    ## #  test= 168 :
    ##   n= 10 
    ##   path not found...
    ## #  test= 169 :
    ##   n= 20 
    ##   path not found...
    ## #  test= 170 :
    ##   n= 15 
    ##   path not found...
    ## #  test= 171 :
    ##   n= 23 
    ##   path not found...
    ## #  test= 172 :
    ##   n= 34 
    ##   path not found...
    ## #  test= 173 :
    ##   n= 18 
    ##   path not found...
    ## #  test= 174 :
    ##   n= 37 
    ##   path not found...
    ## #  test= 175 :
    ##   n= 48 
    ##   path not found...
    ## #  test= 176 :
    ##   n= 40 
    ##   path not found...
    ## #  test= 177 :
    ##   n= 49 
    ##   path not found...
    ## #  test= 178 :
    ##   n= 45 
    ##   path not found...
    ## #  test= 179 :
    ##   n= 20 
    ##   path not found...
    ## #  test= 180 :
    ##   n= 49 
    ##   path not found...
    ## #  test= 181 :
    ##   n= 29 
    ##   path not found...
    ## #  test= 182 :
    ##   n= 31 
    ##   path not found...
    ## #  test= 183 :
    ##   n= 30 
    ##   path not found...
    ## #  test= 184 :
    ##   n= 26 
    ##   path not found...
    ## #  test= 185 :
    ##   n= 7 
    ##   sum= 3354
    ##     path: 611+712+817+532+633+108+-59 
    ## #  test= 186 :
    ##   n= 26 
    ##   path not found...
    ## #  test= 187 :
    ##   n= 18 
    ##   path not found...
    ## #  test= 188 :
    ##   n= 5 
    ##   sum= 2178
    ##     path: 368+473+678+361+298 
    ## #  test= 189 :
    ##   n= 4 
    ##   path not found...
    ## #  test= 190 :
    ##   n= 12 
    ##   path not found...
    ## #  test= 191 :
    ##   n= 37 
    ##   path not found...
    ## #  test= 192 :
    ##   n= 19 
    ##   path not found...
    ## #  test= 193 :
    ##   n= 27 
    ##   path not found...
    ## #  test= 194 :
    ##   n= 15 
    ##   path not found...
    ## #  test= 195 :
    ##   n= 31 
    ##   path not found...
    ## #  test= 196 :
    ##   n= 14 
    ##   path not found...
    ## #  test= 197 :
    ##   n= 12 
    ##   sum= 6614
    ##     path: 953+514+439+262+977+878+819+604+753+150+-49+314 
    ## #  test= 198 :
    ##   n= 16 
    ##   path not found...
    ## #  test= 199 :
    ##   n= 32 
    ##   path not found...
    ## #  test= 200 :
    ##   n= 47 
    ##   path not found...
