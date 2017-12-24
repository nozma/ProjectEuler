# Ploblem 21 Amicable numbers
# d(n)をnの真の約数(n以外の約数)の和とする。
# d(a) = bかつd(b) = a (a != b)のとき、aとbは友愛数であるとする。
# 10000未満の友愛数の和を求めよ

# constant ----
limit = 10000

# function ----
d <- function(n){
  if(n == 1) return(0)
  if(n == 2) return(1)
  sum((1:(n-1))[n %% (1:(n-1)) == 0])
}

d <- function(n){
  result = 1
  for(i in 2:sqrt(n)){
    if(n %% i == 0){
      result = result + i + n/i
    }
    if(i * i == n) {
      result = result - i
    }
  }
  return(result)
}

Rcpp::sourceCpp("Problem21/sumfct.cpp")

# solve ----
f1 <- function(){
  result <- 0
  for(i in 2:(limit)){
    d1 <- d(i)
    d2 <- d(d1)
    if(i == d2 && i != d1){
      result <- result + i
    }
  }
  return(result)
}

f2 <- function(){
  result <- 0
  for(i in 2:limit){
    d1 <- dC(i)
    d2 <- dC(d1)
    if(i == d2 && i != d1){
      result <- result + i
    }
  }
  return(result)
}

microbenchmark::microbenchmark(f1())
microbenchmark::microbenchmark(f2())
microbenchmark::microbenchmark(getans())
