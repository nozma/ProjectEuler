# 文字や数字を反転させる(stringi::stri_reverseで同様のことができる)
strrev <- function(x){
  paste(substring(x, nchar(x):1, nchar(x):1), collapse = "")
}

# エラトステネスの篩
sieve <- function(limit = 1e6, return =  FALSE){
  ## 2i + 1 = nとし、3から開始する
  sieve <- logical((limit-1)/2)         # 1を除く数の半分が上限
  for(i in 1:(floor(sqrt(limit)-1)/2)){
    if(!sieve[i]){
      ## 数nのindexは(n-1)/2で求められる
      ## n = 2i + 1 なので n^2 = 4i^2 + 4i + 1, そのindexは
      ## ((4i^2 + 4i + 1) - 1)/2 = 2i(i + 1)
      ## 数xのindexは(x-1)/2
      ## 数xに2nを足すと、x + 2n = x + 4i + 2、そのindexは
      ## ((x + 4i + 2) - 1)/2 = (x -1  + 4i + 2)/2 で2i+1増えてる
      ## よって2nを足すごとにindexは2i + 1ずつ増える
      sieve[seq(2*i*(i+1), length(sieve), by = 2*i+1)] <- TRUE
    }
  }
  prime <- c(2, (1:length(sieve))[!sieve] * 2 + 1)
  if(return) prime
}
