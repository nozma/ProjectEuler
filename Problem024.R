# sort(sapply(combinat::permn((0:9)[-3]), paste0, collapse=""))[1e6 - factorial(9)*2]
# 11.05966 secs

limit = 1e6
cnt = 1
digits = as.character(0:9)
digp = 1
rest = 9 # 残り桁数
ans = character(0)

while(rest > 0){
  if(limit >= cnt + factorial(rest)){
    cnt = cnt + factorial(rest)
    digp = digp + 1
  } else {
    rest = rest - 1
    ans = c(ans, digits[digp])
    digits = digits[-digp]
    digp = 1
  }
}

paste0(c(ans, digits), collapse="")

# 0.01370597 secs

