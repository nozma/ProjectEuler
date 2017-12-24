start = Sys.time()
#limit <- 20161
limit <- 28123

#temp[i]がiの約数の総和になるようにする
temp <- integer(limit)

for(i in 1:limit){
  temp[(1:(limit %/% i)) * i] <- temp[(1:(limit %/% i)) * i] + i
}
abdnumlist <- (1:limit)[temp > 2 * (1:limit)]

sieve = logical(limit*2)
# ちょうど2つの過剰数で表せる自然数のindexをTRUEに
for(i in seq_along(abdnumlist)){
  sieve[abdnumlist[1:i] + abdnumlist[i]] <- TRUE
}
sieve <- sieve[1:limit]
sum((1:limit)[!sieve])


print(start-Sys.time())
