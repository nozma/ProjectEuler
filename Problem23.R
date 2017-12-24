
start = Sys.time()
# limit = 20161
limit = 28123
abdnumlist <- (1:limit)[sapply(1:limit, function(n) sum((1:n)[n %% (1:n) == 0]) - n) > 1:limit]

sumofadb <- function(n){
  low = 1
  high = length(abdnumlist)
  sum = abdnumlist[low] + abdnumlist[high]
  while(low < high && sum != n){
    if(sum > n){
      high <- high - 1
    } else {
      low <- low + 1
    }
    sum = abdnumlist[low] + abdnumlist[high]
  }
  return(sum == n)
}
sum((1:limit)[!sapply(1:limit, sumofadb)])
end = Sys.time()

print(start - end)


start = Sys.time()
limit<-20161

#temp[i]がiの約数の総和になるようにする
temp <- rep(0, limit)
for(i in 1:limit){
  temp[(1:(limit %/% i)) * i] <- temp[(1:(limit %/% i)) * i] + i
}
abdnumlist <- (1:limit)[temp > 2 * (1:limit)]

#x1はちょうど1個の過剰数の和で書ける数の集合
#x2はちょうど2個の過剰数の和で書ける数の集合
x <- numeric(0)
for(i in seq_along(abdnumlist)){
  x <- union(x, abdnumlist[1:i] + abdnumlist[i])
}
?Reduce
Reduce(function(x=numeric(0), i){c(x, abdnumlist[1:i] + abdnumlist[i])},
       1:1000)

sieve = logical(limit*2)
for(i in seq_along(abdnumlist)){
  sieve[abdnumlist[1:i]+abdnumlist[i]] <- TRUE
}
sieve <- sieve[1:limit]
sum((1:limit)[!sieve])

sum(1:limit) - sum(x[x <= limit])
print(start-Sys.time())