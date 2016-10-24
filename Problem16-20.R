# Problem 16: Power digit sum ----
library(gmp)
n <- as.character(pow.bigz(2, 1000))
sum(as.numeric(substring(n, 1:nchar(n), 1:nchar(n))))

# Problem 17: Number letter counts ----
n_l <- numeric(1000)
one_nine <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
target <- 1:9 + rep(seq(0, 90, 10)[-2], rep(9, 9)) + rep(seq(0, 900, 100), rep(81, 10))
n_l[target] <- n_l[target] + nchar(one_nine)

ten_and_teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
                   "sixteen", "seventeen", "eighteen", "nineteen")
target <- 10:19 + rep(rep(seq(0, 900, 100)), rep(10, 10))
n_l[target] <- n_l[target] + nchar(ten_and_teens)

tys <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
target <- 20:99 + rep(seq(0, 900, 100), rep(length(20:99), 10))
n_l[target] <- n_l[target] + nchar(rep(tys, rep(10, 8)))

hundreds <- paste(one_nine, "hundredand", sep = "")
target <- 100:999
n_l[target] <- n_l[target] + nchar(rep(hundreds, 100, 9))

# remove "and"
target <- seq(100, 900, 100)
n_l[target] <- n_l[target] - 3

# "one thousand"
n_l[1000] <- 11

sum(n_l)

# Problem 18: Maximum path sum I ----
tri <- read.table("tri_18.txt", sep = " ", fill = TRUE, col.names = 1:15)
for(i in 14:1){
  for(j in 1:i){
    tri[i, j] <- tri[i, j] + max(tri[i+1, j], tri[i+1, j+1])
  }
}
tri[1,1]

# Problem 19: Counting Sundays ----
d <- 366 # d %% 7 == 0 : sunday
cnt <- 0
for(y in 1901:2000){
  for(m in 1:12){
    if(d %% 7 == 0) cnt <- cnt + 1
    if(m %in% c(4, 6, 9, 11)) d <- d + 30
    if(m %in% c(1, 3, 5, 7, 8, 10, 12)) d <- d + 31
    if(m == 2){
      if(y %% 4 != 0 || (y %% 400 != 0 && y %% 100 == 0)){ 
        d <- d + 28
      } else {
        d <- d + 29
      }
    }
  }
}
cnt

# Problem 20: Factorial digit sum ----
library(gmp)
sum(as.numeric(unlist(strsplit(as.character(factorialZ(100)), split = ""))))
