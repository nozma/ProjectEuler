# Problem 1: Multiples of 3 and 5 ----
x <- 1:999
sum(x[x %% 3 == 0 | x %% 5 == 0])

# Problem 2: Even Fibonacci numbers ----
library(gmp)
result <- 0
i <- 1
while((fn <- fibnum(i)) < 4000000){
  if(fn %% 2 == 0) result <- result + fn
  i <- i + 1
}
result

# Problem 3: Largest prime factor ----
max(factorize(600851475143))

# Problem 4: Largest palindrome product ----
library("stringi")
is.palindrome <- function(x){
  x[1] == stri_reverse(x[1])
}
for(i in (999*999):(100:100)){
  if(is.palindrome(i)){
    cond <- i %% 999:100 == 0
    if(any(cond)){
      if(i / (999:100)[cond][1] < 1000) break
    }
  }
}
i

# Problem 5: Smallest multiple ----
library(gmp)
target <- factorialZ(20)
for(i in 20:2){
  repeat{
    if(sum(mod.bigz(target/i, 1:20)) == 0){
      target <- target / i
    } else {
      break
    }
  }
}
target

# Problem 6: Sum square difference ----
sum(1:100)^2 - sum((1:100)^2)

# Problem 7: 10001st prime ----
library(gmp)
p <- 2
for(i in 1:10000) p <- nextprime(p)
p

# Problem 8: Largest product in a series ----
num_string <- 
"73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450"
num_string <- gsub("\n", "", num_string)
max_prod <- 0
nchar(num_string) - 12
for(i in 0:(nchar(num_string) - 12)){
  if(max_prod < (tmp <- prod(as.numeric(unlist(strsplit(substring(num_string, i, i + 12), split = "")))))){
    max_prod <- tmp
  }
}
max_prod


# Problem 9: Special Pythagorean triplet ----
library(labeledLoop)
a <- 1
A %._.% for(a in 1:1000){
  for(b in a:1000){
    if(2*(a + b) -(a*b/500) == 1000) ._.(A)
  }
}
a * b * (sqrt(a^2 + b^2))

# Problem 10: Summation of primes ----
sieve <- function(limit = 1e6, return =  FALSE){
  sieve <- logical((limit-1)/2)
  for(i in 1:(floor(sqrt(limit)-1)/2)){
    if(!sieve[i]){
      sieve[seq(2*i*(i+1), length(sieve), by = 2*i+1)] <- TRUE
    }
  }
  prime <- c(2, (1:length(sieve))[!sieve] * 2 + 1)
  if(return) prime
}
sum(sieve(2e6, return = TRUE))
