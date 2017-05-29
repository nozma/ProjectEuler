library(stringi)
f <- function(){
  s <- scan("Problem22/p022_names.txt", what = character(), sep = ",",
            na.strings = "")
  s <- sort(s)
  
  letters_table <- seq_along(LETTERS)
  names(letters_table) <- LETTERS
  
  result <- 0
  for(i in 1:length(s)){
    result <- result +
      sum(letters_table[stri_split_boundaries(s[i], type="character")[[1]]])*i
  }
  result
}
f()
microbenchmark::microbenchmark(f())

