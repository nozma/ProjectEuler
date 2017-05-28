#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int dC(int n) {
  int result = 1;
  if (n <= 1) {
    return 0;
  } else if (n == 2) {
    return 1;
  } else {
    for(int i = 2; i*i <= n; i++){
      if(n % i == 0){
        result += i + n/i;
      } 
    }
  }
  return result;
}

int LIMIT = 10000;

// [[Rcpp::export]]
int getans() {
  int result = 0;
  for(int i = 2; i <= LIMIT; i++){
    int d1 = dC(i);
    int d2 = dC(d1);
    if(i == d2 & i != d1){
      result += i;
    }
  }
  return result;
}
