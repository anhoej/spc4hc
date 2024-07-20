a3 <- function(n) {
  3 / (c4(n) * sqrt(n))
}

b3 <- function(n) {
  pmax(0, 1 - 3 * c5(n) / c4(n))
}

b4 <- function(n) {
  1 + 3 * c5(n) / c4(n)
}

c4 <- function(n) {
  n[n <= 1] <- NA
  sqrt(2 / (n - 1)) * exp(lgamma(n / 2) - lgamma((n - 1) / 2))
}

c5 <- function(n) {
  sqrt(1 - c4(n)^2)
}
