runs.analysis <- function(y, cl) {
  # trichotomise data according to position relative to CL
  # -1 = below, 0 = on, 1 = above
  runs            <- sign(y - cl)
  
  # remove NAs and data points on the CL
  runs            <- runs[runs != 0 & !is.na(runs)]
  
  # find run lengths
  run.lengths     <- rle(runs)$lengths
  
  # find number of useful observations (data points not on CL)
  n.useful        <- sum(run.lengths)
  
  # find longest run above or below CL
  longest.run     <- max(run.lengths)
  
  # find number of times adjacent data points are on opposite sides of CL
  n.crossings     <- length(run.lengths) - 1
  
  # find upper limit for longest run
  longest.run.max <- round(log2(n.useful)) + 3
  
  # find lower limit for number of crossing
  n.crossings.min <- qbinom(0.05, n.useful - 1, 0.5)
  
  # return result, TRUE if either of the two tests is true, otherwise FALSE
  longest.run > longest.run.max | n.crossings < n.crossings.min
}
