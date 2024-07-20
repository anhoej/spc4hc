spc <- function(
    x,           # x axis values
    y   = NULL,  # data values
    cl  = NA,    # centre line
    lcl = NA,    # lower control limit
    ucl = NA,    # upper control limit
    ...          # other parameters passed to the plot() function
) {
  # if y is missing, set y to x and make a sequence for x
  if (is.null(y)) {
    y <- x
    x <- seq_along(y)
  }
  
  # repeat line values to match the length of y
  if (length(cl) == 1)
    cl <- rep(cl, length(y))
  
  if (length(lcl) == 1)
    lcl <- rep(lcl, length(y))
  
  if (length(ucl) == 1)
    ucl <- rep(ucl, length(y))
  
  # find data points outside control limits
  sigma.signal <- y < lcl | y > ucl
  sigma.signal[is.na(sigma.signal)] <- FALSE
  
  # runs analysis
  if (all(is.na(cl))) {
    runs.signal <- FALSE
  } else {
    runs            <- sign(y - cl)
    runs            <- runs[runs != 0 & !is.na(runs)]
    run.lengths     <- rle(runs)$lengths
    n.useful        <- sum(run.lengths)
    longest.run     <- max(run.lengths)
    n.crossings     <- length(run.lengths) - 1
    longest.run.max <- round(log2(n.useful)) + 3
    n.crossings.min <- qbinom(0.05, n.useful - 1, 0.5)
    runs.signal     <- longest.run > longest.run.max |
                       n.crossings < n.crossings.min
  }

  # make empty plot
  plot(x, y, 
       type = 'n',
       ylim = range(y, lcl, ucl, na.rm = TRUE),
       ...)
  # add centre line, coloured and dashed if unusually long runs or unusuall
  # few crossing were identified by the runs analysis
  lines(x, cl,
        col = runs.signal + 1,
        lty = runs.signal + 1)
  # add control limits
  lines(x, lcl)
  lines(x, ucl)
  
  # add data line and points, colour data points outside control limits
  lines(x, y)
  points(x, y,
         pch = 19,
         col = sigma.signal + 1)
}
