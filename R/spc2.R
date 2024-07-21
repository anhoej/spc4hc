spc <- function(
    x,           # x axis values
    y   = NULL,  # data values
    cl  = NULL,  # centre line
    lcl = NA,    # lower control limit
    ucl = NA,    # upper control limit
    ...          # other parameters passed to the plot() function
) {
  # load runs analysis function from R script
  source('R/runs.analysis.R')
  
  # if y is missing, set y to x and make a sequence for x
  if (is.null(y)) {
    y <- x
    x <- seq_along(y)
  }
  
  # if cl is missing use median of y
  if (is.null(cl))
    cl <- median(y, na.rm = TRUE)
  
  # repeat line values to match the length of y
  if (length(cl) == 1)
    cl <- rep(cl, length(y))
  
  if (length(lcl) == 1)
    lcl <- rep(lcl, length(y))
  
  if (length(ucl) == 1)
    ucl <- rep(ucl, length(y))
  
  # find data points outside control limits (freaks)
  sigma.signal <- y < lcl | y > ucl
  sigma.signal[is.na(sigma.signal)] <- FALSE
  
  # check for sustained shifts and trends using runs analysis
  runs.signal <- runs.analysis(y, cl)
  
  # make empty plot
  plot(x, y, 
       type = 'n',
       ylim = range(y, cl, lcl, ucl, na.rm = TRUE),
       ...)
  
  # add centre line, coloured and dashed if shifts or trends were identified by
  # the runs analysis
  lines(x, cl,
        col = runs.signal + 1,
        lty = runs.signal + 1)
  
  # add control limits
  lines(x, lcl)
  lines(x, ucl)
  
  # add data line and points, colour freak data points (outside control limits)
  lines(x, y)
  points(x, y,
         pch = 19,
         col = sigma.signal + 1)
}
