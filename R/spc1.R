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
  
  # plot the dots and draw the lines
  plot(x, y, 
       type = 'o',
       ylim = range(y, lcl, ucl, na.rm = TRUE),
       ...)
  lines(x, cl)
  lines(x, lcl)
  lines(x, ucl)
}
