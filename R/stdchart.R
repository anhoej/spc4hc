stdchart <- function(
    x,
    y   = NULL,
    cl  = 0,
    lcl = -3,
    ucl = 3,
    ra = T
) {
  library(ggplot2)
  source('R/runs.analysis.R')
  
  col1    <- 'steelblue'
  col2    <- 'tomato'
  linecol <- 'gray50'
  
  if (is.null(y)) {
    y <- x
    x <- seq_along(y)
  }
  
  if (length(cl) == 1)
    cl <- rep(cl, length(y))
  
  if (length(lcl) == 1)
    lcl <- rep(lcl, length(y))
  
  if (length(ucl) == 1)
    ucl <- rep(ucl, length(y))
  
  sigma.signal <- y < lcl | y > ucl
  sigma.signal[is.na(sigma.signal)] <- FALSE
  
  runs.signal <- ifelse(ra, runs.analysis(y, cl), F)
  
  dotcol  <- ifelse(sigma.signal, col2, col1)
  clcol   <- ifelse(runs.signal[1], col2, linecol)
  cltyp   <- ifelse(runs.signal[1], 'dashed', 'solid')
  
  data.frame(x, y, cl, lcl, ucl) |> 
    ggplot(aes(x, y)) +
    geom_line(aes(y  = lcl),
              colour = linecol, 
              na.rm  = TRUE) +
    geom_line(aes(y  = ucl),
              colour = linecol, 
              na.rm  = TRUE) +
    geom_line(aes(y   = cl),
              colour  = clcol, 
              na.rm   = T, 
              linetype = cltyp) +
    geom_line(colour    = col1, 
              na.rm     = TRUE, 
              linewidth = 1) +
    geom_point(colour = dotcol, 
               na.rm  = TRUE, 
               size   = 1.6) +
    annotate('text', 
             max(x), 
             tail(lcl, 1), 
             label = 'LCL', 
             adj   = -0.2, 
             size  = 3.1) +
    annotate('text',
             max(x),
             tail(cl, 1),
             label = 'CL', 
             adj   = -0.2, 
             size  = 3.1) +
    annotate('text', 
             max(x), 
             tail(ucl, 1),
             label = 'UCL', 
             adj   = -0.2, 
             size  = 3.1) +
    labs(y = 'SD units',
         x = 'Subgroup #') +
    theme_light() +
    theme(panel.grid   = element_blank(),
          panel.border = element_blank(),
          axis.line    = element_line(colour = 'gray')) +
    scale_y_continuous(breaks = -3:3)
  
  
}
