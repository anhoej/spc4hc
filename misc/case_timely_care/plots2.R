library(tidyverse)
library(qicharts2)

d <- read_csv('MyData.csv',
              locale = locale(date_format = '%d/%m/%Y'))
d$notes <- NA
d$notes[51] <- 'intervention'

qic(wkco, avgbedocc, data = d, notes = notes,
    ylab = NULL,
    xlab = 'Week')
qic(wkco, bedocc, data = d)
qic(wkco, readmrate, data = d)
