# bacteremias
bact <- read.csv('data/bacteremia.csv',
                 comment.char = '#',
                 colClasses = c(
                   'Date',
                   'integer',
                   'integer',
                   'integer',
                   'integer'
                 ))

# blood pressure

# cdiffs
cdiff <- read.csv('data/cdiff.csv',
                  comment.char = '#',
                  colClasses = c('Date',
                                 'integer',
                                 'integer'))

# csections
csect <- read.csv('data/csection_delay.csv',
                  comment.char = '#',
                  colClasses   = c('POSIXct',
                                   'Date',
                                   'integer'))

# emergency admissions

# on-time CT

# renography doses

# robson group 1 births