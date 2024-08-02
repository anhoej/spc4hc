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
                                 'integer')
                  )

# csections
csect <- read.csv('data/csection_delay.csv',
                  comment.char = '#',
                  colClasses   = c('POSIXct',
                                   'Date',
                                   'integer'))

# emergency admissions
admis <- read.csv('data/emergency_admission.csv',
                  comment.char = '#',
                  colClasses = c('Date',
                                 'integer',
                                 'integer'))

# on-time CT
ct <- read.csv('data/ontime_ct.csv',
               comment.char = '#',
               colClasses = c('Date',
                              'integer',
                              'integer'))

# renography doses
reno <- read.csv('data/renography_doses.csv',
                 comment.char = '#',
                 colClasses = c('Date',
                                'Date',
                                'double'))

# robson group 1 births
births <- read.csv('data/robson1_births.csv',
                   comment.char = '#',
                   colClasses   = c('POSIXct',
                                    'Date',
                                    'logical',
                                    'logical',
                                    'integer',
                                    'integer',
                                    'integer',
                                    'double',
                                    'logical'))
