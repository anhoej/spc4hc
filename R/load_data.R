# bacteremias ----
bact <- read.csv('data/bacteremia.csv',
                 comment.char = '#',
                 colClasses = c(
                   'Date',
                   'integer',
                   'integer',
                   'integer',
                   'integer'
                 ))

# systolic blood pressure (MAM) ----
systolic <- c(169, 172, 175, 174, 161, 142,
              174, 171, 168, 174, 180, 194,
              161, 181, 175, 176, 186, 166,
              157, 183, 177, 171, 185, 176,
              181, 174)

# blood pressure (JA) ----
blood_pressure <- read.csv('data/blood_pressure.csv',
                           comment.char = '#',
                           colClasses = c('Date',
                                          'integer',
                                          'integer',
                                          'integer')
)

# cdiffs ----
cdiff <- read.csv('data/cdiff.csv',
                  comment.char = '#',
                  colClasses = c('Date',
                                 'integer',
                                 'integer')
                  )

# csections ----
csect <- read.csv('data/csection_delay.csv',
                  comment.char = '#',
                  colClasses   = c('POSIXct',
                                   'Date',
                                   'integer'))

# emergency admissions ----
admis <- read.csv('data/emergency_admission.csv',
                  comment.char = '#',
                  colClasses = c('Date',
                                 'integer',
                                 'integer'))

# on-time CT ----
ct <- read.csv('data/ontime_ct.csv',
               comment.char = '#',
               colClasses = c('Date',
                              'integer',
                              'integer'))

# renography doses ----
reno <- read.csv('data/renography_doses.csv',
                 comment.char = '#',
                 colClasses = c('Date',
                                'Date',
                                'double'))

# robson group 1 births ----
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

# systolic blood pressure ----
systolic <- c(169, 172, 175, 174, 161, 142,
              174, 171, 168, 174, 180, 194,
              161, 181, 175, 176, 186, 166,
              157, 183, 177, 171, 185, 176,
              181, 174)

# HbA1c data from children with diabetes
diabetes <- read.csv('data/diabetes_hba1c.csv',
                     comment.char = '#',
                     colClasses = c('Date',
                                    'double',
                                    'integer'))

# Adverse events ----
ae <- read.csv('data/adverse_events.csv',
               comment.char = '#',
               colClasses = c('character',
                              'character'))
