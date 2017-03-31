# https://cran.r-project.org/web/packages/qicharts/vignettes/controlcharts.html
library(qicharts)
# Setup parameters
m.beds       <- 300
m.stay       <- 4
m.days       <- m.beds * 7
m.discharges <- m.days / m.stay
p.pu         <- 0.08

# Simulate data
discharges  <- rpois(24, lambda = m.discharges)
patientdays <- round(rnorm(24, mean = m.days, sd = 100))
n.pu        <- rpois(24, lambda = m.discharges * p.pu * 1.5)
n.pat.pu    <- rbinom(24, size = discharges, prob = p.pu)
week        <- seq(as.Date('2014-1-1'),
                   length.out = 24, 
                   by         = 'week') 

# Combine data into a data frame
d <- data.frame(week, discharges, patientdays,n.pu, n.pat.pu)
d

qic(n.pu, 
    n        = patientdays,
    x        = week,
    data     = d,
    chart    = 'u',
    multiply = 1000,
    main     = 'Hospital acquired pressure ulcers (U chart)',
    ylab     = 'Count per 1000 patient days',
    xlab     = 'Week')