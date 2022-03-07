#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# libraries   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(data.table)
library(ggplot2)
library(gt)

options(pipload.verbose = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
yrs   <- c(2016, 2017, 2018)
yrs   <- c(1984:2019)

# ld   <- pipload::pip_load_cache("CHN", yrs, type = "list")
ld   <- pipload::pip_load_cache("CHN", type = "list")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## population --------

pop <- pipload::pip_load_aux("pop")
pop <- pop[country_code == "CHN"]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Group data means in LCU --------

gdm <- pipload::pip_load_aux("gdm")
means <-
  gdm[country_code == "CHN"
  ][,
    mean := survey_mean_lcu* (12/365)
  ][,
    .(surveyid_year, pop_data_level, mean )]

means <- split(means, by = "surveyid_year")

means <-
  purrr::map(.x = means,
             .f = ~{
               y <-       .x[, mean]
               names(y) <- .x[, pop_data_level]
               y
             })

years     <- names(means)
names(ld) <- years

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


gmeans <- function(df, mn) {

  levels <- df[, unique(as.character(reporting_level))]

  y <- purrr::map(.x = levels,
                  .f = ~{

                    dfx <- df[reporting_level == .x]
                    mnx <- mn[.x]

                    cpi  <- dfx[, unique(cpi)]
                    ppp  <- dfx[, unique(ppp)]

                    mnx <- wbpip::deflate_welfare_mean(welfare_mean = mnx,
                                                       ppp = ppp,
                                                       cpi = cpi)

                    return(mnx)
                  })
  return(y)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Execution   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dd <- purrr::map(.x = years,
                 .f = ~{
                   mn <- means[[.x]]
                   df <- ld[[.x]]
                   z <- gmeans(df, mn)
                   # z <- copy(z)
                   # z[, year := as.numeric(.x)]
                   return(z)
                 })


ff <- as.data.table(dd)

setnames(ff, names(ff), years)
ff[,
   area := c("rural", "urban")]

ff <- melt(ff, id = "area",
           variable.name = "year",
           value.name    = "means",
           variable.factor = FALSE)

ff <-
  ff[,
     year := as.numeric(year)
  ][year  >= 2011
  ][, means := {
    x <- as.numeric(means)
    x <- x*(365/12)
  }
  ]


ff <- dcast(ff, year ~ area, value.var = "means")


gt_ff <- gt(ff)

gt_ff %>%
  tab_header(
    title = "Monthly mean in 2011 PPPs"
  )
