#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# libraries   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(data.table)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
yrs   <- c(2016, 2017, 2018)
yrs   <- c(1984:2018)

# ld   <- pipload::pip_load_cache("CHN", yrs, type = "list")
ld   <- pipload::pip_load_cache("CHN", type = "list")


pop <- pipload::pip_load_aux("pop")
gdm <- pipload::pip_load_aux("gdm")
inv <- pipload::pip_inventory()
inv <- inv[country_code == "CHN",
           filename]


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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Functions   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

synth <- function(level,
                  dt,
                  pop,
                  mean     ) {
  rr <-
    pipdm:::get_synth_vector(dt        = dt,
                             pop_table = pop,
                             mean      = mean,
                             level     = level)
  if (level == "rural") {
    cov <- 0
  } else {
    cov <- 1
  }

  rr[,
     urban := cov]

  return(rr)

}

sy_mean <- function(dt, mean, pop) {
  setDT(dt)


  syd <- purrr::map_dfr(.x = names(mean),
                        synth,
                        dt = dt,
                        pop = pop,
                        mean = mean)
  return(syd)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Execution   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# debugonce(wbpip:::create_functional_form_lb)
lsy <- purrr::map2(.x = ld,
                   .y = means,
                   .f = sy_mean, pop = pop)



purrr::map(lsy,
           .f = ~{
             .x[, .(mean  = weighted.mean(x = welfare,
                                          w = weight)),
                by = urban
             ]
           })



survey_id <- purrr::map_chr(ld,
                            .f = ~{
                              .x[, unique(as.character(survey_id))]
                            })

filename <- gsub("(.+)(PIP_.+)", "\\1GMD_PCN-A.dta", survey_id)

purrr::walk2(.x = lsy,
             .y = filename,
             .f = ~{
               haven::write_dta(.x, paste0(tdirp, .y))
             })

