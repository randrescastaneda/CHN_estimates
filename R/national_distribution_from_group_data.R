#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# libraries   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(data.table)
options("joyn.verbose" = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
yrs   <- c(1984:2019)

# ld   <- pipload::pip_load_cache("CHN", yrs, type = "list")
ld   <- pipload::pip_load_cache("CHN", type = "list")


pop <- pipload::pip_load_aux("pop")
gdm <- pipload::pip_load_aux("gdm")

# cpi
cpi <- pipload::pip_load_aux("cpi")
cpi <- cpi[country_code == "CHN",
           c("survey_year", "cpi_data_level", "cpi")
           ][,
             urban := fcase(cpi_data_level == "urban", 1,
                            cpi_data_level == "rural", 0,
                            default = NA)]
# ppp
ppp <- pipload::pip_load_aux("ppp")
ppp <- ppp[country_code == "CHN" & ppp_default == TRUE,
           c("ppp_data_level", "ppp")
           ][,
             urban := fcase(ppp_data_level == "urban", 1,
                            ppp_data_level == "rural", 0,
                            default = NA)
             ][!is.na(urban),
               c("urban", "ppp")]

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


# displau means
purrr::map(lsy,
           .f = ~{
             .x[, .(mean  = weighted.mean(x = welfare,
                                          w = weight)),
                by = urban
             ]
           })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## convert  to PPP values --------



svy_id <- names(lsy)


ls2 <- purrr::map(.x = svy_id,
                  .f = ~{

                    yr  <- gsub("(.+_)([0-9]{4})(_.+)", "\\2", .x)

                    x <-  lsy[[.x]]

                    x <- joyn::merge(x, cpi[survey_year == yr,
                                            c("urban", "cpi")],
                                     by  =  "urban",
                                     match_type = "m:1",
                                     reportvar = FALSE)

                    x <- joyn::merge(x, ppp,
                                     by  =  "urban",
                                     match_type = "m:1",
                                     reportvar = FALSE)

                    x[,
                      welfare_ppp := wbpip:::deflate_welfare_mean(welfare, ppp, cpi)]

                  })



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## save files --------

survey_id <- purrr::map_chr(ld,
                            .f = ~{
                              .x[, unique(as.character(survey_id))]
                            })

filename <- gsub("(.+)(PIP_.+)", "\\1GMD_PCN-A.dta", survey_id)

purrr::walk2(.x = ls2,
             .y = filename,
             .f = ~{
               haven::write_dta(.x, fs::path(tdirp,"CHN_distribution", .y))
             })

