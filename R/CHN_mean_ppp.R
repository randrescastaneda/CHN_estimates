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

# cpi
cpi <- pipload::pip_load_aux("cpi")
cpi <-
  cpi[
  country_code == "CHN",
  c("survey_year", "cpi_data_level", "cpi")
  ][
  cpi_data_level != "national"
    ]

setnames(cpi, c("survey_year", "cpi_data_level"), c("year", "area"))

# ppp
ppp <- pipload::pip_load_aux("ppp")
ppp <- ppp[country_code == "CHN" & ppp_default == TRUE,
           c("ppp_data_level", "ppp")
][
  ppp_data_level != "national"
]

setnames(ppp, "ppp_data_level", "area")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Group data means in LCU --------

gdm <- pipload::pip_load_aux("gdm")
ff <-
  gdm[country_code == "CHN"
  ][,
    mean := survey_mean_lcu* (12/365)
  ][,
    .(surveyid_year, pop_data_level, mean )]

setnames(ff, c("surveyid_year", "pop_data_level"), c("year", "area"))


ff <- joyn::merge(ff, ppp,
                  by         = "area",
                  match_type = "m:1",
                  reportvar  = FALSE)

ff <- joyn::merge(ff, cpi,
                  by         = c("year", "area"),
                  match_type = "m:1",
                  reportvar  = FALSE)


ff[,
   mean_ppp := wbpip:::deflate_welfare_mean(mean, ppp, cpi)]


# save
filename <- fs::path(tdirp, "CHN_mean", ext = "dta")
haven::write_dta(ff,filename)





#
# gt_ff <- gt(ff)
#
# gt_ff %>%
#   tab_header(
#     title = "Monthly mean in 2011 PPPs"
#   )
