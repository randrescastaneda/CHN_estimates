#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# libraries   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(data.table)
library(ggplot2)

options(pipload.verbose = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
yrs   <- c(2016, 2017, 2018)
yrs   <- c(1984:2018)

# ld   <- pipload::pip_load_cache("CHN", yrs, type = "list")
ld   <- pipload::pip_load_cache("CHN", type = "list")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## population --------

pop <- pipload::pip_load_aux("pop")
pop <- pop[country_code == "CHN"
           ][,
             year := as.numeric(year)]

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


gd_povstats <- function(df, mn) {

  levels <- df[, unique(as.character(reporting_level))]

  y <- purrr::map_df(.x = levels,
                     .f = ~{

                       dfx <- df[reporting_level == .x]
                       mnx <- mn[.x]

                       cpi  <- dfx[, unique(cpi)]
                       ppp  <- dfx[, unique(ppp)]

                       mnx <- wbpip::deflate_welfare_mean(welfare_mean = mnx,
                                                          ppp = ppp,
                                                          cpi = cpi)

                       st <-
                         purrr::map_df(
                           .x = c(1.9, 3.2, 5.5),
                           .f = ~{
                             rs <- wbpip:::gd_compute_poverty_stats(welfare = dfx$welfare,
                                                                    population = dfx$weight,
                                                                    povline = .x,
                                                                    requested_mean = mnx)

                             rs <- as.data.table(rs)
                           })
                       st <- copy(st)

                       st[, data_level := .x]
                       return(st)
                     })
  return(y)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Execution   ---------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# mn <- means[[1]]
# df <- ld[[1]]
# z <- gd_povstats(df, mn)


dd <- purrr::map_df(.x = years,
                    .f = ~{
                      mn <- means[[.x]]
                      df <- ld[[.x]]
                      z <- gd_povstats(df, mn)
                      z <- copy(z)
                      z[, year := as.numeric(.x)]
                      return(z)
                    })

df <- joyn::merge(dd, pop, by = c("year", "data_level = pop_data_level"),
                  match_type = "m:1",
                  keep = "left",
                  reportvar = FALSE)

dfn <- df[,
          lapply(.SD, weighted.mean, w = pop),
          .SDcols = c("headcount", "poverty_gap", "poverty_severity", "watts"),
          by = c("year", "poverty_line")
][,
  data_level := "national"]


vars <- names(dfn)
dff = rbindlist(list(df[, ..vars], dfn), use.names = TRUE)

setorder(dff, year, poverty_line, data_level)


# save
filename <- fs::path(tdirp, "CHN_pov", ext = "dta")
haven::write_dta(dff,filename)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## compare to povcalnet --------

pcn <- purrr::map_df(.x = c(1.9, 3.2, 5.5),
                     .f = ~{
                       povcalnetR::povcalnet(country = "CHN",
                                             povline = .x)
                     })
pcn <- as.data.table(pcn)


tokeep <- c("datayear", "data_level", "povertyline", "headcount")

pcn2 <-
  pcn[,
      data_level := fcase(coveragetype == "A", "national",
                          coveragetype == "U", "urban",
                          coveragetype == "R", "rural"
      )
  ][,
    ..tokeep]

dfc <- joyn::merge(dff, pcn2, by = c("year = datayear",
                                     "data_level",
                                     "poverty_line = povertyline"),
                   match_type = "1:1",
                   keep = "right",
                   keep_y_in_x = TRUE)


dfc[,
    diff := abs(headcount - headcount.y)]


# differences
collapse::qsu(dfc, diff ~ data_level+year)



# chart
ggplot(dff[year >= 2000],
       aes(x = year,
           y = headcount,
           color = as.factor(poverty_line))
) +
  geom_line() +
  facet_wrap(~data_level) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")


