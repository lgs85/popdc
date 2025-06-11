#' Title simulate a period of time
#'
#' @param pop df generated using `generate_pop()`
#' @param ndays number of days to simulate
#' @param params params file
#'
#' @returns stacked data frame per patient per day
#' @export
#'
#' @examples
#' pop <- generate_pop(params)
#' sim_period(pop,7,params)
#'

sim_period <- function(pop,ndays,params)
{
  days <- rep(names(params$gp_appointments_daily_weight$value),ceiling(ndays/7))

  pop <- generate_pop(params)
  popl <- list()

  ceiling(ndays/7)

  for(i in 1:ndays)
  {
    pop <- sim_day(pop,day = days[i],params) |>
      mutate(day = i)

    popsum <- pop |>
      group_by(age,segment, day) |>
      summarise(n = n(),
                gp_app = sum(gp_app),
                ae_att = sum(ae_att))

    popl[[i]] <- popsum
    cat('.')
  }

  out <- popl |>
    bind_rows()

  return(out)
}

