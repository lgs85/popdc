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
    popl[[i]] <- pop
  }

  out <- popl |>
    bind_rows()

  return(out)
}

