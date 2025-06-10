#' Simulate a baseline population
#'
#' @param params a parameters object, read in from a yaml file using `read_yaml()`
#'
#' @returns a dataframe
#' @importFrom dplyr mutate
#' @importFrom purrr map_dbl map2_lgl
#' @export
#'
#' @examples
#' params <- read_yaml('params.yaml')
#' generate_pop(params)
generate_pop <- function(params)
{
  pop <- tibble(id = c(1:params$pop_size$value),
                age = sample(names(params$age_group_probs$value),
                             params$pop_size$value,
                             prob = as.numeric(params$age_group_probs$value),
                             replace = TRUE))

  pop <- pop |>
    mutate(segment = map_chr(age, ~ sample_segment_age(age = .x,params))) |>
    mutate(wl = map2_lgl(age,segment, ~ sample_segment_age_wl(age = .x, segment = .y, params)))


# Waiting times using lognormal distribution
  meanlog <- log(params$elective_wait_times$value$median)
  sdlog <- (log(params$elective_wait_times$value$q90) - meanlog) / qnorm(0.9)

  pop <- pop |>
    mutate(wl_time_days = rlnorm(params$pop_size$value, meanlog = meanlog, sdlog = sdlog))
  pop$wl_time_days[!pop$wl] <- NA

  return(pop)
}
