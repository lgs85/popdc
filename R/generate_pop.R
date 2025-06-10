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

  pop$segment <- NA
  pop$segment[pop$age == 'child'] <- sample_segment_age('child', params, sum(pop$age == 'child'))
  pop$segment[pop$age == 'adult'] <- sample_segment_age('adult', params, sum(pop$age == 'adult'))
  pop$segment[pop$age == 'elderly'] <- sample_segment_age('elderly', params, sum(pop$age == 'elderly'))

  df_wl_prob <- as_tibble(params$on_waiting_list_prob$value) |>
    unnest() |>
    mutate(segment = names(params$on_waiting_list_prob$value[['child']])) |>
    pivot_longer(child:elderly,
                 names_to = 'age',
                 values_to = 'wl_prob')

  pop <- pop |> left_join(df_wl_prob) |>
    mutate(wl = rbinom(n(), 1, wl_prob))

# Waiting times using lognormal distribution
  meanlog <- log(params$elective_wait_times$value$median)
  sdlog <- (log(params$elective_wait_times$value$q90) - meanlog) / qnorm(0.9)

  pop <- pop |>
    mutate(wl_time_days = rlnorm(params$pop_size$value, meanlog = meanlog, sdlog = sdlog))
  pop$wl_time_days[!pop$wl] <- NA

  return(pop)
}
