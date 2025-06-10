#' Title sample a vector of segments using age groups
#'
#' @param vector of age groups
#' @param params parameters object
#'
#' @returns a vector of segments
#' @export
#'
#' @examples
#' sample_segment_age(pop$age, params)
sample_segment_age <- function(age,params)
{
  out <- sample(names(params$segment_probs_by_age_group$value[[age]]),
                1,
                prob = as.numeric(params$segment_probs_by_age_group$value[[age]]))
  return(out)
}

#' Title sample a vector of waiting list booleans using age and segment
#'
#' @param age vector of age groups
#' @param segment vector of segment states
#' @param params parameters object
#'
#' @returns a vector of booleans
#' @export
#'
#' @examples
#' sample_segment_age_wl(pop$age,pop$segment,params)
sample_segment_age_wl <- function(age,segment,params)
{
  out <- rbernoulli(
    n = 1,
    p = as.numeric(params$on_waiting_list_prob$value[[age]][[segment]]))
  return(out)
}
