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
sample_segment_age <- function(age,params, n)
{
  out <- sample(names(params$segment_probs_by_age_group$value[[age]]),
                                            n,
                                            replace = TRUE,
                                            prob = params$segment_probs_by_age_group$value[[age]])
  return(out)
}
