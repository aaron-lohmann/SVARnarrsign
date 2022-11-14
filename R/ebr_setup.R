#' Define the elasticity bound restrictions.
#'
#' ebr_setup is used to define the elasticity restrictions to be applied. Note,
#' that these elasticities are computed as fractions.
#'
#' @param shock_names A vector of shock names on which elasticity bounds are
#' placed. Note, each shock can be mentioned multiple times.
#' @param num_variable A vector of variables which are in the numerator.
#' Should have the same length as shock_names. Each variable refers to one
#' shock.
#' @param denom_variable A vector of variables which are in the denominator.
#' Should have the same length as shock_names. Each variable refers to one
#' shock.
#' @param horizon A vector of horizons for which elasticity bound restrictions
#' should hold. This should have the same length as shock_names.
#' @param bounds A vector of numerics which specify the elasticity bounds. This
#' should have the same length as shock_names.
#'
#' @return A tibble which contains the elasticity bound restrictions.
#'
#' @export
#'
#' @examples
#'
#' data(kilian_2009)
#' varnames_kilian_2009 <- colnames(kilian_2009)
#'
#'
#'
#' ebr_kilian_2009_shocknames <- c("Aggregate Demand", "Oil specific Demand")
#' ebr_kilian_2009_first <- c(varnames_kilian_2009[2], varnames_kilian_2009[2])
#' ebr_kilian_2009_second <- c(varnames_kilian_2009[3], varnames_kilian_2009[3])
#'
#' ebr_kilian_2009_horizon <- c(1, 1)
#' ebr_kilian_2009_maxbounds <- c(0.0258, 0.0258)
#'
#'
#' ebr_kilian_2009 <- ebr_setup(
#'   shock_names = ebr_kilian_2009_shocknames,
#'   first_variable = ebr_kilian_2009_first,
#'   second_variable = ebr_kilian_2009_second,
#'   horizon = ebr_kilian_2009_horizon,
#'   bounds = ebr_kilian_2009_maxbounds
#' )
#'
#' ebr_kilian_2009
#'
ebr_setup <- function(shock_names = NULL,
                      first_variable = NULL,
                      second_variable = NULL,
                      horizon = NULL,
                      bounds = NULL) {
  # TODO: Include input checks.

  # collect all specified inputs in a list.
  ebr <- list(shock_names, first_variable, second_variable, horizon, bounds)

  ebr
}
