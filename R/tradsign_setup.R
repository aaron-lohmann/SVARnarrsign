#' Setup up of traditional sign restrictions.
#'
#' tradsign_setup is used to define the inputs for narrsign() defining the trad
#' itional sign restrictions.
#'
#' @param data Orignal data used for estimating the model.
#' @param shock_names A vector of shock names on which traditional sign restric
#' tions should be imposed.
#' @param restr_matrix A matrix of dimenstions (length of shocknames)x(number o
#' f system variables) which capture the sign restrictions. Suppose a system
#' with one shock and 4 variables. Variable 1 and 3 are restricted to positive
#' signs, variable 2 is restricted to negative sign and variable 4 is
#' unrestricted. The defined matrix would be matrix(c(1,-2,3,NA))
#' @param hor_matrix A matrix which defines the hoirzons for which the restrict
#' ions are supposed to hold. This matrix has dimensions (length of shock names)
#' x2. Where the first column refers to the starting horizon and the second
#' column refers to the end horizon. Note, for convenience time period zero
#' and 1 are both treated as "on-impact".
#' @param cum A vector of length K indicating which variables should be treated
#' as cumulative.
#'
#' @return A list which contains restrictions hoirzons and shock names
#' @export
#'
#' @examples
#' 
tradsign_setup <- function(data = NULL,
                           shock_names = NULL,
                           restr_matrix = NULL,
                           hor_matrix = NULL,
                           cum = NULL) {
  # TODO: Add input checks here.

# replacing all zeros in the horizon matrix with 1 as some researchers
# consider the starting period 0 to be the first and some other use a 1 to
# indicate the starting period.

  hor_matrix[hor_matrix == 0] <- 1

  tradsign_setup <- list(
    restrictions = restr_matrix,
    horizons = hor_matrix,
    shocknames = shock_names,
    cum = cum
  )

  tradsign_setup
}
