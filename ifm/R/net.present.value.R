#' Difference between the present values of cash inflows and outflows
#' 
#' @name net.present.value
#' 
#' @aliases npv
#' 
#' @param cfs A vector with a series of cash flows.
#' @param interest.rate A number that represents the nominal Interest Rate, 
#' presented by year.
#' @param begin.of.period A boolean that represents if the Tax Rate will be 
#' applied at the begining of period. FALSE by default, the Tax Rate will be 
#' applied to the second period.
#' @description calculates the difference between the present values of cash 
#' inflows and outflows.
#' 
#' @export
#' 
#' @return The sum of cash flows incomes/outcomes applying the Tax Rate to the 
#' present time
#' 
#' @keywords presentValue
#' 
#' @family financial
#' 
#' @examples
#'
#' ex.npv <- net.present.value(c(-350,100,200,150,75), 0.0619, TRUE)
#' 
net.present.value <- npv <-
function(cfs = c(-350,100,200,150,75), 
         interest.rate = 0.0619, 
         begin.of.period = TRUE) 
  {
    time.instance <- if(begin.of.period) {0:(length(cfs)-1)} else {1:(length(cfs))}
    tax.rate <- (1 / ((1 + (interest.rate)) ^ time.instance))
    return (sum(cfs * tax.rate))
  }
