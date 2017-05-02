#
# It indicates the cash flow with lower indebtedness until it begins to generate positive flow
#' @name mmf.get.minloan
#' @param mmf.cfs A list with a vector with a series of cash flows for each MMF 
#' sechedule.
#' @return The CFS with minimum loan
#' @export
#' @keywords minimum loan, minimum indebtedness
#' @family finantial
#' @examples
#' 
#' ex.mmf.minloan <- mmmf.get.minloan(ex.mmf[['cfs.discounted']])
#' 
mmf.get.minloan <- function(mmf.cfs) {
  totalseq <- length(ex.mmf.seq)
  loan <- list(mmf.cfs, length = totalseq)
  totalloan <- list(mmf.cfs, length = totalseq)
  minloan <- vector(mode = "numeric", length = 1)
  ex.selftime <- mmf.get.selffunding(mmf.cfs)
  totalcfs <- mmf.cfs
  for (i in 1:totalseq){
        loan[[i]] <- head(totalcfs[[i]], ex.selftime[[i]][1]-1)
        totalloan[[i]]<-sum(loan[[i]])
  }
  minloan <- which.max(totalloan)
  return(minloan)
}


