#' Generates all minimum makespan schedules for a cpm network
#' 
#' @name cpm.all.schedule
#' 
#' @aliases cpm_all_schedule genAllCpmSched 
#' 
#' @param vector with early start time for all activities
#' @param vector with slack for all activities
#' 
#' @description Generates all minimum makespan schedules for a cpm network
#' 
#' @export
#' 
#' @return Matrix with all mininum makespan cpm schedules (start time for all activities) 
#' 
#' @keywords critical path, scheduling, project activities
#' 
#' @family scheduling
#' 
#' @examples
#' 
#' ex.cpm.activities.duration <- c(1,4,5,7,2,3,1)
#' ex.cpm.activities.successors <- list(c(2,3), 4, c(4,5), 6, 7, 7, c(0))
#' ex.cpm <- cpm(ex.cpm.activities.duration, 
#'               ex.cpm.activities.successors)
#'
#' #ex.cpm is a list:
#' # - est (Early Start Time)  - ex.cpm$est
#' # - eft (Early Finish Time) - ex.cpm$eft
#' # - lst (Late Start Time)   - ex.cpm$lst
#' # - lft (Late Finish Time)  - ex.cpm$lft
#'
#' ex.cpm.activities.schedule <- cpm.all.schedule(ex.cpm)
#'                                                
cpm.all.schedule <- cpm_all_schedule <- genAllCpmSched <- 
  function(cpm){
  est <- cpm$est
  slack <- (cpm$lst - cpm$est)
  
  # Constants
  slack <- slack + 1
  m <- prod(slack)
  n <- length(slack)
  
  # Variables
  current.schedule <- vector(mode = "numeric", length = n)
  schedule.matrix <- matrix(ncol = n, nrow = m)
  
  #Algorithm
  schedule.matrix[1,] <- current.schedule + est
  k <- 2 #row index for current.schedule
  
  for (j in 1:m){
    carry <- 1
    for (i in n:1){
      v <- current.schedule[i] + carry            
      if (v >= slack[i]){
        current.schedule[i] <- 0
        carry <- 1                
      } else {                
        current.schedule[i] <- v  
        carry <- 0                
        sched <- current.schedule + est                
        schedule.matrix[k,] <- sched
        k <- k+1    
        break
      }
    }
  }
  return(schedule.matrix)
}