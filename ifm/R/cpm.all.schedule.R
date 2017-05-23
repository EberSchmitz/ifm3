#' Generates all possible schedules for a cpm network
#' 
#' @name cpm.all.schedule
#' 
#' @aliases cpm_all_schedule genAllCpmSched 
#' 
#' @param est early start time vector
#' @param slack activities slack
#' 
#' @description Generates all possible schedules for a cpm network
#' 
#' @export
#' 
#' @return Matrix with all mininum makespan (the time to complete all jobs) cpm start time schedules
#' 
#' @keywords critical path, scheduling, project activities
#' 
#' @family scheduling
#' 
#' @examples
#' #Use critical.path.method function to calculate a set of project 
#' #activities:
#' 
#' ex.cpm.activities.duration <- c(1,4,5,7,2,3,1)
#' ex.cpm.activities.successors <- list(c(2,3), 4, c(4,5), 6, 7, 7, c(0))
#' ex.cpm <- cpm(ex.cpm.activities.duration, 
#'               ex.cpm.activities.successors)
#'
#' # Now, we have the CPM vector with:
#' # - est (Early Start Time)  - ex.cpm["est"]
#' # - eft (Early Finish Time) - ex.cpm["eft"]
#' # - lst (Late Start Time)   - ex.cpm["lst"]
#' # - lft (Late Finish Time)  - ex.cpm["lft"]
#'
#' ex.cpm.activities.schedule <- 
#'    cpm.all.schedule(ex.cpm)
#'                                                
cpm.all.schedule <- cpm_all_schedule <- genAllCpmSched <- 
  function(cpm, predecessors){
    est <- cpm$est
    lst <- cpm$lst
    slack <- (cpm$lst - cpm$est)
    
    durations <- (cpm$eft - cpm$est)
    
    m <- 1
    
    for(i in 1:length(slack)){
      if (slack[i] != 0) {
        m <- m*slack[i]
      }
    }
    
    slack <- slack + 1
    
    # m <- prod(slack)
    
    n <- length(slack)
    
    # Variables
    current.schedule <- vector(mode = "numeric", length = n)
    schedule.matrix <- matrix(ncol = n, nrow = m)
    
    #Algorithm
    schedule.matrix[1,] <- current.schedule + est
    k <- 2 #row index for current.schedule
    
    for (j in 2:m){
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
          schedule.matrix[j,] <- sched
          break
        }
      }
    }
    
    for(i in 1:nrow(schedule.matrix)) {
      for(j in 1:ncol(schedule.matrix)) {
        if (slack[j] > 0){
          stMax <- 0
          for (k in predecessors[j]) {
            if(isTRUE((schedule.matrix[i,j] < (schedule.matrix[i,k] + durations[k])) && (stMax < (schedule.matrix[i,k] + durations[k])))) {
              stMax <- (schedule.matrix[i,k] + durations[k])
            }
          }
          
          if(stMax != 0) {
            schedule.matrix[i,j] <- stMax
          }
        }
      }
    }
    
    return(schedule.matrix)
  }
