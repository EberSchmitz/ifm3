#'The Critical Path Method (CPM) is a scheduling algorithm that produces the  
#'minimum makespan schedule for a project with unlimited resources. As a byproduct, it generates
#'the slacks for the non-critical activities. 
#'Inputs: activity precedence relation, activity duration relation  
#'Output: early and late start and finish time for all project activities
#'  
#' @name cpm
#' 
#' @aliases critical.path.method critical_path_method
#' 
#' @param activities.duration: vector with the duration of the project activities.
#' @param activities.successors: list with the set of successors for each activity.
#' 
#' @description The Critical Path Method (CPM) is an algorithm that generates the minimum makespan
#' schedule for a given project activity network with unlimited resources. 
#' 
#' CPM generates also the list of non-critical activities with their respective slack.
#' @export
#' @return Returns a list with 4 vectors: (1) EST (Early Start Time), (2) EFT(Early Finish Time), (3) LST(Lately Start Time), 
#' (4) LFT (Lately Finish Time) 
#' 
#' @keywords critical path, project scheduling, project activities
#' 
#' @family scheduling
#' 
#' @examples
#' ex.cpm.activities.duration <- c(1,4,5,7,2,3,1)
#' ex.cpm.activities.successors <- list(c(2,3), 4, c(4,5), 6, 7, 7, c(0))
#' ex.cpm <- cpm(ex.cpm.activities.duration, 
#'               ex.cpm.activities.successors)
#' 
cpm <- critical.path.method <- critical_path_method <- 
  function(activities.duration = c(1,4,5,7,2,3,1), 
           activities.successors = list(c(2,3), 4, c(4,5), 6, 7, 7, c(0)))
    {
  
      activities.predecessors <- utils.suc2pred(activities.successors)
      
      activities.quantity <- length(activities.duration)
 
      #generates early and late start times of for a cpm network
      est<-vector (mode="numeric", length=activities.quantity)
      eft<-vector (mode="numeric", length=activities.quantity)
      lst<-vector (mode="numeric", length=activities.quantity)
      lft<-vector (mode="numeric", length=activities.quantity)
  
      #Functions
      cpm.forward <- function(s=1){
        #find early start and finish times
        eft[s] <<- est[s] + activities.duration[s]    
        if (activities.successors[[s]][1]!=0){
          for (i in activities.successors[[s]]){
            if (est[i] < eft[s]){
              est[i]<<-eft[s]
            }   
            cpm.forward(i)
          }	
        }
      }
  
      cpm.backward<-function(s){
        #find late start and finish times
        lst[s]<<-lft[s] - activities.duration[s]      
        if (activities.predecessors[[s]][1]!=0){
          for (i in activities.predecessors[[s]]){
            if (lft[i]>lst[s]){
              lft[i]<<-lst[s]            
            }                  
            cpm.backward(i)
          }
        }
      }
  
      #Main
      cpm.forward()
      lft<-rep(eft[activities.quantity], times=activities.quantity)
      cpm.backward(activities.quantity)
      return(list('est' = est, 'eft' = eft, 
                  'lst' = lst, 'lft' = lft))
    }