refgenie_listr <- function(refgenie_ex){
    sys::exec_background(cmd = refgenie_ex, 
                         args = c("listr")) 
}