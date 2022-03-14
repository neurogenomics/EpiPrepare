refgenie_seekr <- function(refgenie_ex,
                           asset_registry_paths=c()){
    out <- sys::exec_wait(cmd = refgenie_ex, 
                          args = c("seekr",
                                   paste(asset_registry_paths, collapse = " "))
                          ) 
}