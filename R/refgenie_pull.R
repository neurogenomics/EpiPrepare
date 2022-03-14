#' Pull iGenome assets with refgenie
#' 
#' refgenie pull
#' @source \href{http://refgenie.databio.org/en/latest/seek/}{
#' refgenie docs: seek}
refgenie_pull <- function(asset_registry_paths,
                          conda_env="epiprepare",
                          path=NULL,
                          verbose=TRUE){  
     
   refgenie_ex <- refgenie_setup(conda_env = conda_env,
                                 path=path,
                                 verbose = verbose) 
    # refgenie_seekr(refgenie_ex = refgenie_ex,
    #                asset_registry_paths = "hg19/fasta")
    assets <-  paste(unique(asset_registry_paths), collapse = " ")
    messager("Preparing",length(assets),"asset(s).",v=verbose)
    out <- sys::exec_background(
        cmd = refgenie_ex, 
        args = c("pull",
                 "--no-overwrite",
                 assets
                 )
        ) 
    out2 <- system(paste(c(refgenie_ex,
                           "seek",
                           assets),
                         collapse = " "),
           intern = TRUE)   
    messager("Returning path to asset(s).",v=verbose)
    return(out2)
}
