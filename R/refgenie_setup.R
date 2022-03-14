refgenie_setup <- function(conda_env=NULL,
                           path=NULL,
                           verbose=TRUE){
    
    if(!is.null(path) && file.exists(path)){
        messager("Supplied binary path exists. Using path to refgenie.",
                 v=verbose)
        return(path)
    }
    #### Install refgenie ####
    if (is.null(path) || !file.exists(path)){
        pkgs <- echoconda::find_packages(packages = "refgenie",
                                         conda_env = conda_env,
                                         verbose = verbose)
        data.table::setkeyv(pkgs, "package")
        if(nrow(pkgs)>0){
            messager("Conda installation of refgenie found in environment:",
                     conda_env,v=verbose)
            path <- pkgs$path[[1]] 
        } 
    }
    if (is.null(path) || !file.exists(path)){
        path <- find_bin_package(package = "refgenie")[1]  
    } 
    if (is.null(path) || !file.exists(path)){
        messager("No conda installation of refgenie could be found.",
                 "Installing via pip instead.",v=verbose) 
        
        out <- sys::exec_background(cmd = "pip",
                                    args = c( "install","--user","refgenie"))  
        path <- find_bin_package(package = "refgenie")[1]
    } 
    if (is.null(path) || !file.exists(path)){
        messager("WARNING: refgenie installatin failed. Returning NULL.")
        return(NULL)
    }
    #### Initialize refgenie #####
    messager("Initializing refgenie",v=verbose) 
    Sys.setenv(PATH=paste(dirname(refgenie_ex),Sys.getenv("PATH"),sep = ':'))
    genome_config <- "genome_config.yaml"
    Sys.setenv(REFGENIE=genome_config)
    if(!file.exists(genome_config)){
        sys::exec_background(cmd = refgenie_ex, 
                             args = c("init",
                                      "-c",genome_config))
    } 
    messager("Returning path to refgene binary.",v=verbose)
    return(path)
}
