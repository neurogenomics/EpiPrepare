#' TrimGalore
#' 
#' Run TrimGalore
#' 
#' @source{https://github.com/FelixKrueger/TrimGalore/issues/53}{
#' Issues with Conda}
#' @export
trimgalore <- function(design,
                       outdir = file.path(tempdir(),
                                          "EpiPrepare_results",
                                          "trimgalore"),
                       conda_env = "epiprepare",
                       threads=8, 
                       verbose = TRUE){ 
    pkgs <- echoconda::find_packages(packages = c("trim-galore","cutadapt"),
                                     conda_env = conda_env,
                                     verbose = verbose) 
    data.table::setkeyv(pkgs, "package")
    cutadapt_ex <- pkgs["cutadapt"]$path[[1]][1]
    trimgalore_ex <- pkgs["trim-galore",]$path[[1]][1]
    python <- echoconda::find_python_path(conda_env = conda_env)
    Sys.setenv(python=python)
    Sys.setenv(PATH=paste(dirname(cutadapt_ex),Sys.getenv("PATH"),sep = ':'))
    # tg <- echoconda::import_cli(trimgalore_ex)
    
    #### Check if design file ####
    if(methods::is(design,"data.frame")){  
        out_log <- parallel::mclapply(seq_len(nrow(design)), function(i){
            ROW <- design[i,]
            messager("Processing sample:",
                     paste0("(",i,"/",nrow(design),")"),v=verbose) 
            print(ROW)
            cmd <- paste(trimgalore_ex,
                         "-o",outdir,
                         # Using multi-cores for trimming with 'gzip' 
                         # only has only very limited effect! 
                         # "--gzip",
                         "--paired",
                         ## "Diminishing returns" message if you use >8
                         "--cores",max(threads,8),
                         # "--basename",base_name,
                         "--path_to_cutadapt",cutadapt_ex,
                         paste(c(ROW$fastq_1, ROW$fastq_2),collapse = " ")
            )
            echoconda::cmd_print(cmd)
            system(cmd) #system2(cmd, env=env)
        }, mc.cores = 1) 
    }
    #### Gather file paths ####
    out_list <- list.files(outdir, full.names = TRUE)
    return(out_list)
}