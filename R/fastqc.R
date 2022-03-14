#' Run FASTQC
#'
#' Run FASTQC on fastq files.
#' @param files Paths to files of the type specified in \code{format}.
#' @param dir  Selects a directory to be used for temporary files written when
#' generating report images. Defaults to system temp directory if
#' not specified.
#' @param outdir Create all output files in the specified output directory.
#' Please note that this directory must exist as the program
#' will not create it.  If this option is not set then the
#' output file for each sequence file is created in the same
#' directory as the sequence file which was processed.
#' @param format Bypasses the normal sequence file format detection and
#' forces the program to use the specified format.  Valid
#' formats are bam,sam,bam_mapped,sam_mapped and fastq.
#' @param threads Specifies the number of files which can be processed
#' simultaneously.  Each thread will be allocated 250MB of
#' memory so you shouldn't run more threads than your
#' available memory will cope with, and not more than
#' 6 threads on a 32 bit machine.
#' @param force_new If FASTQ results for these files already exist,
#'  overwrite them (default: \code{FALSE}).
#' @param return_all Return all FASTQ results located in the \code{outdir}, 
#' even if they are not from one of the \code{files} in this run
#'  (default: \code{FALSE}).
#' @param ex Path to \code{fastqc} software executable.
#' @param verbose Print messages.
#' @param ... Additional string arguments to be passed to \code{fastqc}.
#' @export
#' @examples 
#' 
fastqc <- function(files,
                   outdir = file.path(tempdir(),
                                      "EpiPrepare_results",
                                      "fastqc"),
                   dir = file.path(tempdir(),"fastqc_temp"),
                   format = "fastq",
                   threads=1,
                   background=FALSE,
                   force_new=FALSE,
                   return_all=FALSE,
                   conda_env="epiprepare",
                   verbose=TRUE,
                   ...){
    add_names <- function(files){
        suffixes <- c(".fq.gz$",".fastq.gz$","_fastqc.html$","_fastqc.zip$")
        names(files) <- gsub(paste(suffixes,collapse = "|"),"",basename(files))
        return(files)
    }
    #### Name files ####
    files <- add_names(files)
    #### Check if input files exist #### 
    files <- files[file.exists(files)] 
    if(length(files)==0) stop("Cannot find any of the requested fastq files.")
    #### Check if output files exist ####
    out_files_exist <- list.files(outdir, full.names = TRUE)
    out_files_exist <- out_files_exist[file.exists(out_files_exist)]
    out_files_exist <- add_names(out_files_exist)
    in_files_exist <- names(files) %in% unique(names(out_files_exist))
    if(sum(in_files_exist)>0){  
        if(force_new==FALSE){
            if(all(in_files_exist)){
                messager("All",length(files),"files already processed.",
                         "Returning processed file paths.",v=verbose)
                return(out_files_exist) 
            } else {
                messager(sum(in_files_exist),"/",length(files),
                         "files already processed. Skipping.",v=verbose)
                files <- files[!in_files_exist]
            }
        } 
    } 
    #### Set up folders #####
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    echoconda::set_permissions(path = dir)
    echoconda::set_permissions(path = outdir)
    #### Get executable path ####
    fastqc_ex = echoconda::find_packages("fastqc",
                                         conda_env = conda_env, 
                                         return_path = TRUE)[[1]][1]
    #### Run fastqc #### 
    cmd <- paste(fastqc_ex, 
                 "--outdir",outdir,
                 "--dir",dir,
                 "--format",format,
                 "--threads",threads,
                 if(!verbose) "--quiet" else NULL,
                 # paste(..., collapse = " "),
                 paste(files,collapse = " "))
    if(background) cmd <- paste0(cmd,"&")
    message("---")
    echoconda::cmd_print(cmd)
    message("---")
    system(cmd)
    #### List outputs ###
    out_files <- list.files(outdir,
                            pattern = ".html$|.zip$",
                            full.names = TRUE)
    out_files <- add_names(out_files)
    #### Return results from only the files inputted ####
    if(!return_all){
        out_files <- out_files[names(out_files) %in% names(files)]
    } 
    #### Return results #### 
    return(out_files)
}
