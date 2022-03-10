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
#' @param fastqc_x Path to \code{fastqc} software executable.
#' @param ... Additional string arguments to be passed to \code{fastqc}.
#' @export
fastqc <- function(files,
                   outdir = file.path(tempdir(),
                                      "processed_data",
                                      "batch1",
                                      "fastqFileQC",
                                      basename(fq)),
                   dir = "./fastqc_temp",
                   format = "fastq",
                   threads=1,
                   background=FALSE,
                   fastqc_x = echoconda::find_packages(
                     "fastqc",conda_env = "epiprocess")$path,
                   ...){
  out <- lapply(files, function(f){
    if(!file.exists(f)) stop("Cannot find fastq file:",f)
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
    echoconda::set_permissions(path = dir)
    echoconda::set_permissions(path = outdir)
    cmd <- paste(fastqc_x,
                 "--dir",dir,
                 "--outdir",outdir,
                 "--format",format,
                 "--threads",threads,
                 paste(..., collapse = " "),
                 f)
    if(background) cmd <- paste0(cmd,"&")
    echoconda::cmd_print(cmd)
    system(cmd)
    return(data.table::data.table(file=f,
                                  outdir=outdir,
                                  dir=dir,
                                  format=format,
                                  threads=threads,
                                  cmd=cmd))
  })
  out <- data.table::rbindlist(out)
  return(out)
}
