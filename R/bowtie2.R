#' bowtie2
#' 
#' Run bowtie2
#'
#' @export
bowtie2 <- function(design,
                    outdir = file.path(tempdir(),
                                          "EpiPrepare_results",
                                          "bowtie2"),
                    conda_env = "epiprepare",
                    bt2Index=NULL,
                    force_new=FALSE,
                    outputType="sam",
                    threads=8, 
                    verbose = TRUE){ 
    ## No longer needed since Rbowtie2 compiles bowtie2 from source
    # pkgs <- echoconda::find_packages(packages = c("bowtie2"),
    #                                  conda_env = conda_env,
    #                                  verbose = verbose) 
    # data.table::setkeyv(pkgs, "package")
    # bowtie2_ex <- pkgs["bowtie2"]$path[[1]][1] 
     
    #### Get reference file ####
    if(is.null(bt2Index) | !file.exists(bt2Index)){
        bt2Index <- refgenie_pull(asset_registry_paths="hg19/bowtie2_index",
                                  conda_env=conda_env,
                                  path=NULL, 
                                  verbose=verbose)
    }
     
    #### Check if design file ####
    if(!methods::is(design,"data.frame")){ 
        stop("Design must a designf file provided as a data.frame.")
    }
    output_list <- lapply(seq_len(nrow(design)), function(i){
        ROW <- design[i,]
        messager("Processing sample:",
                 paste0("(",i,"/",nrow(design),")"),v=verbose) 
        print(ROW)
        output <- file.path(outdir,paste0(SAMPLE_NAME,"_trimmed_bowtie2")) 
        if(file.exists(output) & force_new==FALSE){
            messager("Returning path to previously processed results file(s).",
                     v=verbose)
        } else {
             cmdout <- Rbowtie2::bowtie2_samtools(
                    bt2Index = bt2Index,
                    output = output,
                    outputType = outputType,
                    seq1 = design$fastq_1[i],
                    seq2 = design$fastq_2[i],
                    overwrite = TRUE,
                    bamFile = NULL,
                    "--threads",threads,
                    "--local", 
                    "--very-sensitive",
                    "--no-mixed",
                    "--no-discordant",
                    "--phred33",
                    "-I",10,
                    "-X",700)
            # head(readLines(file.path(td, "result.sam"))) 
        } 
        return(output)
    }) 
    #### Gather file paths ####
    # output_list <- list.files(outdir, full.names = TRUE)
    return(output_list)
}