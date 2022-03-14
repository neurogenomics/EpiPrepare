#' Make design files
#' 
#' Makes design files.
#' @export
make_design_files <- function(root,
                              pattern = "*.fq.gz",
                              split_batches = FALSE,
                              omit_processed = TRUE,
                              force_new = TRUE,
                              method = c("novogene"),
                              save_dir = tempdir(),
                              return_table=TRUE){ 
    requireNamespace("stringr")
    
    method <- tolower(method)[1]
    fq <- list.files(path = file.path(root, "raw_data"), 
                     pattern = pattern, 
                     recursive = TRUE, full.names = TRUE) 
    method_opts <- eval(formals(make_design_files)$method)
    if(!method %in% method_opts) {
        stp <- paste0("method must be one of:\n",
                     paste(" -",method_opts,collapse = "\n"))
        stop(stp)
    }
    if(method=="novogene"){
        d <- data.table::data.table(fq=fq) %>% 
            dplyr::mutate(batch=basename(dirname(dirname(dirname(dirname(fq))))),
                          batch_id=basename(dirname(dirname(dirname(fq)))),
                          batch_dir=dirname(dirname(dirname(dirname(fq)))),
                          sample=basename(dirname(fq)),
                          id=gsub(pattern,"",basename(fq)), 
                          control_group=1) 
        d$lane <- stringr::str_split(d$id,"_",simplify = TRUE)[,5]
        d$fq_num <- stringr::str_split(d$id,"_",simplify = TRUE)[,6]
        d$replicate <- 1
    } 
    #### Omit any data that's already been processed #### 
    if(omit_processed){
        processed <- basename(list.dirs(file.path(root,"processed_data"),
                                        recursive = FALSE))
        d <- subset(d, !batch %in% processed)
    } 
    #### Iterate over batches #### 
    batch_list <- if(split_batches) unique(d$batch) else "all"
    out_list <- lapply(batch_list, function(b){
        if(!split_batches) {
            message("Merging all bathces into 1 file")
        } else {
            message(b)
        }
        #### Spread data into wide format #### 
        ## With separate columns for fastq_1 and fastq_2 
        dat <- tidyr::pivot_wider(data =
                                      if(split_batches) {
                                          subset(d, batch==b)
                                      } else {d}, 
                                  id_cols = c("batch","batch_id",
                                              "replicate","control_group",
                                              "sample","lane"), 
                                  names_from = "fq_num",
                                  names_glue="fastq_{fq_num}",
                                  values_from = "fq") %>%
            ## Group change depending on analysis goals 
            ## Required cols: "group","replicate","control_group",
            #  "fastq_1","fastq_2","sample"
            dplyr::mutate(group=paste(sample,batch,sep=".")) %>%
            dplyr::select(group,
                          replicate,control_group,
                          fastq_1,fastq_2,
                          sample,lane,batch) %>%
            data.table::data.table()
        #### Save design file ####
        save_dir <- if(is.null(save_dir)){
            if(split_batches){
                dirname(dirname(dirname(dirname(dat$fastq_1[1])))) 
            } else {
                root
            } 
        } else {save_dir} 
        fpath <- file.path(save_dir,
                           "design.csv") 
        if(!file.exists(fpath) || force_new){
            message("Writing ==> ",fpath)
            echoconda::set_permissions(path = save_dir)
            dir.create(save_dir, showWarnings = FALSE, recursive = TRUE)
            utils::write.csv(x = dat, file = fpath, row.names = FALSE)
        }
        if(return_table){
            message("Returning design table(s).")
            return(dat) 
        } else {
            message("Returning paths to saved design table(s).")
            return(fpath)
        }
    }) %>% `names<-`(batch_list)
    if(length(out_list)==1) out_list <- out_list[[1]]
    return(out_list)
}
