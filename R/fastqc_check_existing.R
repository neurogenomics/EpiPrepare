fastqc_check_existing <- function(out_dir,
                                  files){
    out_files_exist <- list.files(out_dir, full.names = TRUE)
    out_files_exist <- out_files_exist[file.exists(out_files_exist)]
    out_files_exist <- add_names(out_files_exist)
    in_files_exist <- names(files)[
        names(files) %in% unique(names(out_files_exist))
    ]
    if(length(in_files_exist)>0){
        files <- files[!files %in% in_files_exist]
        if(length(files)==0){
            messager("All files already processed.",
                     "Returning processed file paths.")
            return(out_files_exist)
        }
    }
}
