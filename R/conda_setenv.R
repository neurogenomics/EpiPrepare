conda_setenv <- function(conda_env=NULL){
    python <- echoconda::find_python_path(conda_env = conda_env)
    Sys.setenv(python=python)
    Sys.setenv(PATH=paste(dirname(python),Sys.getenv("PATH"),sep = ':'))
}