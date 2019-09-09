#' A function to import excel files as quickly as possible, the use of Addins is also highly recommended
#'
#' @return a data frame object called "paste" in .GlobalEnv
#' @export
#'
#' @examples
#'
#' #Copy an/a excel/google sheets table
#'
#' #Run
#' copypaste()
#'
#' #Close notepad file
#'
#' paste
#'
copypaste<-function(){
  base::writeLines("Paste content","Paste.txt")
  file.show('Paste.txt')
  Abierto1<-Abierto<-file.info("Paste.txt")$mtime
  time<-0
  while(Abierto1==Abierto | time < 15){
    Sys.sleep(1)
    Abierto<-file.info("Paste.txt")$mtime
    time<-time+1
  }
  suppressWarnings(try(assign("paste",read.table("Paste.txt",sep="\t"),.GlobalEnv)
                       ,silent = T))
  unlink('Paste.txt')
}

# cp()
