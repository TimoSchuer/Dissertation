#' send Praat
#'
#' @param data data.frame created bei ExmaraldaR with function readExb has to contain the columns "pathAudio", "Start_time" and "End_time"
#' @param rowNumber row number of the data frame to be sent to Praat
#' @param pathPraat path to the folder containing the Praat executable 'sendpraat-win.exe'
#'
#' @return nothing
#' @export
#'
#' @examples
sendPraat <- function(data=data.frame(),rowNumber=1, pathPraat=character()){
  stopifnot(length(pathPraat)==1)
  paste(pathPraat,"/sendpraat-win.exe praat ",
        paste0("\"Open long sound file: \\\"",data[rowNumber, "pathAudio"],"\\\"\"",sep=""), sep="") %>%
    system()
        paste0(pathPraat,'/sendpraat-win.exe praat ',
             '"selectObject: ', '\\"LongSound ',
             data[rowNumber,] %>%
               dplyr::pull(pathAudio) %>%
               stringr::str_extract("/[^/]*\\.") %>%
               stringr::str_remove_all("\\.") %>%
               stringr::str_remove_all("/") ,'\\""',
             " \"View\"",
             paste(" \"editor: ", '\\"LongSound ',
                   data[rowNumber,] %>%
                     dplyr::pull(pathAudio) %>%
                     stringr::str_extract("/[^/]*\\.") %>%
                     stringr::str_remove_all("\\.") %>%
                     stringr::str_remove_all("/") ,
                   '\\""' ,

                   sep=""),
             paste(" \"Zoom: ",data[rowNumber, "Start_time"]-3,", ",data[rowNumber, "End_time"]+3,"\"",sep=""),
             " endeditor",
             sep="") %>% system()

}
