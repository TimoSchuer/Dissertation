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
