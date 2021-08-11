playSequence <- function(exb, pathFile= character(0),pathPraat= getwd()){

  line1 <- paste0("Open long sound file: ","\"",pathFile,"\"")
    line2 <- paste0("Play part: ",exb$Start_time[1], ", ",exb$End_time[nrow(exb)], sep="")
  praat <- paste(line1,line2, sep="\n")
  write(praat, file= paste(pathPraat,"/","playEvent.praat", sep = ""))
  cmd <- paste0(pathPraat,"/Praat.exe"," --run ", pathPraat, "/playEvent.praat") %>% stringr::str_replace_all("/","\\\\\\")
  system(cmd)
  unlink(paste(pathPraat, "/playEvent.praat", sep = ""))
}
