showSequence <- function(exb, pathFile= character(0),pathPraat= getwd()){

  line1 <- paste0("Open long sound file: ","\"",pathFile,"\"")
  line2 <- paste0("editor: \"LongSound", exb$File[1],"\"")
  line3 <- paste0("Zoom: ",exb$Start_time[1], ", ",exb$End_time[nrow(exb)], sep="")
  praat <- paste(line1,line2,line3, sep="\n")
  write(praat, file= paste(pathPraat,"/","showSequence.praat", sep = ""))
  cmd <- paste0(pathPraat,"/Praat.exe"," --run ", pathPraat, "/showSequence.praat") %>% stringr::str_replace_all("/","\\\\\\")
  system(cmd)
  unlink(paste(pathPraat, "/showSequence.praat", sep = ""))
}
