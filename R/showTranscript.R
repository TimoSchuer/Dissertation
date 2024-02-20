#' Title
#'
#' @param data data
#' @param corpus corpus
#' @param rowNumber row from data
#' @param tier tier to show transkript
#' @param lengthTranscript length of the transkript
#'
#' @return
#' @export
#'
#' @examples
showTranscript <- function(data= data.frame(),
                           corpus=data.frame(),
                           rowNumber=1,
                           tier=c("beides","Standard","Transkript"),
                           lengthTranscript=10){
  if(length(tier)>1){
    tier <- "beides"
  }
  selectedIPId <- data[rowNumber,] %>% pull(IPId)
  selected <- which(corpus$IPId==selectedIPId)
  if(length(selected)==0){
    return(data.frame(error="Fehler bei der Transkriptanzeige"))
  } else if(length(selected)>1){
    selected <- selected[1]
  }
  if(selected<=20){
    selected <- 21
  }
  if(tier=="beides"){
    if(selected<=lengthTranscript){
      selected<- 11
    }
    transcript <- corpus %>%
      dplyr::select(IPId,Name,Text) %>%
      dplyr::slice(seq(selected-lengthTranscript,selected+lengthTranscript))
  }else if(tier=="Standard" &
           stringr::str_detect(corpus[selected,"File"],"Weg")){
    transcript <- corpus %>%
      dplyr::slice(seq(selected-lengthTranscript,selected+lengthTranscript)) %>%
      dplyr::filter(TierCategory=="min") %>%
      dplyr::select(IPId,Name,Text)
  }else if(tier=="Standard" &
           stringr::str_detect(corpus[selected,"File"],"Dia|WEBER")){
    transcript <- corpus%>%
      dplyr::slice(seq(selected-lengthTranscript,selected+lengthTranscript)) %>%
      dplyr::filter(TierCategory=="SD"|TierCategory=="Übersetzungsspur") %>%
      dplyr::select(IPId,Name,Text)
  }else if(tier=="Transkript" &
           stringr::str_detect(corpus[selected,"File"],"Weg")&
           corpus %>%
           dplyr::filter(File==corpus[1,"File"]) %>%
           dplyr::filter(TierCategory=="bas") %>%
           nrow()> 0){
    selected_start <-  which(corpus$Start_time==corpus[selected,"Start_time"]&
                               corpus$TierCategory=="bas" &
                               corpus$File== corpus[selected,"File"])
    selected_end <- which(corpus$End_time==corpus[selected,"End_time"]&
                            corpus$TierCategory=="bas"&
                            corpus$File== corpus[selected,"File"])
    selected_start <- intersect(selected_start,selected_end)
    if(selected_start %>% length()==0){
      transcript <- corpus %>%dplyr::select(IPId,Name,Text) %>%
        dplyr::slice(seq(selected-lengthTranscript,selected+lengthTranscript))
    }else{
      transcript <- corpus %>%
        dplyr::slice(seq(selected_start-lengthTranscript,selected_start+lengthTranscript)) %>%
        dplyr::filter(TierCategory=="bas") %>%
        dplyr::select(IPId,Name,Text)

    }
  }else if(tier=="Transkript" &
           stringr::str_detect(corpus[selected,"File"],"Weg")&
           corpus %>%
           dplyr::filter(File==corpus[1,"File"]) %>%
           dplyr::pull(TierCategory) %>%
           unique() %>%
           dplyr::intersect("bas") %>%
           length()==0){
    transcript <- corpus %>%
      dplyr::select(IPId,Name,Text) %>%
      dplyr::slice(seq(selected-lengthTranscript,selected+lengthTranscript))
  }else if(tier=="Transkript" &
           stringr::str_detect(corpus[selected,"File"],"Dia|WEBER")){
    selected_start <-  which(corpus$Start_time==corpus[selected,"Start_time"]&
                               (corpus$TierCategory=="ND"| corpus$TierCategory=="platt")&
                               corpus$File== corpus[selected,"File"])
    selected_end <- which(corpus$End_time==corpus[selected,"End_time"]&
                            (corpus$TierCategory=="ND"| corpus$TierCategory=="platt")&
                            corpus$File== corpus[selected,"File"])
    selected_start <- intersect(selected_start,selected_end)
    selected_start <- selected_start[1]

    if(selected_start %>% length()==0){
      transcript <- corpus %>%dplyr::select(IPId,Name,Text) %>%
        dplyr::slice(seq(selected-lengthTranscript,selected+lengthTranscript))
    }else{
      transcript <- corpus %>%
        dplyr::slice(seq(selected_start-lengthTranscript,selected_start+lengthTranscript)) %>%
        dplyr::filter(TierCategory=="ND"|TierCategory=="platt") %>%
        dplyr::select(IPId,Name,Text)

    }

  }
  if(exists("transcript")==FALSE){
    return(data.frame(error="Fehler bei der Transkriptanzeige"))
  }
return(transcript)
}
