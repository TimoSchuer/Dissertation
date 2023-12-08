#' Title
#'
#' @param x data.frame from read_exb
#' @param assign_times assign time
#' @param transcription_text text column that should be split
#'
#' @return data.frame
#' @export
#'
#' @examples
tokenize_exb <- function(x, assign_times=TRUE, transcription_text= "Text"){
  if(assign_times==FALSE){
    tokenized <- x %>%
      tidytext::unnest_tokens(output = {{transcription_text}}, input = .data[[transcription_text]])
  }else if(assign_times==TRUE){
    maxIP <- x %>% group_by(IPId) %>% mutate(IPn=cur_group_id()) %>% pull(IPn) %>% max()
    if(x %>%
       tidytext::unnest_tokens(output = {{transcription_text}}, input = .data[[transcription_text]]) %>%
       dplyr::mutate(TextIP=  paste0(Text, collapse = " "), .by = IPId) %>%
       dplyr::mutate(IPn= cur_group_id()) %>%
       dplyr::mutate(TimePerToken=(max(End_time)- min(Start_time))/n(), .by= IPId ) %>%
       filter(TimePerToken<=0.05) %>% nrow()>0){
      cat("There are some problems in the transcripts. Events are too short. Check returned data.frame")
      # return(x %>%
      #          tidytext::unnest_tokens(output = {{transcription_text}}, input = .data[[transcription_text]]) %>%
      #          dplyr::mutate(TextIP=  paste0(Text, collapse = " "), .by = IPId) %>%
      #          dplyr::mutate(IPn= cur_group_id()) %>%
      #          dplyr::mutate(TimePerToken=(max(End_time)- min(Start_time))/n(), .by= IPId ) %>%
      #          filter(TimePerToken<=0.05))
    }


    tokenized<-  x %>%
      tidytext::unnest_tokens(output = {{transcription_text}}, input = .data[[transcription_text]]) %>%
      dplyr::mutate(TextIP=  paste0(Text, collapse = " "), .by = IPId) %>%
      dplyr::mutate(IPn= cur_group_id()) %>%
      dplyr::mutate(TimePerToken=(max(End_time)- min(Start_time))/n(), .by= IPId ) %>%
      dplyr::mutate(Start_time_IP= min(Start_time), .by = IPId) %>%
      dplyr::mutate(End_time_IP= max(End_time), .by = IPId) %>%
      dplyr::mutate(length_token= (End_time_IP- Start_time_IP) /n(), .by= IPId) %>%
      dplyr::mutate(Start_time= if_else(row_number()==1, Start_time_IP, Start_time_IP + cumsum(length_token)- length_token),.by = IPId) %>%
      dplyr::mutate(End_time = if_else(row_number()==1, Start_time_IP + length_token,Start_time_IP+cumsum(length_token)), .by = IPId) %>%
      dplyr::mutate(TokenID= row_number() %>% paste(IPId,., sep =  "_"), .by = IPId) %>%
      dplyr::mutate(Start_time=round(Start_time,6)) %>%
      dplyr::mutate(End_time=round(End_time,6))
      #select(-c(length_token, Start_time_IP,End_time_IP,IPn))
    ##timeline konsistent machen
    #Durch die Anapassung der Start und Endzeitpunkte kann es vorkommen, dass der Start des nächsten Ereignisses vor dem Ende des vorherigen liegt,
    #das lässt sich anpassen indem man den endzeitpunkt des vorherigen als Startzeitpunkt des nächsten nimmt.
    #In wenigen Fällen führt dies dazu dass dieses Ereignis nun nicht mehr passt diese Ereignisse muss man aussortieren


    # t <- data.frame()
    # for (sp in unique(x$Name)) {
    #   h <- tokenized %>% filter(Name==sp) %>%
    #     dplyr::mutate(Start_time=if_else(Start_time< lag(End_time)& row_number()!=1 & End_time- lag(End_time)>=0, lag(End_time),Start_time)) %>%
    #     dplyr::mutate(TimePerToken=(max(End_time)- min(Start_time))/n(), .by= IPId ) %>%
    #     dplyr::mutate(Start_time_conf= if_else(lag(End_time)> Start_time& !is.na(lag(End_time)), TRUE, FALSE)) %>%
    #     dplyr::mutate(tok_length_conf=if_else(End_time - Start_time < 0.2, TRUE, FALSE))
    #   ##lässt einige Fehler übrigen, irgendwann mach ich das ordentlich...
    #   t <- bind_rows(t,h)
    # }
    #tokenized <- t %>% arrange(IPId) %>% select(-c(IPn,TextIP,TimePerToken,TimePerToken_after, Start_time_IP, End_time_IP,length_token,Start_time_conf, tok_length_conf))
  }
  return(tokenized)
}

#'
#' #' Title
#' #'
#' #' @param x data.frame from read_exb
#' #' @param assign_times assign time
#' #' @param transcription_text text column that should be split
#' #'
#' #' @return data.frame
#' #' @export
#' #'
#' #' @examples
#' tokenize_exb <- function(x, assign_times=TRUE, transcription_text= "Text"){
#'   if(assign_times==FALSE){
#'     tokenized <- x %>%
#'       tidytext::unnest_tokens(output = {{transcription_text}}, input = .data[[transcription_text]])
#'   }else if(assign_times==TRUE){
#'     maxIP <- x %>% group_by(IPId) %>% mutate(IPn=cur_group_id()) %>% pull(IPn) %>% max()
#'     tokenized<-  x %>%
#'       tidytext::unnest_tokens(output = {{transcription_text}}, input = .data[[transcription_text]]) %>%
#'       dplyr::mutate(TextIP=  paste0(Text, collapse = " "), .by = IPId) %>%
#'       dplyr::mutate(IPn= cur_group_id()) %>%
#'       dplyr::mutate(TimePerToken=(max(End_time)- min(Start_time))/n(), .by= IPId ) %>%
#'       dplyr::mutate(End_time=if_else(row_number()==n() & TimePerToken<0.05 & IPn==1, End_time+((0.07- TimePerToken)*n()), End_time), .by = IPId) %>% #für erste Zeile
#'       dplyr::mutate(Start_time=if_else(row_number()==1 & TimePerToken<0.05& IPn!=0, Start_time-((0.07- TimePerToken)*n()/2), Start_time), .by = IPId)%>%
#'       dplyr::mutate(End_time=if_else(row_number()==n() & TimePerToken<0.05 & IPn==maxIP, End_time+((0.07- TimePerToken)*n()/2), End_time), .by = IPId) %>%
#'       dplyr::mutate(Start_time=if_else(row_number()==1 & TimePerToken<0.05& IPn==maxIP, Start_time-((0.07- TimePerToken)*n()/2), Start_time), .by = IPId) %>% # für letzte zeile
#'       dplyr::mutate(TimePerToken_after=(max(End_time)- min(Start_time))/n(), .by= IPId ) %>%
#'       dplyr::mutate(Start_time=if_else(Name==lag(Name) & Start_time< lag(End_time) &row_number() !=1 , lag(End_time),Start_time)) %>%
#'       dplyr::mutate(Start_time_IP= min(Start_time), .by = IPId) %>%
#'       dplyr::mutate(End_time_IP= max(End_time), .by = IPId) %>%
#'       dplyr::mutate(length_token= (End_time_IP- Start_time_IP) /n(), .by= IPId) %>%
#'       dplyr::mutate(Start_time= if_else(row_number()==1, Start_time_IP, Start_time_IP + cumsum(length_token)- length_token),.by = IPId) %>%
#'       dplyr::mutate(End_time = if_else(row_number()==1, Start_time_IP + length_token,Start_time_IP+cumsum(length_token)), .by = IPId) %>%
#'       dplyr::mutate(TokenID= row_number() %>% paste(IPId,., sep =  "_"), .by = IPId) #%>%
#'     #select(-c(length_token, Start_time_IP,End_time_IP,IPn))
#'     ##timeline konsistent machen
#'     #Durch die Anapassung der Start und Endzeitpunkte kann es vorkommen, dass der Start des nächsten Ereignisses vor dem Ende des vorherigen liegt,
#'     #das lässt sich anpassen indem man den endzeitpunkt des vorherigen als Startzeitpunkt des nächsten nimmt.
#'     #In wenigen Fällen führt dies dazu dass dieses Ereignis nun nicht mehr passt diese Ereignisse muss man aussortieren
#'
#'
#'     t <- data.frame()
#'     for (sp in unique(x$Name)) {
#'       h <- tokenized %>% filter(Name==sp) %>%
#'         dplyr::mutate(Start_time=if_else(Start_time< lag(End_time)& row_number()!=1 & End_time- lag(End_time)>=0, lag(End_time),Start_time)) %>%
#'         dplyr::mutate(TimePerToken=(max(End_time)- min(Start_time))/n(), .by= IPId ) %>%
#'         dplyr::mutate(Start_time_conf= if_else(lag(End_time)> Start_time& !is.na(lag(End_time)), TRUE, FALSE)) %>%
#'         dplyr::mutate(tok_length_conf=if_else(End_time - Start_time < 0.2, TRUE, FALSE))
#'       ##lässt einige Fehler übrigen, irgendwann mach ich das ordentlich...
#'       t <- bind_rows(t,h)
#'     }
#'     tokenized <- t %>% arrange(IPId) %>% select(-c(IPn,TextIP,TimePerToken,TimePerToken_after, Start_time_IP, End_time_IP,length_token,Start_time_conf, tok_length_conf))
#'   }
#'   return(tokenized)
#' }
