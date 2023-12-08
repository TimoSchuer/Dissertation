#' Title
#'
#' @param exb_tok tokenisiertes exb objekt
#' @param window fenster um Zielwort/wörter herum
#' @param pattern Zielwort/phrase
#' @param textCol Zeile mit Text aufgrunddessen die KWi
#' @param summarise_pattern if True rows for pattern are summarised
#'
#' @return
#' @export
#'
#' @examples
#'
#'
createKWIC <- function(exb_tok = data.frame(),
                       window = 2,
                       pattern = "",
                       textCol = "Text",
                       summarise_pattern = FALSE,
                       IPintern=TRUE) {

  if (window > length(exb_tok[[textCol]])) {
    stop("Error: window size is greater than the length of the text column.")
  }

  # Split the pattern into individual words
  pattern_words <- stringr::str_split(pattern, " ")[[1]]
  Text <- exb_tok %>%
    mutate(Text= str_remove_all(Text,"\\W") %>% str_trim()) %>%
    pull(Text)
  searchMat <-    sapply(
    1:length(Text),
    \(x) paste(Text[x:min(x+length(pattern_words)-1, nrow(exb_tok))], collapse = " ")
  )


  # Find the positions where the pattern matches the text
  exb_tok <- exb_tok %>% mutate(rowNumber= row_number())
  match_positions <- which(searchMat==pattern)
  result <- data.frame()
  i <- 1
  for (k in match_positions) {
    rows <- seq(k-window, k +length(pattern_words)-1+window)
    if(min(rows)<0){
      rows <- rows[which(rows>=0)]
    }
    if(max(rows>nrow(exb_tok))){
      rows <- rows[which(rows<=nrow(exb_tok))]
    }
    match <- exb_tok[rows,] %>%
      mutate(BelegNr=i) %>%
      mutate(pos=case_when(rowNumber %in% seq(k, k+length(pattern_words)-1)~ 0,
                           .default = NA))
    result <- bind_rows(result, match)
    i <- i+1
  }
  matches <- c()
  for (t in match_positions) {
    matches <- c(matches, seq(t,t+length(pattern_words)-1))
  }
  # Set the pos column to 0 for the matching positions
 result <-  result %>%
#    mutate(pos = if_else(rowNumber %in% matches, 0, NA)) %>%
    group_by(BelegNr,pos) %>%
    mutate(MatchPos=if_else(pos==0,row_number(),NA)) %>%
    ungroup() %>%
    mutate(RNumber= row_number(), .by= BelegNr) %>%
    mutate(pos= case_when(is.na(MatchPos)& RNumber<=RNumber[which(MatchPos==1)]~ RNumber-RNumber[which(MatchPos==1)],
                          !is.na(MatchPos)~0,
                          is.na(MatchPos)& RNumber>=RNumber[which.max(MatchPos)]~RNumber-RNumber[which.max(MatchPos)]), .by = BelegNr)

  if(IPintern){
    result <- result %>%
      dplyr::filter(IPId==IPId[which(MatchPos==1)], .by=BelegNr)
  }
  # Optionally summarise the pattern matches
  if (summarise_pattern) {
    pattern_summary <- paste(pattern_words, collapse = " ")
    exb_tok <- exb_tok %>%
      distinct_at(vars(textCol, pos)) %>%
      mutate(Text = if_else(pos == 0, pattern_summary, Text))
  }

  result <- result %>% select(!c(rowNumber,MatchPos,RNumber))
  # Return the modified data frame
  return(result)

}

# createKWIC <- function(exb_tok,window=2,
#                        pattern="",
#                        textCol="Text",
#                        summarise_pattern=FALSE){
#   pattern_split <- stringr::str_split(pattern, " ")[[1]]
#   #firsts <- exb_tok %>% pull(.data[[textCol]]) %>% grep(pattern[i],.)
#   result <- exb_tok %>%
#     mutate(MatchPos= if_else(str_remove(.data[[textCol]],"\\W")== pattern_split[1],1,NA))
#   if(length(pattern_split)==1){
#     result <-result %>%
#       mutate(MatchID=if_else(MatchPos==1,"Beleg","none"), .by = IPId) %>%
#       mutate(MatchID= row_number(),.by=MatchID) %>%
#       mutate(MatchID=if_else(MatchPos==1,MatchID,NA)) %>%
#       mutate(BelegInDerGruppe=cumsum(case_when(stringi::stri_detect_regex(MatchPos,"1")~1,
#                                                .default = 0)),.by = IPId) %>%
#       mutate(BelegInDerGruppe=if_else(!is.na(MatchPos),BelegInDerGruppe,NA)) %>%
#       mutate(MatchPos=if_else(BelegInDerGruppe==1,MatchPos,NA))
#     z <- result %>% filter(!is.na(MatchPos)) %>% pull(IPId) %>% unique()
#     result <- result %>% filter(IPId %in% z) %>%
#       mutate(BelegID= paste0(IPId,"_1"), .by=IPId)
#
#     if(result %>% filter(BelegInDerGruppe>1) %>% nrow() !=0 ){
#       for (k in result %>% filter(BelegInDerGruppe>1) %>% pull(IPId) %>% unique()) {
#         n <- result %>%
#           filter(IPId==k& !is.na(BelegInDerGruppe)) %>%
#           pull(BelegInDerGruppe) %>%
#           unique() %>%
#           max()
#         for (i in 2:n) {
#           result <- result %>%
#             filter(IPId==k) %>%
#             mutate(MatchPos=if_else(BelegInDerGruppe==i, row_number(),NA), .by = BelegInDerGruppe) %>%
#             mutate(BelegID=paste0(IPId,"_",i)) %>%
#             bind_rows(result,.)
#         }
#       }
#     }
#     result <- result %>%
#       mutate(RNumber= row_number(), .by= IPId) %>%
#       mutate(pos= case_when(is.na(MatchPos)& RNumber<=RNumber[which(MatchPos==1)]~ RNumber-RNumber[which(MatchPos==1)],
#                             !is.na(MatchPos)~0,
#                             is.na(MatchPos)& RNumber>=RNumber[which.max(MatchPos)]~RNumber-RNumber[which.max(MatchPos)]), .by = BelegID) %>%
#       filter(abs(pos)<=window)
#     return(result)
#   }
#   for (i in 2:length(pattern_split)) {
#     result <- result %>%
#       mutate(MatchPos=case_when(MatchPos<=i-2~MatchPos,
#                                 str_remove(.data[[textCol]],"\\W") ==pattern_split[i-1] &str_remove(lead(.data[[textCol]]),"\\W")== pattern_split[i]~ i-1,
#                                 str_remove(.data[[textCol]],"\\W")== pattern_split[i] & str_remove(lag(.data[[textCol]]),"\\W")== pattern_split[i-1]~ i,
#                                 .default = NA))
#   }
#
#   result <-result %>%
#     mutate(MatchID=if_else(MatchPos==1,"Beleg","none"), .by = IPId) %>%
#     mutate(MatchID= row_number(),.by=MatchID) %>%
#     mutate(MatchID=if_else(MatchPos==1,MatchID,NA)) %>%
#     mutate(MatchID=case_when(MatchPos==1~MatchID ,
#                              !is.na(lag(MatchID))& !is.na(MatchPos)~lag(MatchID),
#                             .default =   NA)) %>%
#     mutate(BelegInDerGruppe=cumsum(case_when(stringi::stri_detect_regex(MatchPos,"1")~1,
#                                       .default = 0)),.by = IPId) %>%
#     mutate(BelegInDerGruppe=if_else(!is.na(MatchPos),BelegInDerGruppe,NA)) %>%
#     #mutate(MatchID=if_else(!is.na(BelegInDerGruppe),MatchID,NA)) %>%
#     mutate(MatchPos=if_else(BelegInDerGruppe==1,MatchPos,NA))
#   z <- result %>% filter(MatchPos==length(pattern_split)) %>% pull(IPId) %>% unique()
#   result <- result %>% filter(IPId %in% z) %>%
#     mutate(BelegID= paste0(pattern,"_", IPId,"_1"), .by=IPId)
#   if(length(pattern_split>2)){
#     for (k in 2:length(pattern_split)) {
#       result <- result %>%
#         mutate(MatchID=case_when(MatchPos==1~MatchID ,
#                                  !is.na(lag(MatchID))& !is.na(MatchPos)~lag(MatchID),
#                                  .default =   NA))
#     }
#   }
#
#
#   if(result %>% filter(BelegInDerGruppe>1) %>% nrow() !=0 ){
#     for (k in result %>% filter(BelegInDerGruppe>1) %>% pull(IPId) %>% unique()) {
#       n <- result %>%
#         filter(IPId==k& !is.na(BelegInDerGruppe)) %>%
#         pull(BelegInDerGruppe) %>%
#         unique() %>%
#         max()
#      for (i in 2:n) {
#         result <- result %>%
#          filter(IPId==k) %>%
#          mutate(MatchPos=if_else(BelegInDerGruppe==i, row_number(),NA), .by = BelegInDerGruppe) %>%
#           mutate(BelegID=paste0(pattern,"_", IPId,"_",i)) %>%
#           bind_rows(result,.)
#       }
#     }
#   }
# ## relative postions to match
# result <- result %>%
#   mutate(RNumber= row_number(), .by= IPId) %>%
#   mutate(pos= case_when(is.na(MatchPos)& RNumber<=RNumber[which(MatchPos==1)]~ RNumber-RNumber[which(MatchPos==1)],
#                         !is.na(MatchPos)~0,
#                         is.na(MatchPos)& RNumber>=RNumber[which.max(MatchPos)]~RNumber-RNumber[which.max(MatchPos)]), .by = BelegID)  %>%
#   filter(abs(pos)<=window) %>%
#   select(!c(BelegInDerGruppe,MatchPos,RNumber,MatchID))
# if(summarise_pattern==TRUE){
#   result <- result %>%   summarise(across(c(IPId:TierCategory,Name), ~first(.x)),
#                                    across(c(Start,Start_time),~first(.x)),
#                                    across(c(End,End_time),~last(.x)),
#                                    across(where(is.character) &Text:last_col(),~stringr::str_flatten(.x,collapse=" ", na.rm = TRUE)),
#                                    across(where(is.numeric)&Text:last_col(),~sum(.x, na.rm = TRUE)),
#                                    .by = c(BelegID,pos))
# }
#
# return(result)
#
# }
#
#
