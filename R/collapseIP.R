#' Title
#'
#' @param exb data.frame
#' @param coerce_cols cols that are pasted together (names)
#' @param annotation_dummies should the (character) annotation colums be converted to dummies and be added up per ips
#' @param annotation_cols character annotation colums (names)
#'
#' @return
#' @export
#'
#' @examples
collapseIP <- function(exb,coerce_cols="Text", annotation_dummies=FALSE, annotation_cols=c() ){
  if(annotation_dummies==TRUE & length(annotation_cols)!=0){
    ann <- exb %>%
      mutate(across(annotation_cols, ~tidyr::replace_na(.x,"NA"))) %>%
      mutate(across(annotation_cols, ~as.factor(.x))) %>%
      pivot_longer(cols = annotation_cols,
                   names_to = "dummy_names",
                   values_to = "dummy_levels") %>%
      mutate(dummy_value = 1) %>%
      pivot_wider(id_cols= c(IPId,EventID),
                    names_from = c(dummy_names, dummy_levels),
                  values_from = dummy_value,
                  values_fill = 0) %>%
      select(-EventID) %>%
      summarise(across(everything(), ~sum(.x)), .by=IPId)
    exb <- exb %>%
      summarise(IPId=first(IPId),
                IPNumber= first(IPNumber),
                File=first(File),
                Speaker=first(Speaker),
                TierID= first(TierID),
                TierCategory=first(TierCategory),
                Start=first(Start),
                End=last(End),
                Name=first(Name),
                Start_time=min(Start_time),
                End_time=max(End_time),
                across(all_of(coerce_cols),~paste(.x,collapse=" ")), .by = IPId )%>%
      left_join(ann, by="IPId")
    return(exb)

  }
 exb <- exb %>%
   summarise(IPId=first(IPId),
             IPNumber= first(IPNumber),
             File=first(File),
             Speaker=first(Speaker),
             TierID= first(TierID),
             TierCategory=first(TierCategory),
             Start=first(Start),
             End=last(End),
             Name=first(Name),
             Start_time=min(Start_time),
             End_time=max(End_time),
             across(all_of(coerce_cols),~paste(.x,collapse=" ")), .by = IPId )

return(exb)
}
