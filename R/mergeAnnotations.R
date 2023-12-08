#' Title
#'
#' @param newData newData that should be merged into old data
#' @param original original data
#' @param columnsToChange colums that are affected
#' @param changes vector with ids of changed rows, optional
#'
#' @return
#' @export
#'
#' @examples
mergeAnnotations <- function(newData,original,columnsToChange=names(newData), changes=c()){
  if(length(changes)!=0){
    for (ID in changes) {
      for (col in columnsToChange ) {
        original <- original %>%
          mutate({{col}} := case_when(TokenID==ID~ newData[newData$TokenID==ID,{{col}}],
                                      .default = original[[col]]))
      }

    }
    return(original)
  }
  for (i in 1:nrow(newData)) {
    for (col in columnsToChange ) {
    original <- original %>%
      mutate({{col}} := case_when(TokenID==newData[i,"TokenID"]~ newData[i,{{col}}],
                               .default = original[[col]]))
    }
  }
  return(original)
}
