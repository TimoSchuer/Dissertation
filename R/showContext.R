showContext <- function(exb, IPId=1, width= 10, format= "transcript"){
  n <- which(exb$IPId==IPId)
  if(n<= width){
    s <- 1
  }else{
    s <- n-width
  }
  if(n+width >= nrow(exb)){
    e <- nrow(exb)
  }else{
    e <- n+width
  }
  if(format== "transcript"){
    exb[s:e,] %>% dplyr::select(IPId, Name, Text) %>% return()
  }else{
  return(exb[s:e,])
    }
}
