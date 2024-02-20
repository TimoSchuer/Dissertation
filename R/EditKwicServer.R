addDropdown <- function(kwic, AnnValues){
  for (Val in names(AnnValues)){
    kwic[[Val]] <- as.character(kwic[[Val]])
    for (i in 1:nrow(kwic)) {
      kwic[i,Val] <- shiny::selectInput(paste0(Val,"_",i),
                                        label = "",
                                        choices = AnnValues[[Val]] %>% as.character() %>% unique(),
                                        selected = kwic[i,Val],
                                        width = "100px") %>% as.character()
  }
  }

  return(kwic)
}

addDropdownInputValues <- function(kwic, selectInputIDs, input){
  for (Var in str_remove_all(selectInputIDs,"_.*") %>% unique()){
    for (i in 1:nrow(kwic)) {
      Value <- paste(Var,"_",i,sep="")
      kwic[i,Var] <- shiny::selectInput(paste0(Var,"_",i),
                                        label = "",
                                        choices=kwic[[Var]] %>% as.character() %>% unique(),
                                        selected = input[[Value]],
                                        width = "100px") %>% as.character()
    }
  }

  return(kwic)
}
