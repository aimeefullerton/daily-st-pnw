fncChoose <- function(choices){
  print(choices)
  idx <- readline("Choose which dataset to use by its number ")
  ifelse(idx == "", return(choices[1]), return(choices[as.numeric(idx)]))
}
#usage: fncChoose(choices = c("full", "season", "region", "season-region"))