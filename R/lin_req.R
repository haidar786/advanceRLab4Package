
linreq <- function(formula, data) {
  LinreqClass <- setRefClass("linreq", fields = list(balance = "numeric"))
  a <- LinreqClass$new(balance = 100)
  return(a)
}
linreq(4,5)
