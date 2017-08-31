#' @importFrom utils getFromNamespace
utils::getFromNamespace

#' @importFrom utils str
utils::str

#' @export
print.timeManip <- function(x,...){
   str(x$summary())
}
