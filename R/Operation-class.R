### Simple implementation of command using a closure
## setClassUnion("OptionalFunction", c("function", "NULL"))

setClass("Operation",
         representation(do = "function", undo = "OptionalFunction"),
         contains = "Command")

quoteCommand <- function(do, undo = NULL) {
  do <- as.function(list(substitute(do)), parent.frame())
  if (!is.null(undo))
    undo <- as.function(list(substitute(undo)), parent.frame())
  new("Operation", do = do, undo = undo)
}

## Methods:
## - eval()

setMethod("eval", "Operation",
          function(expr, envir = parent.frame(),
                   enclos = if(is.list(envir) || is.pairlist(envir))
                   parent.frame() else baseenv())
          {
            expr@do()
          })

## - rev() (for undo)

setMethod("rev", "Operation", function(x) {
  tmp <- x@undo
  if (is.null(tmp))
    stop("no undo operation defined")
  x@undo <- x@do
  x@do <- tmp
  x
})
