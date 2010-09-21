### The pipeline is a series of protocols that implement stages

setClassUnion("OptionalCharacter", c("character", "NULL"))

setClass("Pipeline", representation(dispName = "OptionalCharacter"),
         contains = "list")

Pipeline <- function(..., dispName = NULL) {
  protos <- list(...)
  if (!all(sapply(protos, is, "Protocol")))
    stop("All arguments in '...' must be Protocol instances")
  if (!is.null(dispName) && (!is.character(dispName) || length(dispName) != 1))
    stop("'dispName' should be a single character string or NULL")
  new("Pipeline", protos, dispName = dispName)
}

## protocol accessors
setGeneric("protocol", function(object, ...) standardGeneric("protocol"))
setGeneric("protocol<-", function(object, ..., value)
           standardGeneric("protocol<-"))

## extract a pipeline from an object
setGeneric("pipeline", function(object, ...) standardGeneric("pipeline"))

## The display name accessor
setMethod("dispName", "Pipeline", function(object) {
  if (!length(object@dispName))
    object@name
  else object@dispName
})

## name of type to accept as input
## this is a single class, but it could be a class union
setGeneric("inType", function(object, ...) standardGeneric("inType"))

setMethod("inType", "Pipeline", function(object) {
  first <- head(object, 1)
  if (length(first))
    inType(first[[1]])
  else NULL
})

## name of type to produce
setGeneric("outType", function(object, ...) standardGeneric("outType"))

setMethod("outType", "Pipeline", function(object) {
  last <- tail(object, 1)
  if (length(last))
    outType(last[[1]])
  else NULL
})

## parameters controlling protocol behavior
setGeneric("parameters", function(object) standardGeneric("parameters"))

setMethod("parameters", "Pipeline", function(object) {
  lapply(object, parameters)
})

## return the longest range of protocols that goes from inType to outType
setGeneric("pipeline", function(object, ...) standardGeneric("pipeline"))
setMethod("pipeline", "Pipeline",
          function(object, intype = "ANY", outtype = "ANY")
          {
            inmatch <- sapply(sapply(object, inType), extends, intype)
            outmatch <- sapply(sapply(object, outType), extends, outtype)
            pipeline <- initialize(object, list())
            if (any(inmatch) && any(outmatch)) {
              pipeline <- object
              pipeline@.Data <- pipeline[which(inmatch)[1]:tail(which(outmatch),1)]
            }
            pipeline
          })

setGeneric("findProtocols", function(object, ...)
           standardGeneric("findProtocols"))
setMethod("findProtocols", "Pipeline",
          function(object, role, method = character())
          {
            which(sapply(object, is, protocolClass(role, method)))
          })

## FIXME: need [[ method for getting a protocol by name
## FIXME: need [ method for getting a pipeline with a selected protocol
setMethod("protocol", "Pipeline",
          function(object, role, method = character(), ...)
          {
            protos <- findProtocols(object, role, method)
            if (!length(protos))
              NULL
            else object[[protos[1]]]
          })

setReplaceMethod("protocol", "Pipeline",
                 function(object, role, value)
                 {
                   protos <- findProtocols(object, role)
                   if (length(protos))
                     object@.Data[[protos[1]]] <- value
                   else object@.Data <- c(object, value)
                   object
                 })

## perform an operation on a data structure and return the result
## methods are defined via setProtocol()
setGeneric("perform",
           function(object, data = NULL, ...) standardGeneric("perform"))

## Perform all component protocols
setMethod("perform", "Pipeline", function(object, data, ...)
          {
            for (proto in object)
              data <- perform(proto, data)
            data
          })

setMethod("show", "Pipeline", function(object) {
  cat("Pipeline with", length(object@.Data), "protocol(s):\n")
  for (i in seq_along(object@.Data)) {
    cat("[", i, "] ", sep = "")
    show(object[[i]])
  }
})

