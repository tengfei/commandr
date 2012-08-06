### The pipeline is a series of protocols that implement stages
setClassUnion("OptionalCharacter", c("character", "NULL"))

setClass("Pipeline", representation(dispName = "OptionalCharacter"),
         contains = "list")

## constructor
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

## try to get Pipelines before certain protocol
setGeneric("processProto",function(object,...)
           standardGeneric("processProto"))
setMethod("processProto","Pipeline",function(object,
                                                     role="genProfile",
                                                     method=defaultMethod(role)){
  protos <- findProtocols(object,role,method)
  if(!length(protos))
    NULL
  else{
    npps <- object[1:protos[1]]
    return(npps)
  }
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


setMethod("c","Pipeline",function(x,...,recursive=FALSE){
  if(recursive)
    stop("'recursive mode is not supported'")
  arg <- unlist(list(x,...))
  do.call('Pipeline',arg)
})

setMethod("[","Pipeline",
          function(x,i,j,...,drop=FALSE){
            if(!missing(j)||length(list(...))>0)
              stop("invalid subsetting")
            N <- length(x@.Data)
            if(min(i)<1 | max(i)>N)
              stop("Subscript is out of boundary")
            if(!missing(i)){
              return(do.call("Pipeline",x@.Data[i]))
            }
          })

setReplaceMethod("[","Pipeline",
                 function(x,i,j,...,value){
                   if(!missing(j)||length(list(...))>0)
                     stop("invalid replacing")
                   N <- length(x@.Data)
                   if(min(i)<1 | max(i)>N)
                     stop("Subscript is out of boundary")
                   if(length(i)>1)
                     stop('Multiple replacement is not supported yet')
                   if(!missing(i) & !missing(value)){
                     res <- x@.Data
                     res[i] <- value
                     return(do.call("Pipeline",res))
                   }
                 })
## not supported names yet
## setMethod("[[","Pipeline",
##           function(x,i,j,...){
            
##           })
