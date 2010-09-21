setClass("PipelineData", representation(pipeline = "Pipeline"))

setMethod("pipeline", "PipelineData",
          function(object, ancestry = TRUE, local = TRUE)
          {
            pipeline <- object@pipeline
            locals <- pipeline@.Data
            me <- sapply(sapply(pipeline, outType), extends, class(object))
            if (any(!me))
              locals <- tail(pipeline, -tail(which(!me), 1))
            ancestors <- list()
            if (ancestry)
              ancestors <- head(pipeline, -length(locals))
            pipeline@.Data <- c(ancestors, if (local) locals)
            pipeline
          })

## explore the data in the context of the last applied protocol
setMethod("explore", c("PipelineData", "missing"),
function(object, protocol, ...)
  {
    proto <- NULL
    if (length(object@pipeline@.Data))
      proto <- tail(object@pipeline,1)[[1]]
    explore(object, proto, ...)
  })
