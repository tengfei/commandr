\name{PipelineData-class}
\docType{class}

\alias{class:PipelineData}
\alias{PipelineData-class}

\alias{explore}
\alias{explore,ANY,missing-method}
\alias{pipeline,ANY-method}

\title{PipelineData: Data with history}

\description{
  \code{PipelineData} is a virtual class representing a dataset with an
  attached pipeline that describes the series of steps that produced the
  object. The storage of the data is up to the implementation. The
  methods described here apply equally to \code{PipelineData} and any
  other object that has \code{pipeline} as a slot/attribute.
}

\section{Methods}{
  \describe{
    \item{}{\code{pipeline(object, ancestry = TRUE, local = TRUE)}:
      Gets the pipeline that produced the object. If \code{ancestry} is
      \code{TRUE}, the returned pipeline includes the protocols that
      produced predecessors of a different type. If \code{local}
      is \code{TRUE}, the pipeline includes protocols after the last
      protocol that output an object of a different type, i.e., all
      local protocols have this type as both their input and output.
    }
    \item{}{\code{explore(object)}:
      Produces an interactive, exploratory visualization of this data,
      in the context of the last applied protocol.
    }
  }
}

\author{ Michael Lawrence }
\examples{
## A non-PipelineData data example
setStage("average", intype = "numeric")
setProtocol("mean", fun = mean, parent = "average")
setProtocol("quantile", representation = list(probs = "numeric"),
            fun = quantile, parent = "average")
setProtocol("range", representation = list(low = "numeric", high = "numeric"), 
            fun = function(x, low = 0, high = Inf) x[x >= low & x <= high],
            parent = setStage("trim", intype = "numeric"))

d <- c(1, 2, 4)
p <- Pipeline("trim", "average")
d2 <- perform(p, d)
attr(d2, 'pipeline')
pipeline(d2)
\dontrun{
## this will give an error, no slot called pipelinem, just numeric value.
d2@pipeline
}

setClass("ProcessNumeric", contains = c("numeric", "PipelineData"))
d <- new("ProcessNumeric", c(1, 2, 4))
d@pipeline
setStage("average", intype = "ProcessNumeric")
setProtocol("mean", fun = function(x) new("ProcessNumeric", mean(x)), parent = "average")
setProtocol("quantile", representation = list(probs = "numeric"),
            fun = function(x) new("ProcessNumeric", quantile(x)), parent = "average")
setProtocol("range", representation = list(low = "numeric", high = "numeric"), 
            fun = function(x, low = 0, high = Inf) new("ProcessNumeric",
                                          x[x >= low & x <= high]),
            parent = setStage("trim", intype = "ProcessNumeric"))

p <- Pipeline("trim", "average")
d2 <- perform(p, d)
attr(d2, 'pipeline')
pipeline(d2)
class(d2)
d2@pipeline
}