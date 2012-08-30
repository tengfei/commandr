\name{Protocol-class}
\docType{class}

\alias{class:Protocol}
\alias{Protocol-class}

% constructor
\alias{Protocol}

% accessors
\alias{parameters}
\alias{parameters,Protocol-method}
\alias{stage}
\alias{stage,Protocol-method}
\alias{method,Protocol-method}
\alias{pipeline,Protocol-method}
\alias{method}

% perform
\alias{perform}

% other
\alias{callNextProtocol}
\alias{protocolClass}

\title{Protocol: Concrete Step in a Pipeline}

\description{
  A \code{Protocol} object performs a \code{\linkS4class{Stage}} in a
  particular way, as part of a \code{\linkS4class{Pipeline}}. Most users
  will simply construct a \code{Protocol} and add it to a pipeline. To
  define a new type of \code{Protocol}, see \code{\link{setProtocol}}.
}

\section{Constructors}{
  \describe{
    \item{}{\code{Protocol(role, method = defaultMethod(role), ...)}:
      Creates a protocol of the stage identified by \code{role} and
      method given by \code{method}. The \code{role} argument may be
      either a \code{Stage} object or a string naming the
      role. Parameters in \code{...} are passed to the initializer,
      i.e., they specify parameters of the protocol by name.
    }
  }
}

\section{Accessors}{
  \describe{
    \item{}{\code{stage(object, where = topenv(parent.frame()))}: Return
      a \code{\linkS4class{Stage}} object that represents the role this
      protocol plays in a pipeline. Searches for the class definition of
      the stage in \code{where}.
    }
    \item{}{\code{method(object, where = topenv(parent.frame()))}:
      Returns the method name of this protocol. This is derived from the
      class name of the protocol by removing the stage name. The
      environment \code{where} should contain the definition of the
      stage class.
    }
    \item{}{\code{displayName(object)}: Gets the name for displaying
      this protocol in a user interface.
    }
    \item{}{\code{inType(object)}: Gets the class of data that this
      protocol accepts as input.}
    \item{}{\code{outType(object)}: Gets the class of data that this
      protocol yields as output.}
    \item{}{\code{parameters(object)}: Gets the list of parameters,
      i.e., the slots of the object, that control the execution of the
      protocol.
    }
    \item{}{\code{pipeline(object)}: Some protocols delegate to a
      secondary pipeline, i.e., they have a slot named "pipeline". This
      function retrieves that, or returns \code{NULL} if the protocol
      does not delegate to a pipeline.
    }
  }
}

\seealso{\code{\link{setProtocol}} for defining new types of protocols}

\author{ Michael Lawrence }

\examples{
setStage("average", intype = "numeric")
setProtocol("mean", fun = mean, parent = "average")
setProtocol("quantile", representation = list(probs = "numeric"),
            fun = quantile, parent = "average")

proto_avg_mean <- Protocol("average")
proto_avg_mean <- Protocol("average", "mean")
proto_avg_quantile <- Protocol("average", "quantile")
proto_avg_quantile_075 <- Protocol("average", "quantile", probs = 0.75)

proto <- proto_avg_quantile_075
proto
stage(proto)
inType(proto)
parameters(proto)
}
