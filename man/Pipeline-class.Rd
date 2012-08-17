\name{Pipeline-class}
\docType{class}

\alias{class:Pipeline}
\alias{Pipeline-class}

\alias{Pipeline}


% subsetting
\alias{findProtocols}
\alias{pipeline}
\alias{protocol}
\alias{protocol<-}
\alias{[,Pipeline-method}
\alias{[<-,Pipeline-method}
\alias{head,Pipeline-method}
\alias{tail,Pipeline-method}
\alias{pipeline,Pipeline-method}
\alias{protocol,Pipeline-method}
\alias{protocol<-,Pipeline-method}
\alias{findProtocols,Pipeline-method}

% combination
\alias{c,Pipeline-method}

% accessors
\alias{displayName,Pipeline-method}
\alias{inType,Pipeline-method}
\alias{outType,Pipeline-method}
\alias{parameters,Pipeline-method}

% perform
\alias{perform,Pipeline-method}

\title{Pipeline: A Sequence of Protocols}

\description{
  A \code{Pipeline} represents a sequence of
  \code{\linkS4class{Protocol}}s. When executed, it executes each of the
  protocols in turn, where each protocol is passed the output of the
  previous protocol. \code{Pipeline} extends \code{list}, so, in
  general, it can be treated as one when it comes to subsetting, etc.
}

\section{Constructor}{
  \describe{
    \item{}{\code{Pipeline(..., displayName = NULL)}: Constructs a
      \code{Pipeline} object with the protocols named in \code{...}. The
      arguments in \code{...} should be named and be either a
      \code{Protocol} object or a role name for which a default
      \code{Protocol} is constructed. Optionally, a user-readable
      display name can be specified as \code{displayName}.
    }
  }
}

\section{Execution}{
  \describe{
    \item{}{\code{perform(object, data, ...)}: Executes the protocols in
      order, with the output of each pipeline passed as input to the
      next. Takes \code{data} as input to the first protocol and returns
      the output of the last protocol.
    }
  }
}

\section{Accessors}{
  \describe{
    \item{}{\code{inType(object)}: Get the input type of the first
      protocol in the pipeline, or \code{NULL} if there are no protocols.
    }
    \item{}{\code{outType(object)}: Get the input type of the first
      protocol in the pipeline, or \code{NULL} if there are no protocols.
    }
    \item{}{\code{parameters(object)}: Obtains a list, each element of which is
      a list of the parameters for the corresponding protocol.
    }
    \item{}{\code{protocol(object, role, method = character())}: Gets
      the first protocol with the matching \code{role} and
      \code{method}.
    }
    \item{}{\code{protocol(object, role) <- value}: Replace the first
      protocol with the role given by \code{role} with \code{value}.
    }
  }
}

\section{Subsetting}{
  \describe{
    \item{}{\code{pipeline(object, intype = "ANY", outtype = "ANY")}:
      Gets the sub-pipeline that spans from the first protocol with the
      input type derived from \code{intype} and the last protocol with
      the output type derived from \code{outtype}.
    }
    \item{}{\code{head(x, n = 6L, role, method = character(), outtype)}:
      Like ordinary \code{\link{head}}, takes a prefix of the
      pipeline. If \code{outtype} is provided, this returns the pipeline
      through the first protocol with \code{outtype} as its output
      type. Otherwise, if \code{role} is specified, the result is the
      pipeline through the first protocol performing that role. This can
      optionally be qualified by \code{method}. If neither \code{role}
      nor \code{outtype} are specified, the first \code{n} elements are
      returned, as usual.
    }
    \item{}{\code{tail(x, n = 6L, role, method = character(), intype)}:
      Like ordinary \code{\link{tail}}, takes a suffix of the
      pipeline. If \code{intype} is provided, this returns the pipeline
      starting at the last protocol with \code{intype} as its input
      type. Otherwise, if \code{role} is specified, the result is the
      pipeline starting at the last protocol performing that role. This
      can optionally be qualified by \code{method}. If neither
      \code{role} nor \code{outtype} are specified, the last \code{n}
      elements are returned, as usual.
    }
    \item{}{\code{Pipeline} extends \code{list}, so, in general, it can
      be treated as one when it comes to subsetting, etc. For example, \code{[}
      method works for subsetting of \code{Pipeline} object. 
    }
  }
}

\section{Utilities}{
  \describe{
    \item{}{\code{findProtocols(object, role, method = character())}:
      Get the indices of the protocols in the pipeline with the
      specified \code{role} and \code{method}.
    }
  }
}

\author{ Michael Lawrence }

\examples{
setStage("average", intype = "numeric")
setProtocol("mean", fun = mean, parent = "average")
setProtocol("quantile", representation = list(probs = "numeric"),
            fun = quantile, parent = "average")
setProtocol("range", representation = list(low = "numeric", high = "numeric"), 
            fun = function(x, low = 0, high = Inf) x[x >= low & x <= high],
            parent = setStage("trim", intype = "numeric"))

d <- c(1, 2, 4)
p <- Pipeline("trim", "average")
perform(p, d)

p <- Pipeline(Protocol("trim", low = 2), "average")
perform(p, d)

p <- Pipeline(Protocol("trim", low = 2),
              Protocol("average", "quantile", probs = 0.75),
              displayName = "Filter and Average")
perform(p, d)

## accessor
inType(p)
outType(p)
parameters(p)
protocol(p, "average")
protocol(p, "average", "quantile")
displayName(p)

## utils
findProtocols(p, "average")

## subsetting
# make a new example
setStage("DemoCastN2C", intype = "numeric", outtype = "character")
setProtocol("cast", fun = function(x){
               message("Convert from numeric to character")
               as.character(x)
            },
            parent = "DemoCastN2C")

setStage("DemoCastC2F", intype = "character", outtype = "factor")
setProtocol("cast", fun = function(x){
               message("Convert from character to factor")
               as.factor(x)
            },
            parent = "DemoCastC2F")

setStage("DemoCastF2L", intype = "factor", outtype = "list")
setProtocol("cast", fun = function(x){
               message("Convert from factor to list")
               as.list(x)
            },
            parent = "DemoCastF2L")

d <- 1:3
p <- Pipeline(Protocol("DemoCastN2C"),
              Protocol("DemoCastC2F"),
              Protocol("DemoCastF2L"))
p
perform(p, d)
# subsetting
# convert to a factor
p12 <- p[1:2]
p12
perform(p12, d)

#
p23 <- pipeline(p, intype = "character")
p23
perform(p23, as.character(d))

#
p12 <- head(p, 2)
p12
#or
head(p, outtype = "factor")
head(p, role = "DemoCastC2F")

tail(p, 2)
tail(p, intype = "character")
tail(p, intype = "factor")
tail(p, role = "DemoCastC2F")


}
