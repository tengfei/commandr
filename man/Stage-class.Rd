\name{Stage-class}
\docType{class}

\alias{class:Stage}
\alias{Stage-class}

% constructor
\alias{Stage}

% accessors
\alias{defaultMethod}
\alias{defaultMethod,Stage-method}
\alias{defaultMethod,character-method}
\alias{defaultMethod,missing-method}
\alias{defaultMethod<-}
\alias{defaultMethod<-,Stage-method}
\alias{inType}
\alias{outType}
\alias{role}
\alias{role,Stage-method}

\title{Stage: Abstract Step in a Pipeline}

\description{
  A \code{Stage} object represents a role to be played by protocols in a
  pipeline. In other words, a stage is an abstract step that transforms
  one data type into another. The transformation may be implemented in a
  number of ways, each corresponding to a protocol. Users normally do
  not have to interact with this object. Developers can define new types
  of stages with \code{\link{setStage}}.
}

\section{Constructors}{
  \describe{
    \item{}{\code{Stage(role)}:
      Creates a stage object given the \code{role} name.
    }
  }
}

\section{Accessors}{
  \describe{
    \item{}{\code{role(object)}: Gets the name of the role represented by
      this stage.
    }
    \item{}{\code{displayName(object)}: Gets the name for displaying
      this stage in a user interface.
    }
    \item{}{\code{inType(object)}: Gets the class of data that protocols
      of this stage accept as input.}
    \item{}{\code{outType(object)}: Gets the class of data that protocols
      of this stage yield as output.}
    \item{}{\code{defaultMethod(object)}: Gets the name of the default
      method associated with the role of this stage. If not explicitly
      set, this becomes the first protocol registered for the stage.
    }
    \item{}{\code{defaultMethod(object) <- value}: Sets the name of the
      default method associated with the role of this stage. 
    }
    \item{}{\code{methodNames(object, where =
      topenv(parent.frame()))}: Gets the names of the methods
      for this stage, looking in \code{where} for the protocol classes.
    }
  }
}

\seealso{\code{\link{setStage}} for defining new types of stages}

\author{ Michael Lawrence }

\examples{
setStage("average", "Average Numbers", intype = "numeric")
setProtocol("mean", fun = mean, parent = "average")
setProtocol("median", fun = median, parent = "average")

stage <- Stage("average")
stage

defaultMethod(stage)
defaultMethod(stage) <- "median"
defaultMethod(stage)
}
