\name{setProtocol}
\alias{setProtocol}
\title{Define a Protocol Type}
\description{
  This function defines new derivatives of the \code{Protocol} class. It
  is a wrapper around \code{\link{setClass}} and thus has a very similar
  interface.
}
\usage{
setProtocol(method, dispname = method, representation = list(), fun,
            parent, prototype = list(), validity = NULL,
            where = topenv(parent.frame()))
}

\arguments{
  \item{method}{
    The name of the method performed by protocols of this type
  }
  \item{dispname}{
    The display name for protocols of this type
  }
  \item{representation}{
    A list declaring the names and types of the parameters, implemented
    as slots in the S4 class
  }
  \item{fun}{
    The function implementing the protocol. If omitted, this protocol
    type will be virtual. This function will be passed the input data,
    any parameters named in its formals, and any arguments passed
    to \code{perform}. Default values for its arguments override values
    in \code{prototype}. Use \code{\link{callNextProtocol}} to chain up
    to the implementation of a parent protocol.
  }
  \item{parent}{
    The single parent/super class for this protocol class. Usually, this
    is the role, i.e., the name of the \code{\linkS4class{Stage}} for
    this protocol type. Also could be the name of a class inheriting
    from \code{Protocol}, or the concantenation of the role and method
    names.
  }
  \item{prototype}{
    As in \code{\link{setClass}}, the list indicating initial values for
    the parameters/slots. Usually not necessary, because it is derived
    from the formals of \code{fun}.
  }
  \item{validity}{
    The function for checking the validity of an object, see
    \code{\link{setClass}}. 
  }
  \item{where}{
    The environment in which this protocol class is defined.
  }
}

\details{
  Every type of protocol in a pipeline is implemented as an S4 class,
  ultimately derived from \code{Protocol}. The parameters controlling
  the execution of the protocol are represented by slots in that S4
  class.

  Through S4 inheritance, each protocol is associated with a
  \code{Stage}, which represents the role a protocol plays in the
  pipeline. For example, a protocol might have an \dQuote{average}
  stage, with two protocols: \dQuote{mean} and \dQuote{median}. Here,
  \dQuote{average} would be the role name and would have an associated
  \code{Stage} derivative. Meanwhile, \dQuote{mean} and \dQuote{median}
  are \code{method} names and would each have a corresponding
  \code{Protocol} derivative. Protocols that have the same stage all
  derive from a common, virtual \code{Protocol} derivative corresponding
  to that stage. In our example, we would have two protocol classes:
  \code{ProtoAverageMean} and \code{ProtoAverageMedian}. Both would
  inherit from \code{ProtoAverage}, which in turn inherits from
  \code{Protocol}.

  Another side effect of this function is that a generic is defined,
  named of the form \code{role.Method}, that performs this protocol,
  given the data and additional arguments. There is a method for the
  \code{inType} of the stage. Thus, in our example, we would have
  generics \code{average.mean} and \code{average.median}.
}

\value{
  The name of the class
}

\author{
  Michael Lawrence
}

\seealso{
  \code{\link{Protocol}} for constructing protocol
  objects, \code{\link{setStage}} for defining \code{Stage} classes.
}

\examples{
  setStage("average")
  setProtocol("mean", fun = mean, parent = "average")
  setProtocol("median", fun = median, parent = "average")
  d <- c(1, 2, 4)
  average(d)
  average(d, "median")
  average.median(d)
}
