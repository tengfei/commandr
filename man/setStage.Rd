\name{setStage}
\alias{setStage}
\title{
  Define a Stage Class
}
\description{
  This function defines new derivatives of the \code{Stage} class. It is
  a wrapper around \code{\link{setClass}} and thus has a similar
  interface.
}
\usage{
setStage(name, dispname = name, intype = "ANY", outtype = intype,
         where = topenv(parent.frame()))
}
\arguments{
  \item{name}{
    The name of the stage, i.e., the role string
  }
  \item{dispname}{
    The name for display in a user interface
  }
  \item{intype}{
    The class of the data that protocols of this stage accept as input
  }
  \item{outtype}{
    The class of the data that protocols of this stage accept as output
  }
  \item{where}{
    The environment in which to define the stage class
  }
}
\details{
  Calling \code{setStage} defines two classes:
  \itemize{
    \item{A derivative of \code{\linkS4class{Stage}} that represents
      this stage}
    \item{A derivative of \code{\linkS4class{Protocol}} from which
      all protocols that implement this stage derive}
  }
  For example, if we had a stage named \dQuote{average},
  calling \code{setStage} would create the classes \code{StageAverage}
  and \code{ProtoAverage}.
  
  The function also defines a generic, named by the \code{name}
  argument, that performs a protocol of this stage. There is a method
  that takes an object of type \code{intype} as first argument and a
  method name as its second. Additional arguments are passed to
  the \code{perform} method of the protocol. In our prior example, there
  would be a generic called \code{average} and
  method \code{average,numeric} if \code{intype} was given as
  \dQuote{numeric}.

  It also defines a generic of the form \code{nameProto} that serves
  as an accessor for protocols of this stage. A method is defined for
  \code{Pipeline} and \code{outtype}, so that one could retrieve our
  \dQuote{average} protocol with \code{averageProto(pipeline)} or
  \code{averageProto(result)}. Similarly, a replacement generic and
  methods are defined.
}

\value{
  The name of the role
}
\author{
  Michael Lawrence
}

\seealso{
  The \code{\linkS4class{Stage}} class; \code{\link{setProtocol}} for
  defining a new type of protocol
}

\examples{
## simplest definition
setStage("average")
## add a display name and specialize to numeric input
setStage("average", "Average Vector", "numeric")
}
