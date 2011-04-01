\name{Operation-class}
\docType{class}

\alias{class:Operation}
\alias{Operation-class}

\alias{Operation}
\alias{OperationQ}

\alias{eval,Operation-method}
\alias{rev,Operation-method}

\title{Operation: A Simple Command Implementation}

\description{
  An \code{Operation} object implements a \code{Command} with an R
  closure. This may be useful as a base for customized \code{Command}
  derivatives. Otherwise, consider it a skeletal proof-of-concept.
}

\section{Constructors}{
  \describe{
    \item{}{\code{Operation(do, undo = NULL)}: Constructs
      an \code{Operation} that invokes the closure \code{do}. If
      \code{undo} is specified, it is the closure invoked for the
      reverse operation.
    }
    \item{}{\code{OperationQ(do, undo)}: Constructs
      an \code{Operation} that invokes a function with the body taken from
      the quoted argument \code{do}. The function is enclosed in the
      calling environment.  If \code{undo} is specified, it is
      treated equivalently to \code{do}, except it should implement the
      reverse operation.
    }
  }
}

\section{Methods}{
  \describe{
    \item{}{\code{eval(expr)}: Executes the operation by evaluating the
      closure.}
    \item{}{\code{rev(x)}: Returns a new \code{Operation} that performs
      the reverse operation, as long as \code{undo} was provided.
    }
  }
}

\author{ Michael Lawrence }

\examples{
  op <- OperationQ(message("hello world"))
  eval(op)

  x <- 2
  op <- OperationQ(x^2, sqrt(x))
  x <- eval(op)
  rop <- rev(op)
  identical(2, eval(rop))
}
