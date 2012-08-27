\name{Command-class}
\docType{class}

\alias{class:Command}
\alias{Command-class}

\alias{displayName}
\alias{displayName,ANY-method}

\alias{widget}

\alias{active}
\alias{active,Command-method}

\alias{active<-}
\alias{active<-,Command-method}

\title{Virtual Command Class}

\description{
  \code{Command} represents any generic operation and is often combined
  with other such objects in a sequence. This is the foundation for
  implementations of pipelines, logging, undo stacks, etc, according
  to the Command design pattern. Since \code{Command} is virtual, it
  cannot be constructed directly. Rather, one should construct an
  instance of a concrete derivative, like \code{\linkS4class{Operation}}
  or \code{\linkS4class{Protocol}}.
}

\details{
  Developers are encouraged to extend \code{Command} for new
  implementations of the Command design pattern.

  There are a number of generics for which a \code{Command} derivative
  should provide methods:
  \describe{
    \item{}{\code{displayName(object)}: Gets the display name of the
      command, i.e., the name displayed in a user interface. The default
      implementation returns the class name. A \code{Command} may
      provide other visual attributes; the display name is considered
      fundamental for integration with user interfaces.
    }
    \item{}{\code{widget(object, ...)}: Creates a widget for viewing and
      controlling this object. No default implementation.
    }
    \item{}{\code{rev(object)}: Returns a \code{Command} that performs
      the opposite action, i.e., to undo an operation. This will not
      always be possible, so it is acceptable for a subclass to leave
      this unimplemented.
    }
    \item{}{\code{active(object)}: Gets whether the command is
      considered active. This is meant for temporarily disabling or
      enabling a command without removing it completely from, e.g., a
      pipeline or GUI menu. No default implementation.
    }
  }

  Of course, a \code{Command} implementation also needs a method that
  executes the command. The signature for such a function highly depends
  on the nature of the command, so the generic depends on the class.
}

\author{ Michael Lawrence }
