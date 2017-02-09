## ------------------------------------------------------------------------
dots <- function(...) {
  eval(substitute(alist(...)))
}
isTheListNamed <- function (l) {
    nms <- names(l)
    if (any(is.null(nms)) || any(nms == ""))
        return(FALSE)
    else
        return(TRUE)
}

## ------------------------------------------------------------------------
#' Build Cacheable Functions
#' 
#' \code{reactFunc} is a function generator that returns a function that is cacheable when
#' combined with the use of \code{reactArg}.  See example.
#'
#' @name reactFunc
#'
#' @param ARGV
#'     A named list or vector that represent the formal arguments of the returned function.
#' @param ... 
#'     Name-expression pairs that describe the reactive expressions defined in the parent
#'     environment of the returned function. The last one will be used as the returned value
#'     of the returned function.  See example.
#' @return
#'     \code{reactFunc} returns a closure (function); \code{name} should be used inside the
#'     \code{...} arguments to describe the argument as a reactive source.
#' @export
#' @import shiny
#' @importFrom pryr make_function
#' @examples
#' ## Build
#' rf <- reactFunc(
#'     ARGV = alist(x = 42, y = ),
#'     x = reactArg(x),
#'     y = reactArg(y),
#'     a = {
#'         print("Getting a()...")
#'         Sys.sleep(0.5)
#'         x() + 1
#'     },
#'     b = {
#'         print("Getting b()...")
#'         Sys.sleep(0.5)
#'         y() + 1
#'     },
#'     ans = {
#'         print("Getting ans()")
#'         Sys.sleep(0.5)
#'         a() + b()
#'     }
#' )
#' ## Properties
#' #1. Definition
#' rf
#' #2. First run
#' m <- 6; n <- 9
#' system.time(ans <- rf(x = m, y = n))
#' ans
#' #3. Seconde run with the same arguments
#' system.time(ans <- rf(x = m, y = n))
#' ans
#' #4. Third run with an updated argument
#' n <- 7
#' system.time(ans <- rf(x = m, y = n))
#' ans
#' #5. Change the value of `x` to default
#' system.time(ans <- rf(y = n))
#' ans
reactFunc <- function (ARGV, ...) {
    reactContexts <- dots(...)
    reactNames <- names(reactContexts)
    arglist <- as.pairlist(ARGV)
    
    if (!isTheListNamed(reactContexts))
        stop("Names of the ... arguments must be specified.")
    if (!isTheListNamed(arglist))
        stop("ARGV must be a named list or vector.")
    
    ## The reactive expressions are defined here
    for (i in seq_along(reactContexts)) {
        assign(reactNames[[i]], shiny::reactive(reactContexts[[i]], quoted = TRUE))
    }
    
    ## The returned function will assign its arguments to this
    ## handler to make their values reactive.
    ReactiveSources <- shiny::reactiveValues()
    
    pryr::make_function(
        args = arglist,
        body = quote({
            assignedArgs <- #c(as.list(environment()), list(...))
                            as.list(environment())
            assignedArgNames <- names(assignedArgs)

            for (i in seq_along(assignedArgs)) {
                do.call(
                    what = `[[<-`,
                    args = list(
                        eval(parse(text = "ReactiveSources")),
                        assignedArgNames[[i]],
                        eval(parse(text = assignedArgNames[[i]]))
                    ),
                    envir = parent.env(environment())
                )
            }
            
            # Get result that produced by reactive expressions
            lastExprName <- tail(reactNames, 1)
            result <- shiny::isolate(
                do.call(lastExprName, args = list())
            )
            result
        })
    )
}

## ------------------------------------------------------------------------
## We need this function because `reactiveValues` can not be copied by hand:
## > r <- reactiveValues(x = 34)
## > x <- r$x     # Not copyable
#' @rdname reactFunc
#' @param name
#'     A symbol that corresponds to one of the names of \code{ARGV} (one of the argument name).
#'     See example.
#' @export
reactArg <- function (name) {
    namechr <- deparse(substitute(name))
    intended.expr <- parse(
        text = paste0("ReactiveSources$",namechr)
    )
    eval(intended.expr, envir = parent.frame())
}

