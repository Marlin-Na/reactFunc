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
#' @param ARGV
#'     A named list or vector that represent the formal arguments of the returned function.
#' @param ... 
#'     Name-expression pairs that describe the reactive expressions defined in the parent
#'     environment of the returned function. The last one will be used as the returned value
#'     of the returned function.  See example.
#' @return
#'     \code{reactFunc} returns a function that caches its intermediate results.
#'     Upon each call to the returned function, if the arguments does not change, the function
#'     will return the cached result, otherwise it will recalculate the needed parts.
#'     See example.
#' @export
#' @import shiny
#' @importFrom pryr make_function
#' @examples
#' ## Build
#' rf <- reactFunc(
#'     ARGV = alist(x = 42, y = ),
#'     a = {
#'         print("Getting a()..."); Sys.sleep(0.5)
#'         x + 1
#'     },
#'     b = {
#'         print("Getting b()..."); Sys.sleep(0.5)
#'         y + 1
#'     },
#'     ans = {
#'         print("Getting ans()"); Sys.sleep(0.5)
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
    .reactContexts <- dots(...)
    .reactNames <- names(.reactContexts)
    .arglist <- as.pairlist(ARGV)
    
    if (!isTheListNamed(.reactContexts))
        stop("Names of the ... arguments must be specified.")
    if (!isTheListNamed(.arglist))
        stop("ARGV must be a named list or vector.")
    if (anyDuplicated(names(.arglist)))
        warning("Names of the ... arguments have duplicated values,",
                "which may cause unexpected results.")
    if (anyDuplicated(.reactNames))
        warning("Names of the ARGV argument have duplicated values,",
                "which may cause unexpected results.")
    if (anyDuplicated(c(names(.arglist), .reactNames)))
        warning("Names of the ARGV argument and names of the ... arguments",
                "have mutual values, which may cause unexpected results.")
    
    ## The reactive expressions are defined here
    for (i in seq_along(.reactContexts)) {
        assign(.reactNames[[i]], shiny::reactive(.reactContexts[[i]], quoted = TRUE))
    }
    
    pryr::make_function(
        args = .arglist,
        body = quote({
            assignedArgs <- #c(as.list(environment()), list(...))
                            as.list(environment())
            assignedArgNames <- names(assignedArgs)

            for (i in seq_along(assignedArgs)) {
                assign(
                    x = assignedArgNames[[i]],
                    value = assignedArgs[[i]],
                    #value = eval(parse(text = assignedArgNames[[i]])),
                    envir = parent.env(environment())
                )
                if (!exists(paste0(".has_reactive_binding.", assignedArgNames[[i]]))) {
                    shiny::makeReactiveBinding(
                        symbol = assignedArgNames[[i]],
                        env = parent.env(environment())
                    )
                    assign(
                        paste0(".has_reactive_binding.", assignedArgNames[[i]]),
                        value = TRUE,
                        envir = parent.env(environment())
                    )
                }
            }
            
            # Get result that produced by reactive expressions
            lastExprName <- tail(.reactNames, 1)
            result <- shiny::isolate(
                do.call(lastExprName, args = list())
            )
            result
        
        })
    )
}

## ---- purl=TRUE----------------------------------------------------------
#' Convert a cacheable function to a normal function
#'
#' Functions built with \code{\link{reactFunc}} can be converted to a normal
#' version so that you do not need to replicate yourself.
#' 
#' @param f
#'     A function built with \code{\link{reactFunc}}.
#' @return
#'     Return a function having similar behavior with the input function.
#' @export
#'
#' @examples
#' refunc <- reactFunc(ARGV = alist(x = , y = ),
#'     ans = a() * b(),
#'     a = x + 1,
#'     b = y - 3,
#'     ret = ans()
#' )
#' normfunc <- asNormalFunc(refunc)
#' normfunc
#' identical(refunc(6,9), normfunc(6,9))
asNormalFunc <- function (f) {
    stopifnot(is.function(f))
    stopifnot(exists(".reactContexts", envir = environment(f)))
    reactContexts <- get(".reactContexts", environment(f))
    assignments <- mapply(
        function (var, value) bquote(.(as.symbol(var)) <- function () .(value)),
        var = names(reactContexts),
        value = reactContexts,
        SIMPLIFY = FALSE
    )
    retchr <- list(
        bquote(return(.(call(tail(names(reactContexts),1)))))
    )
    funcbody <- {
        exprlist <- c(quote(`{`), c(assignments, retchr))
        as.call(exprlist)
    }
    arglist <- get(".arglist", environment(f))
    pryr::make_function(args = arglist, body = funcbody, parent.frame())
}

