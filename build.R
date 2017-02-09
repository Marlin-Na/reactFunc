#!/usr/bin/env Rscript


purlRmd2R <- function (Source, Destination) {
    tmpfile <- tempfile()
    knitr::purl(
        input = Source,
        output = tmpfile
    )
    if (file.exists(Destination)) {
        if (tools::md5sum(tmpfile) != tools::md5sum(Destination)) {
            file.remove(Destination)
            file.copy(tmpfile, Destination)
            cat(paste("Remove and generate file to", Destination))
        }
    }
    else {
        file.copy(tmpfile, Destination)
        cat(paste("Generate file to", Destination))
    }
}

purlRmd2R("vignettes/reactFunc.Rmd", "R/reactFunc-out.R")

devtools::document()


