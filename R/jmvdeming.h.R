
# This file is automatically generated, you probably don't want to edit this

jmvdemingOptions <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jmvdemingOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            method1 = NULL,
            method2 = NULL,
            ciWidth = 95,
            testValue = 1,
            plotcon = FALSE,
            plotcheck = FALSE,
            weighted = FALSE,
            xlabel = "Method: 1",
            ylabel = "Method: 2", ...) {

            super$initialize(
                package="SimplyAgree",
                name="jmvdeming",
                requiresData=TRUE,
                ...)

            private$..method1 <- jmvcore::OptionVariable$new(
                "method1",
                method1,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"),
                rejectInf=FALSE)
            private$..method2 <- jmvcore::OptionVariable$new(
                "method2",
                method2,
                suggested=list(
                    "continuous"),
                permitted=list(
                    "numeric"),
                rejectInf=FALSE)
            private$..ciWidth <- jmvcore::OptionNumber$new(
                "ciWidth",
                ciWidth,
                min=50,
                max=99.9,
                default=95)
            private$..testValue <- jmvcore::OptionNumber$new(
                "testValue",
                testValue,
                default=1)
            private$..plotcon <- jmvcore::OptionBool$new(
                "plotcon",
                plotcon,
                default=FALSE)
            private$..plotcheck <- jmvcore::OptionBool$new(
                "plotcheck",
                plotcheck,
                default=FALSE)
            private$..weighted <- jmvcore::OptionBool$new(
                "weighted",
                weighted,
                default=FALSE)
            private$..xlabel <- jmvcore::OptionString$new(
                "xlabel",
                xlabel,
                default="Method: 1")
            private$..ylabel <- jmvcore::OptionString$new(
                "ylabel",
                ylabel,
                default="Method: 2")

            self$.addOption(private$..method1)
            self$.addOption(private$..method2)
            self$.addOption(private$..ciWidth)
            self$.addOption(private$..testValue)
            self$.addOption(private$..plotcon)
            self$.addOption(private$..plotcheck)
            self$.addOption(private$..weighted)
            self$.addOption(private$..xlabel)
            self$.addOption(private$..ylabel)
        }),
    active = list(
        method1 = function() private$..method1$value,
        method2 = function() private$..method2$value,
        ciWidth = function() private$..ciWidth$value,
        testValue = function() private$..testValue$value,
        plotcon = function() private$..plotcon$value,
        plotcheck = function() private$..plotcheck$value,
        weighted = function() private$..weighted$value,
        xlabel = function() private$..xlabel$value,
        ylabel = function() private$..ylabel$value),
    private = list(
        ..method1 = NA,
        ..method2 = NA,
        ..ciWidth = NA,
        ..testValue = NA,
        ..plotcon = NA,
        ..plotcheck = NA,
        ..weighted = NA,
        ..xlabel = NA,
        ..ylabel = NA)
)

jmvdemingResults <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jmvdemingResults",
    inherit = jmvcore::Group,
    active = list(
        text = function() private$.items[["text"]],
        demtab = function() private$.items[["demtab"]],
        plotcon = function() private$.items[["plotcon"]],
        plotcheck = function() private$.items[["plotcheck"]]),
    private = list(),
    public=list(
        initialize=function(options) {
            super$initialize(
                options=options,
                name="",
                title="Deming Regression")
            self$add(jmvcore::Html$new(
                options=options,
                name="text"))
            self$add(jmvcore::Table$new(
                options=options,
                name="demtab",
                title="Regression Coefficients",
                rows=2,
                columns=list(
                    list(
                        `name`="var", 
                        `title`="", 
                        `type`="text"),
                    list(
                        `name`="estimate", 
                        `title`="Estimate", 
                        `type`="number"),
                    list(
                        `name`="se", 
                        `title`="SE", 
                        `type`="number"),
                    list(
                        `name`="df", 
                        `title`="df", 
                        `type`="number"),
                    list(
                        `name`="lowerci", 
                        `title`="Lower C.I.", 
                        `type`="number"),
                    list(
                        `name`="upperci", 
                        `title`="Upper C.I", 
                        `type`="number"))))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotcon",
                title="Line-of-Identity Plot",
                visible="(plotcon)",
                renderFun=".plotcon",
                width=450,
                height=400))
            self$add(jmvcore::Image$new(
                options=options,
                name="plotcheck",
                title="Check Assumptions",
                visible="(plotcheck)",
                renderFun=".plotcheck",
                width=650,
                height=400))}))

jmvdemingBase <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "jmvdemingBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package = "SimplyAgree",
                name = "jmvdeming",
                version = c(1,0,0),
                options = options,
                results = jmvdemingResults$new(options=options),
                data = data,
                datasetId = datasetId,
                analysisId = analysisId,
                revision = revision,
                pause = NULL,
                completeWhenFilled = FALSE,
                requiresMissings = FALSE)
        }))

#' Deming Regression
#'
#' 
#' @param data Data
#' @param method1 Name of column containing 1st Vector of data
#' @param method2 Name of column containing Vector of data
#' @param ciWidth a number between 50 and 99.9 (default: 95), the width of
#'   confidence intervals
#' @param testValue Ratio of the two error variances. Default is 1.
#' @param plotcon \code{TRUE} or \code{FALSE} (default), for Bland-Altman plot
#' @param plotcheck \code{TRUE} or \code{FALSE} (default), assumptions plots
#' @param weighted \code{TRUE} or \code{FALSE}
#' @param xlabel The label for the x-axis
#' @param ylabel The label for the y-axis
#' @return A results object containing:
#' \tabular{llllll}{
#'   \code{results$text} \tab \tab \tab \tab \tab a html \cr
#'   \code{results$demtab} \tab \tab \tab \tab \tab a table \cr
#'   \code{results$plotcon} \tab \tab \tab \tab \tab an image \cr
#'   \code{results$plotcheck} \tab \tab \tab \tab \tab an image \cr
#' }
#'
#' Tables can be converted to data frames with \code{asDF} or \code{\link{as.data.frame}}. For example:
#'
#' \code{results$demtab$asDF}
#'
#' \code{as.data.frame(results$demtab)}
#'
#' @export
jmvdeming <- function(
    data,
    method1,
    method2,
    ciWidth = 95,
    testValue = 1,
    plotcon = FALSE,
    plotcheck = FALSE,
    weighted = FALSE,
    xlabel = "Method: 1",
    ylabel = "Method: 2") {

    if ( ! requireNamespace("jmvcore", quietly=TRUE))
        stop("jmvdeming requires jmvcore to be installed (restart may be required)")

    if ( ! missing(method1)) method1 <- jmvcore::resolveQuo(jmvcore::enquo(method1))
    if ( ! missing(method2)) method2 <- jmvcore::resolveQuo(jmvcore::enquo(method2))
    if (missing(data))
        data <- jmvcore::marshalData(
            parent.frame(),
            `if`( ! missing(method1), method1, NULL),
            `if`( ! missing(method2), method2, NULL))


    options <- jmvdemingOptions$new(
        method1 = method1,
        method2 = method2,
        ciWidth = ciWidth,
        testValue = testValue,
        plotcon = plotcon,
        plotcheck = plotcheck,
        weighted = weighted,
        xlabel = xlabel,
        ylabel = ylabel)

    analysis <- jmvdemingClass$new(
        options = options,
        data = data)

    analysis$run()

    analysis$results
}

