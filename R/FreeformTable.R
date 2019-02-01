#' Create a table of dimensions and metrics
#'
#' This function mimics the functionality of the Freeform Table panel in Analysis
#' Workspace.The main difference between this function and Analysis Workspace is
#' that individual row drill-downs aren't possible due to the constraints of
#' the data.frame data structure. Thus, requesting to drill-down by a second
#' dimension will do so for all rows, not just a single row.
#'
#'
#' @param rsid (character) The report suite ID
#' @param dimension (character) Dimension (props, eVars, etc.) for report breakdown
#' @param startDate (character/Date) Global report filter: start date
#' @param endDate (character/Date) Global report filter: end date
#' @param locale
#' @param globalFilters
#' @param search
#' @param statistics
#' @param rowContainer
#' @param anchorDate
#' @param as.data.frame
#'
#' @return data.frame or S3 "FreeformTableList"
#' @export
#'
#' @seealso \code{\link{GetDimensions}} \code{\link{GetMetrics}}
#'
#' @examples
FreeformTable <- function(rsid,
                          dimension,
                          startDate,
                          endDate,
                          locale=NULL,
                          globalFilters=NULL,
                          search=NULL,
                          statistics=NULL,
                          rowContainer=NULL,
                          anchorDate=NULL,
                          as.data.frame=TRUE,
                          show=FALSE){

  ####
  #### validate inputs
  ####

  assertthat::assert_that(is.character(rsid),
                          msg="rsid required to be class 'character'")

  assertthat::assert_that(is.character(dimension),
                          msg="dimension required to be class 'character'")

  assertthat::assert_that(is.character(startDate) || assertthat::is.date(startDate),
                          msg="startDate required to be class 'character' or 'Date'")

  assertthat::assert_that(is.character(endDate) || assertthat::is.date(endDate),
                          msg="endDate required to be class 'character' or 'Date'")

  assertthat::assert_that(as.Date(endDate) >= as.Date(startDate),
                          msg="endDate must be >= startDate")

  ####
  #### convert inputs to valid values
  ####

  if(!startsWith(dimension, "variables/")){
    dimension <- paste("variables/", dimension, sep="")
  }

  #dateRange filter required
  gfilters <-list()

  dr <- list(type = "dateRange",
             dateRange = sprintf("%sT00:00:00.000/%sT23:59:59.999",
                                 startDate,
                                 endDate)
             )
  gfilters <- append(gfilters, list(dr))
  #TODO: incorporate globalFilters kwarg

  ####
  #### retrieve results
  ####

  r <- list()
  page <- 0

  repeat{

      request <- list(

        #single string
        rsid = rsid,

        #single string
        dimension = dimension,

        #compound object
        #TODO: make a locale function
        locale = locale,

        #compound object, list for each object
        #TODO: make append for all filters, kwarg for additional filters
        globalFilters = gfilters,

        #compound object
        #TODO: make append for search, kwarg for additional filters
        search = search,

        #flat object, named list directly specifies settings
        #TODO: find other settings, dimensionSort might not always make sense as asc
        settings = list(dimensionSort = "asc", limit=50, page=page),

        #compound object
        #TODO: make append for all filters, kwarg for additional filters
        statistics = statistics,

        #compound object, metrics and metricsFilter only two top-level choices?
        #TODO: make append for all filters, kwarg for additional filters
        metricContainer = c(list(
          metrics = list(
            list(columnId = "pageviews", id = "metrics/pageviews"),
            list(columnId = "visits", id = "metrics/visits")
          ) #metrics

        )  #inner metricContainer
        ), #outer metricContainer

        #compound object
        #TODO: make append for all filters, kwarg for additional filters
        rowContainer = rowContainer,

        #single string
        #TODO: make append for all filters, kwarg for additional filters
        anchorDate = anchorDate
      )

      if(show){
        print("-----------------")
        print(jsonlite::toJSON(request,
                               auto_unbox = TRUE,
                               pretty=TRUE,
                               null='null'))
        print("-----------------")
      }

      tmp <- adobe_post("/reports", request)
      r <- append(r, list(tmp))
      page <- page + 1

      #Catch error if it happens
      #If no error, check if its last page and break if it is
      #Otherwise, keep paging over results
      if(length(tmp$response$columns$columnErrors$errorDescription) > 0){
        #Just print off first error, hopefully enough to help user fix
        stop(tmp$response$columns$columnErrors$errorDescription[1])

      } else if(tmp$response$lastPage){
        break
      }

  }

  #Set S3 method for easier parsing later
  class(r) <- append(class(r), "FreeformTableList")

  #Return a data.frame or just an S3 object
  if(as.data.frame){

    #pull result out of list of lists, flatten to single df
    list_df <- lapply(r, function(x) x$response$rows)
    flattened_df <- dplyr::bind_rows(list_df)

    #parse out metrics from data list col
    #go over column as a list, transpose cell so data.frame comes out correct
    #while this may be inefficient, it is one pass over the data, so maybe ok
    parsed_data <- dplyr::bind_rows(lapply(flattened_df$data, function(x) as.data.frame(t(x))))
    names(parsed_data) <- c("pageviews", "visits") #TODO: get this automatically

    #don't need data column after its split into multiple cols
    flattened_df$data <- NULL

    return(cbind(flattened_df, parsed_data))
  }

  return(r)

}
