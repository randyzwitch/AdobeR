#' Title
#'
#' @param rsid (character) The report suite ID
#'
#' @return
#' @export
#'
#' @examples
FreeformTable <- function(rsid,
                          as.data.frame=TRUE){

  assertthat::assert_that(is.character(rsid),
                          msg="rsid required to be class 'character'")

  r <- list()
  page <- 0

  repeat{

      request <- list(
        #single string
        rsid = rsid,

        #single string
        dimension = "variables/daterangeday",

        #compound object
        locale = list(),

        #compound object, list for each object
        globalFilters = list(
          list(
            type = "dateRange",
            dateRange = "2017-12-31T00:00:00.000/2019-01-06T23:59:59.999"
              )
        ),

        #compound object
        search = list(),

        #flat object, named list directly specifies settings
        #TODO: find other settings, dimensionSort might not always make sense as asc
        settings = list(dimensionSort = "asc",
                        limit=50,
                        page=page),

        #compound object
        statistics = list(),

        #compound object, metrics and metricsFilter only two top-level choices?
        metricContainer = c(list(
          metrics = list(
            list(columnId = "pageviews", id = "metrics/pageviews"),
            list(columnId = "visits", id = "metrics/visits")
          ) #metrics

        ) #inner metricContainer
        ), #outer metricContainer

        #compound object
        rowContainer = list(),

        #single string
        anchorDate = NULL
      )

      tmp <- adobe_post("/reports", request)
      r <- append(r, list(tmp))
      page <- page + 1

      if(tmp$response$lastPage){
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
