library(httr)

build_query <- function(meter_ids) {
  url <- "https://facilities.app.vanderbilt.edu/elx/elx.html?open=consumption&meterIds=4295030764&timeframe=custom&aggregationLevel=Daily&startTime=2018-03-22&endTime=2019-03-21#!/consumption-demand"
  purl <- parse_url(url)
  purl$query$meterIds <- meter_ids
  purl$query$aggregationLevel <- "Hourly"
  build_url(purl)
}


