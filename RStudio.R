library(httr)

build_query <- function(meter_ids) {
  url <- "https://facilities.app.vanderbilt.edu/elx/elx.html?open=consumption&meterIds=4295029715&timeframe=custom&aggregationLevel=Hourly&startTime=2018-01-27&endTime=2019-02-27"
  purl <- parse_url(url)
  purl$query$meterIds <- meter_ids
  build_url(purl)
}
