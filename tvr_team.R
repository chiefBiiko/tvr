# tvr_team

TVRT <- list()
TVRT$NAME <- 'Biiko'  # 'Balou', 'Christian
TVRT$STORE_ID <- '1h2msv'
TVRT$DATA <- file.path(.libPaths()[1], 'tvr', 'tvr.Rda')

tvr_push <- function(name=TVRT$NAME, store_id=TVRT$STORE_ID) {
  # Update and view ur team's remote store
  # @param {character} name Ur name
  # @param {character} store_id Ur team's remote store identifier
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(isTRUE(require('jsonlite')), isTRUE(require('httr')), isTRUE(require('timevis')), 
            nchar(name) > 0, nchar(store_id) > 0, file.exists(TVRT$DATA))
  load(TVRT$DATA)
  tvr.team <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
  tvr.team$tsks <- rbind.data.frame(tvr.team$tsks, tvr.data)  # merging
  if (nrow(tvr.team$tsks) > 0) tvr.team$tsks$id <- 1:nrow(tvr.team$tsks)  # reassigning ids here
  rownames(tvr.team$tsks) <- NULL
  re <- httr::PUT(paste0('https://api.myjson.com/bins/', store_id), body=tvr.team, encode='json')
  if (httr::status_code(re) != 200) stop('Upload error.')
  tvr.team <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
  View(tvr.team$tsks)
  return(timevis::timevis(tvr.team$tsks, tvr.team$hash))
}

tvr_team <- function(store_id=TVRT$STORE_ID) {
  # Pull and view ur team's remote store
  # @param {character} store_id Ur team's remote store identifier
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(isTRUE(require('jsonlite')), isTRUE(require('timevis')), nchar(store_id) > 0)
  tvr.team <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
  return(timevis::timevis(tvr.team$tsks, tvr.team$hash))
}