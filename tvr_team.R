# tvr_team

lapply(list('jsonlite', 'httr', 'timevis'), function(p) {
  if (!p %in% installed.packages()) install.packages(p)
})

TVRT <- list()
TVRT$NAME <- 'Biiko'  # 'Balou', 'Christian'
TVRT$STORE_ID <- '1h2msv'
TVRT$DATA <- file.path(.libPaths()[1], 'tvr', 'tvr.Rda')
TVRT$ID <- sapply(list(TVRT$NAME), function(n) {
  hash <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', TVRT$STORE_ID))$hash
  return(hash[hash$content == n, 'id'])  # ur personal ID
})

tvr_team <- function(store_id=TVRT$STORE_ID) {
  # Pull and view ur team's remote store
  # @param {character} store_id Ur team's remote store identifier
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(nchar(store_id) > 0)
  tvr.team <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
  print.data.frame(tvr.team$tsks)
  return(timevis::timevis(tvr.team$tsks, tvr.team$hash))
}

tvr_push <- function(name=TVRT$NAME, store_id=TVRT$STORE_ID, data=TVRT$DATA, id=TVRT$ID) {
  # Update and view ur team's remote store
  # @param {character} name Ur name
  # @param {character} store_id Ur team's remote store identifier
  # @param {character} data Path 2 ur personal tvr data
  # @param {integer} id Ur personal identifier
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(nchar(name) > 0, nchar(store_id) > 0, file.exists(data), is.integer(id))
  load(data)
  tvr.team <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
  tvr.team$tsks <- tvr.team$tsks[tvr.team$tsks$group != id, ]  # cutting b4 merging
  tvr.team$tsks <- rbind.data.frame(tvr.team$tsks, tvr.data, make.row.names=F, stringsAsFactors=F)
  if (nrow(tvr.team$tsks) > 0) tvr.team$tsks$id <- 1:nrow(tvr.team$tsks)  # reassigning ids here
  re <- httr::PUT(paste0('https://api.myjson.com/bins/', store_id), body=tvr.team, encode='json')
  if (re$status_code != 200) stop('Upload error.')
  tvr.team <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
  print.data.frame(tvr.team$tsks)
  return(timevis::timevis(tvr.team$tsks, tvr.team$hash))
}