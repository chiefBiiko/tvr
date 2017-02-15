# tvr_team

NAME <- 'Biiko'  # 'Balou'
STORE_ID <- '1h2msv'
tvr_DATA <- file.path(.libPaths()[1], 'tvr', 'tvr.Rda')
tvr_TEAM <- file.path(.libPaths()[1], 'tvr', 'tvr_team.Rda')

tvr_push <- function (name=NAME, store_id=STORE_ID) {
  # Update and view ur team's remote store
  stopifnot(isTRUE(require('timevis')), nchar(name) > 0, nchar(store_id) > 0,
            file.exists(tvr_DATA))
  load(tvr_DATA)
  tvr.team <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
  save(tvr.team, file=tvr_TEAM)
  # prepare 4 PUT ... aka merge tvr.data and tvr.team
  # ... PUT
  # show response data
}

tvr_team <- function (store_id=STORE_ID) {
  # Pull and view ur team's remote store
  stopifnot(isTRUE(require('timevis')), nchar(store_id) > 0)
  tvr.team <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', store_id))
  save(tvr.team, file=tvr_TEAM)
  return(timevis::timevis(tvr.team))
}
