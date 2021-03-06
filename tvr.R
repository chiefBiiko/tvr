# tvr (task viewer)
# 
# Note
#   designed 2 be stoopid simple
#   @tvr_add()
#     4 ease of use avoid exact times, instead just pass values of Sys.Date(),
#       Sys.Date() + 4, etc. 2 @params start and end
#     allow every task 2 take at least 1 day
#     if u add multiple tasks at once make sure @params content, start, and end
#       r all of the same length
# 
# Data format (just 4 u 2 know)
#   tvr.data <- data.frame(id=NULL, content=NULL, start=NULL, end=NULL, group=NULL)
#   @column {integer} id Task identifier
#   @column {character} content Task description
#   @column {character} start Task start date
#   @column {character} end Task end date
#   @column {integer} group Ur personal identifier [TVR$ID]
#   tvr.data is saved as ur personal task store @ TVR$DATA
#
# Usage
#   tvr()  # view ur tasks
#   # add tasks
#   tvr_add(c('new task', 'foo task'), rep(Sys.Date(), 2), rep(Sys.Date() + 1, 2))
#   tvr_rm(1:2)  # remove tasks
#   # working in a team
#   tvr_team()  # pull and view ur team's data
#   tvr_push()  # update ur team's remote store with ur own data

lapply(list('jsonlite', 'timevis'), function(p) {
  if (!p %in% .packages(T)) install.packages(p)
})

TVR <- list()
TVR$NAME <- 'Biiko'  # 'Balou', 'Christian'
TVR$STORE_ID <- '1h2msv'
TVR$DATA <- file.path(.libPaths()[1], 'tvr', 'tvr.Rda')  # path 2 ur own data
if (!dir.exists(file.path(.libPaths()[1], 'tvr'))) {
  dir.create(file.path(.libPaths()[1], 'tvr'))
}
TVR$ID <- sapply(list(TVR$NAME), function(n) {
  hash <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', TVR$STORE_ID))$hash
  return(hash[hash$content == n, 'id'])  # ur personal ID
})

tvr <- function(data=TVR$DATA) {
  # Loads and renders ur tvr data.
  # @param {character} data Path 2 ur personal tvr data
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(nchar(data) > 0)
  if (file.exists(data)) {
    load(data)
  } else {
    tvr.data <- data.frame(id=NULL, content=NULL, start=NULL, end=NULL, group=NULL)
    save(tvr.data, file=data)
  }
  print.data.frame(tvr.data)
  return(timevis::timevis(tvr.data))
}

tvr_add <- function(content=NULL, start=NULL, end=NULL, data=TVR$DATA, id=TVR$ID) {
  # Adds tasks 2 ur tvr data and renders a new plot.
  # @param {character} content Vector of task descriptions
  # @param {Date/character} start Vector of Sys.Date() values or strings in the format '%Y-%m-%d'
  # @param {Date/character} end Vector of Sys.Date() values or strings in the format '%Y-%m-%d'
  # @param {character} data Path 2 ur personal tvr data
  # @param {integer} id Ur personal identifier
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(!missing(content), !missing(start), !missing(end),
            nchar(data) > 0, is.integer(id))
  if (file.exists(data)) {
    load(data)
  } else {
    tvr.data <- data.frame(id=NULL, content=NULL, start=NULL, end=NULL, group=NULL)
  }
  new <- data.frame(id=integer(length(content)), content=as.character(content),
                    start=format(start, format='%Y-%m-%d'), end=format(end, format='%Y-%m-%d'),
                    group=rep(id, length(content)), stringsAsFactors=F)
  tvr.data <- rbind.data.frame(tvr.data, new, make.row.names=F, stringsAsFactors=F)
  tvr.data$id <- 1:nrow(tvr.data)  # reassigning ids here
  save(tvr.data, file=data)
  print.data.frame(tvr.data)
  return(timevis::timevis(tvr.data))
}

tvr_rm <- function(id=NULL, data=TVR$DATA) {
  # Removes tasks from ur tvr data and renders a new plot.
  # @param {integer} id Vector of task identifier/s
  # @param {character} data Path 2 ur personal tvr data
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(file.exists(data), !missing(id))
  load(data)
  tvr.data <- tvr.data[!tvr.data$id %in% id, ]
  if (nrow(tvr.data) > 0) tvr.data$id <- 1:nrow(tvr.data)
  row.names(tvr.data) <- NULL
  save(tvr.data, file=data)
  print.data.frame(tvr.data)
  return(timevis::timevis(tvr.data))
}