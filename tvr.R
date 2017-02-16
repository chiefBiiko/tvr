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
#   @column {double} id Task identifier
#   @column {character} content Task description
#   @column {Date} start Task start date
#   @column {Date} end Task end date
#   @column {double} group Ur personal identifier [TVR$ID]
#   tvr.data is saved as ur personal task store @ TVR$DATA
#
# Usage
#   tvr()  # view ur tasks
#   # add tasks
#   tvr_add(c('new task', 'foo task'), rep(Sys.Date(), 2), rep(Sys.Date() + 1, 2))
#   tvr_rm(1:2)  # remove tasks

TVR <- list()
TVR$NAME <- 'Biiko'  # 'Balou', 'Christian'
TVR$STORE_ID <- '1h2msv'
TVR$ID <- sapply(list(TVR$NAME), function(n) {  # ur personal ID
  hash <- jsonlite::fromJSON(paste0('https://api.myjson.com/bins/', TVR$STORE_ID))$hash
  return(hash[hash$content == TVR$NAME, 'id'])
}) 

if (!dir.exists(file.path(.libPaths()[1], 'tvr'))) dir.create(file.path(.libPaths()[1], 'tvr'))
TVR$DATA <- file.path(.libPaths()[1], 'tvr', 'tvr.Rda')  # ur own data

tvr <- function() {
  # Loads ur tvr data and returns an interactive timeplot of it.
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(isTRUE(require('timevis')))
  if (file.exists(TVR$DATA)) {
    load(TVR$DATA)
  } else {
    tvr.data <- data.frame(id=NULL, content=NULL, start=NULL, end=NULL, group=NULL)
    save(tvr.data, file=TVR$DATA)
  }
  return(timevis::timevis(tvr.data))
}

tvr_add <- function(content=NULL, start=NULL, end=NULL) {
  # Adds tasks 2 ur tvr data and renders a new plot.
  # @param {character} content Vector of task descriptions
  # @param {Date} start Vector of Sys.Date() values
  # @param {Date} end Vector of Sys.Date() values
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(isTRUE(require('timevis')), !missing(content), !missing(start), !missing(end))
  if (file.exists(TVR$DATA)) {
    load(TVR$DATA)
  } else {
    tvr.data <- data.frame(id=NULL, content=NULL, start=NULL, end=NULL, group=NULL)
  }
  new <- data.frame(id=double(length(content)), content=content, start=start, end=end,
                    group=rep(TVR$ID, length(content)))
  tvr.data <- rbind.data.frame(tvr.data, new)
  tvr.data$id <- 1:nrow(tvr.data)  # reassigning ids here
  rownames(tvr.data) <- NULL
  save(tvr.data, file=TVR$DATA)
  View(tvr.data)
  return(timevis::timevis(tvr.data))
}

tvr_rm <- function(id=NULL) {
  # Removes tasks from ur tvr data and renders a new plot.
  # @param {double} id Vector of task identifier/s
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(isTRUE(require('timevis')), file.exists(TVR$DATA), !missing(id))
  load(TVR$DATA)
  tvr.data <- tvr.data[!tvr.data$id %in% id, ]
  if (nrow(tvr.data) > 0) tvr.data$id <- 1:nrow(tvr.data)
  rownames(tvr.data) <- NULL
  save(tvr.data, file=TVR$DATA)
  View(tvr.data)
  return(timevis::timevis(tvr.data))
}