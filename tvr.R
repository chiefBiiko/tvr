# tvr (task viewer)
# 
# Note
#   designed 2 be stoopid simple - 3 functions only: view, add, rm
#   4 ease of use avoid exact times, instead just pass values of Sys.Date(),
#   Sys.Date() + 4, etc. 2 @params start and end of tvr_add(), allow every
#   task 2 take at least 1 day
# 
# Data format (just 4 u 2 know)
#   tvr.data <- data.frame(id=NULL, content=NULL, start=NULL, end=NULL)
#   @column {double} id Task identifier
#   @column {character} content Task description
#   @column {Date} start Task start date
#   @column {Date} end Task end date
#
# Examples
#   ...

tvr <- function() {
  # Loads ur tvr data and returns an interactive timeplot of it.
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(isTRUE(require('timevis')))
  if (file.exists('tvr.Rda')) {  # make absolute
    load('tvr.Rda')  # make absolute
  } else {
    tvr.data <- data.frame(id=NULL, content=NULL, start=NULL, end=NULL)
    save(tvr.data, file='tvr.Rda')  # make absolute
  }
  rownames(tvr.data) <- NULL
  View(tvr.data)
  return(timevis::timevis(tvr.data))
}

tvr_add <- function(content=NULL, start=NULL, end=NULL) {
  # Adds a task 2 ur tvr data and renders a new plot.
  # @param {character} content Short description of task
  # @param {Date} start Plain Sys.Date() object
  # @param {Date} end Plain Sys.Date() object
  # @param ... Further arguments 2 timevis::timevis()
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(isTRUE(require('timevis')),
            !missing(content), !missing(start), !missing(end))
  if (file.exists('tvr.Rda')) {  # make absolute
    load('tvr.Rda')  # make absolute
  } else {
    tvr.data <- data.frame(id=NULL, content=NULL, start=NULL, end=NULL)
  }
  new <- data.frame(id=nrow(tvr.data) + 1, content=content, start=start, end=end)
  tvr.data <- rbind(tvr.data, new)
  rownames(tvr.data) <- NULL
  save(tvr.data, file='tvr.Rda')  # make absolute
  View(tvr.data)
  return(timevis::timevis(tvr.data))
}

tvr_rm <- function(id=NULL) {
  # Removes tasks from ur tvr data and renders a new plot.
  # @param {double} id Vector of task identifier/s
  # @return {Object.<htmlwidgets>} Interactive timevis plot
  stopifnot(isTRUE(require('timevis')), file.exists('tvr.Rda'), !missing(id))  # make absolute
  load('tvr.Rda')  # make absolute
  tvr.data <- tvr.data[!tvr.data$id %in% id, ]
  rownames(tvr.data) <- NULL
  save(tvr.data, file='tvr.Rda')  # make absolute
  View(tvr.data)
  return(timevis::timevis(tvr.data))
}