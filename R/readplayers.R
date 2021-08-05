#' Turn text file into list
#'
#' @param path Path to text file
#' @return List of player names
#' @export
#' @examples
#' team1 <- readplayers("./team.txt")

readplayers <- function(path) {

  playerlist <- read.table(path)
  playerlist <- as.list(levels(playerlist$V1))

  return(playerlist)

}
