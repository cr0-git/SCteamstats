#' Create table of player stats but its easier to enter names
#'
#' @param ... A list of valid player names
#' @return Table of player stats
#' @export
#' @examples
#' quickteam(Cr0, Gladiator)

quickteam <- function(...) {

  playerlist <- as.list(as.character((match.call()))[-1])

  players <- length(playerlist)

  name     <- c(1:players)
  battles  <- c(1:players)
  winratio <- c(1:players)
  assists  <- c(1:players)
  deaths   <- c(1:players)
  dps      <- c(1:players)
  hps      <- c(1:players)
  kills    <- c(1:players)
  kdratio  <- c(1:players)

  team_matrix <- data.frame(name, battles, winratio, assists, deaths, dps, hps, kills)

  for (i in 1:players) {

    url_json <- paste0("https://gmt.star-conflict.com/pubapi/v1/userinfo.php?nickname=", playerlist[i])

    raw_json <- jsonlite::fromJSON(url_json)

    if (raw_json$result == "ok") {

      team_matrix$name[i]     <- raw_json$data$nickName

      team_matrix$battles[i]  <- raw_json$data$pvp$gamePlayed

      team_matrix$winratio[i] <- (raw_json$data$pvp$gameWin) / (raw_json$data$pvp$gamePlayed - raw_json$data$pvp$gameWin)

      team_matrix$assists[i]  <- (raw_json$data$pvp$totalAssists) / (raw_json$data$pvp$gamePlayed)

      team_matrix$deaths[i]   <- (raw_json$data$pvp$totalDeath) / (raw_json$data$pvp$gamePlayed)

      team_matrix$dps[i]      <- (raw_json$data$pvp$totalDmgDone) * 1000 / (raw_json$data$pvp$totalBattleTime)

      team_matrix$hps[i]      <- (raw_json$data$pvp$totalHealingDone) * 1000 / (raw_json$data$pvp$totalBattleTime)

      team_matrix$kills[i]    <- (raw_json$data$pvp$totalKill) / (raw_json$data$pvp$gamePlayed)

      team_matrix$kdratio[i]  <- (raw_json$data$pvp$totalKill) / (raw_json$data$pvp$totalDeath)

    } else {

      team_matrix$name[i]     <- "N/A"

    }

  }

  team_matrix <- team_matrix[!grepl("N/A", team_matrix$name),]

  rownames(team_matrix) <- NULL

  return(team_matrix)

}
