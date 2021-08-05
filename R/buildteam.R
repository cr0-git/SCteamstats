#' Create table of player stats
#'
#' @param playerlist A list of valid player names
#' @return Table of player stats
#' @export
#' @examples
#' buildteam(c("Cr0", "Gladiator"))

buildteam <- function(playerlist) {

  players <- length(playerlist)

  name     <- c(1:players)
  battles  <- c(1:players)
  winratio <- c(1:players)
  assists  <- c(1:players)
  deaths   <- c(1:players)
  damage   <- c(1:players)
  healing  <- c(1:players)
  kills    <- c(1:players)
  kdratio  <- c(1:players)

  team_matrix <- data.frame(name, battles, winratio, assists, deaths, damage, healing, kills)

  for (i in 1:players) {

    url_json <- paste0("https://www.badboytool.com/tool/sc/api.php?nickname=", playerlist[i], "&get=absolute&limit=1")

    raw_json <- jsonlite::fromJSON(url_json)

    date <- as.Date(raw_json$data$lastCheck) - 1

    command <- paste0("raw_json$data$history$absolute$`", date)

    team_matrix$name[i] <- raw_json$data$nickName

    team_matrix$battles[i]  <- eval(parse(text = paste0(
      command, "`$pvp$gamePlayed"
    )))

    team_matrix$winratio[i] <- eval(parse(text = paste0(
      "(", command, "`$pvp$gameWin)/((", command, "`$pvp$gamePlayed)-(", command, "`$pvp$gameWin))"
    )))

    team_matrix$assists[i]  <- eval(parse(text = paste0(
      "(", command, "`$pvp$totalAssists)/(", command, "`$pvp$gamePlayed)"
    )))

    team_matrix$deaths[i]   <- eval(parse(text = paste0(
      "(", command, "`$pvp$totalDeath)/(", command, "`$pvp$gamePlayed)"
    )))

    team_matrix$damage[i]   <- eval(parse(text = paste0(
      "(", command, "`$pvp$totalDmgDone)/(", command, "`$pvp$gamePlayed)"
    )))

    team_matrix$healing[i]   <- eval(parse(text = paste0(
      "(", command, "`$pvp$totalHealingDone)/(", command, "`$pvp$gamePlayed)"
    )))

    team_matrix$kills[i]   <- eval(parse(text = paste0(
      "(", command, "`$pvp$totalKill)/(", command, "`$pvp$gamePlayed)"
    )))

    team_matrix$kdratio[i]   <- eval(parse(text = paste0(
      "(", command, "`$pvp$totalKill)/(", command, "`$pvp$totalDeath)"
    )))

  }

  return(team_matrix)

}
