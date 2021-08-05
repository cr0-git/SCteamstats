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

    team_matrix$name[i] <- raw_json$data$nickName
    team_matrix$battles[i] <- raw_json$data$history$absolute$`2021-08-04`$pvp$gamePlayed

    team_matrix$winratio[i] <- (raw_json$data$history$absolute$`2021-08-04`$pvp$gameWin) / ((raw_json$data$history$absolute$`2021-08-04`$pvp$gamePlayed) - (raw_json$data$history$absolute$`2021-08-04`$pvp$gameWin))
    team_matrix$assists[i]  <- (raw_json$data$history$absolute$`2021-08-04`$pvp$totalAssists) / (raw_json$data$history$absolute$`2021-08-04`$pvp$gamePlayed)
    team_matrix$deaths[i]   <- (raw_json$data$history$absolute$`2021-08-04`$pvp$totalDeath) / (raw_json$data$history$absolute$`2021-08-04`$pvp$gamePlayed)
    team_matrix$damage[i]   <- (raw_json$data$history$absolute$`2021-08-04`$pvp$totalDmgDone) / (raw_json$data$history$absolute$`2021-08-04`$pvp$gamePlayed)
    team_matrix$healing[i]  <- (raw_json$data$history$absolute$`2021-08-04`$pvp$totalHealingDone) / (raw_json$data$history$absolute$`2021-08-04`$pvp$gamePlayed)
    team_matrix$kills[i]    <- (raw_json$data$history$absolute$`2021-08-04`$pvp$totalKill) / (raw_json$data$history$absolute$`2021-08-04`$pvp$gamePlayed)
    team_matrix$kdratio[i]  <- (raw_json$data$history$absolute$`2021-08-04`$pvp$totalKill) / (raw_json$data$history$absolute$`2021-08-04`$pvp$totalDeath)

  }

  return(team_matrix)

}
