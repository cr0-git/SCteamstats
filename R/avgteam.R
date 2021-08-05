#' Find averages of stats
#'
#' @param teammatrix A matrix of player stats created by buildteam()
#' @param w TRUE/FALSE for weighting stats by games played
#' @return Table of average player stats
#' @export
#' @examples
#' team1 <- buildteam(c("Cr0", "Gladiator"))
#' avgteam(team1)

avgteam <- function(teammatrix, w = FALSE) {

  if (isTRUE(w)) {

    battles  <- mean(teammatrix$battles)
    winratio <- sum(teammatrix$winratio * teammatrix$battles) / sum(teammatrix$battles)
    assists  <- sum(teammatrix$assists  * teammatrix$battles) / sum(teammatrix$battles)
    deaths   <- sum(teammatrix$deaths   * teammatrix$battles) / sum(teammatrix$battles)
    dps      <- sum(teammatrix$dps      * teammatrix$battles) / sum(teammatrix$battles)
    hps      <- sum(teammatrix$hps      * teammatrix$battles) / sum(teammatrix$battles)
    kills    <- sum(teammatrix$kills    * teammatrix$battles) / sum(teammatrix$battles)
    kdratio  <- sum(teammatrix$kdratio  * teammatrix$battles) / sum(teammatrix$battles)

  } else {

    battles  <- mean(teammatrix$battles)
    winratio <- mean(teammatrix$winratio)
    assists  <- mean(teammatrix$assists)
    deaths   <- mean(teammatrix$deaths)
    dps     <- mean(teammatrix$dps)
    hps      <- mean(teammatrix$hps)
    kills    <- mean(teammatrix$kills)
    kdratio  <- mean(teammatrix$kdratio)

  }

  avg_matrix <- data.frame(battles, winratio, assists, deaths, dps, hps, kills, kdratio)

  return(avg_matrix)

}
