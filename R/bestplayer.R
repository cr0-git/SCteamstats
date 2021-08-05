#' Find best player
#'
#' @param teammatrix A matrix of player stats created by buildteam()
#' @return Stats of the best player
#' @export
#' @examples
#' team1 <- buildteam(c("Cr0", "Gladiator"))
#' bestplayer(team1)

bestplayer <- function(team_matrix) {

  team_matrix$deaths <- -team_matrix$deaths

  team_ranks <- apply(-team_matrix[-1], 2, rank)

  team_matrix$rank <- apply(team_ranks, 1, sum)

  return(team_matrix[which.min(team_matrix$rank),1:9])

}
