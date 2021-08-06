#' Find best team
#'
#' @param ... A number of team matrices created by buildteam()
#' @return Average stats of the best team
#' @export
#' @examples
#' team1 <- buildteam(c("Cr0", "Gladiator"))
#' team2 <- buildteam(c("Cr0", "JellyG"))
#' bestplayer(team1, team2)

bestteam <- function(...) {

  team_list  <- list(...)
  team_names <- as.list(as.character((match.call()))[-1])

  averages <- lapply(team_list[], avgteam)

  comparison <- data.frame(matrix(NA, nrow = length(team_list), ncol = 9))

  colnames(comparison) <- colnames(team_list[[1]])

  for (i in 1:length(team_list)) {

    comparison[i, 2:9] <- unlist(averages[i])
    comparison[i, 1]   <- team_names[i]

  }

  comparison$deaths <- -comparison$deaths

  comparison_ranks <- apply(-comparison[-1], 2, rank)

  comparison$rank <- apply(comparison_ranks, 1, sum)

  return(comparison[which.min(comparison$rank),1:9])

}
