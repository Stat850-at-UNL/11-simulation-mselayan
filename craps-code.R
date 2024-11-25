# This is a copy of the code in craps-code.qmd for use in simulation.qmd

library(dplyr)

roll_dice <- function() {
  # Input: None
  # Output: An integer from 2 to 12
  # Description: Simulates rolling two dice and returns their sum

  d1 <- sample(1:6, 1)
  d2 <- sample(1:6, 1)
  return(d1 + d2)
}

eval_first_roll <- function(roll) {
  # Input: Integer roll (sum of two dice)
  # Output: A character ('w', 'l', or 'c')
  # Description: Evaluates the first roll for win, loss, or continue

  if (roll %in% c(7, 11)) {
    return("w") # Win
  } else if (roll %in% c(2, 3, 12)) {
    return("l") # Loss
  } else {
    return("c") # Continue
  }
}

eval_point_roll <- function(point, roll) {
  # Input: Integer point (point to match), Integer roll (sum of two dice)
  # Output: A character ('w', 'l', or 'c')
  # Description: Evaluates a roll during the Point Phase for win, loss, or continue

  if (roll == point) {
    return("w") # Win
  } else if (roll == 7) {
    return("l") # Loss
  } else {
    return("c") # Continue
  }
}

simulate_craps_game <- function() {
  # Input: None
  # Output: A data frame with columns id, roll, and outcome
  # Description: Simulates a single game of craps

  data <- data.frame(id = integer(), roll = integer(), outcome = character(), stringsAsFactors = FALSE)
  id <- 1

  # First roll
  roll <- roll_dice()
  outcome <- eval_first_roll(roll)
  data <- rbind(data, data.frame(id = id, roll = roll, outcome = outcome))

  # If Point Phase, continue rolling
  if (outcome == "c") {
    point <- roll
    while (TRUE) {
      id <- id + 1
      roll <- roll_dice()
      outcome <- eval_point_roll(point, roll)
      data <- rbind(data, data.frame(id = id, roll = roll, outcome = outcome))
      if (outcome != "c") {
        break
      }
    }
  }

  return(data)
}

summarize_craps_game <- function(game_data) {
  # Input: A data frame with columns id, roll, and outcome
  # Output: A single-row data frame with summary statistics for the game
  # Description: Summarizes a single game of craps

  n_rolls <- nrow(game_data)
  outcome <- game_data$outcome[n_rolls]
  point <- ifelse(any(game_data$outcome == "c"), game_data$roll[game_data$outcome == "c"][1], NA)
  first_roll <- game_data$roll[1] # Record the first rolled number

  return(data.frame(n_rolls = n_rolls, first_roll = first_roll, point = point, outcome = outcome, stringsAsFactors = FALSE))
}

run_craps_simulation <- function(N) {
  # Input: An integer N (number of games to simulate)
  # Output: A data frame summarizing all games
  # Description: Simulates N games of craps and summarizes results

  game_summaries <- data.frame(n_rolls = integer(), point = integer(), outcome = character(), stringsAsFactors = FALSE)

  for (i in 1:N) {
    game_data <- simulate_craps_game()
    game_summary <- summarize_craps_game(game_data)
    game_summaries <- rbind(game_summaries, game_summary)
  }

  return(game_summaries)
}

