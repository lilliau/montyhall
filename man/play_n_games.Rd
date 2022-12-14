% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{play_n_games}
\alias{play_n_games}
\title{Loop the Game}
\usage{
play_n_games(n = 100)
}
\arguments{
\item{...}{There is one argument to this function: n
n is the argument that will determine the number of
times the Monty Hall game is played}
}
\value{
The function returns a table of cumulative outcomes with
four numeric decimal values indicating the proportionate
results of games that were won or lost based on the strategies
of "switch" or "stay"
}
\description{
\code{play_n_games()} generates a new game n number of times
and displays the cumulative results in a table by the
proportion of games lost or won.
}
\details{
The Monty Hall game is played repeatedly for the number
of times indicated by n. The results of each game are
collected and listed prior to generating the next game.
The cumulative results are then counted based on the strategy
and outcome of the strategy used. The table results will show
the cumulative proportions of the games lost or won by
the contestant based on the strategy.
}
\examples{
  play_n_games (n = 100)

}
