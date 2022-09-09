#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Contestant Selects a Door.
#'
#' @description
#'   `select_door()` generates an initial selection of a door by 
#'   the contestant through a numeric vector of 1, 2, and 3.
#'
#' @details
#'   The contestant will make an initial selection of one of the 
#'   three doors of the game, not knowing if there is a goat or
#'   car behind the door they select.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length of one numeric vector, either
#'   1, 2, or 3 and is saved as 'a.pick'.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Host Opens Goat Door
#'
#' @description
#'   `open_goat_door()` generates a selection of a door by the
#'   host through a numeric vector of 1, 2, or 3, but will not 
#'   be `a.pick` nor a numeric vector value that corresponds to 
#'   the character vector value of "car" by `create_game()`.
#'
#' @details
#'   The host will make a selection of one of the two remaining 
#'   doors after the contestant has made their initial selection. 
#'   If the contestant initially selected a door with a goat 
#'   behind it, the host will choose the remaining door with a 
#'   goat. If the contestant initially selected a door with the 
#'   car, the host will select either one of the two remaining 
#'   doors with a goat behind it.
#'
#' @param ...
#'  This function has two arguments: game and a.pick:
#'  game is inputted by the create_game function 
#'  and a.pick is inputted by the select_door function.
#'
#' @return The function returns a length of one numeric vector 
#'   that is not `a.pick`, nor a numeric value that corresponds 
#'   to "car" by the `create_game()` function. This will be 
#'   saved as 'opened.door'.
#'
#' @examples
#'   open_goat_door( game, a.pick )
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   Contestant Switches or Stays with Door
#'
#' @description
#'   `change_door()` generates a selection of a door by 
#'   the contestant through a numeric vector of 1, 2, or 3, 
#'   depending on if the strategy "stay" is true (stay = T) 
#'   or false (stay = F).
#'
#' @details
#'   The contestant will be given an opportunity to stay 
#'   with the same door initially selected or to switch to 
#'   the other unopened door. If the contestant chooses to 
#'   stay with the same door that was initially selected 
#'   ( stay = T ), this will generate `a.pick`, the initial
#'   door selection. If the contestant decides to switch doors, 
#'   where ( stay = F ), the door that remains closed and 
#'   is not `a.pick` will be selected.
#'
#' @param ...
#'   This function has three arguments: stay = T or stay = F, 
#'   opened.door, and a.pick: stay is the argument that will 
#'   reflect if the door selection is switched ( stay = F ) or 
#'   stays as the initial selection ( stay = T ). The argument 
#'   opened.door is the argument that indicates the door that 
#'   has been opened by the host. The argument a.pick is the 
#'   argument that indicates the door that was initially selected 
#'   by the contestant
#'
#' @return 
#'   The function returns a length of one numeric vector 
#'   that is either `a.pick`, or of the remaining unopened 
#'   door that is not `opened_door`. This will be saved 
#'   as `final.pick`.
#'
#' @examples
#'   change_door ( stay=T, opened.door, a.pick )
#'   change_door ( stay=F, opened.door, a.pick )
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine If Contestant Won the Game
#'
#' @description
#'   `determine_winner()` generates the results of whether
#'   the contestant has won or lost the game based on the 
#'   'final.pick'
#'
#' @details
#'   The contestant will be determined a winner of the game if
#'   the final door selected by the contestant shows to have 
#'   the car behind it. If the contestant makes a final selection 
#'   of a door that has a goat behind it, the contestant 
#'   will have lost the game.
#'
#' @param ...
#'   This function has two arguments: final.pick and game
#'   The argument final.pick is the argument that indicates the 
#'   ultimate selection of door by the contestant. The argument game 
#'   is the argument that indicates the current game that is being 
#'   played
#'
#' @return 
#'   The function returns a character vector of "lost" if the
#'   `final.pick` reflects a door with a "goat", or a character 
#'   vector of "won" if the `final.pick` reflects a door with
#'   the "car".
#'
#' @examples
#'   determine_winner( final.pick, game )
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Play a Full Game of Monty Hall
#'
#' @description
#'   `play_game()` generates all the steps of one full game
#'   including `create_game()`, `select_door(), `open_goat_door()`,
#'   `change_door()`, and `determine_winner()`. This function
#'   shows the results of the game in a table.
#'
#' @details
#'   Playing a full game of Monty Hall will include having
#'   the contestant making an initial selection of one of
#'   three doors. Two of the doors have a goat behind it, and
#'   one door has the car behind it, which is the prize.
#'   The host will then open a door with a goat behind it.
#'   The contestant will be given the choice to either stay
#'   with their door selection or switch it to the other unopened
#'   door. Once the contestant's final selection is made, the 
#'   game will then determine the winner of the game. If the 
#'   contestant ultimately chose the door with the car behind 
#'   it, the contestant will win the game. If the contestant 
#'   ultimately chose the door with a goat behind it, the 
#'   contestant will have lost the game.
#'
#' @param ... no arguments are used by the function.
#'
#' @return 
#'   The function returns a character vector table of two 
#'   columns, "strategy" and "outcome", and two rows under 
#'   the strategy column, "stay" or "switch". The outcome
#'   column will show the character vector results of the 
#'   game, "win" or "lose" based on the strategy used. 
#'
#' @examples
#'   play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Loop the Game
#'
#' @description
#'   `play_n_games()` generates a new game n number of times
#'   and displays the cumulative results in a table by the 
#'   proportion of games lost or won.
#'
#' @details
#'   The Monty Hall game is played repeatedly for the number
#'   of times indicated by n. The results of each game are
#'   collected and listed prior to generating the next game.
#'   The cumulative results are then counted based on the strategy
#'   and outcome of the strategy used. The table results will show
#'   the cumulative proportions of the games lost or won by 
#'   the contestant based on the strategy.
#'
#' @param ...
#'   There is one argument to this function: n 
#'   n is the argument that will determine the number of
#'   times the Monty Hall game is played
#'
#' @return The function returns a table of cumulative outcomes with 
#'   four numeric decimal values indicating the proportionate 
#'   results of games that were won or lost based on the strategies 
#'   of "switch" or "stay"
#'
#' @examples
#'   play_n_games (n = 100)
#'
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
