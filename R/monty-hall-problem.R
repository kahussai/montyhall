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
#'	Contestant makes their first pick!
#' @description
#'	`select_door()` generates at random an integer between 1 and 3
#'	inclusive, representing a door number in the game.
#' @details
#'	This follows the game setup again. The chosen door may or may not
#'	have a car behind it. But the contestant has to make their initial
#'	pick as per design. 
#' @param ... no arguments are used by the function
#' @return The function returns a single integer value which is
#'	stored in `a.pick`.
#' @examples
#'	select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'	Host opens the door with a goat behind it.
#' @description
#'	`open_goat_door()` will return the integer (1 or 2 or 3)
#'	representing the door number behind which there
#'	is a goat and is not the initial pick.
#' @details
#'	The host, as per the original show, picks a door 
#'	where he knows there is a goat behind and was not
#'	the initial pick of the contestant. This will setup 
#'	our Monty Hall problem as it will influence the decision 
#'	to "stay" or "switch" for the contestant.
#' @param ... This function takes two arguments, `game` which is 
#'	a character vector of length 3 has two "goat" strings and 1
#'	"car". The other argument is `a.pick`, a whole number, which 
#'	represents the initial door picked by the contestant.
#' @return The function returns an integer between 1 to 3, associated
#'	with the position of the "goat" in the vector `game` sans the
#'	the position already taken by `a.pick`. This is stored in
#'	`opened.door`.
#' @examples
#'	open_goat_door(game=a.game,a.pick=1)
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
#'	Contestant chooses their strategy.
#' @description
#'	`change_door()` function will return the final door number picked
#'	based on whether a "stay" or "switch" strategy was opted.
#' @details
#'	When the door with the goat behind it is opened, the contestant
#'	has to now decide whether to stay with their initial pick or to
#'	switch to the other door before the outcome can be revealed.
#' @param ... This function takes three arguments. The first is a boolean (`stay`), 
#'	which determines whether to stick with the initial door picked by contestant
#'	(`a.pick`) or to pick the remaining door, which is represented by the integer 
#'	not occupied by `opened.door` or `a.pick`. For the former, `stay` should be T,
#'	for True and for the latter, `stay` should be F, or False, i.e. the strategy
#'	adopted by the contestant is "switch". `opened.door` is the door with a goat
#'	behind it. `a.pick` is the initial door picked by contestant. Both are integers 
#'	between 1 and 3 inclusive.
#' @return This function returns an integer value between 1 and 3 inclusive. `a.pick`
#'	if `stay`=T, and another value which is not `opened.door` or `a.pick` if `stay`=F.
#'	This value is stored in `final.pick`.
#' @examples
#'	change_door(stay=F,opened.door=2,a.pick=1)
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
#'	Did you win?
#' @description
#'	`determine_winner()` will return the outcome string corresponding to whether
#'	the final door picked by the contestant did or did not have a car behind it.
#' @details
#'	After the contestant chooses to stay with their initial pick or switch to the
#'	remaining door, the outcome is declared. The declaration is through a character
#'	string, either "WIN" if the car was indeed behind the final door chosen by the contestant	
#'	and "LOSE" otherwise.
#' @param ... This function takes two arguments. `final.pick` which is an integer and the door
#'	number behind which the contestant hopes the car to be. `game` is the length 3 character
#'	vector of two "goat" strings and one "car".
#' @return 
#'	The function returns a character string, either "WIN" or "LOSE" based on which character is
#'	returned for the position `final.pick` in the `game` character vector.
#' @examples
#'	determine_winner(final.pick=3,game=a.game)
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
#'	Let us play the entire Monty Hall Game for you!
#' @description
#'	`play_game()` will run the entire one-game simulation of 
#'	the Monty Hall Problem for you.
#' @details
#'	This function goes through all the steps in the Monty Hall
#'	problem, randomly creating the game with two doors hiding 
#'	a goat and one a car. It will then select a door at random, 
#'	open a door hiding a goat that is different from the initial pick
#'	and declare a winner before presenting the final result for both
#'	"stay" and "switch" strategies and their respective outcomes.
#' @param ... no arguments are used in this function
#' @return This function returns a 2x2 dataframe, with columns
#'	strategy and outcome based on the random Monty Hall game played. 
#' @examples
#'	play_game()
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
#'	Let us simulate the Monty Hall problem n times!
#' @description
#'	`play_n_games()` will simulate the Monty Hall problem
#'	a specified n number of times. Returning the aggregated results 
#'	of the simulations in a combined table.
#' @details
#'	This function uses iterative looping to run the `play_game()` function
#'	a specified n times. It will store the results of each simulation in the same
#'	list, which is then binded together to form a dataframe. This attempts to find
#'	an answer to the Monty Hall problem through iterative runs of the game and 
#'	presenting the compiled results.
#' @param ... This function takes only one argument, a whole number n, representing
#'	the number of times the game should be played.
#' @return This function returns a dataframe with labels "stay" and "switch" in column strategy,
#'	and win and lose as the other columns. In these columns, the proportion of outcomes corresponding
#'	to the relevant strategy and outcome are populated.
#' @examples
#'	play_n_games(n=200)
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
