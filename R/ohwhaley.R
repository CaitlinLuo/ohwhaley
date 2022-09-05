#' Summon a whale for a customisable pick-me-up
#'
#' @description Summon a whale for a customisable pick-me-up
#' @usage say(what)
#' @param what text string; This is the phrase you want the whale to echo. If not supplied a random phrase is chosen.
#'
#' @return text string; Whale shaped message
#' @export
#'
#' @examples
#' say("You're whale-come")
say <- function(what){
  #Defining the whale ASCII
  whale = "\n            ------ \n           %s \n            ------ \n               \\\   \n                \\\  \n                 \\\
     .-'
'--./ /     _.---.
'-,  (__..-`       \\
   \\          .     |
    `,.__.   ,__.--/
     '._/_.'___.-`
"
  #Finding the position of where to print message
  what_pos_start <-
    regexpr('%s', whale)[1] - 1

  what_pos_end <- what_pos_start + 3

  #If what isn't supplied...
  if(missing(what)){
    what <- phrases %>% sample(size = 1) #See here's the pipe!
  }

  #Combining positions, message and whale together
  out <- paste0(substr(whale, 1, what_pos_start), #Top of speech bubble
                what, #User specified message
                substr(whale, what_pos_end, nchar(whale))) #Bottom of speech bubble + whale

  #Return whale message
  message(out)
}


#' Whale pun phrases used in say()
#' @keywords internal
phrases <-  c(
  "You're whale-come!",
  "How are you? I'm whaley good!",
  "I'm having a whale of a time!",
  "Whale, whale, whale...look who's here!",
  "Gee...this is over-whalming!",
  "Everything whale be alright!",
  "Get whale soon!",
  "Whale done! This looks great!",
  "What do you call a pod of noisy whales?        An ORCASTRA!"
)

what <- c("This is a character string")
say(what)
expect_type(what, "character")
expect_gt(length(what), 0)
expect_length(say(), 0) #Returns something with a length of 0
expect_null(say()) #Returns null
expect_invisible(say()) #Returns invisibly
expect_message(say()) #Returns a message
