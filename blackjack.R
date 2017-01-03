library(dplyr)
llibrary(ggplot)

decks_nb <- 6

#The dealer hand

dealer <- function() {
  deck <- tibble(card = as.integer(1:10), count = decks_nb*as.integer(c(rep(4, 9), 16))) #The 6 decks
  first <- numeric(0) # The first card drawn by the dealer
  values <- 0 #the value(s) of the dealer's hand
 
  while(all(values < 17)  |  (any(values < 17) & any(values > 21))){    #while all the values are < 17 or if there is one above but it's already bust
    draw <- sample(deck$card, size = 1, prob = deck$count)    #draw a card from the deck
    deck$count[deck$card  == draw] <- deck$count[deck$card  == draw] - 1  #remove it from the deck
   
    first <- ifelse(length(first) == 0, draw, first) # keep the first draw
    values <- c(values + draw, values + if(draw == 1) 11) # Add the draw to the value of the hand, you double the number of values if it's an Ace
     }
 
  result <- values[values <= 21] #Keep only values under 21
  result <- if(length(result) == 0) min(values) else result #Except if there is none
  result <- max(result) #And then keep the highest value
 
  return(c(first,result))
}

x <- 10000
df <- data.frame(matrix(unlist(lapply(1:x, function(x) dealer())), ncol = 2, byrow = T))
proba <- as.data.frame.matrix(table(df$X1, df$X2 > 21))
colnames(proba) <- c('faux', 'vrai')
proba$p <- proba$vrai/(proba$vrai + proba$faux)
ggplot(proba, aes(x= factor(first_card), y = p)) + geom_bar(stat = 'identity')

