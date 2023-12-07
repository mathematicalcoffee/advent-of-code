rm(list = ls())
library(stringi)
library(data.table)

Q1_CARD_ORDERING <- c(2:9, "T", "J", "Q", "K", "A")
Q2_CARD_ORDERING <- c("J", 2:9, "T", "Q", "K", "A")

df <- data.table(read.delim("input.txt", sep = " ", header = FALSE))
setnames(df, c("hand", "bet"))

# build a dataframe
parse_hand <- function(hand, card_ordering) {
  cards <- stri_split_boundaries(hand, type = "character")[[1]]
  list(
    n.unique = uniqueN(cards),
    max.freq = max(table(cards)),
    card.i = paste0("card", seq_along(cards)),
    card.str = cards,
    card.value = match(cards, card_ordering),
    # for Q2
    n.jacks = sum(cards == "J"),
    max.freq.without.jacks = ifelse(
      all(cards == "J"), as.integer(0), max(table(cards[cards != "J"]))
    )
  )
}

# produce tuple n-unique, max-freq, card1, card2, card3, card4, card5
# order hands weakest to best, tiebreaking on numeric card value.
# best hand has least n-unique and most max-freq
#      5-of-a-kind has 1 unique, max-freq of 5
#      4-of-a-kind has 2 unique, max-freq of 4
#      full-house  has 2 unique, max-freq of 3
# e.g. 3-of-a-kind has 3 unique, max-freq of 3
#      2-pair      has 3 unique, max-freq of 2
#      1-pair      has 4 unique, max-freq of 2
#      high-card   has 5 unique, max-freq of 1

# ***Q1***
# 1. make df with 1 row per card:: (hand, card.i, card {, other metadata})
hands <- df[,
  c(parse_hand(hand, Q1_CARD_ORDERING), bet = bet),
  by = .(hand)
]
# 2. cast dataframe from long to wide format
#    (cards in the columns, one row per hand)
hands <- dcast(
  hands,
  hand + n.unique + max.freq + bet ~ card.i, value.var = "card.value"
)
# 3. rank and get answer
hands[
  rev(order(n.unique, -max.freq, -card1, -card2, -card3, -card4, -card5)),
  print(sum(seq_len(.N) * bet))
]

# ***Q2***
hands <- df[,
  c(parse_hand(hand, Q2_CARD_ORDERING), bet = bet),
  by = .(hand)
]
# the jacks can increase the max-freq and reduce the n.unique
# but leave JJJJJ alone
hands[n.jacks %between% c(1, 4), n.unique := n.unique - 1]
hands[, max.freq := max.freq.without.jacks + n.jacks]

# cast dataframe from long to wide format (cards in the columns)
hands <- dcast(
  hands,
  hand + n.unique + max.freq + bet ~ card.i, value.var = "card.value"
)
hands <- hands[
  rev(order(n.unique, -max.freq, -card1, -card2, -card3, -card4, -card5)),
  print(sum(seq_len(.N) * bet))
]
