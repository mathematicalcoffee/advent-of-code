rm(list = ls())
library(stringi)
library(data.table)

HAND_TYPES <- c(
  "five-of-a-kind",
  "four-of-a-kind",
  "full-house",
  "three-of-a-kind",
  "two-pair",
  "one-pair",
  "high card"
)
Q1_CARD_ORDERING <- c(2:9, "T", "J", "Q", "K", "A")
Q2_CARD_ORDERING <- c("J", 2:9, "T", "Q", "K", "A")

.classify_hand <- function(n_unique_cards, freq) {
  # can't tie this properly to the HAND_TYPES factor ("enum") because it's R
  # 1 == highest rank five of a kind
  # 2 == four of a kind
  # 3 == full house
  # 4 == three of a kind
  # 5 == two pair
  # 6 == one pair
  # 7 == high card
  if (n_unique_cards == 1) {
    return(1)
  }
  if (n_unique_cards == 2) {
    if (max(freq) == 4) {
      return(2)
    }
    return(3)
  }
  if (n_unique_cards == 3) {
    if (max(freq) == 3) {
      return(4)
    }
    if (max(freq) == 2) {
      return(5)
    }
  }
  if (n_unique_cards == 4) {
    return(6)
  }
  return(7)
}
classify_hand <- function(cards, jacks_as_wildcards = FALSE) {
  n_jacks <- sum(cards == "J")

  if (jacks_as_wildcards && n_jacks < length(cards)) {
    cards_no_jacks <- cards[cards != "J"]
    n_unique <- uniqueN(cards_no_jacks)
    freq <- sort(table(cards_no_jacks), decreasing = TRUE)
    freq[1] <- freq[1] + n_jacks
  } else {
    n_unique <- uniqueN(cards)
    freq <- table(cards)
  }
  return(
    ordered(
      HAND_TYPES[.classify_hand(n_unique, freq)],
      levels = rev(HAND_TYPES) # ordering goes the other way round, woops
    )
  )
}
cards_to_numeric <- function(cards, card_ordering) {
  # takes string cards and produces numbers such that sorting these will sort
  #  the cards. the actual numbers produced don't matter so long as they
  #  order correctly.
  return(match(cards, card_ordering))
}
get_tiebreak_rank_for_hands <- function(cards) {
  # assumed that the cards are numeric and in order.
  # Produces a single number for the hand such that the number functions as a
  #  tiebreak (compare card1s, then card2s, then ... until tie is broken)
  # could also just map each card to an alphabetic letter and do a string-sort...
  # for this implementation ::
  # e.g. if all the cards were 1-9, we'd have
  #   cards (1,3,3,4) -> 1334;
  #   cards (1,4,3,4) -> 1434;
  # but since some of the numbers are 2 digits long, space everyone out a bit
  return(sum(cards * 10^seq(to = 0, by = -2, length.out = length(cards))))
}

rank_hands <- function(hand_df, card_ordering, jacks_as_wildcards) {
  return(hand_df[,
    {
      cards <- stri_split_boundaries(hand, type = "character")[[1]]
      cards.numeric <- cards_to_numeric(cards, card_ordering)
      .(
        hand.rank = classify_hand(cards, jacks_as_wildcards),
        hand.rank.tiebreaker = get_tiebreak_rank_for_hands(cards.numeric)
      )
    },
    by = .(hand, bet)
  ][order(hand.rank, hand.rank.tiebreaker), rank := seq_len(.N)])
}

df <- data.table(read.delim("input.txt", sep = " ", header = FALSE))
setnames(df, c("hand", "bet"))
rank_hands(df, Q1_CARD_ORDERING, FALSE)[, print(sum(rank * bet))]
rank_hands(df, Q2_CARD_ORDERING, TRUE)[, print(sum(rank * bet))]
