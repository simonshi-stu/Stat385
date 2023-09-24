kelly_bet <- function(bankroll, p, wager, payout){
  return((p - (1-p)/(payout / wager)) * bankroll)
}
temp1 = kelly_bet(bankroll = 1000, p = 0.6, wager = 1, payout = 1)
print(temp1)
temp2 = kelly_bet(bankroll = 500, p = 0.9, wager = 60, payout = 10)
print(temp2)
