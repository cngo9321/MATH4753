#' Title Ntickets
#'
#' @param N Number of flight seats
#' @param gamma Probability of overbooking
#' @param p Probability of showing
#'
#' @return A discrete and continuous curve representing the optimal number of tickets to sell given probability of show & overbooking
#' @export
#'
#' @examples
ntickets = function(N, gamma, p) {

  # Discrete
  n1 <- seq(N, floor(N + N/10), by = 1)
  obj1 <- 1 - gamma - pbinom(q = N, size = n1, prob = p)
  ind <- which.min(abs(obj1))
  nd <- n1[ind]

  # Continous
  n2 <- seq(N, floor(N + N/10), by = 0.0001)
  obj2 <- 1 - gamma - pnorm(q = N, mean = n2*p, sd = sqrt(n2*p*(1-p)))
  ind1 <- which.min(abs(obj2))
  nc <- n2[ind1]

  # Discrete plot
  plot(n1, obj1, type = "n", ylab = "Objective", pch = 16, col = ifelse(n1 == nd, "pink", "black"), main = paste("Objective vs n to Find Optimal Tickets Sold (", nd, ")\n", "gamma =", gamma, "N =", N, " Discrete"))

  points(n1, obj1, col = ifelse(n1 == nd, "pink", "black"))

  lines(n1, obj1, col = ifelse(n1 == nd, "pink", "black"))

  abline(h = obj1[ind], v = nd, col = "pink")

  # Continuous plot
  plot(n2, obj2, type = "l", xlab = "n", ylab = "Objective", main = paste("Objective vs n to Find Optimal Tickets Sold (",round(nc, 4),")\n", "gamma =", gamma, " N =", N, "Continuous"))

  abline(h = obj2[ind1], v = nc, col = "hotpink")


  listof6 = list(ndiscrete = nd, ncontinuous = nc, N = N, gamma = gamma, prob = p)
  return(listof6)

}
