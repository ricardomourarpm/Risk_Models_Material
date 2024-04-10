accidents = c(0,1,2,3,4)

people = c(81714, 11306, 1618, 250, 40)

cens_accident = 5

cens_people = 7

minus_log_likel = function(lambda){
  -sum(people*dpois(accidents,lambda,log = TRUE))-cens_people*ppois(cens_accident, lambda, log.p = TRUE, lower.tail = FALSE)
}

nlm(minus_log_likel,1)



minus_log_likel = function(qu){
  -sum(people*dbinom(accidents,8, qu,log = TRUE))-cens_people*pbinom(cens_accident, 8, qu, log.p = TRUE, lower.tail = FALSE)
}

nlm(minus_log_likel,0.8)

