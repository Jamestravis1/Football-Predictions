install.packages("engsoccerdata")
library(dplyr)
library(engsoccerdata)
head(england)
##Select season and tier
england %>% 
  filter(Season == 2015,tier==1) %>% 
  mutate(home = as.character(home),
         visitor = as.character(visitor))-> dat


dat %>% filter(home=='Arsenal')

(all_teams <- sort(unique(dat$home)))

(n_teams <- length(all_teams))

# list of parameters with initial values. sum of attack parameters =n

parameters <- list(attack = rep(0.1, n_teams-1), defense = rep(-0.1, n_teams), home = 0.1,  rho= 0.00)


names(parameters$attack) <- all_teams[-n_teams] ##added in later
names(parameters$defense) <- all_teams

parameters$attack
parameters$defense


##model (4.2) Bivariate Poisson with Dixon Coles adjustement: 
##compromise for lack of independence between Poisson rv's for low scoring games

tau <- Vectorize(function(x, y, lambda, mu, rho){
  if (x == 0 & y == 0){return(1 - (lambda*mu*rho))}
  else if (x == 0 & y == 1){return(1 + (lambda*rho))}
  else if (x == 1 & y == 0){return(1 + (mu*rho))}
  else if (x == 1 & y == 1){return(1 - rho)}
  else {return(1)}
}
)



dc_negloglik <- function(params, goals_home, goals_visitor, team_home, team_visitor, structure){
  
  coef <- relist(params, structure)
  
  # The defense parameter for the last team calculated from the rest
  coef$attack <- c(coef$attack, sum(coef$attack)*-1)
  names(coef$attack)[n_teams] <- names(coef$defense[n_teams])# add name to first element.
  
  # Home team expected goals
  lambda_home <- exp(coef$attack[team_home] + coef$defense[team_visitor] + coef$home)
  
  # Away team expected goals
  lambda_visitor <- exp(coef$attack[team_visitor] + coef$defense[team_home])
  
  # DC adjustment
  dc_adj <- tau(goals_home, goals_visitor, lambda_home, lambda_visitor, rho = coef$rho)
  
  if (any(dc_adj <= 0))
  {return(Inf)}
  
  
  
  log_lik_home <- dpois(goals_home, lambda = lambda_home, log=TRUE)
  log_lik_visitor <- dpois(goals_visitor, lambda = lambda_visitor, log=TRUE)
  log_lik <- sum((log_lik_home + log_lik_visitor + log(dc_adj)))
  
  return(log_lik*-1)
  
}

###


#######

##optimisation function using negative loglik

optim_res <- optim(par = unlist(parameters), fn=dc_negloglik,
                   goals_home = dat$hgoal,
                   goals_visitor = dat$vgoal,
                   team_home = dat$home, team_visitor = dat$visitor,
                   structure=parameters, method = 'BFGS')



# relist, and calculate the remaining parameter. 
estimate <- relist(optim_res$par, parameters)
estimate$attack <- c(estimate$attack, sum(estimate$attack) * -1)
names(estimate$attack)[n_teams] <- names(estimate$defense[n_teams]) 
estimate
sum(estimate$defense)


#
## Expected goals home
(lambda <- exp(estimate$home + estimate$attack['Bolton Wanderers'] + estimate$defense['Bolton Wanderers']))

# Expected goals away
(mu <- exp(estimate$attack['Blackburn Rovers'] + estimate$defense['Bolton Wanderers']))

### considering one team's Home matches
team <- 1
###
rep(0,n_teams) ->  lambda2
rep(0,n_teams) ->  mu2

for (i in 1:n_teams) {
  lambda2[i] <- exp(estimate$home + estimate$attack[team] + estimate$defense[i]) ##goals scored against team i
  mu2[i] <- exp(estimate$attack[i] + estimate$defense[team]) ## goals conceded against team i
}

names(lambda2) <- all_teams
names(mu2) <- all_teams

lambda2[team] <- NA
mu2[team] <- NA   ##no interpretation for a team playing against theirselves
print(team)
print(lambda2)
print(mu2)


#### results: prediction results for each team 

for(team in 1:n_teams){
  
  ###
  rep(0,n_teams) ->  lambda2
  rep(0,n_teams) ->  mu2
  
  for (i in 1:n_teams) {
    lambda2[i] <- exp(estimate$home + estimate$attack[team] + estimate$defense[i]) ##goals scored by 'team' against 'i'
    mu2[i] <- exp(estimate$attack[i] + estimate$defense[team]) ## goals conceded by 'team' against team i'
  }
  
  names(lambda2) <- all_teams
  names(mu2) <- all_teams
  
  lambda2[team] <- NA
  mu2[team] <- NA
  print(paste0("Home team=",all_teams[team]))
  print("Expected Goals for")
  print(lambda2)
  print("Expected Goals against")
  print(mu2)
  
}

##home and away matches for a single team
team <- 3 ##change this param for consider different teams
#####

##initialise empty vectors to be populated by for loops
rep(0,n_teams) ->  lambda_h
rep(0,n_teams) ->  mu_h
rep(0,n_teams) ->  lambda_a
rep(0,n_teams) ->  mu_a


##home match: results for 'team playing at home against every other team in the league
for (i in 1:n_teams) {
  lambda_h[i] <- exp(estimate$home + estimate$attack[team] + estimate$defense[i]) ##goals scored against team i
  mu_h[i] <- exp(estimate$attack[i] + estimate$defense[team]) ## goals conceded against team i
}

##away match
for (i in 1:n_teams) {
  lambda_a[i] <- exp(estimate$home + estimate$attack[i] + estimate$defense[team]) ##goals scored against team i
  mu_a[i] <- exp(estimate$attack[team] + estimate$defense[i]) ## goals conceded against team i
}


names(lambda_h) <- all_teams
names(mu_h) <- all_teams
names(lambda_a) <- all_teams
names(mu_a) <- all_teams

lambda_h[team] <- NA
mu_h[team] <- NA   ##no interpretation for a team playing against theirselves
lambda_a[team] <- NA
mu_a[team] <- NA
print(paste0("Considered Team=",all_teams[team]))
##considered team at Home
print(lambda_h)
print(mu_h)
##considered team away
print(lambda_a)
print(mu_a)

for( j in 1:20){
  cat(all_teams[team],'vs', all_teams[j],round((as.vector(lambda_h))[j],0) ,':',round( (as.vector(mu_h))[j] ,0))
  cat("\n")
}

for( j in 1:20){
  cat(all_teams[j],'vs', all_teams[team],round((as.vector(lambda_a))[j],0) ,':', round((as.vector(mu_a))[j],0) )
  cat("\n")
}
