
library(stringr)
library(dplyr)
library(MASS)

####################
### Fonction BMS ###
####################

set.BMS_levels <- function(ell.max, ell.min, Psi, df2){
  tempo1 <- df2[which(df2$contract.number == 1),]
  tempo1$BMS.level <- 100
  tempo2 <- df2[which(df2$contract.number == 2),]
  tempo2$BMS.level <- pmin(ell.max, pmax(ell.min, 100 - tempo2$lagk1 + Psi*tempo2$lagn1))
  tempo3 <- df2[which(df2$contract.number == 3),]
  tempo3$BMS.level <- pmin(ell.max, pmax(ell.min, 100 - tempo3$lagk2 + Psi*tempo3$lagn2))
  tempo3$BMS.level <- pmin(ell.max, pmax(ell.min, tempo3$BMS.level - tempo3$lagk1 + Psi*tempo3$lagn1))
  tempo4 <- df2[which(df2$contract.number == 4),]
  tempo4$BMS.level <- pmin(ell.max, pmax(ell.min, 100 - tempo4$lagk3 + Psi*tempo4$lagn3))
  tempo4$BMS.level <- pmin(ell.max, pmax(ell.min, tempo4$BMS.level - tempo4$lagk2 + Psi*tempo4$lagn2))
  tempo4$BMS.level <- pmin(ell.max, pmax(ell.min, tempo4$BMS.level - tempo4$lagk1 + Psi*tempo4$lagn1))
  tempo5 <- df2[which(df2$contract.number == 5),]
  tempo5$BMS.level <- pmin(ell.max, pmax(ell.min, 100 - tempo5$lagk4 + Psi*tempo5$lagn4))
  tempo5$BMS.level <- pmin(ell.max, pmax(ell.min, tempo5$BMS.level - tempo5$lagk3 + Psi*tempo5$lagn3))
  tempo5$BMS.level <- pmin(ell.max, pmax(ell.min, tempo5$BMS.level - tempo5$lagk2 + Psi*tempo5$lagn2))
  tempo5$BMS.level <- pmin(ell.max, pmax(ell.min, tempo5$BMS.level - tempo5$lagk1 + Psi*tempo5$lagn1))
  data <- rbind(tempo1, tempo2, tempo3, tempo4, tempo5)
  data <- data %>%
    arrange(policy_no, veh.num, renewal_date)
  return(data)
}

###################
### Data ##########
###################

load('Data/df2.Rda')

###################
### Basic Count ###
###################

score.nbclaim <- as.formula(NbClaims ~ car_color + need_glasses + territory + language + food + offset(log(risk_expo)))

db.train <- df2 %>% filter(Type=='TRAIN')
db.test <- df2 %>% filter(Type=='TEST')

Poisson   <- glm(score.nbclaim, family=poisson(link=log), data=db.train)
parm <- Poisson$coefficients
parm[9] <- NA
nb2.MASS <- glm.nb(score.nbclaim, data=db.train) 
MASS.parm <- c(nb2.MASS$coefficients, nb2.MASS$theta)

parm.basic <- cbind(parm, MASS.parm)
ll.basic <- c(logLik(Poisson), logLik(nb2.MASS))
basic <- rbind(parm.basic, ll.basic)

###################
### MVNB ##########
###################

db.train <- df2 %>% filter(Type=='TRAIN')
db.test <- df2 %>% filter(Type=='TEST')

max.loglike.MVNB <- function(parm){
  beta <- parm[1:8]
  alpha <- 1/exp(parm[9])
  
  data$lambda <- data$risk_expo*exp(parm[1] + 
                                 parm[2]* (data$car_color=="Red") +
                                 parm[3]*(data$need_glasses=='Yes') +
                                 parm[4]*(data$territory=='Suburban') +
                                 parm[5]*(data$territory=='Urban') +
                                 parm[6]*(data$language=='French') +
                                 parm[7]*(data$food=='Vegan') +
                                 parm[8]*(data$food=='Vegetarian') )
  data <- data %>%
    group_by(policy_no, veh.num) %>%
    mutate(l.bullet = cumsum(lambda) - lambda,
           nb.bullet = cumsum(NbClaims) - NbClaims)%>%
    ungroup()
  
  data$tau.s = alpha + data$l.bullet
  data$alpha.s = alpha + data$nb.bullet

  ll <- lgamma(data$NbClaims + data$alpha.s) - lgamma(data$alpha.s) - lgamma(data$NbClaims+1) + 
    data$alpha.s*log(data$tau.s) - data$alpha.s*log(data$lambda+data$tau.s) +
    data$NbClaims*log(data$lambda) - data$NbClaims*log(data$lambda+data$tau.s)
  return(-sum(ll))
}

data <- db.train
parm <- c(Poisson$coefficients, -10)
max.loglike.MVNB(parm)
logLik(Poisson)

optim.MVNB <- optim(par = parm, max.loglike.MVNB)
optim.MVNB$value
optim.MVNB$par

MVNB.parm <- c(optim.MVNB$par[1:8], exp(-optim.MVNB$par[9]))

###############
##  NB-Beta ###
###############

max.loglike.NBbeta <- function(parm){
  a <- exp(parm[9])
  b <- exp(parm[10])
  
  data$lambda <- data$risk_expo*exp(parm[1] + 
                                      parm[2]* (data$car_color=="Red") +
                                      parm[3]*(data$need_glasses=='Yes') +
                                      parm[4]*(data$territory=='Suburban') +
                                      parm[5]*(data$territory=='Urban') +
                                      parm[6]*(data$language=='French') +
                                      parm[7]*(data$food=='Vegan') +
                                      parm[8]*(data$food=='Vegetarian') )
  
  data <- data %>%
    group_by(policy_no, veh.num) %>%
    mutate(l.bullet = cumsum(lambda) - lambda,
           nb.bullet = cumsum(NbClaims) - NbClaims)%>%
    ungroup()
  
  data$a.s <- a + data$l.bullet
  data$b.s <- b + data$nb.bullet
  
  part1 <- lgamma(data$NbClaims + data$lambda) - lgamma(data$lambda) - lgamma(data$NbClaims+1)
  part2 <- lgamma(data$a.s + data$b.s) - lgamma(data$a.s) - lgamma(data$b.s)
  part3 <- lgamma(data$lambda + data$a.s) + lgamma(data$NbClaims + data$b.s) - lgamma(data$lambda + data$a.s + data$NbClaims + data$b.s)
  
  ll <- part1 + part2 + part3
  return(-sum(ll))
}

data <- db.train
parm <- c(Poisson$coefficients, 0, 0)
optim.NBbeta <- optim(par = parm, max.loglike.NBbeta)
optim.NBbeta$value



###################
### Kappa-N #######
###################

score.nbclaim <- as.formula(NbClaims ~ car_color + need_glasses + territory + language + food + past.n + past.k + offset(log(risk_expo)))
db.train <- df2 %>% filter(Type=='TRAIN')
db.test <- df2 %>% filter(Type=='TEST')

Poisson   <- glm(score.nbclaim, family=poisson(link=log), data=db.train)
parm <- Poisson$coefficients
parm[11] <- NA
nb2.MASS <- glm.nb(score.nbclaim, data=db.train) 
MASS.parm <- c(nb2.MASS$coefficients, nb2.MASS$theta)

parm.kappan <- cbind(parm, MASS.parm)
ll.kappan <- c(logLik(Poisson), logLik(nb2.MASS))
kappan <- rbind(parm.kappan, ll.kappan)

max(data$BMS.level)
min(data$BMS.level)

###################
### BMS ###########
###################

## Verif ##
data <- set.BMS_levels(ell.max=500, ell.min=5, Psi=-parm[9]/parm[10], df2)
db.train <- data %>% filter(Type=='TRAIN')
db.test <- data %>% filter(Type=='TEST')

score.nbclaim <- as.formula(NbClaims ~ car_color + need_glasses + territory + language + food + BMS.level + offset(log(risk_expo)))
PoissonBMS   <- glm(score.nbclaim, family=poisson(link=log), data=db.train)
parmBMS <- PoissonBMS$coefficients

c(logLik(PoissonBMS), logLik(Poisson))
c(-parm[10], parmBMS[9])

## grid ##

Psi     <- seq(1, 10, length.out = 10)
ell.min <- seq(96, 99, length.out = 4)
ell.max <- seq(101, 120, length.out = 20)
grid <- expand.grid(ell.max = ell.max, ell.min = ell.min, Psi = Psi)
grid$llPoisson <- NA
grid$llNB2 <- NA

db.train <- df2 %>% filter(Type=='TRAIN')
db.test <- df2 %>% filter(Type=='TEST')
for(ii in 1:nrow(grid)){
  print(ii)
  data <- set.BMS_levels(ell.max=grid[ii,1], ell.min=grid[ii,2], Psi=grid[ii,3], db.train)
  PoissonBMS   <- glm(score.nbclaim, family=poisson(link=log), data=data)
  nb2BMS <- glm.nb(score.nbclaim, data=data)
  grid[ii,4] <- logLik(PoissonBMS)
  grid[ii,5] <- logLik(nb2BMS)
}

#84     104      96   2 -44447.66#
print(grid[grid$llPoisson==max(grid$llPoisson),])
print(grid[grid$llNB2==max(grid$llNB2),])
