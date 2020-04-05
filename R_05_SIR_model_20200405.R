# Original code from Joachim Gassen
# COVID-19: The Case of Germany
# https://blog.ephorie.de/covid-19-the-case-of-germany

# adapted to many countries and an intervention using a compound SIR model
# Matjaz Jeran
# last update on 05.04.2020

# changes from 02.04. to 03.04.: a graph after intervention - corrected calculation of ylim
# added print of R0 in simuation graph
# changes on 05.04.: added R0 and StartData and N.int1 to simulation graph, added Belgium

# input:
#  data file on number of infected, dead, and recovered
#  data file from UN and World bank about country statistics on demography
# output:
#  number of graphs showing number of infected, daily differencec of infected
#  and simulated numbers produced from SIR model that optimally fits the real data
# R0 before and after intervention is computed

# some adjustments of input parameters for a selection of countries is provided
# can be added for any country where data is available

# share of patients in hospitals, severe cases and deaths as in code from Joachim Gassen
# but should be adapted for each country demographics and health system

rm (list = ls (all = TRUE))

library (readr)
library (deSolve)

### warning: adjust working directory as needed

if (Sys.info () ["sysname"] == "Windows") setwd ("C:/Moji Dokumenti/Corona")
if (Sys.info () ["sysname"] == "macOS")   setwd ("/home/matjaz/Corona")
if (Sys.info () ["sysname"] == "Linux")   setwd ("/home/matjaz/Corona")

### warning: adjust file name contains to current date for update of data if reading csv files
covid.file <- "./Podatki/jh_covid19_data_2020-04-02.csv"
wbank.file <- "./Podatki/jh_add_wbank_data.csv"

# for loading stored data
covid.dfile <- "./Podatki/jh_covid19_data.RData"
wbank.dfile <- "./Podatki/jh_add_wbank_data.RData"

MaxOpt <- 15  # maximum limit for searching beta and gamma using stats::optim

# in order to select country, keep all countries commented out
# and remove the comment in front of desired country:
# any country can be selected, a popular selection below
# set starting date StartDate: initially first day when Infected > 0
# set starting day of intervention such as quarantine, closure of shops etc N.int when you see change of curves behaviour
# set simulation length NSim 

# experience in use of this progarm shows that setting of N.int1 has big effect on fitting data
# if no fit is achieved try to change N.int1 to day or two ahead or backwards

# Country <- 'CHN'  # China - 03.04.: reasonably well fits data also peak
# Country <- 'ITA'  # Italy - 03.04.: fits data
# Country <- 'AUT'  # Austria - - 03.04.: fits data, peak of epidemy detected and confirmed
# Country <- 'HRV'  # Croatia - 03.04.: fits data, peak not yet confirmed, daily increase of infections has dropped down
# Country <- 'BEL'  # Belgium - 05.04.: fits data, peak not yet confirmed
# Country <- 'DEU'  # Germany - 03.04.: fits data, peak not yet confirmed
# Country <- 'ESP'  # Spain - 03.04.: fits data, peak not yet confirmed, daily increase of infections has dropped down
# Country <- 'GBR'  # UK - 03.04.: fits data
# Country <- 'CHE'  # Switzerland - 03.04.: fits data, peak of epidemy detected and confirmed
# Country <- 'SWE'  # Sweden - 03.04.: fits data for a long and high epidemy
# Country <- 'SGP'  # Singapore  - 05.04.: unstable from day to day, long epidemy ahead?
# Country <- 'KOR'  # Korea South - 03.04.: does not fit too well, model very dependent on the day of intervention N.int1
# Country <- 'USA'  # USA - 03.04.: fits data, unstable from day to day (unreported data?)
 Country <- 'SVN'  # Slovenia - 03.04.: fits data but duration has unstable results from day to day (lack of data on recovered?)

if (! exists ('Country')) print ("**** No country defined! ****")


## jh_covid19_data <- readr::read_csv (covid.file)
load (file = covid.dfile)  # jh_covid19_data
data <- subset (jh_covid19_data, subset = iso3c == Country)

## wb_cs <- readr::read_csv (wbank.file)
load (file = wbank.dfile)  # wb_cs
N <- wb_cs $population [wb_cs $iso3c == Country]


## here is the part to define parameters for each country

# experience in use of this program shows that setting of N.int1 has big effect on fitting data
# if no fit is achieved try to change N.int1 to day or two ahead or backwards


if (Country == "CHN") {
 StartDate <- "2020-01-31"  # Starting day when first infected was counted > 0
 N.int1 <- 16 # starting day of intervention
 NSim <- 100  # No of days for simulation

# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}

if (Country == "ITA") {
 StartDate <- "2020-01-31"  # Starting day when first infected was counted > 0
 N.int1 <- 45 # starting day of intervention
 NSim <- 120  # No of days for simulation

# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}

if (Country == "AUT") {
 StartDate <- "2020-02-25"  # Starting day when first infected was counted > 0
 N.int1 <- 20 # starting day of intervention
 NSim <- 80  # No of days for simulation

# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}

if (Country == "HRV") {
 StartDate <- "2020-02-25"  # Starting day when first infected was counted > 0
 N.int1 <- 23 # starting day of intervention
 NSim <- 80  # No of days for simulation

# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}

if (Country == "BEL") {
 StartDate <- "2020-03-01"  # Starting day when first infected was counted > 0
 N.int1 <- 10 # starting day of intervention
 NSim <- 100  # No of days for simulation

# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}


if (Country == "DEU") {
 StartDate <- "2020-01-27"  # Starting day when first infected was counted > 0
 N.int1 <- 60 # starting day of intervention
 NSim <- 100  # No of days for simulation

# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}

if (Country == "ESP") {
 StartDate <- "2020-03-05"  # Starting date selected by trial and error
 N.int1 <- 18 # starting day of intervention
 NSim <- 80  # No of days for simulation

# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}

if (Country == "GBR") {
 StartDate <- "2020-02-29"  # Starting date selected by trial and error
 N.int1 <- 21 # starting day of intervention - none intervention really so far - so last day
 NSim <- 80  # No of days for simulation

# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}

if (Country == "CHE") {
 StartDate <- "2020-03-01"  # Starting day set manually
 N.int1 <- 11 # starting day of intervention set manually
 NSim <- 80  # No of days for simulation

# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}

if (Country == "SWE") {
 StartDate <- "2020-02-25"  # Starting day set manually
 N.int1 <- 17 # starting day of intervention set manually
 NSim <- 120  # No of days for simulation

# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}

if (Country == "SGP") {
 StartDate <- "2020-01-23"  # Starting day set manually
 N.int1 <- 40 # starting day of intervention set manually
 NSim <- 100  # No of days for simulation
# Singapore is a difficult case that deserves special treatment as there were two vawes of epidemy
# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}

if (Country == "KOR") {
 StartDate <- "2020-01-22"  # Starting day when first infected was counted > 0
 N.int1 <- 27 # starting day of intervention set manually  reasonably good fit for 29,  but awfull for 28 and 30
 NSim <- 100  # No of days for simulation
# Singapore is a difficult case that deserves special treatment as there were two vawes of epidemy
# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}

if (Country == "USA") {
 StartDate <- "2020-03-01"  # Starting day set manually
 N.int1 <- 31 # starting day of intervention - not really started
 NSim <- 120  # No of days for simulation

# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}

if (Country == "SVN") {
 StartDate <- "2020-03-05"  # Starting day when first infected was counted > 0
 N.int1 <- 11 # starting day of intervention
 NSim <- 100  # No of days for simulation

# parameters describing medical care - very dependent on the medical resources - so far the same as in original code
ShareSevereCase    <- 0.2   # share of severe cases - sent to hospital
ShareIntensiveCare <- 0.06  # share of cases with need for intensive care
# https://www.newscientist.com/article/mg24532733-700-why-is-it-so-hard-to-calculate-how-many-people-will-die-from-covid-19/
ShareFatal         <- 0.007 # share of fatal cases
}
 
 
## here is the common part for the simulation for any country

## the necessary functions

# prepare for SIR model
# see Compartmental models in epidemiology
# https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology

SIR <- function (time, state, parameters) {
  par <- as.list (c (state, parameters))
  with (par, {
    dS <- - beta * I * S / N
    dI <- (beta * I * S / N) - (gamma * I)
    dR <- gamma * I
    list (c (dS, dI, dR))
    })
}

# find optimal parameters beta and gamma before intervention
RSS1 <- function (parameters) {
  names (parameters) <- c ("beta", "gamma")
  out <- deSolve::ode (y = init1, times = t1, func = SIR, parms = parameters)
  fit1 <- out[ , 3]
  sum ((tmpInf1 - fit1)^2)
}

# find optimal parameters beta and gamma after intervention
RSS2A <- function (parameters) {
  names (parameters) <- c ("beta", "gamma")
  out <- deSolve::ode (y = init2, times = t2, func = SIR, parms = parameters)
  fit2 <- out[ , 3]
  sum ((tmpInf2 - fit2)^2)
}


## the code

Infected <- data$confirmed [data$date >= StartDate]
Dead <- data$deaths [data$date >= StartDate]
Recovered <- data$recovered [data$date >= StartDate]
Infected <- Infected - Recovered

tSim <- 1:NSim
Day <- 1:(length(Infected))

# plot data so far
dev.new ()
old <- par (mfrow = c(1, 2))
plot (Day, Infected, type ="b", xlab = "Day", ylab = 'Infected (linear scale)')
plot (Day, Infected, type = "p", log = "y", xlab = "Day", ylab = 'Infected (log scale)')
abline (lm (log10 (Infected) ~ Day))
title (paste ("Current infections COVID-19", Country), outer = TRUE, line = -3)


# plot total no of recovered
dev.new ()
plot (Day, Recovered, type ="b", xlab = "Day", ylab = 'Recovered (linear scale)', main = paste ("Total recovereds COVID-19", Country))


# plot daily increase of infected
dev.new ()
plot (Day [2:length(Day)], diff (Infected), type = "b", xlab = "Day", 
 ylab = 'Daily increase of infected', main = paste ("Daily increase of infections of COVID-19", Country))

t1 <- 1:N.int1 # time in days until intervention
tmpInf1 <- Infected[t1]
init1 <- c (S = N - tmpInf1[1], I = tmpInf1[1], R = 0)

# get beta and gama of the best fitted curve
# originally optimal for whole range was calculated
# now only from start until intervention

Opt1 <- stats::optim (par = c(0.5, 0.5), fn = RSS1, method = "L-BFGS-B", lower = c (0, 0), upper = c (MaxOpt, MaxOpt))
par1 <- data.frame (beta = Opt1$par[1], gamma = Opt1$par[2], R0 = Opt1$par[1] / Opt1$par[2])
print (par1)


dev.new ()
parameters <- par1 [1, 1:2]
fit1 <- data.frame (deSolve::ode (y = init1, times = t1, func = SIR, parms = parameters))
plot (I ~ t1, data = fit1, type = "l", col = "blue", ylim = c (0, max (I, tmpInf1)), 
 main = paste ("Total infections of COVID-19", Country, "before intervention"))
points (tmpInf1 ~ t1, type = "b", col = "red")
legend ("topleft", legend = c ("Simulation", "Real data"), col = c ("blue", "red"), pch = 1)

t2 <- (N.int1+1):length(Infected) # time in days after intervention as long data is available
t2.end <- (N.int1+1):NSim  # time from intervention until end of simulation
tmpInf2 <- Infected[t2]
# initial values for SIM model 2 are the last values of SIM model 1
init2 <- c (S = fit1$S[N.int1], I = fit1$I[N.int1], R = 10)

# possible of more than one variant here - but only variant A kept

Opt2A <- stats::optim (par = c(0.5, 0.5), fn = RSS2A, method = "L-BFGS-B", lower = c (0, 0), upper = c (MaxOpt, MaxOpt))
par2A <- data.frame (beta = Opt2A$par[1], gamma = Opt2A$par[2], R0 = Opt2A$par[1] / Opt2A$par[2])
print (par2A)

parameters <- par2A [1, 1:2]

dev.new ()
fit2A <- data.frame (deSolve::ode (y = init2, times = t2, func = SIR, parms = parameters))
plot (I ~ t2, data = fit2A, type = "l", col = "blue", ylim = c (min (fit2A$I, tmpInf2), max (fit2A$I, tmpInf2)),
 xlab = "Days", ylab = "Number of infections", main = paste ("Infections of COVID-19", Country, "after intervention"))
points (tmpInf2 ~ t2, type = "b", col = "red")
legend ("topleft", legend = c ("Simulation", "Real data"), col = c ("blue", "red"), pch = 1)


# plot joint curve fit1 + fit2A
dev.new ()
fit <- rbind (fit1, fit2A)
plot (I ~ Day, data = fit, type = "l", col = "blue", xlab = "Days", ylab = "Number of infections",
 ylim = c (0, max (I, Infected)), main = paste ("Infections of COVID-19", Country,  " - fitted simulation model"))
points (Infected ~ Day, type = "b", col = "red")
legend ("topleft", legend = c ("Simulation", "Real data"), col = c ("blue", "red"), pch = 1)

# compute simulation of whole epidemy
sim2A <- data.frame (deSolve::ode (y = init2, times = t2.end, func = SIR, parms = parameters))

# plot joint simulation with additional estimates
sim <- rbind (fit1, sim2A)
hospital <- sim$I * ShareSevereCase
intensive <- sim$I * ShareIntensiveCare
fatal <- sim$I * ShareFatal

dev.new ()
plot (I ~ tSim, data = sim, type = "l", col = "blue", xlab = "Days", ylab = "Number of persons (linear scale)",
ylim = c (0, max (sim$I, Infected)))

lines (hospital ~ tSim, col = "orange")
lines (intensive ~ tSim, col = "brown")
lines (fatal ~ tSim, col = "black")
points (Infected ~ Day, type = "b", col = "red")
title (paste ("Total infections of COVID-19", Country, " - whole simulation model"),
 sub = paste ("StartDate = ", StartDate, "  N.int =", N.int1))
legend ("topleft", legend = c ("Infected (simulation)", "In hospital (simulation)", 
"Intensive care (simulation)", "Fatal (simulation)", "Confirmed infection"),
col = c ("blue", "orange", "brown", "black", "red"), pch = 1)
legend ("topright", legend = c (paste ("R0_1 =", par1$R0), paste ("R0_2 =", par2A$R0)))

print (N.int1)
print (par1)
print (par2A)

print (max (sim$I))
print (max (hospital))
print (max (intensive))
print (max (fatal))

# remove all graph windows
# graphics.off ()
