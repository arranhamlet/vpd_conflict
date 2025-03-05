
# Compartments ------------------------------------------------------------
#Susceptible
deriv(S) <- S + into_S - leaving_S
#Exposed
deriv(E) <- E + into_E - leaving_E
#Infectious
deriv(I) <- I + into_I - leaving_I
#Recovered
deriv(R) <- R + into_R - leaving_R
#Infectious severe
deriv(Is) <- Is + into_Is - leaving_Is
#Recovered complications
deriv(Rc) <- Rc + into_Rc - leaving_Rc


# Compartment dimensions ------------------------------------------
dim(S) <- c(age_groups, vaccination_groups)
dim(E) <- c(age_groups)
dim(I) <- c(age_groups)
dim(R) <- c(age_groups)
dim(Is) <- c(age_groups)
dim(Rc) <- c(age_groups)

age_groups <- parameter()
vaccination_groups <- parameter()

# Initial compartment values ------------------------------------------------------
initial(S) <- c()
initial(E) <- c()
initial(I) <- c()
initial(R) <- c()
initial(Is) <- c()
initial(Rc) <- c()


# Parameters --------------------------------------------------------------
N_fertile <- sum(N)
birth_rate <- parameter()
recovery_rate <- parameter()
foi <- parameter()
background_death <- parameter()
incubation_period <- parameter()
prop_severe <- parameter()
prop_complications <- parameter()
natural_waning <- parameter()
vaccine_waning <- parameter()
death_rate_Is <- parameter()
death_rate_I <- parameter()

#Susceptible
into_S <- N_fertile * birth_rate + R * recovery_rate
leaving_S <- S * foi + S * background_death

#Exposed
into_E <- S * foi
leaving_E <- E * incubation_period + E * background_death

#Infectious
into_I <- E * incubation_period * (1 - prop_severe)
leaving_I <- I * recovery_rate + I * background_death + I * death_rate_I

#Recovered
into_R <- I * recovery_rate + Is * recovery_rate * (1 - prop_complications)
leaving_R <- R * natural_waning + R * background_death

#Infectious severe
into_Is <- E * incubation_period * prop_severe
leaving_Is <- Is * severe_recovery_rate + Is * background_death + Is * death_rate_Is

#Recovered complications
into_Rc <- Is * recovery_rate * prop_complications
leaving_Rc <- Rc * natural_waning + Rc * background_death

