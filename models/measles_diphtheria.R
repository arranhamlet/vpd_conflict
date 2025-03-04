
# Compartments ------------------------------------------------------------
#Susceptible
deriv(S[]) <- S[] + into_S[] - leaving_S[]
#Exposed
deriv(E[]) <- E[] + into_E[] - leaving_E[]
#Infectious
deriv(I[]) <- I[] + into_I[] - leaving_I[]
#Recovered
deriv(R[]) <- R[] + into_R[] - leaving_R[]
#Infectious severe
deriv(Is[]) <- Is[] + into_Is[] - leaving_Is[]
#Recovered complications
deriv(Rc[]) <- Rc[] + into_Rc[] - leaving_Rc[]
#First vaccination
deriv(Vfirst[]) <- Vfirst[] + into_Vfirst[] - leaving_Vfirst[]
#Second vaccination
deriv(Vsecond[]) <- Vsecond[] + into_Vsecond[] - leaving_Vsecond[]
  

# Compartment dimensions ------------------------------------------
dim(S[]) <- c()
dim(E[]) <- c()
dim(I[]) <- c()
dim(R[]) <- c()
dim(Is[]) <- c()
dim(Rc[]) <- c()
dim(Vfirst[]) <- c()
dim(Vsecond[]) <- c()


# Initial compartment values ------------------------------------------------------
initial(S[]) <- c()
initial(E[]) <- c()
initial(I[]) <- c()
initial(R[]) <- c()
initial(Is[]) <- c()
initial(Rc[]) <- c()
initial(Vfirst[]) <- c()
initial(Vsecond[]) <- c()


# Parameters --------------------------------------------------------------
N_fertile <- sum(N[])
birth_rate <- user()
waningfirst <- user()
waningsecond <- user()
recovery_rate <- user()
vaccinated_first[] <- user()
foi[] <- user()
background_death[] <- user()
incubation_period <- user()
prop_severe <- user()
prop_complications <- user()
natural_waning <- user()

#Susceptible
into_S[] <- N_fertile[] * birth_rate + Vfirst[] * waningfirst + Vsecond[] * waningsecond + R[] * recovery_rate
leaving_S[] <- S[] * vaccinated_first[] + S[] * foi[] + S[] * background_death[]

#Exposed
into_E[] <- S[] * foi[]
leaving_E[] <- E[] * incubation_period + E[] * background_death[]

#Infectious
into_I[] <- E[] * incubation_period * (1 - prop_severe)
leaving_I[] <- I[] * recovery_rate + I[] * background_death[]

#Recovered
into_R[] <- I[] * recovery_rate + Is[] * recovery_rate * (1 - prop_complications)
leaving_R[] <- R[] * natural_waning + R[] * background_death[]

#Infectious severe
into_Is[] <- E[] * incubation_period * prop_severe
leaving_Is[] <- Is[] * severe_recovery_rate + I[] * background_death[]

#Recovered complications
into_Rc[] <- Is[] * recovery_rate * prop_complications
leaving_Rc[] <- Rc[] * natural_waning + Rc[] * background_death[]

#First vaccination
into_Vfirst[] <- S[] * vaccinated_first[]
leaving_Vfirst[] <- Vfirst[] * vaccinated_second[] + Vfirst[] * waningfirst - Vfirst[] * background_death[]

#Second vaccination
into_Vsecond[] <- Vfirst[] * vaccinated_second[]
leaving_Vsecond <- Vsecond[] * waningsecond - Vsecond[] * background_death[]
