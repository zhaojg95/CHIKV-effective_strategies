# initial value
Np <- 9698900  #https://tjgb.hongheiku.com/djs/63215.html
# k <- 15 # The initial ratio of larvae and adult mosquitoes to humans
# Nm <- Np*k

# fixed parameters
parms.fixed<- c(a       = 0.145,   #Natural birth rate of Aedes mosquitoes
                n       = 0.0181,  #Proportion of infection through mosquito eggs
                mu_a    =  0.026,     # Mortality rate of aquatic stage
                lambda  = 1/11.089,  #Emergence rate
                e_p = 0.859, #Proportion of emergence 
                b = 0.5, #Biting rate
                mu_m       = 1/7.4, #Natural mortality rate of Aedes mosquitoes
                omega_m = 1/5.5,  #1/Extrinsic incubation period(EIP)
                omega_p = 1/4.5, # 1/Human incubation period
                q       = 0.155,  #Proportion of asymptomatic cases
                eta = 1,  #transmissibility coefficient of asymptomatic infections relative to symptomatic infections
                gamma   = 1/7, #1/removed
                gamma1  = 1/7 #1/removed
)
