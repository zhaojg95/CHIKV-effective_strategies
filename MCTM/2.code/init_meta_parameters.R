# Initial Values & Fixed Parameters for Foshan Metapopulation ---------------

# 1. Geography & Demographics 
Districts <- c("Shunde", "Chancheng", "Nanhai", "Gaoming", "Sanshui")

# Population data: https://www.hongheiku.com/tag/%E4%BD%9B%E5%B1%B1 (accessed January 13, 2026)
Pop_dis <- c(3268600, 1361400, 3701900, 477900, 889100)
names(Pop_dis) <- Districts
Total_Pop <- sum(Pop_dis)

# 2. Gravity Model Setup 
# Distance Matrix 

dist_matrix <- read.xlsx("1.data/OD(fs).xlsx") %>%  # the distance between different districts
  as.matrix()  
colnames(dist_matrix) <- rownames(dist_matrix) <- Districts

# Gravity Parameters 
u <- 1.0      
v <- 1.0      
w <- 2.0  

# Calculate Gravity Flow Intensity G_ij (Before Normalization)
G_matrix <- matrix(0, nrow = 5, ncol = 5)
for(i in 1:5){
  for(j in 1:5){
    if(i != j){
      G_matrix[i, j] <- (Pop_dis[i]^u * Pop_dis[j]^v) / (dist_matrix[i, j]^w)
    }
  }
}

# Calculate Weight Matrix M_ij 
# M_ij: 
M_matrix <- matrix(0, nrow = 5, ncol = 5)
for(i in 1:5){
  row_sum_G <- sum(G_matrix[i, ])
  if(row_sum_G > 0){
    M_matrix[i, ] <- G_matrix[i, ] / row_sum_G
  }
}

# 3. Fixed Biological & Migration Parameters 
parms.fixed <- list(
  # Mosquito Parameters 
  a       = 0.145,     
  n       = 0.0181,    
  mu_a    = 0.026,      
  lambda  = 1/11.089,   
  e_p     = 0.859,     
  b       = 0.5,        
  mu_m    = 1/7.4,     
  omega_m = 1/5.5,     
  
  # Human Parameters 
  omega_p = 1/4.5,      
  q       = 0.155,     
  eta     = 1.0,       
  gamma   = 1/7,        
  gamma1  = 1/7,        
  theta_b = 0.0000285,    
  theta_d = 0.0000175,    
  
  
  # Migration Parameters
  rho_mig = 44520,      
  phi     = 17.8,       
  
  # External Inflow Infection Proportions 
  p_e_S = 1-10e-6-10e-6,       
  p_e_E = 10e-6,       
  p_e_I = 10e-6,       
  p_e_A = 0.0,         
  p_e_R = 0.0           
)

# Add Matrix and Vectors to the parms list
parms.fixed$M_matrix <- M_matrix
parms.fixed$Pop_dis  <- Pop_dis

# 4. Parameters to be fitted
# beta_mp0, beta_pm0, k
# total parameters : 5 districts × 3 = 15

fit.par.names <- c(
  outer(c("beta_mp0", "beta_pm0", "k"), Districts, FUN = paste, sep = "_")
)

cat("Parameters to be fitted:\n")
print(fit.par.names)
cat("\nTotal number of parameters:", length(fit.par.names), "\n")
