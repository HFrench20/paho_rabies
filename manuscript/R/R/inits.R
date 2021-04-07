# PARAMETERS & INITIALIZATION for dog rabies models
year <- 365.25
finalTime <- 6
repTime <- 4/52 # check every 4 weeks (roughly 1 month intervals)

# DEMOGRAPHY
# r = br-dr (0.09 - 0.5), K = carrying capacity (25000)
# gamma = density dependence = r/K, 0.015-0.25 (0.0009-0.004 Kitala)
br = 0.42 # 0.42-1
dr = 0.33 # 0.33-0.5
K = 1000 # depends on density or numerical parameterization - lets assume starting pop of 1000...
r = br-dr
gamma = (br-dr)/K

# DISEASE
# mu = 1/inc. period: 12.44-18 (12.47 C&D95)
# alpha = disease induced mortality: 63-73 (64 C&D95), 118 for SD!
mu <- 1/(22.3/year) # rate at which exposed move to infected class (1/incubation period)
alpha <- 1/(3.1/year) # death rate due to disease (1/infectious period)
R0 <- 1.3
beta = R0*((mu+br)*(alpha+br))/(mu*K)    # beta = ~14-80/yr, or 0.0015/wk,
# beta = R0*alpha/N # for FD transmission beta varies with N - so at N = 1000, get beta = 0.15

# CONTROL
vr <- 0     # vaccination rate;
il <- 0.4  	# loss of immunity from vaccination
# rho_delay = delay before vaccination,
# vac_res = No. vaccines issued/ rabies case

# The key parameters that change the period of epidemics are:
# 1) as R0 inc, period decreases (at R0 --> 1.2 cycles lost)
# 2) as br inc, period decreases (K or gamma density dependence has no effect)
# 3) as incubation period increases, period increases (but only slightly)


params=list(births=br, deaths=dr, loss=il, R0=R0, beta=beta, mu=mu, alpha=alpha)

# set up initial population with some latent dogs and proportion vaccinated
pop=list(susc=1000, vacc=10, latent=3, infect=0)


# Reframe beta in terms of R0
# parameters from fitting a negative binomial distribution to biting behaviour
mu_bites = 2.15562140679189
var_bites = 16.0248484990027
k = (mu_bites^2)/(var_bites-mu_bites)
p = 0.49





