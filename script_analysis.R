######################################################################################################################
# PAPER REEF-BUILDING CORALS
######################################################################################################################

# SETTING UP OUR R SESSION

# Remove previously loaded items and graphics
rm(list=ls())
graphics.off()

# Set working directory
setwd("")

# Load SIBER package into the session's environment
library(SIBER)
library(rjags) 
# Need JAGS available at http://www.sourceforge.net/projects/mcmc-jags/files
# set env path: Sys.setenv(JAGS_HOME='C:/Program Files/JAGS/JAGS-4.3.1')



#####################################
# CORAL HOSTS FULL = ALL CONDITIONS #
#####################################

# Import the data, that are stored in a CSV file
fulldata.H.FULL <- read.csv("Data/CORALH_full.csv", header=T)

# Create the key for the community and group codes (useful for quick reference later)
key.H.FULL <- read.csv("Data/KeyFULL.csv", header=F)

# Create a SIBER object that can be manipulated by the SIBER package
siber.full.H.FULL <- createSiberObject(fulldata.H.FULL)

# PLOTTING USING R'S BASE FUNCTIONS (BETTER THAN SIBER PACKAGE TO MAKE PLOT)

# Split the dataset by species
Species.H.FULL.Am <- fulldata.H.FULL[fulldata.H.FULL$group==1,]
Species.H.FULL.Ip <- fulldata.H.FULL[fulldata.H.FULL$group==2,]
Species.H.FULL.Pl <- fulldata.H.FULL[fulldata.H.FULL$group==3,]
Species.H.FULL.Pp <- fulldata.H.FULL[fulldata.H.FULL$group==4,]
Species.H.FULL.Sp <- fulldata.H.FULL[fulldata.H.FULL$group==5,]
Species.H.FULL.Tc <- fulldata.H.FULL[fulldata.H.FULL$group==6,]

# Define colors
colAm <- rgb(128,128,128,max = 200, alpha = 150)
colIp <- rgb(153,102,51,max = 250, alpha = 150)
colPl <- rgb(217,217,38,max = 250, alpha = 150)
colPp <- rgb(120,0,0,max = 250, alpha = 150)
colSp <- rgb(250,20,147,max = 250, alpha = 150)
colTc <- rgb(250,102,0,max = 250, alpha = 150)

# Plot species Am as grey dots
par(mfrow = c(1,1)) # nombre de figures dans la figure g?n?rale 
plot(Species.H.FULL.Am$iso1, Species.H.FULL.Am$iso2, las = 1, type="p", pch = 19, col = colAm, 
     xlim = c(-23,-10), ylim = c(0,12),
     xlab = expression({delta}^13*C~'(\u2030)'),# donne une l?gende sur l'axe des abscisses x
     ylab = expression({delta}^15*N~'(\u2030)'),# donne une l?gende sur l'axe des ordonn?es y
     main=("Coral host ellipses"),# donne un titre au graphique
     bty = "L"
)

# Plot the other species with different colors
points(Species.H.FULL.Ip$iso1, Species.H.FULL.Ip$iso2, type="p", pch = 19, col = colIp)
points(Species.H.FULL.Pl$iso1, Species.H.FULL.Pl$iso2, type="p", pch = 19, col = colPl)
points(Species.H.FULL.Pp$iso1, Species.H.FULL.Pp$iso2, type="p", pch = 19, col = colPp)
points(Species.H.FULL.Sp$iso1, Species.H.FULL.Sp$iso2, type="p", pch = 19, col = colSp)
points(Species.H.FULL.Tc$iso1, Species.H.FULL.Tc$iso2, type="p", pch = 19, col = colTc)

# Add a legend.
legend("topright", 
       legend=c("Acropora muricata", "Isopora palifera", "Porites lutea", "Psammocora profundacella", "Stylophora pistillata", "Tubastraea cf coccinea"), 
       col=c(colAm, colIp, colPl, colPp, colSp, colTc), 
       pch=c(16,16,16,16,16,16), bty="n")

# Add the ellipse of Am as a solid grey line
ellipse.H.FULL.Am <- addEllipse(siber.full.H.FULL$ML.mu[[1]][ , , 1],
                                siber.full.H.FULL$ML.cov[[1]][ , , 1],
                                m = NULL,
                                n = 100,
                                p.interval = NULL,
                                ci.mean = FALSE,
                                col = colAm,
                                lty = 1,
                                lwd = 3)

# Add the ellipse of other species
ellipse.H.FULL.Ip <- addEllipse(siber.full.H.FULL$ML.mu[[1]][ , , 2],
                                siber.full.H.FULL$ML.cov[[1]][ , , 2],
                                m = NULL,
                                n = 100,
                                p.interval = NULL,
                                ci.mean = FALSE,
                                col = colIp,
                                lty = 1,
                                lwd = 3)

ellipse.H.FULL.Pl <- addEllipse(siber.full.H.FULL$ML.mu[[1]][ , , 3],
                                siber.full.H.FULL$ML.cov[[1]][ , , 3],
                                m = NULL,
                                n = 100,
                                p.interval = NULL,
                                ci.mean = FALSE,
                                col = colPl,
                                lty = 1,
                                lwd = 3)

ellipse.H.FULL.Pp <- addEllipse(siber.full.H.FULL$ML.mu[[1]][ , , 4],
                                siber.full.H.FULL$ML.cov[[1]][ , , 4],
                                m = NULL,
                                n = 100,
                                p.interval = NULL,
                                ci.mean = FALSE,
                                col = colPp,
                                lty = 1,
                                lwd = 3)

ellipse.H.FULL.Sp <- addEllipse(siber.full.H.FULL$ML.mu[[1]][ , , 5],
                                siber.full.H.FULL$ML.cov[[1]][ , , 5],
                                m = NULL,
                                n = 100,
                                p.interval = NULL,
                                ci.mean = FALSE,
                                col = colSp,
                                lty = 1,
                                lwd = 3)

ellipse.H.FULL.Tc <- addEllipse(siber.full.H.FULL$ML.mu[[1]][ , , 6],
                                siber.full.H.FULL$ML.cov[[1]][ , , 6],
                                m = NULL,
                                n = 100,
                                p.interval = NULL,
                                ci.mean = FALSE,
                                col = colTc,
                                lty = 1,
                                lwd = 3)

# Now we have a pretty graph, let's have a look at the standard ellipse areas (SEA)
group.ML.H.FULL <- groupMetricsML(siber.full.H.FULL)
print(group.ML.H.FULL)

# We will now use a Bayesian model to estimate SEA. To do that, we need parameters (parms) and priors (priors)
parms <- list()
parms$n.iter <- 2 * 10^5   # Number of model iterations (probably I need to increase but be careful to computation time; Lejeune et al 2017 = 10^5; Lejeune et al Am Nat = 2 * 10^5; Radice et al 2019 = 2 * 10^6).
parms$n.burnin <- 1 * 10^4 # Number of initial discarded values (LM default values = 1 * 10^3; Lejeune et al Am Nat = 1 * 10^4; Radice et al 2019 = 10^4)
parms$n.thin <- 50     # Interval to thin the posterior (LM default values = 10; Lejeune et al Am Nat Thins = 50)
parms$n.chains <- 2        # Number of chains to run (LM default values = 2; Lejeune et al Am Nat = 2 chains)

# Define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# Run the model
ellipses.post.H.FULL <- siberMVN(siber.full.H.FULL, parms, priors)

# Sample our posterior distribution
SEA.B.H.FULL <- siberEllipses(ellipses.post.H.FULL)

# Plot our model's output
siberDensityPlot(SEA.B.H.FULL, xticklabels = colnames(group.ML.H.FULL), 
                 xlab = c("Species"),
                 ylab = expression("Standard Ellipse Area  " ('\u2030' ^2) ),
                 bty = "L",
                 las = 1,
                 main = "Model estimations of SEA",
                 ylims = c(0,10)
)

# Add red dots for the SEA computed earlier from the covariance matrix (optional)
points(1:6, group.ML.H.FULL[3,], col = "red", pch = 16)

# Calculate SEAB modes and credible intervals
SEA.B.credibles <- lapply(as.data.frame(SEA.B.H.FULL),function(x,...){tmp<-hdrcde::hdr(x)$hdr}, prob = cr.p)
SEA.B.credibles
SEA.B.modes <- lapply(as.data.frame(SEA.B.H.FULL),function(x,...){tmp<-hdrcde::hdr(x)$mode}, prob = cr.p, all.modes=T)
SEA.B.modes

#########################################
# CORAL SYMBIONTS FULL = ALL CONDITIONS #
#########################################

# Import the data, that are stored in a CSV file
fulldata.S.FULL <- read.csv("Data/CORALS_full.csv", header=T)

# Create the key for the community and group codes (useful for quick reference later)
key.S.FULL <- read.csv("Data/KeyFULL.csv", header=F)

# Create a SIBER object that can be manipulated by the SIBER package
siber.full.S.FULL <- createSiberObject(fulldata.S.FULL)

# PLOTTING USING R'S BASE FUNCTIONS (BETTER THAN SIBER PACKAGE TO MAKE PLOT)

# Split the dataset by species
Species.S.FULL.Am <- fulldata.S.FULL[fulldata.S.FULL$group==1,]
Species.S.FULL.Ip <- fulldata.S.FULL[fulldata.S.FULL$group==2,]
Species.S.FULL.Pl <- fulldata.S.FULL[fulldata.S.FULL$group==3,]
Species.S.FULL.Pp <- fulldata.S.FULL[fulldata.S.FULL$group==4,]
Species.S.FULL.Sp <- fulldata.S.FULL[fulldata.S.FULL$group==5,]

# Define colors
colAm <- rgb(128,128,128,max = 200, alpha = 150)
colIp <- rgb(153,102,51,max = 250, alpha = 150)
colPl <- rgb(217,217,38,max = 250, alpha = 150)
colPp <- rgb(120,0,0,max = 250, alpha = 150)
colSp <- rgb(250,20,147,max = 250, alpha = 150)

# Plot species Am as grey dots
par(mfrow = c(1,1)) # nombre de figures dans la figure g?n?rale 
plot(Species.S.FULL.Am$iso1, Species.S.FULL.Am$iso2, las = 1, type="p", pch = 19, col = colAm, 
     xlim = c(-23,-10), ylim = c(0,12),
     xlab = expression({delta}^13*C~'(\u2030)'),# donne une l?gende sur l'axe des abscisses x
     ylab = expression({delta}^15*N~'(\u2030)'),# donne une l?gende sur l'axe des ordonn?es y
     main=("Coral symbiont ellipses"),# donne un titre au graphique
     bty = "L"
)

# Plot the other species with different colors
points(Species.S.FULL.Ip$iso1, Species.S.FULL.Ip$iso2, type="p", pch = 19, col = colIp)
points(Species.S.FULL.Pl$iso1, Species.S.FULL.Pl$iso2, type="p", pch = 19, col = colPl)
points(Species.S.FULL.Pp$iso1, Species.S.FULL.Pp$iso2, type="p", pch = 19, col = colPp)
points(Species.S.FULL.Sp$iso1, Species.S.FULL.Sp$iso2, type="p", pch = 19, col = colSp)

# Add a legend.
legend("topright", 
       legend=c("Acropora muricata", "Isopora palifera", "Porites lutea", "Psammocora profundacella", "Stylophora pistillata"), 
       col=c(colAm, colIp, colPl, colPp, colSp), 
       pch=c(16,16,16,16,16), bty="n")

# Add the ellipse of Am as a solid grey line
ellipse.S.FULL.Am <- addEllipse(siber.full.S.FULL$ML.mu[[1]][ , , 1],
                                siber.full.S.FULL$ML.cov[[1]][ , , 1],
                                m = NULL,
                                n = 100,
                                p.interval = NULL,
                                ci.mean = FALSE,
                                col = colAm,
                                lty = 1,
                                lwd = 3)

# Add the ellipse of other species
ellipse.S.FULL.Ip <- addEllipse(siber.full.S.FULL$ML.mu[[1]][ , , 2],
                                siber.full.S.FULL$ML.cov[[1]][ , , 2],
                                m = NULL,
                                n = 100,
                                p.interval = NULL,
                                ci.mean = FALSE,
                                col = colIp,
                                lty = 1,
                                lwd = 3)

ellipse.S.FULL.Pl <- addEllipse(siber.full.S.FULL$ML.mu[[1]][ , , 3],
                                siber.full.S.FULL$ML.cov[[1]][ , , 3],
                                m = NULL,
                                n = 100,
                                p.interval = NULL,
                                ci.mean = FALSE,
                                col = colPl,
                                lty = 1,
                                lwd = 3)

ellipse.S.FULL.Pp <- addEllipse(siber.full.S.FULL$ML.mu[[1]][ , , 4],
                                siber.full.S.FULL$ML.cov[[1]][ , , 4],
                                m = NULL,
                                n = 100,
                                p.interval = NULL,
                                ci.mean = FALSE,
                                col = colPp,
                                lty = 1,
                                lwd = 3)

ellipse.S.FULL.Sp <- addEllipse(siber.full.S.FULL$ML.mu[[1]][ , , 5],
                                siber.full.S.FULL$ML.cov[[1]][ , , 5],
                                m = NULL,
                                n = 100,
                                p.interval = NULL,
                                ci.mean = FALSE,
                                col = colSp,
                                lty = 1,
                                lwd = 3)

# Now we have a pretty graph, let's have a look at the standard ellipse areas (SEA)
group.ML.S.FULL <- groupMetricsML(siber.full.S.FULL)
print(group.ML.S.FULL)

# We will now use a Bayesian model to estimate SEA. To do that, we need parameters and priors.
parms <- list()
parms$n.iter <- 2 * 10^5   # Number of model iterations (probably increase but be careful to computation time).
parms$n.burnin <- 1 * 10^4 # Number of initial discarded values
parms$n.thin <- 50     # Interval to thin the posterior
parms$n.chains <- 2        # Number of chains to run

# We also need to define the priors.
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# Run the model
ellipses.post.S.FULL <- siberMVN(siber.full.S.FULL, parms, priors)

# Sample our posterior distribution
SEA.B.S.FULL <- siberEllipses(ellipses.post.S.FULL)

# Let's plot our model's output
siberDensityPlot(SEA.B.S.FULL, xticklabels = colnames(group.ML.S.FULL), 
                 xlab = c("Species"),
                 ylab = expression("Standard Ellipse Area  " ('\u2030' ^2) ),
                 bty = "L",
                 las = 1,
                 main = "Model estimations of SEA",
                 ylims = c(0,10)
)

# Add red dots for the SEA computed earlier from the covariance matrix (optional)
points(1:5, group.ML.S.FULL[3,], col = "red", pch = 16)

# Calculate SEAB modes and credible intervals
SEA.B.credibles <- lapply(as.data.frame(SEA.B.S.FULL),function(x,...){tmp<-hdrcde::hdr(x)$hdr}, prob = cr.p)
SEA.B.credibles
SEA.B.modes <- lapply(as.data.frame(SEA.B.S.FULL),function(x,...){tmp<-hdrcde::hdr(x)$mode}, prob = cr.p, all.modes=T)
SEA.B.modes

######################################################################################################################
#END OF SCRIPT
######################################################################################################################

