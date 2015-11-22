# {r LCA on the class types, tidy=TRUE}
library(poLCA)

# load the workspace with the relevant objects in it
load("~/Dropbox/LKMco Work/Pearson/Pearson data and analysis/LCA_workspace.RData")

# store all the teacher types in one variable
names(join4)
f1 <- cbind(whyjoin_type, whystay_type, whereteach_type, whymove_type) ~ 1

# 1. First model, two classes
m1 <- poLCA(f1, maxiter=50000, nclass=2, nrep=30, data = join4)
# view the plot
plot(m1)

# 2. Second model, three classes
m2 <- poLCA(f1, maxiter=50000, nclass=3, nrep=30, data = join4)
# view the plot
plot(m2)

# Fit has improved slightly

# 3. Third model, four classes
m3 <- poLCA(f1, maxiter=50000, nclass=4, nrep=30, data = join4)
# view the plot
plot(m3)

# 4. Fourth model, five classes
m4 <- poLCA(f1, maxiter=50000, nclass=5, nrep=30, data = join4)
# view the plot
plot(m4)

# store the fit statistics of these four models in an array
fitstats <- array(NA, dim = c(5, 6))
colnames(fitstats) <- c("AIC", "BIC", "G2", "Chi2", "MaxLL", "df")
fitstats[1,] <- c(m1$aic, m1$bic, m1$Gsq, m1$Chisq, m1$llik, m1$resid.df)
fitstats[2,] <- c(m2$aic, m2$bic, m2$Gsq, m2$Chisq, m2$llik, m2$resid.df)
fitstats[3,] <- c(m3$aic, m3$bic, m3$Gsq, m3$Chisq, m3$llik, m3$resid.df)
fitstats[4,] <- c(m4$aic, m4$bic, m4$Gsq, m4$Chisq, m4$llik, m4$resid.df)

# look at the fit statistics
fitstats

# the numbers keep going down (except BIC, which has gone up once we have 5 classes) but I haven't reached saturation point yet, so let's run a few more models

# 5. Fifth model, six classes
m5 <- poLCA(f1, maxiter=50000, nclass=6, nrep=30, data = join4)
fitstats[5,] <- c(m5$aic, m5$bic, m5$Gsq, m5$Chisq, m5$llik, m5$resid.df)

# check the fitstats again
fitstats