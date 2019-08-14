library(ltm)
library(lattice)
library(psych)
library(car)
library(pastecs)
library(scales)
library(ggplot2)
library(arules)
library(plyr)
library(Rmisc)
library(GPArotation)
library(gdata)
library(MASS)
library(qpcR)
library(dplyr)
library(gtools)
library(Hmisc)
# Select only your variables of interest for the PCA
dataset = mydata[, c('select_var1','select_var1',
                     'select_var2','select_var3','select_var4',
                     'select_var5','select_var6','select_var7')]

# Create matrix: some tests will require it
data_matrix = cor(dataset, use = 'complete.obs')

# See intercorrelations
round(data_matrix, 2)

# Bartlett's
cortest.bartlett(dataset)

# KMO
KMO(data_matrix)

# Determinant
det(data_matrix)
# Start off with unrotated PCA
pc1 = psych::principal(dataset, nfactors = 
                         length(dataset), rotate="none")
pc1

plot(pc1$values, type = 'b')
# Now with varimax rotation, Kaiser-normalized 
# by default:
pc2 = psych::principal(dataset, nfactors=2, 
                       rotate = "varimax", scores = TRUE)
pc2
pc2$loadings

# Healthcheck
pc2$residual
pc2$fit
pc2$communality

dataset = cbind(dataset, pc2$scores)
summary(dataset$RC1, dataset$RC2)

ggplot(dataset,
       aes(RC1, RC2, label = as.character(main_eng))) +
  aes (x = RC1, y = RC2, by = main_eng) + stat_density2d(color = "gray87")+
  geom_text(size = 7) +
  ggtitle ('English properties') +
  theme_bw() +
  theme(  
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
  ) +
  theme(axis.line = element_line(color = 'black')) + 
  theme(axis.title.x = element_text(colour = 'black', size = 23, 
                                    margin=margin(15,15,15,15)),
        axis.title.y = element_text(colour = 'black', size = 23, 
                                    margin=margin(15,15,15,15)),
        axis.text.x  = element_text(size=16),
        axis.text.y  = element_text(size=16)) +
  labs(x = "", y = "Varimax-rotated Principal Component 2") +
  theme(plot.title = element_text(hjust = 0.5, size = 32, face = "bold",
                                  margin=margin(15,15,15,15)))
