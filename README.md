# R--Multivariate-Analysis

library(readxl)
library(ggplot2)
library(plotly)
library(dplyr)


Data <- read_xlsx("D:/Utiva/Statistics/projetdemultivariatestatistics/new_data_new.xlsx")
as.data.frame(Data)

Data_Del <- as.data.frame(Data)
Data_Dev <- as.data.frame(Data[,c(4,5,14:21)])

##################### KHI SQUARED ###################
## PB_Del_j//Del_sans_j
base=as.data.frame(Data[,c(1,2)])
base
bas<-table(base)
bas
test1=chisq.test(bas,correct = FALSE)
test1

## PB_Del_J//Del_expe
base=as.data.frame(Data[,c(1,3)])
base
bas<-table(base)
bas
test2=chisq.test(bas,correct = FALSE)
test2

## PB_Del_sans_J //Del_expe
base=as.data.frame(Data[,c(2,3)])
base
bas2<- table(base)
bas
test3=chisq.test(bas,correct = FALSE)
test3

##TO do more.. 


################# ACOBI ################


#Frequency Table
freq=bas/sum(bas)

#Calculs préliminaires Preliminary Calculations
#Margianl effects
apply(bas, 1, sum) #Line marginal effects
apply(bas, 2, sum) #Column marginal effects

#Table of line profiles
profl=prop.table(bas, 1)
profl

#Table of column profiles
profc=prop.table(bas, 2)
profc

test=chisq.test(bas)
test
qchisq(0.95, df=9) #0.95 quqntile order of the chi-square law at 9 degrees of freedom
test$expected 
test$observed 
test$expected/sum(bas) #expected relative frequencies


#Building the attraction/repulsion matrix
D=(test$observed)/(test$expected)
D


#Phi^2=The total inertia of point cloud
phi2=test$statistic/sum(bas)
phi2



#################### CLUSTERING TRY ###########@#

d=dist(na.omit(Burt), method="euclidean") 
clustS=hclust(d, method="single") 
clust1=hclust(d, method="ward.D") 
plot(clustS)
groups=cutree(clustS, k=5) 
rect.hclust(clustS, k=4, border="red")

View(Data_Del[c(17,42),])  


################# ACM FOR DELINQUENCE ######################
#Using dummies table de contingence 
library(dummies)
Delinquence <- dummy.data.frame(Data_Del)
Delinquence
G_l=colSums(Delinquence)/sum(Delinquence)
G_l

plot(G_l)
abline(h = 0.1)
identify()


#Using Burt table 
library(GDAtools)
Burt = burt(Data_Del)
bas = Burt

library("FactoMineR")
library("factoextra")

data_mca1= dummy.data.frame(na.omit(Delinquence)) 
mca1=CA(Delinquence) 
summary(mca1)


data_mca= dummy.data.frame(na.omit(Burt)) 
mca= CA(Burt, ncp = 3) 
summary(mca)


autoplot(mca, axes = c(1, 2), mod = TRUE,
         quali.sup = TRUE, ind = FALSE, filtre = 0, axis.plot = TRUE,
         alpha = 1, point.type = "petit", ellipses = NA, coloriage = NA,
         taille = FALSE, dl.method = "smart.grid", labels = "all",
         label.size = 5)

install.packages("ca")
library(ca)
ca.fit <- ca(Burt)
ca.plot <- plot(mca1)

make.ca.plot.df <- function (ca.plot.obj,
                             row.lab = "Rows",
                             col.lab = "Columns") {
  df <- data.frame(Label = c(rownames(ca.plot.obj$rows),
                             rownames(ca.plot.obj$cols)),
                   Dim1 = c(ca.plot.obj$rows[,1], ca.plot.obj$cols[,1]),
                   Dim2 = c(ca.plot.obj$rows[,2], ca.plot.obj$cols[,2]),
                   Variable = c(rep(row.lab, nrow(ca.plot.obj$rows)),
                                rep(col.lab, nrow(ca.plot.obj$cols))))
  rownames(df) <- 1:nrow(df)
  df
}

##getting variance

ca.sum <- summary(ca.fit)
dim.var.percs <- ca.sum$scree[,"values2"]
dim.var.percs

library(ggplot2)
library(ggrepel)
p <- ggplot(ca.plot.df, aes(x = Dim1, y = Dim2,
                            col = Variable, shape = Variable,
                            label = Label, size = Size)) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .5) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .5) +
  geom_point()

ca.plot.df <- make.ca.plot.df(ca.plot,
                              row.lab = "Construction",
                              col.lab = "Medium")
ca.plot.df$Size <- ifelse(ca.plot.df$Variable == "Construction", 2, 1)

p <- ggplot(ca.fit, aes(x = Dim1, y = Dim2,
                            col = Variable, shape = Variable,
                            label = Label, size = Size)) +
  geom_vline(xintercept = 0, lty = "dashed", alpha = .5) +
  geom_hline(yintercept = 0, lty = "dashed", alpha = .5) +
  geom_point()

ca.sum <- summary(ca.fit)
dim.var.percs <- ca.sum$scree[,"values2"]
dim.var.percs

fviz_pca_var(mca)
mca$eig
barplot(mca$eig[,1])
abline(h = 0.1)


eig.val <- res.mca$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")

##################### PLOT FOR DESCRIPTIVE STATISTICS #############














#####Delinquence justice 
par(mfrow=c(2,2), "las" = 2)
plot(as.factor(Data$fr_del_j), main = "A.Frequencies of Delinquence
     with consequences with the Justice")
plot(na.omit(as.factor(Data$Sum_types_del_j[which(Data$fr_del_j != "NA")])), main = "B. Numbers of types of delinquancy ")
plot(na.omit(as.factor(Data$Sum_drugs_del_j[which(Data$fr_del_j != "NA")])), main = "C. Numbers of types of drugs 
     taken during the delinquancy")
dat = unique(Data$Phase_del_j)
levels(Data$Phase_del_j) = c("D.N.K", "Psycho" , "S.Dep.", "L/M Dep." , "Mela.", "Hypom.","Euthy." ,"Mixte", "High m." , "Mania")
plot(as.factor(Data$Phase_del_j[which(Data$fr_del_j != "NA")]), main = "D. Main phase during the delinquency")

h<-table(as.factor(Data$fr_del_j))/113
h<-table(as.factor(Data$Sum_types_del_j[which(Data$fr_del_j != "NA")]))/length(Data$Sum_types_del_j[which(Data$fr_del_j != "NA")])
h<-table(as.factor(Data$Sum_drugs_del_j[which(Data$fr_del_j != "NA")])) ### number 
h<-table(as.factor(Data$Sum_drugs_del_j[which(Data$fr_del_j != "NA")]))/length(Data$Sum_drugs_del_j[which(Data$fr_del_j != "NA")]) ## frequence in function 
h <-table(as.factor(Data$Phase_del_j[which(Data$fr_del_j != "NA")]))
h <-table(as.factor(Data$Phase_del_j[which(Data$fr_del_j != "NA")]))/length(Data$Phase_del_j[which(Data$fr_del_j != "NA")])

sum(h)
#####Delinquence without justice 
par(mfrow=c(2,2), "las" = 2)
plot(as.factor(Data$fr_del_sans_j), main = "A.Frequencies of Delinquence 
     whitout consequences with the justice")
plot(na.omit(as.factor(Data $sum_types_del_sj [which(Data$fr_del_sans_j != "NA")])), main = "B. Numbers of types of delinquancy ")
plot(na.omit(as.factor(Data$sum_drugs_del_sj[which(Data$fr_del_sans_j != "NA")])), main = "C. Numbers of types of drugs 
     taken during the delinquancy")
plot(as.factor(Data$Phase_del_sj[which(Data$fr_del_sans_j != "NA")]), main = "D. Main phase during the delinquency")

######Experienced
par(mfrow=c(2,2), "las" = 2)
plot(as.factor(Data$fr_del_subie), main = "A.Frequencies of Delinquence 
     exprienced")
plot(na.omit(as.factor(Data $sum_types_del_subie [which(Data$fr_del_subie != "NA")])), main = "B. Numbers of types of delinquancy")
plot(as.factor(Data$Phase_del_subie[which(Data$fr_del_subie != "NA")]), main = "D. Main phase during the delinquency")


###Deviance perso 
par(mfrow=c(2,2), "las" = 2)
plot(as.factor(Data$fr_dev_repercussion_perso), main = "A.Frequencies of Deviance self-reporched")
plot(na.omit(as.factor(Data$sum_type_dev_record_perso [which(Data$fr_dev_repercussion_perso != "NA")])), main = "B. Numbers of types of Deviance ")
plot(na.omit(as.factor(Data$sum_drug_dev_repercussion [which(Data$fr_dev_repercussion_perso != "NA")])), main = "C. Numbers of types of drugs 
     taken during the deviance")
plot(as.factor(Data$Phase_dev_perso[which(Data$fr_dev_repercussion_perso != "NA")]), main = "D. Main phase during the deviance")

### Deviance autrui
par(mfrow=c(2,2), "las" = 2)
plot(as.factor(Data$fr_dev_repercussion_autrui), main = "A.Frequencies of Deviance reporched by other")
plot(na.omit(as.factor(Data$sum_dev_type_record_autrui [which(Data$fr_dev_repercussion_autrui != "NA")])), main = "B. Numbers of types of Deviance ")
plot(na.omit(as.factor(Data$sum_drugs_dev_autrui[which(Data$fr_dev_repercussion_autrui != "NA")])), main = "C. Numbers of types of drugs 
     taken during the deviance")
plot(as.factor(Data$Phase_dev_autrui[which(Data$fr_dev_repercussion_autrui != "NA")]), main = "D. Main phase during the deviance")
















