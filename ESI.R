install.packages("simstandard")
library(simstandard)

# lavaan syntax for model used to simulate data
PRCQ<-'
	IC =~ 0.8*I1 + 0.8*I2 + 0.8*I3 + 0.8*I4
	CC =~ 0.8*I5 + 0.8*I6 + 0.8*I7 + 0.8*I8
	AC =~ 0.8*I9 + 0.8*I10 + 0.8*I11 + 0.8*I12

	IC  =~  0.3*CC 
	IC  =~  0.2*AC
	CC  =~  0.1*AC
'

set.seed(1234)
STEM <- sim_standardized(PRCQ, n = 1000, observed = T, latent = F, errors = F)
nonSTEM <- sim_standardized(PRCQ, n = 1000, observed = T, latent = F, errors = F)

STEM$group<-"STEM"
nonSTEM$group<-"nonSTEM"

combined<-rbind(STEM, nonSTEM)

library(psych)
describe(combined)
describeBy(combined, group="group")

## Simulate low loading for I10 in non-STEM majors
PRCQ.invar.load<-'
	IC =~ 0.8*I1 + 0.8*I2 + 0.8*I3 + 0.8*I4
	CC =~ 0.8*I5 + 0.8*I6 + 0.8*I7 + 0.8*I8
	AC =~ 0.8*I9 + 0.3*I10 + 0.8*I11 + 0.8*I12

	IC  =~  0.3*CC 
	IC  =~  0.2*AC
	CC  =~  0.1*AC
'

nonSTEM.invar.load <- sim_standardized(PRCQ.invar.load, n = 1000, observed = T, latent = F, errors = F)

nonSTEM.invar.load$group<-"nonSTEM"

combined.invar.load<-rbind(STEM, nonSTEM.invar.load)

describeBy(combined.invar.load, group="group")

## Simulate high mean for I3 in STEM majors group
STEM.invar.mean<-STEM
STEM.invar.mean$I3<-STEM.invar.mean$I3+2

STEM.invar.mean$group<-"STEM"

combined.invar.mean<-rbind(STEM.invar.mean, nonSTEM)

describeBy(combined.invar.mean, group="group")

## Visualizing data
library(dplyr)
library(corrplot)

## Combined dataset
combined %>% select(I1:I12) %>%   cor() %>% 
  corrplot(., method="color", tl.pos="d", tl.col="black", type="lower", 
           addgrid.col="grey",
           mar=c(0,0,1,0))
            
combined %>% filter(group=="STEM") %>% select(I1:I12)  %>%   cor() %>% 
  corrplot(., method="color", tl.pos="d", tl.col="black", type="lower", 
           addgrid.col="grey", 
           title="STEM Majors", mar=c(0,0,1,0))
           
           
combined %>% filter(group=="nonSTEM") %>% select(I1:I12)  %>%   cor() %>% 
  corrplot(., method="color", tl.pos="d", tl.col="black", type="lower", 
           addgrid.col="grey",
           title="Non-STEM Majors", mar=c(0,0,1,0))   
           
## Low Loading
combined.invar.load %>% select(I1:I12) %>%   cor() %>% 
  corrplot(., method="color", tl.pos="d", tl.col="black", type="lower", 
           addgrid.col="grey",
           title="Combined Data Varied\n Strength of Association for I10",
            mar=c(0,0,1,0))
            
combined.invar.load %>% filter(group=="STEM") %>% select(I1:I12)  %>%   cor() %>% 
  corrplot(., method="color", tl.pos="d", tl.col="black", type="lower", 
           addgrid.col="grey",
           title="STEM Majors", mar=c(0,0,1,0))
           
           
combined.invar.load %>% filter(group=="nonSTEM") %>% select(I1:I12)  %>%   cor() %>% 
  corrplot(., method="color", tl.pos="d", tl.col="black", type="lower", 
           addgrid.col="grey",
           title="Non-STEM Majors", mar=c(0,0,1,0))  
           
## High Mean 
combined.invar.mean %>% select(I1:I12) %>%   cor() %>% 
  corrplot(., method="color", tl.pos="d", tl.col="black", type="lower", 
           addgrid.col="grey",
           title="Combined Data\n Varied Mean for I3",
            mar=c(0,0,1,0))
            
combined.invar.mean %>% filter(group=="STEM") %>% select(I1:I12)  %>%   cor() %>% 
  corrplot(., method="color", tl.pos="d", tl.col="black", type="lower", 
           addgrid.col="grey",
           title="STEM Majors", mar=c(0,0,1,0))
           
           
combined.invar.mean %>% filter(group=="nonSTEM") %>% select(I1:I12)  %>%   cor() %>% 
  corrplot(., method="color", tl.pos="d", tl.col="black", type="lower", 
           addgrid.col="grey",
           title="Non-STEM Majors", mar=c(0,0,1,0))
           
## Create boxplot
library(ggplot2)
library(reshape2)
combined.invar.mean$group<-ifelse(combined.invar.mean$group=="STEM", "STEM Majors", "Non-STEM Majors")
combined.invar.mean$group<-ordered(combined.invar.mean$group,levels=c("STEM Majors", "Non-STEM Majors"))

## Reformat data
melt.mean<-combined.invar.mean %>% select(I1:I12, group) %>% melt(id="group")
melt.mean$group<-melt.mean$group %>% as.factor()  

ggplot(melt.mean, aes(x=group, y=value, fill=group))+ geom_boxplot() +  
  facet_grid(.~variable) + theme_bw() + 
  theme(axis.title.x=element_blank(),  axis.text.x=element_blank(),
       axis.ticks.x=element_blank(), axis.title.y=element_blank(), legend.position="bottom") +
  scale_fill_discrete(name="Group") 
  
## Recreating Table 1 Fit Indices

## Step O: Baseline Model for Each Group

## Specify three factor model with four items associated with each factor 
library(lavaan)

model.test<-'
	IC =~ I1 + I2 + I3 + I4
	CC =~ I5 + I6 + I7 + I8
	AC =~ I9 + I10 + I11 + I12
'
## STEM majors data - Step 0
STEM.step0<-cfa(data=combined.invar.mean %>% filter(group=="STEM Majors"), 
				model=model.test, estimator="ML", meanstructure=TRUE)
## Make sure you have run the code in the visualization section so that the group name has changed and the dplyr package is installed

## Generates Figures ESI4 & ESI6
summary(STEM.step0, standardized=TRUE, fit.measures=TRUE) 

## Non-STEM majors data - Step 0
nonSTEM.step0<-cfa(data=combined.invar.mean %>% filter(group=="Non-STEM Majors"),
				   model=model.test, estimator="ML", meanstructure=TRUE)

## Generates Figures ESI5 & ESI7
summary(nonSTEM.step0, standardized=TRUE, fit.measures=TRUE) 

## Step 1
step1.comb.mean<-cfa(data=combined.invar.mean, model=model.test, 
					 group="group", estimator="ML")

## Generates Figure ESI8
summary(step1.comb.mean, standardized=TRUE, fit.measures=TRUE) 

## Step 2
step2.comb.mean<-cfa(data=combined.invar.mean, model=model.test, 
					 group="group", estimator="ML", group.equal=c("loadings"))

## Generates Figures ESI9 & ESI10
summary(step2.comb.mean, standardized=TRUE, fit.measures=TRUE) 

## Step 3
step3.comb.mean<-cfa(data=combined.invar.mean, model=model.test, 
					 group="group", estimator="ML", 
					 group.equal=c("loadings", "intercepts"))

## Generates Figures ESI11 & ESI12
summary(step3.comb.mean, standardized=TRUE, fit.measures=TRUE) 

## Step 4
step4.comb.mean<-cfa(data=combined.invar.mean, model=model.test, 
					 group="group", estimator="ML", 
					 group.equal=c("loadings", "intercepts", "residuals"))

summary(step4.comb.mean, standardized=TRUE, fit.measures=TRUE) 

## Exporting Data from R to Mplus
library(MplusAutomation)

## Change factor variable to numeric
combined.invar.mean$group<-combined.invar.mean$group %>% as.numeric()

## Confirm group assignment
describeBy(combined.invar.mean, group="group")

## Export data
prepareMplusData(combined.invar.mean, filename="InvarianceMean.dat", inpfile = T, 
				 keepCols=c("I1", "I2", "I3", "I4","I5", "I6", "I7", "I8", "I9", 
				 "I10", "I11", "I12", "group"))

## Code for conducting invariance testing steps with other datasets

## Combined Data - STEM Majors Step 0
STEM.step0.comb<-cfa(data = combined %>% filter(group=="STEM Majors"), model = model.test, estimator="ML", meanstructure=TRUE)
## Make sure you have run the code in the visualization section so that the group name has changed and the dplyr package is installed

summary(STEM.step0, standardized=TRUE, fit.measures=TRUE) 

## Combined Data - Non-STEM Majors Step 0
nonSTEM.step0.comb<-cfa(data = combined %>% filter(group=="Non-STEM Majors"), model = model.test, estimator="ML", meanstructure=TRUE)

summary(nonSTEM.step0, standardized=TRUE, fit.measures=TRUE) 

## Combined Data - Step 1
step1.comb<-cfa(data = combined, model = model.test, group="group", estimator="ML")

summary(step1.comb, standardized=TRUE, fit.measures=TRUE) 

## Combined Data - Step 2
step2.comb<-cfa(data = combined, model = model.test, group="group", estimator="ML", group.equal=c("loadings"))

summary(step2.comb, standardized=TRUE, fit.measures=TRUE) 

## Combined Data - Step 3
step3.comb<-cfa(data = combined, model = model.test, group="group", estimator="ML", group.equal=c("loadings", "intercepts"))

summary(step3.comb, standardized=TRUE, fit.measures=TRUE) 

## Combined Data - Step 4
step4.comb<-cfa(data = combined, model = model.test, group="group", estimator="ML", group.equal=c("loadings", "intercepts", "residuals"))

summary(step4.comb, standardized=TRUE, fit.measures=TRUE) 

## Combined Data with Invariant Loadings - STEM Majors Step 0
STEM.step0.load<-cfa(data = combined.invar.load %>% filter(group=="STEM"), model = model.test, estimator="ML", meanstructure=TRUE)
## Make sure you have run the code in the visualization section so that the dplyr package is installed

summary(STEM.step0.load, standardized=TRUE, fit.measures=TRUE) 

## Combined Data with Invariant Loadings - Non-STEM Majors Step 0
nonSTEM.step0.load<-cfa(data = combined.invar.load %>% filter(group=="nonSTEM"), model = model.test, estimator="ML", meanstructure=TRUE)

summary(nonSTEM.step0.load, standardized=TRUE, fit.measures=TRUE) 

## Combined Data with Invariant Loadings - Step 1
step1.comb.load<-cfa(data = combined.invar.load, model = model.test, group="group", estimator="ML")

summary(step1.comb.load, standardized=TRUE, fit.measures=TRUE) 

## Combined Data with Invariant Loadings - Step 2
step2.comb.load<-cfa(data = combined.invar.load, model = model.test, group="group", estimator="ML", group.equal=c("loadings"))

summary(step2.comb.load, standardized=TRUE, fit.measures=TRUE) 

## Combined Data with Invariant Loadings - Step 3
step3.comb.load<-cfa(data = combined.invar.load, model = model.test, group="group", estimator="ML", group.equal=c("loadings", "intercepts"))

summary(step3.comb.load, standardized=TRUE, fit.measures=TRUE) 

## Combined Data with Invariant Loadings - Step 4
step4.comb.load<-cfa(data = combined.invar.load, model = model.test, group="group", estimator="ML", group.equal=c("loadings", "intercepts", "residuals"))

summary(step4.comb.load, standardized=TRUE, fit.measures=TRUE) 
 

## Creating Ordered Categorical Data

## Create new dataset using exisitng dataset as template
STEM.ord<-STEM

## Convert continous data for each item into categorical with 5 bins
for(i in 1:12){  
  var[i]<-paste0("I", i)
  STEM.ord[[var[i]]]<-as.numeric(cut(STEM[[var[i]]], breaks=5))
}

## Confirm that 5 bins have been created
table(STEM.ord$I1)

## Confirm that data in each bin are from correct ranges
plot(STEM$I1, STEM.ord$I1)

nonSTEM.ord<-nonSTEM

for(i in 1:12){  
  var[i]<-paste0("I", i)
  nonSTEM.ord[[var[i]]]<-as.numeric(cut(nonSTEM[[var[i]]], breaks=5))
}

## Create combined dataset form ordinal STEM and nonSTEM majors data
combined.ord<-rbind(STEM.ord, nonSTEM.ord)

## Creating ordinal data from dataset with Loading difference
nonSTEM.invar.load.ord<-nonSTEM.invar.load

for(i in 1:12){  
  var[i]<-paste0("I", i)
  nonSTEM.invar.load.ord[[var[i]]]<-as.numeric(cut(nonSTEM.invar.load[[var[i]]], breaks=5))
}

combined.invar.load.ord<-rbind(STEM.ord, nonSTEM.invar.load.ord)

## Creating ordinal data from dataset with mean difference
STEM.invar.mean.ord<-STEM.invar.mean

for(i in 1:12){  
  var[i]<-paste0("I", i)
  STEM.invar.mean.ord[[var[i]]]<-as.numeric(cut(STEM.invar.mean[[var[i]]], breaks=5))
}

combined.invar.mean.ord<-rbind(STEM.invar.mean.ord, nonSTEM.ord)

## Conceptualizing thresholds by visualizing distributions
plot(density(combined$I1)), 
	 main="Density Plot for Combined Data Item I1 - Continuous",
	 ylab="Frequency", xlab="Response")
	 
barplot(prop.table(table(combined.ord$I1)), 
	    main="Frequency Plot for Combined Data Item I1 - Ordinal", 
	    ylab="Frequency", xlab="Response")

## Request cut points
summary(cut(combined$I1, breaks=5))

## Add cut points to plot
plot(density(combined$I1), 
     main="Density Plot for Combined Data Item I1 - Continuous",
     ylab="Frequency", xlab="Response")
abline(v=c(-1.97, -0.672, 0.624, 1.92), col="grey")

## Fitting a CFA model to the combined.ord data to view thresholds
combined.ord.cfa<-cfa(data = combined.ord, model = model.test, 
					  ordered=c("I1", "I2", "I3", "I4", "I5", "I6",
					  "I7", "I8", "I9", "I10", "I11", "I12"))
summary(combined.ord.cfa, standardized=TRUE, fit.measures=TRUE)

## Exporting categorical data to Mplus
## Turning the grouping variable into a numeric variable
combined.ord$group<-combined.ord$group %>% as.factor() %>% as.numeric()
describeBy(combined.ord, group="group")

prepareMplusData(combined.ord, filename="CombinedOrdinal.dat", 
				 inpfile = T, keepCols=c("I1", "I2", "I3", "I4",
				 "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12", "group"))


## Invariance Testing with Categorical Data

## Ordered Categorical STEM majors data - Step 0
STEM.step0.ord<-cfa(data=combined.invar.mean.ord %>% filter(group=="STEM"), 
				    model=model.test, ordered=c("I1", "I2", "I3", "I4", "I5", 
				    "I6", "I7", "I8", "I9", "I10", "I11", "I12"), 
				    parameterization="theta")

summary(STEM.step0.ord, standardized=TRUE, fit.measures=TRUE) 

## Ordered Categorical nonSTEM majors data - Step 0
nonSTEM.step0.ord<-cfa(data=combined.invar.mean.ord %>% filter(group=="nonSTEM"),
					   model=model.test, ordered=c("I1", "I2", "I3", "I4", "I5", 
					   "I6", "I7", "I8", "I9", "I10", "I11", "I12"), 
					   parameterization="theta")

summary(nonSTEM.step0.ord, standardized=TRUE, fit.measures=TRUE) 

## Ordered Categorical - Step 1
step1.comb.mean.ord<-cfa(data=combined.invar.mean.ord, group="group", 
						 model=model.test, ordered=c("I1", "I2", "I3", 
						 "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12"),
						 parameterization="theta")

summary(step1.comb.mean.ord, standardized=TRUE, fit.measures=TRUE) 

## Ordered Categorical - Step 2
step2.comb.mean.ord<-cfa(data=combined.invar.mean.ord, group="group", 
						 model = model.test, ordered=c("I1", "I2", "I3", 
						 "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12")
						 group.equal=c("loadings"), parameterization="theta")

summary(step2.comb.mean.ord, standardized=TRUE, fit.measures=TRUE) 

## Ordered Categorical - Step 3
step3.comb.mean.ord<-cfa(data=combined.invar.mean.ord, group="group", 
						 model = model.test, ordered=c("I1", "I2", "I3", 
						 "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12"),
						 group.equal=c("loadings", "thresholds"),
						  parameterization="theta")

summary(step3.comb.mean.ord, standardized=TRUE, fit.measures=TRUE) 

## Exporting Categorical Data to Mplus
combined.invar.mean.ord$group<-combined.ord$group %>% as.factor() %>% as.numeric()

prepareMplusData(combined.invar.mean.ord, 
				 filename="CombinedInvarMeanOrdinal.dat", 
				 inpfile = T, keepCols=c("I1", "I2", "I3", 
				 "I4","I5", "I6", "I7", "I8", "I9", "I10", 
				 "I11", "I12", "group"))

## Code for conducting invariance testing steps with other categorical datasets

## Categorical Combined Data - STEM Majors Step 0
STEM.step0.comb.ord<-cfa(data = combined.ord %>% filter(group=="STEM"), model = model.test, ordered=c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12"), parameterization="theta")

summary(STEM.step0.comb.ord, standardized=TRUE, fit.measures=TRUE) 

## Categorical Combined Data - nonSTEM Majors Step 0
nonSTEM.step0.comb.ord<-cfa(data = combined.ord %>% filter(group=="nonSTEM"), model = model.test, ordered=c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12"), parameterization="theta")

summary(nonSTEM.step0.comb.ord, standardized=TRUE, fit.measures=TRUE) 

## Categorical Combined Data - Step 1
step1.comb.ord<-cfa(data = combined.ord, group="group", model = model.test,  ordered=c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12"),  parameterization="theta")

summary(step1.comb.ord, standardized=TRUE, fit.measures=TRUE) 

## Categorical Combined Data - Step 2
step2.comb.ord<-cfa(data = combined.ord, group="group", model = model.test, ordered=c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12"), group.equal=c("loadings"), parameterization="theta")

summary(step2.comb.ord, standardized=TRUE, fit.measures=TRUE) 

## Categorical Combined Data - Step 3
step3.comb.ord<-cfa(data = combined.ord, group="group", model = model.test, ordered=c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12"), group.equal=c("loadings", "thresholds"), parameterization="theta")

summary(step3.comb.ord, standardized=TRUE, fit.measures=TRUE) 

## Categorical Combined Data with Invariant Loadings - Non-STEM Majors Step 0
STEM.step0.comb.load.ord<-cfa(data = combined.invar.load.ord %>% filter(group=="STEM"), model = model.test, ordered=c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12"), parameterization="theta")

summary(STEM.step0.comb.load.ord, standardized=TRUE, fit.measures=TRUE) 

## Categorical Combined Data with Invariant Loadings - nonSTEM Majors Step 0
nonSTEM.step0.comb.load.ord<-cfa(data = combined.invar.load.ord %>% filter(group=="nonSTEM"), model = model.test, ordered=c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12"), parameterization="theta")

summary(nonSTEM.step0.comb.load.ord, standardized=TRUE, fit.measures=TRUE) 

## Categorical Combined Data with Invariant Loadings - Step 1
step1.comb.load.ord<-cfa(data = combined.invar.load.ord, group="group", model = model.test,  ordered=c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12"),  parameterization="theta")

summary(step1.comb.load.ord, standardized=TRUE, fit.measures=TRUE) 

## Categorical Combined Data with Invariant Loadings - Step 2
step2.comb.load.ord<-cfa(data = combined.invar.load.ord, group="group", model = model.test, ordered=c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12"), group.equal=c("loadings"), parameterization="theta")

summary(step2.comb.load.ord, standardized=TRUE, fit.measures=TRUE) 

## Categorical Combined Data with Invariant Loadings - Step 3
step3.comb.load.ord<-cfa(data = combined.invar.load.ord, group="group", model = model.test, ordered=c("I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10", "I11", "I12"), group.equal=c("loadings", "thresholds"), parameterization="theta")

summary(step3.comb.load.ord, standardized=TRUE, fit.measures=TRUE) 
