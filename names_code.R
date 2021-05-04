#########################################################################################
### R code for: Parental Investment Is Biased toward Children Named for Their Fathers ###


setwd ("")

library (missForest)
library (lme4)
library (lmerTest)
library (ordinal)
library (car)
library (coefplot)
library (ggpubr)
library (ggplot2)


####################################
### imputation of missing values ###

names_rdy_to_imp <- read.csv ("names_data_matrix.csv", header=TRUE) # we load the data after excluding those participants for which substantial pieces of information was missing
names_rdy_to_imp_red <- subset (names_rdy_to_imp, upbring==1) # create a subset of participants with bi-parental upbringing only (until 15 years of participant's age)
names_rdy_to_imp_red <- names_rdy_to_imp_red[,-4] # drop bi-parental upbringing variable

cols <- c (1:2,4:64)
names_rdy_to_imp_red[cols] <- lapply (names_rdy_to_imp_red[cols], factor) # convert to factors
names_rdy_to_imp_red$age <- as.numeric (names_rdy_to_imp_red$age) # age to numeric

names_imp <- missForest (names_rdy_to_imp_red) # run the imputation function
names_imp_vals <- names_imp$ximp

cols_imp <- c (17:60) 
names_imp_vals[cols_imp] <- lapply (names_imp_vals[cols_imp], as.numeric) # convert s-EMBU variables back to numeric in order to compute row sums

names_imp_vals$rejection.m <- rowSums (names_imp_vals[,17:23]) # rejection - mother
names_imp_vals$rejection.f <- rowSums (names_imp_vals[,24:30]) # rejection - father
names_imp_vals$overprotection.m <- rowSums (names_imp_vals[,31:39]) # overprotection - mother
names_imp_vals$overprotection.f <- rowSums (names_imp_vals[,40:48]) # overprotection - father
names_imp_vals$emotional.m <- rowSums (names_imp_vals[,49:54]) # emotional warmth - mother
names_imp_vals$emotional.f <- rowSums (names_imp_vals[,55:60]) # emotional warmth - father

names_imp_vals <- names_imp_vals[,-c(17:60)] # drop unnecessary columns
names_imp_vals$sex <- ifelse (names_imp_vals$sex==1,1,0) # recode sex to males=1, females=0
names_imp_vals$sex.orient.bin <- as.factor(names_imp_vals$sex.orient) # create an indicator variable for sexual orientation
levels(names_imp_vals$sex.orient.bin) <- list("0"=c("1","2","3"), "1"=c("4","5","6","7"))

str (names_imp_vals) # ok

write.csv (names_imp_vals, file="names_final.csv") # store the dataset with the imputed values

names_m <- subset (names_imp_vals, sex==1) # create a subset of males to calculate mean age and its standard deviation
names_f <- subset (names_imp_vals, sex==0) # do the same for females

mean (names_m$age, na.rm=TRUE)
sd (names_m$age, na.rm=TRUE)
mean (names_f$age, na.rm=TRUE)
sd (names_f$age, na.rm=TRUE)


#######################
### Chi-square test ###

names <- read.csv ("names_final.csv", header=TRUE)

namesakes <- names$names.bin
names.f <- names$names.f
names.m <- names$names.m
sex <- names$sex
first.born <- names$first.born

### ~ sex

chisq.test (namesakes, sex) # general namesakes
chisq.test (names.f, sex) # paternal namesakes
chisq.test (names.m, sex) # maternal namesakes

### ~ primogeniture

chisq.test (namesakes, first.born) # general namesakes
chisq.test (names.f, first.born) # paternal namesakes
chisq.test (names.m, first.born) # maternal namesakes


################
### Figure 1 ###

# some data adjustments first

sex_rev <- ifelse (names$sex==0,2,1)
first.born_rev <- ifelse (names$first.born==0,2,1)

names$i <- rep (1, nrow(names))

names_sex <- c ("male","female")
names_fb <- c ("first born","later born")

names_bin_cols <- c ("gray75","gray25")
names_f_cols <- c ("gray75","#00AFBB")
names_m_cols <- c ("gray75","#E7B800") 

names_g_sex <- ggplot (names, aes(x=factor(sex_rev), fill=factor(names.bin))) + geom_bar(position=position_dodge(-0.9)) + theme_classic() + ggtitle("a)") + labs (x="",y="sex") + scale_x_discrete (labels=names_sex) + theme (legend.position="none", axis.text.x=element_text(size=16), axis.text.y=element_text(size=14), text=element_text(size=16)) + scale_fill_manual (labels=c("no","yes"), values=names_bin_cols) + guides(fill=guide_legend(title="namesakes", reverse=TRUE)) + annotate("segment", x=1, xend=2, y=540, yend=540) + annotate("text", x=1.5, y=545, label="***", size=8)
names_f_sex <- ggplot (names, aes(x=factor(sex_rev), fill=factor(names.f))) + geom_bar(position=position_dodge(-0.9)) + theme_classic() + ggtitle("b)") + labs (x="",y="") + scale_x_discrete (labels=names_sex) + theme (legend.position="none", axis.text.x=element_text(size=16), axis.text.y=element_text(size=14), text=element_text(size=16)) + scale_fill_manual (values=names_f_cols) + annotate("segment", x=1, xend=2, y=580, yend=580) + annotate("text", x=1.5, y=585, label="***", size=8)
names_m_sex <- ggplot (names, aes(x=factor(sex_rev), fill=factor(names.m))) + geom_bar(position=position_dodge(-0.9)) + theme_classic() + ggtitle("c)") + labs (x="",y="") + scale_x_discrete (labels=names_sex) + theme (legend.position="none", axis.text.x=element_text(size=16), axis.text.y=element_text(size=14), text=element_text(size=16)) + scale_fill_manual (values=names_m_cols) + annotate("segment", x=1, xend=2, y=560, yend=560) + annotate("text", x=1.5, y=565, label="*", size=8)

names_g_fb <- ggplot (names, aes(x=factor(first.born_rev), fill=factor(names.bin))) + geom_bar(position=position_dodge(-0.9)) + theme_classic() + ggtitle("d)") + labs (x="",y="primogeniture") + scale_x_discrete (labels=names_fb) + theme (legend.position="none", axis.text.x=element_text(size=16), axis.text.y=element_text(size=14), text=element_text(size=16)) + scale_fill_manual (values=names_bin_cols) + annotate("segment", x=1, xend=2, y=380, yend=380) + annotate("text", x=1.5, y=390, label="ns", size=6)
names_f_fb <- ggplot (names, aes(x=factor(first.born_rev), fill=factor(names.f))) + geom_bar(position=position_dodge(-0.9)) + theme_classic() + ggtitle("e)") + labs (x="",y="") + scale_x_discrete (labels=names_fb) + theme (legend.position="none", axis.text.x=element_text(size=16), axis.text.y=element_text(size=14), text=element_text(size=16)) + scale_fill_manual (values=names_f_cols) + annotate("segment", x=1, xend=2, y=380, yend=380) + annotate("text", x=1.5, y=390, label="ns", size=6)
names_m_fb <- ggplot (names, aes(x=factor(first.born_rev), fill=factor(names.m))) + geom_bar(position=position_dodge(-0.9)) + theme_classic() + ggtitle("f)") + labs (x="",y="") + scale_x_discrete (labels=names_fb) + theme (legend.position="none", axis.text.x=element_text(size=16), axis.text.y=element_text(size=14), text=element_text(size=16)) + scale_fill_manual (values=names_m_cols) + annotate("segment", x=1, xend=2, y=380, yend=380) + annotate("text", x=1.5, y=390, label="ns", size=6)

ggarrange (names_g_sex, 
           names_f_sex, 
           names_m_sex, 
           names_g_fb, 
           names_f_fb, 
           names_m_fb, 
           ncol=3, 
           nrow=2, 
           common.legend=TRUE, 
           legend="bottom")

dev.off()

# note that the plot was further adjusted in a graphical software


################
### LMM/GLMM ###

### overprotection - father

overp_f <- lmer (overprotection.f ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)
vif (lmer (overprotection.f ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)) # remove birth order
vif (lmer (overprotection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names)) # ok

# include interaction between names.f and sex

overp_f_i1 <- lmer (overprotection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + (1|ses15), data=names)
overp_f <- lmer (overprotection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names) # run a model without interaction
anova (overp_f, overp_f_i1) # compute likelihood ratio test; adding interaction does not lead to a significant decrease in log-likelihood (at p<0.05)

# include interaction between names.f and first.born

overp_f_i2 <- lmer (overprotection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*first.born + (1|ses15), data=names)
anova (overp_f, overp_f_i2) # interaction not significant

# include interaction between names.m and sex

overp_f_i3 <- lmer (overprotection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*sex + (1|ses15), data=names)
anova (overp_f, overp_f_i3) # interaction not significant

# include interaction between names.m and first.born

overp_f_i4 <- lmer (overprotection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*first.born + (1|ses15), data=names)
anova (overp_f, overp_f_i4) # interaction not significant

### get parameter estimates of the model without any interaction 

summary (overp_f)
confint (overp_f)
anova (overp_f, ddf="Kenward-Roger") # estimate degrees of freedom and p-values based on the Kenward-Roger method


### rejection - father

rej_f <- glmer (rejection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names, family=Gamma(link="identity"))
vif (glmer (rejection.f ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names, family=Gamma(link="identity"))) # remove birth order
vif (glmer (rejection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names, family=Gamma(link="identity")))

# include interaction between names.f and sex

rej_f_i1 <- glmer (rejection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + (1|ses15), data=names, family=Gamma(link="identity"))
rej_f <- glmer (rejection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names, family=Gamma(link="identity"))
anova (rej_f, rej_f_i1) # interaction not significant

# include interaction between names.f and first.born

rej_f_i2 <- glmer (rejection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*first.born + (1|ses15), data=names, family=Gamma(link="identity"))
anova (rej_f, rej_f_i2) # interaction not significant

# include interaction between names.m and sex

rej_f_i3 <- glmer (rejection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*sex + (1|ses15), data=names, family=Gamma(link="identity"))
anova (rej_f, rej_f_i3) # interaction not significant

# include interaction between names.m and first.born

rej_f_i4 <- glmer (rejection.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*first.born + (1|ses15), data=names, family=Gamma(link="identity"))
anova (rej_f, rej_f_i4) # interaction not significant

### get parameter estimates of the model without any interaction 

summary (rej_f)
confint (rej_f, method="Wald")


### emotional warmth - father

emo_f <- lmer (emotional.f ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)
vif (lmer (emotional.f ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)) # remove birth order
vif (lmer (emotional.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names)) 

# include interaction between names.f and sex

emo_f_i1 <- lmer (emotional.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + (1|ses15), data=names) 
emo_f <- lmer (emotional.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names) 
anova (emo_f, emo_f_i1) # interaction not significant

# include interaction between names.f and first.born

emo_f_i2 <- lmer (emotional.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*first.born + (1|ses15), data=names) 
anova (emo_f, emo_f_i2) # interaction not significant
 
# include interaction between names.m and sex

emo_f_i3 <- lmer (emotional.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*sex + (1|ses15), data=names) 
anova (emo_f, emo_f_i3) # interaction not significant

# include interaction between names.m and first.born

emo_f_i4 <- lmer (emotional.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*first.born + (1|ses15), data=names) 
anova (emo_f, emo_f_i4) # interaction not significant

### get parameter estimates of the model without any interaction 

summary (emo_f)
confint (emo_f)
anova (emo_f, ddf="Kenward-Roger")


### overprotection - mother

overp_m <- lmer (overprotection.m ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)
vif (lmer (overprotection.m ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)) # remove birth order
vif (lmer (overprotection.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names)) 

# include interaction between names.f and sex

overp_m_i1 <- lmer (overprotection.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + (1|ses15), data=names)
overp_m <- lmer (overprotection.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names)
anova (overp_m, overp_m_i1) # interaction not significant

# include interaction between names.f and first.born

overp_m_i2 <- lmer (overprotection.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*first.born + (1|ses15), data=names)
anova (overp_m, overp_m_i2) # interaction not significant

# include interaction between names.m and sex

overp_m_i3 <- lmer (overprotection.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*sex + (1|ses15), data=names)
anova (overp_m, overp_m_i3) # interaction not significant

# include interaction between names.m and first.born

overp_m_i4 <- lmer (overprotection.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*first.born + (1|ses15), data=names)
anova (overp_m, overp_m_i4) # interaction not significant

### get parameter estimates of the model without any interaction 

summary (overp_m)
confint (overp_m)
anova (overp_m, ddf="Kenward-Roger")


### rejection - mother

rej_m <- glmer (rejection.m ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names, family=Gamma(link="identity"))
vif (glmer (rejection.m ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names, family=Gamma(link="identity"))) # remove birth order
vif (glmer (rejection.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names, family=Gamma(link="identity")))

# include interaction between names.f and sex

rej_m_i1 <- glmer (rejection.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + (1|ses15), data=names, family=Gamma(link="identity"))
rej_m <- glmer (rejection.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names, family=Gamma(link="identity"))
anova (rej_m, rej_m_i1) # interaction significant

# include interaction between names.f and first.born

rej_m_i2 <- glmer (rejection.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*first.born + (1|ses15), data=names, family=Gamma(link="identity"))
anova (rej_m, rej_m_i2) # interaction not significant

# include interaction between names.m and sex

rej_m_i3 <- glmer (rejection.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*sex + (1|ses15), data=names, family=Gamma(link="identity"))
anova (rej_m, rej_m_i3) # interaction not significant

# include interaction between names.m and first.born

rej_m_i4 <- glmer (rejection.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*first.born + (1|ses15), data=names, family=Gamma(link="identity"))
anova (rej_m, rej_m_i4) # interaction not significant

### get parameter estimates of the model with interaction between paternal namesakes and sex

summary (rej_m_i1)
confint (rej_m_i1)


### emotional warmth - mother

emo_m <- lmer (emotional.m ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)
vif (lmer (emotional.f ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)) # remove birth order
vif (lmer (emotional.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names)) 

# include interaction between names.f and sex

emo_m_i1 <- lmer (emotional.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + (1|ses15), data=names)
emo_m <- lmer (emotional.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names)
anova (emo_m, emo_m_i1) # interaction not significant

# include interaction between names.f and first.born

emo_m_i2 <- lmer (emotional.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*first.born + (1|ses15), data=names)
anova (emo_m, emo_m_i2) # interaction not significant

# include interaction between names.m and sex

emo_m_i3 <- lmer (emotional.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*sex + (1|ses15), data=names)
anova (emo_m, emo_m_i3) # interaction significant

# include interaction between names.m and first.born

emo_m_i4 <- lmer (emotional.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*first.born + (1|ses15), data=names)
anova (emo_m, emo_m_i4) # interaction not significant

### get parameter estimates of the model with interaction between maternal namesakes and sex

summary (emo_m_i3)
confint (emo_m_i3)
anova (emo_m_i3, ddf="Kenward-Roger")


############
### CLMM ###

### relationship quality - father

rel_f <- clmm (factor(rel.f) ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)
vif (lm (rel.f ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin, data=names)) # remove birth order
vif (lm (rel.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin, data=names))

# include interaction between names.f and sex

rel_f_i1 <- clmm (factor(rel.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + (1|ses15), data=names)
rel_f <- clmm (factor(rel.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names)
anova (rel_f, rel_f_i1) # interaction significant

# include interaction between names.f and first.born

rel_f_i2 <- clmm (factor(rel.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*first.born + (1|ses15), data=names)
anova (rel_f, rel_f_i2) # interaction significant

# include interaction between names.m and sex

rel_f_i3 <- clmm (factor(rel.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*sex + (1|ses15), data=names)
anova (rel_f, rel_f_i3) # interaction not significant

# include interaction between names.m and first.born

rel_f_i4 <- clmm (factor(rel.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*first.born + (1|ses15), data=names)
anova (rel_f, rel_f_i4) # interaction not significant

### get parameter estimates of the model with both interactions

rel_f_i5 <- clmm (factor(rel.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + names.f*first.born + (1|ses15), data=names)
summary (rel_f_i5)
confint (rel_f_i5)


### money - father

money_f <- clmm (factor(money.f) ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)
vif (lm (money.f ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin, data=names)) # remove birth order
vif (lm (money.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin, data=names))

# include interaction between names.f and sex

money_f_i1 <- clmm (factor(money.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + (1|ses15), data=names)
money_f <- clmm (factor(money.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names)
anova (money_f, money_f_i1) # interaction not significant

# include interaction between names.f and first.born

money_f_i2 <- clmm (factor(money.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*first.born + (1|ses15), data=names)
anova (money_f, money_f_i2) # interaction not significant

# include interaction between names.m and sex

money_f_i3 <- clmm (factor(money.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*sex + (1|ses15), data=names)
anova (money_f, money_f_i3) # interaction not significant

# include interaction between names.m and first.born

money_f_i4 <- clmm (factor(money.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*first.born + (1|ses15), data=names)
anova (money_f, money_f_i4) # interaction not significant

### get parameter estimates of the model without interactions

summary (money_f)
confint (money_f)


### time - father

time_f <- clmm (factor(time.f) ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)
vif (lm (time.f ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin, data=names)) # remove birth order
vif (lm (time.f ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin, data=names))

# include interaction between names.f and sex

time_f_i1 <- clmm (factor(time.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + (1|ses15), data=names)
time_f <- clmm (factor(time.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names)
anova (time_f, time_f_i1) # interaction not significant

# include interaction between names.f and first.born

time_f_i2 <- clmm (factor(time.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*first.born + (1|ses15), data=names)
anova (time_f, time_f_i2) # interaction not significant

# include interaction between names.m and sex

time_f_i3 <- clmm (factor(time.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*sex + (1|ses15), data=names)
anova (time_f, time_f_i3) # interaction not significant

# include interaction between names.m and first.born

time_f_i4 <- clmm (factor(time.f) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*first.born + (1|ses15), data=names)
anova (time_f, time_f_i4) # interaction not significant

### get parameter estimates of the model without interactions

summary (time_f)
confint (time_f)


### relationship quality - mother

rel_m <- clmm (factor(rel.m) ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)
vif (lm (rel.m ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin, data=names)) # remove birth order
vif (lm (rel.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin, data=names))

# include interaction between names.f and sex

rel_m_i1 <- clmm (factor(rel.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + (1|ses15), data=names)
rel_m <- clmm (factor(rel.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names)
anova (rel_m, rel_m_i1) # interaction is significant

# include interaction between names.f and first.born

rel_m_i2 <- clmm (factor(rel.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*first.born + (1|ses15), data=names)
anova (rel_m, rel_m_i2) # interaction is not significant

# include interaction between names.m and sex

rel_m_i3 <- clmm (factor(rel.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*sex + (1|ses15), data=names)
anova (rel_m, rel_m_i3) # interaction is not significant

# include interaction between names.m and first.born

rel_m_i4 <- clmm (factor(rel.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*first.born + (1|ses15), data=names)
anova (rel_m, rel_m_i4) # interaction is significant

### get parameter estimates of the model with both interactions

rel_m_i5 <- clmm (factor(rel.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + names.m*first.born + (1|ses15), data=names)
summary (rel_m_i5)
confint (rel_m_i5)


### money - mother

money_m <- clmm (factor(money.m) ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)
vif (lm (money.m ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin, data=names))
vif (lm (money.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin, data=names))

# include interaction between names.f and sex

money_m_i1 <- clmm (factor(money.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + (1|ses15), data=names)
money_m <- clmm (factor(money.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names)
anova (money_m, money_m_i1) # interaction is not significant

# include interaction between names.f and first.born

money_m_i2 <- clmm (factor(money.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*first.born + (1|ses15), data=names)
anova (money_m, money_m_i2) # interaction is not significant

# include interaction between names.m and sex

money_m_i3 <- clmm (factor(money.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*sex + (1|ses15), data=names)
anova (money_m, money_m_i3) # interaction is not significant

# include interaction between names.m and first.born

money_m_i4 <- clmm (factor(money.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*first.born + (1|ses15), data=names)
anova (money_m, money_m_i4) # interaction is not significant

### get parameter estimates of the model without interactions

summary (money_m)
confint (money_m)


### time- mother

time_m <- clmm (factor(time.m) ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin + (1|ses15), data=names)
vif (lm (time.m ~ names.f + names.m + sex + first.born + birth.order + no.children + sex.orient.bin, data=names))
vif (lm (time.m ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin, data=names))

# include interaction between names.f and sex

time_m_i1 <- clmm (factor(time.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*sex + (1|ses15), data=names)
time_m <- clmm (factor(time.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + (1|ses15), data=names)
anova (time_m, time_m_i1) # interaction is not significant

# include interaction between names.f and first.born

time_m_i2 <- clmm (factor(time.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.f*first.born + (1|ses15), data=names)
anova (time_m, time_m_i2) # interaction is not significant

# include interaction between names.m and sex

time_m_i3 <- clmm (factor(time.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*sex + (1|ses15), data=names)
anova (time_m, time_m_i3) # interaction is not significant

# include interaction between names.m and first.born

time_m_i4 <- clmm (factor(time.m) ~ names.f + names.m + sex + first.born + no.children + sex.orient.bin + names.m*first.born + (1|ses15), data=names)
anova (time_m, time_m_i4) # interaction is not significant

### get parameter estimates of the model without interactions

summary (time_m)
confint (time_m)


################
### Figure 2 ###

o <- multiplot (overp_m, overp_f, scales="free_x", innerCI=0, title="a)", ylab="", xlab="", dodgeHeight=-0.5, coefficients=c("names.f", "names.m","sex", "first.born","no.children","sex.orient.bin"), numberAngle=0, shape=c(16,17), zeroLWD=0.5) + scale_y_discrete(limits=c("sex.orient.bin", "no.children", "first.born", "sex", "names.m", "names.f"), labels=c("sexual \norientation", "number of\nchildren", "first-\nborns", "sex", "maternal \nnamesakes", "paternal \nnamesakes")) + theme_classic(base_size=10) + theme(plot.title=element_text(size=16, face="bold"), axis.text.y=element_text(size=16), axis.text.x=element_text(size=16)) + scale_colour_manual(values=c("#00AFBB","#E7B800"), labels=c("father","mother")) + theme(legend.title=element_blank(), legend.text=element_text(size=16)) + theme(legend.position="none") + guides(shape=guide_legend(override.aes=list(c(16, 17)))) 

rj <- multiplot (rej_m_i1, rej_f, scales="free_x", innerCI=0, title="b)", ylab="", xlab="", dodgeHeight=-0.5, coefficients=c("names.f", "names.m","sex", "first.born","no.children","sex.orient.bin", "names.f:sex"), numberAngle=0, shape=c(16,17), zeroLWD=0.5) + scale_y_discrete(limits=c("names.f:sex","sex.orient.bin", "no.children", "first.born", "sex", "names.m", "names.f"), labels=c("p. namesakes:\nsex","sexual \norientation", "number of\nchildren", "first-\nborns", "sex", "maternal \nnamesakes", "paternal \nnamesakes")) + theme_classic(base_size=10) + theme(plot.title=element_text(size=16, face="bold"), axis.text.y=element_text(size=16), axis.text.x=element_text(size=16)) + scale_colour_manual(values=c("#00AFBB","#E7B800"), labels=c("father","mother")) + theme(legend.title=element_blank(), legend.text=element_text(size=16)) + theme(legend.position="none") + guides(shape=guide_legend(override.aes=list(c(16, 17)))) 

e <- multiplot (emo_m_i3, emo_f, scales="free_x", innerCI=0, title="c)", ylab="", xlab="", dodgeHeight=-0.5, coefficients=c("names.f", "names.m","sex", "first.born","no.children","sex.orient.bin", "names.m:sex"), numberAngle=0, shape=c(16,17), zeroLWD=0.5) + scale_y_discrete(limits=c("names.m:sex","sex.orient.bin", "no.children", "first.born", "sex", "names.m", "names.f"), labels=c("m. namesakes:\nsex","sexual \norientation", "number of\nchildren", "first-\nborns", "sex", "maternal \nnamesakes", "paternal \nnamesakes")) + theme_classic(base_size=10) + theme(plot.title=element_text(size=16, face="bold"), axis.text.y=element_text(size=16), axis.text.x=element_text(size=16)) + scale_colour_manual(values=c("#00AFBB","#E7B800"), labels=c("father","mother")) + theme(legend.title=element_blank(), legend.text=element_text(size=16)) + theme(legend.position="none") + guides(shape=guide_legend(override.aes=list(c(16, 17)))) 

rl <- multiplot (rel_m_i5, rel_f_i5, scales="free_x", innerCI=0, title="d)", ylab="", xlab="", dodgeHeight=-0.5, coefficients=c("names.f", "names.m","sex", "first.born","no.children","sex.orient.bin", "names.f:sex","names.f:first.born", "names.m:first.born"), numberAngle=0, shape=c(16,17), zeroLWD=0.5) + scale_y_discrete(limits=c("names.m:first.born","names.f:first.born", "names.f:sex","sex.orient.bin", "no.children", "first.born", "sex", "names.m", "names.f"), labels=c("m. namesakes:\nfirst-borns","p. namesakes:\nfirst-borns","p. namesakes:\nsex","sexual \norientation", "number of\nchildren", "first-\nborns", "sex", "maternal \nnamesakes", "paternal \nnamesakes")) + theme_classic(base_size=10) + theme(plot.title=element_text(size=16, face="bold"), axis.text.y=element_text(size=16), axis.text.x=element_text(size=16)) + scale_colour_manual(values=c("#00AFBB","#E7B800"), labels=c("father","mother")) + theme(legend.title=element_blank(), legend.text=element_text(size=12)) + theme(legend.position="none") + guides(shape=guide_legend(override.aes=list(c(16, 17)))) 

m <- multiplot (money_m, money_f, scales="free_x", innerCI=0, title="e)", ylab="", xlab="", dodgeHeight=-0.5, coefficients=c("names.f", "names.m","sex", "first.born","no.children","sex.orient.bin"), numberAngle=0, shape=c(16,17), zeroLWD=0.5) + scale_y_discrete(limits=c("sex.orient.bin", "no.children", "first.born", "sex", "names.m", "names.f"), labels=c("sexual \norientation", "number of\nchildren", "first-\nborns", "sex", "maternal \nnamesakes", "paternal \nnamesakes")) + theme_classic(base_size=10) + theme(plot.title=element_text(size=16, face="bold"), axis.text.y=element_text(size=16), axis.text.x=element_text(size=16)) + scale_colour_manual(values=c("#00AFBB","#E7B800"), labels=c("father","mother")) + theme(legend.title=element_blank(), legend.text=element_text(size=16)) + theme(legend.position="none") + guides(shape=guide_legend(override.aes=list(c(16, 17)))) 

t <- multiplot (time_m, time_f, scales="free_x", innerCI=0, title="f)", ylab="", xlab="", dodgeHeight=-0.5, coefficients=c("names.f", "names.m","sex", "first.born","no.children","sex.orient.bin"), numberAngle=0, shape=c(16,17), zeroLWD=0.5) + scale_y_discrete(limits=c("sex.orient.bin", "no.children", "first.born", "sex", "names.m", "names.f"), labels=c("sexual \norientation", "number of\nchildren", "first-\nborns", "sex", "maternal \nnamesakes", "paternal \nnamesakes")) + theme_classic(base_size=10) + theme(plot.title=element_text(size=16, face="bold"), axis.text.y=element_text(size=16), axis.text.x=element_text(size=16)) + scale_colour_manual(values=c("#00AFBB","#E7B800"), labels=c("father","mother")) + theme(legend.title=element_blank(), legend.text=element_text(size=16)) + theme(legend.position="none") + guides(shape=guide_legend(override.aes=list(c(16, 17)))) 

ggarrange(o,rj,e,rl,m,t, 
          ncol=3, 
          nrow=2, 
          legend="bottom",  
          common.legend=TRUE)

dev.off()

# note that the plot was further adjusted in a graphical software


##############################
### Supplementary Figure 1 ###

par(mfrow=c(3,2), mar = c(5.5, 6.5, 5, 3.5))

rejection_m <- interaction.plot (names$names.f, names$sex, names$rejection.m, xlab=c("paternal namesakes"), ylab="rejection - mother", col=c("black","red"), trace.label="sex", pch=c(17,15), type="b", legend=FALSE, cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, axes=TRUE, xaxt="n", cex=2)
title("a)", adj = 0, cex.main=2)
axis (1, labels=c("no", "yes"), at=1:2, tick=TRUE, cex.axis=2)
legend ("topleft", legend=c("male", "female"), col=c("red", "black"), pt.cex=1, cex=2, lty=1:2, lwd=2, title="sex", bty="n")

emotional_m <- interaction.plot (names$names.m, names$sex, names$emotional.m, xlab=c("maternal namesakes"), ylab="emotional warmth - mother", col=c("black","red"), trace.label="sex", pch=c(17,15), type="b", legend=FALSE, cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, axes=TRUE, xaxt="n", ylim=c(12,22), cex=2)
title("b)", adj = 0, cex.main=2)
axis (1, labels=c("no", "yes"), at=1:2, tick=TRUE, cex.axis=2)
legend ("topleft", legend=c("male", "female"), col=c("red", "black"), pt.cex=1, cex=2, lty=1:2, lwd=2, title="sex", bty="n")

relationship_f <- interaction.plot (names$names.f, names$sex, names$rel.f, xlab=c("paternal namesakes"), ylab="relationship quality - father", col=c("black","red"), trace.label="sex", pch=c(17,15), type="b", legend=FALSE, cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, axes=TRUE, xaxt="n", cex=2)
title("c)", adj = 0, cex.main=2)
axis (1, labels=c("no", "yes"), at=1:2, tick=TRUE, cex.axis=2)
legend ("topleft", legend=c("male", "female"), col=c("red", "black"), pt.cex=1, cex=2, lty=1:2, lwd=2, title="sex", bty="n")

relationship_f2 <- interaction.plot (names$names.f, names$first.born, names$rel.f, xlab=c("paternal namesakes"), ylab="relationship quality - father", col=c("black","red"), trace.label="first-born", pch=c(17,15), type="b", legend=FALSE, cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, axes=TRUE, xaxt="n", cex=2)
title("d)", adj = 0, cex.main=2)
axis (1, labels=c("no", "yes"), at=1:2, tick=TRUE, cex.axis=2)
legend ("topleft", legend=c("yes", "no"), col=c("red", "black"), pt.cex=1, cex=2, lty=1:2, lwd=2, title="first-born", bty="n")

relationship_m <- interaction.plot (names$names.f, names$sex, names$rel.m, xlab=c("paternal namesakes"), ylab="relationship quality - mother", col=c("black","red"), trace.label="sex", pch=c(17,15), type="b", legend=FALSE, cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, axes=TRUE, xaxt="n", cex=2)
title("e)", adj = 0, cex.main=2)
axis (1, labels=c("no", "yes"), at=1:2, tick=TRUE, cex.axis=2)
legend ("topleft", legend=c("male", "female"), col=c("red", "black"), pt.cex=1, cex=2, lty=1:2, lwd=2, title="sex", bty="n")

relationship_m2 <- interaction.plot (names$names.m, names$first.born, names$rel.m, xlab=c("maternal namesakes"), ylab="relationship quality - mother", col=c("black","red"), trace.label="first-born", pch=c(17,15), type="b", legend=FALSE, cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2, axes=TRUE, xaxt="n", cex=2)
title("f)", adj = 0, cex.main=2)
axis (1, labels=c("no", "yes"), at=1:2, tick=TRUE, cex.axis=2)
legend ("topleft", legend=c("yes", "no"), col=c("red", "black"), pt.cex=1, cex=2, lty=1:2, lwd=2, title="first-born", bty="n")

dev.off()

# note that the plot was further adjusted in a graphical software


############################################
############################################
