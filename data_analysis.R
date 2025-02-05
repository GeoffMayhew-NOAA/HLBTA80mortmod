#' *data analysis*

#' @SEE GitLab repos, I have some analysis in there that may be useful. I don't think I had the decksort simulation though.

library(data.table)
library(ordinal)  # for clm(), cumulative link models and clmm(), the mixed model version
library(ggplot2)
library(FMAtools)

# Load the dataset
gdrive_download("data/hlbt_dat.rdata", gdrive_set_dribble("Analysts/Geoff/HLBTA80mortmod/data"))
(load("data/hlbt_dat.rdata"))
source("functions.R")



# Initial model building ----

# Additive model building - grab the models with the lowest AIC.

#' TODO *Test assumption of proportional odds*
#' 
# What do the models looks like on the full dataset?


clm.0 <- clm(VIABILITY ~ 1, data = hlbt_dat)
clm.1.a <- clm(VIABILITY ~ ASSESSMENT_TIME, data = hlbt_dat) # link = 'logit' by default, aka proportional odds

clm.1.a.loglog <- clm(VIABILITY ~ ASSESSMENT_TIME, data = hlbt_dat, link = 'cloglog')
clm.1.a.loggamma <- clm(VIABILITY ~ ASSESSMENT_TIME, data = hlbt_dat, link = 'log-gamma') # fails to converge?
AIC(clm.1.a, clm.1.a.loglog, clm.1.a.loggamma)  # regular logit looks best

# Testing Proportional Odds Assumption ----
library(brant)
brant(clm.1.a)  # doesn't work with ordinal package? only with polr? A value < 0.5 means failure

# goodness of fit0

#  lipsitz test 
# library(generalhoslem)  # overwrites melt
generalhoslem::lipsitz.test(clm.1.a)   #' *p-value < 2.2e-16, so I have problems?*

#Pulkstenis-Robinson chi-squared test - only needed if you have categorical independent variables?
generalhoslem::pulkrob.chisq(clm.1.a, catvars = "VIABILITY")  


clm.1.b <- clm(VIABILITY ~ HAUL_MT, data = hlbt_dat)
clm.1.c <- clm(VIABILITY ~ TOW_DUR, data = hlbt_dat)
clm.1.d <- clm(VIABILITY ~ WEIGHT_KG, data = hlbt_dat)
anova(clm.0, clm.1.a, clm.1.b, clm.1.c, clm.1.d)
#' *ASSESSMENT_TIME, clm.1.a*  10977

clm.2.a <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT, data = hlbt_dat)
clm.2.b <- clm(VIABILITY ~ ASSESSMENT_TIME + TOW_DUR, data = hlbt_dat)
clm.2.c <- clm(VIABILITY ~ ASSESSMENT_TIME + WEIGHT_KG, data = hlbt_dat)
anova(clm.1.a, clm.2.a, clm.2.b, clm.2.c)
#' *HAUL_MT, clm.2.a*      2333

clm.3.a <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR, data = hlbt_dat)
clm.3.b <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG, data = hlbt_dat)
anova(clm.2.a, clm.3.a, clm.3.b)
#' *TOW_DUR, clm.3.a*     1622

clm.4.a <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat)
anova(clm.3.a, clm.4.a)
#' *WEIGHT_KG, clm.4.a*   666

#' *It appears that all variables improve AIC*, but we should really train/test hauls and see which models have the 
#' best predictive ability

#======================================================================================================================#

# HAUL TARGET GROUPINGS ----
unique(hlbt_dat$TRIP_TARGET) 
# We have a ton of trip targets. Which are more similar to eachother? Would be better if we could reduce the number of target groups
hlbt_dat.haul <- unique(hlbt_dat[, .(CRUISE.PERMIT.HAUL, PERMIT, FISHING_DEPTH, BOTTOM_DEPTH, HAUL_MT, LAT_D, LON_D, HAUL_TARGET, TOW_DUR)])
hlbt_dat.haul

# Principle Component Analysis ----
prcomp.target <- prcomp(~FISHING_DEPTH + BOTTOM_DEPTH + HAUL_MT + LAT_D + LON_D + TOW_DUR, data = hlbt_dat.haul)
prcomp.target

# Cluster Analysis ----
#' [https://www.r-bloggers.com/2021/04/cluster-analysis-in-r/]

# normalize data
z <- hlbt_dat.haul[,-c("CRUISE.PERMIT.HAUL", "HAUL_TARGET")]
means <- apply(z, 2, mean)
sds <- apply(z, 2, sd)
nor <- scale(z, center = means, scale = sds)

hclust(nor)




library(factoextra) 
k2 <- kmeans(nor, centers = 4, nstart = 250)  # sometimes, but doesn't always make a separate island group
fviz_cluster(k2, data = nor, geom = "point") + 
  geom_text(data = hlbt_dat.haul, aes(label = HAUL_TARGET))


a <- fviz_cluster(k2, data = nor, geom = "point")
a1 <- as.data.table(copy(a$data))
a1[, HAUL_TARGET := hlbt_dat.haul$HAUL_TARGET]


a1[, .N, by = .(HAUL_TARGET)][order(-N)]

ggplot(a1, aes(x = x, y = y)) + facet_wrap(~HAUL_TARGET) + 
  geom_point(aes(color = HAUL_TARGET)) + coord_cartesian(xlim = c(-7.5, 3), ylim = c(-5, 5))


# Right side: Y, R, E, P, c?
# Intermediate: B, W, L, K, F, O
# Center: A
# Left side: S, T, M

a1[, TARGET_GROUP := fcase(
  HAUL_TARGET %in% c("A", "Y", "R", "E", "P", "C"), "TG1",
  HAUL_TARGET %in% c("B", "W", "L", "K", "F", "O"), "TG2",
  HAUL_TARGET %in% c("S", "T", "M"), "TG3"
)]

ggplot(a1, aes(x = x, y = y)) + 
  geom_point(aes(color = TARGET_GROUP)) 





ggplot(a1, aes(x = x, y = y)) + 
  geom_point(aes(color = HAUL_TARGET)) + coord_cartesian(xlim = c(-7.5, 3), ylim = c(-5, 5))

ggplot(a1, aes(x = x, y = y)) + 
  geom_text(aes(color = HAUL_TARGET, label = HAUL_TARGET)) + coord_cartesian(xlim = c(-7.5, 3), ylim = c(-5, 5))
# ATKA is grouped, M+T+W+B+K, C+Y+R, L? 


# there's like 5 or 6 hauls that look funny
ggplot(a1, aes(x = x, y = y)) + 
  ggrepel::geom_text_repel(aes(color = HAUL_TARGET, label = name))  + xlim(5, 10) 
stats::prcomp(nor, scale = F, center = F)
hlbt_dat.haul[c(c(8035, 8038, 3981, 3980, 3979, 8034), c(runif(6, 1, .N)))]
# thoese hauls are pretty massive, but still, 75% percentile?
hlbt_dat.haul[, quantile(HAUL_MT, c(0.025, 0.25, 0.5, 0.75, 0.975))]
hlbt_dat.haul[HAUL_MT >= 25]


ggplot(a1[HAUL_TARGET %in% c("R", "Y")], aes(x = x, y = y)) + 
  geom_point(aes(color = HAUL_TARGET), alpha = 0.2) + coord_cartesian(xlim = c(-7.5, 3), ylim = c(-5, 5))

unique(a1$HAUL_TARGET)
table(a1$HAUL_TARGET)
# Y and R are he biggest targets by far and are very much overlapping
ggplot(a1[HAUL_TARGET %in% c("R", "Y", "E")], aes(x = x, y = y)) + 
  geom_point(aes(color = HAUL_TARGET), alpha = 0.2) + coord_cartesian(xlim = c(-7.5, 3), ylim = c(-5, 5))

# L has some overlap but also can differ
ggplot(a1[HAUL_TARGET %in% c("R", "Y", "E", "L")], aes(x = x, y = y)) + facet_grid(~HAUL_TARGET) + 
  geom_point(aes(color = HAUL_TARGET)) + coord_cartesian(xlim = c(-7.5, 3), ylim = c(-5, 5))


ggplot(a1, aes(x = x, y = y)) + facet_wrap(~HAUL_TARGET) + 
  geom_point(aes(color = HAUL_TARGET)) + coord_cartesian(xlim = c(-7.5, 3), ylim = c(-5, 5))


#======================================================================================================================#

hlbt_dat[, TARGET_GROUP := fcase(
  HAUL_TARGET %in% c("A", "Y", "R", "E", "P", "C"), "TG1",
  HAUL_TARGET %in% c("B", "W", "L", "K", "F", "O"), "TG2",
  HAUL_TARGET %in% c("S", "T", "M"), "TG3"
)]

hlbt_dat[, HAUL_TARGET_F := as.factor(HAUL_TARGET)]

hlbt_dat[, TARGET_GROUP_O := factor(TARGET_GROUP, levels = c("TG1", "TG2", "TG3"), ordered = T)]
clm.5.a <-  clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + FISHING_DEPTH, data = hlbt_dat)
clm.5.b <-  clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + TARGET_GROUP, data = hlbt_dat)
clm.5.c <-  clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + TARGET_GROUP_O, data = hlbt_dat)  #Ordering didn't do anything
clm.5.d <-  clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + HAUL_TARGET, data = hlbt_dat) #' *WARNING*
clm.5.e <-  clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + HAUL_TARGET_F, data = hlbt_dat) #' *WARNING*

anova(clm.4.a, clm.5.a, clm.5.b, clm.5.c, clm.5.d, clm.5.e)  # Factor didn't make a difference for haul target... hmm.
# TARGET GROUP IS ACTUALLY MORE USEFUL THAN FISHING DEPTH (and may contain that information)
# HAUL_TARGET Lowers AIC by 1018 pts but I don't think it worked correctly

?clm()
#' TODO When looking at CLM,s what threshold should I be using?
#' flexiblke, symmmetric, symmetric2, or equidistant?
#' For the link, I'm using logit. What about probit? cloglog, etc?
#' We assume that the mortality rates are 0.2, 0.55, and 0.9, which are summetrical around 0.5

# no difference using symmetric
clm.4.a.s <-  clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat, threshold = "symmetric")
anova(clm.4.a, clm.4.a.s)

# Using symmetric2 made AIC worsen
clm.4.a.s <-  clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat, threshold = "symmetric2")
anova(clm.4.a, clm.4.a.s)

# equidistant is also the same
clm.4.a.s <-  clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat, threshold = "equidistant")
anova(clm.4.a, clm.4.a.s)

clm.4.a.s <-  clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat, link = "log-gamma")
anova(clm.4.a, clm.4.a.s)

library(MASS) ## using polr from MASS package, proportional odds logistic regression

clm.4.a.p <- polr(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG , data = hlbt_dat)
clm.4.a.p 
AIC(clm.4.a, clm.4.a.p)  # these are the same.


# TESTING PROPORTIONAL ODDS ASSUMPTION
coefficients <- summary(clm.4.a.p)$coefficients
# calculate p-values
p_value <- (1 - pnorm(abs(coefficients[ ,"t value"]), 0, 1))*2
# bind back to coefficients
(coefficients <- cbind(coefficients, p_value))
# calculate odds ratios
odds_ratio <- exp(coefficients[ ,"Value"])
# combine with coefficient and p_value
(coefficients <- cbind(
  coefficients[ ,c("Value", "p_value")],
  odds_ratio
))
# all of these p-values are 0, which is a good thing.
# We can interpret this as for each minute of ASSESSMENT_TIME, there is a 7.1% (1.0709) higher odds of increasing viability category
# Likewise, for each 1kg of weight, a 4.8% higher odds of a lower viablity category


#' @TODO Should preobably rescale all inputs... we lose the ability to interpret the coefficients but it would probably run a lot faster.

# Mixed Models ----

#' *THIS TAKES FOREVER - maybe rescale?*
if(F) system.time(clm.4.a.m <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|CRUISE.PERMIT.HAUL), data = hlbt_dat))
anova(clm.4.a, clm.4.a.m)

nrow(hlbt_dat) # 106,563 halibut
uniqueN(hlbt_dat$CRUISE.PERMIT.HAUL)  # 13,462 hauls, or 106563/13462 ~ 7.916 halibut per haul

# START OVER HERE ----

# Scale the data, set random variables as to factors

#'\TODO  *If I trim to 35 minutes, I can't be sure if total halibut assessed is correct! CHECK THIS!*

hlbt_dat |>
  _[, TRIP_ID := as.factor(.GRP), by = .(CRUISE, PERMIT, TRIP_SEQ)
  ][, HAUL_ID := as.factor(.GRP), by = .(CRUISE, PERMIT, HAUL_SEQ)]

hlbt_dat.scale <- hlbt_dat[, .(VIABILITY, ASSESSMENT_TIME, SORT_DUR, PRESORTED_NUMBER, LAST_HAL, HAUL_MT, TOW_DUR, FISHING_DEPTH, WEIGHT_KG, TMP_2M, PERMIT, TRIP_SEQ, OBS_ID, TRIP_ID, HAUL_ID)]
hlbt_dat.scale[, ':=' (PERMIT = as.factor(PERMIT), OBS_ID = as.factor(OBS_ID))]
#' Use SORT_DUR + 1 or LAST_HAL to determine the duration of each haul's sorting operation
hlbt_dat.scale[, SORT_END := pmax(SORT_DUR + 1, LAST_HAL)]
# Only 2,080 our of 106,563 halibut, or 1.95%were assessed after 35 minutes. We should truncate our dataset to 35 minutes
hlbt_dat.scale[SORT_END > 35, SORT_END := 35]
hlbt_dat.scale <- hlbt_dat.scale[ASSESSMENT_TIME <= 35 ]

# Scale the numeric variables so models converge more easily
#' TODO scale the values separately - individual vs haul-level metrics!
hlbt_dat.scale.haul <- unique(hlbt_dat.scale[, .(HAUL_ID, HAUL_MT, TOW_DUR, FISHING_DEPTH, TMP_2M)])
hlbt_dat.scale.haul[, c("HAUL_MT.s", "TOW_DUR.s", "FISHING_DEPTH.s", "TMP_2M.s") := lapply(.SD, scale), .SDcols = c("HAUL_MT", "TOW_DUR", "FISHING_DEPTH", "TMP_2M")]
hlbt_dat.scale[, c("HAUL_MT.s", "TOW_DUR.s", "TMP_2M.s") := hlbt_dat.scale.haul[hlbt_dat.scale, .(HAUL_MT.s, TOW_DUR.s, TMP_2M.s), on = .(HAUL_ID)]]
rm(hlbt_dat.scale.haul)
hlbt_dat.scale[, c("ASSESSMENT_TIME.s", "WEIGHT_KG.s") := lapply(.SD, scale), .SDcols = c("ASSESSMENT_TIME", "WEIGHT_KG")]


# Basic Model

#' WTF Now I get errors?  *unable to interpret 'formula', 'scale' or 'nominal'*
m1 <- clm(VIABILITY ~ ASSESSMENT_TIME.s, data = hlbt_dat.scale)

# Testing assumption or proportinoal odds ----

clm.brant <- function(model) {
  library(brant)
  model$zeta <- model$alpha
  model$coefficients <- model$beta
  brant(model) 
}

clm.brant(m1) 

# or use nominal.test
nominal_test


clm.brant(clm(VIABILITY ~ ASSESSMENT_TIME.s + HAUL_MT, data = hlbt_dat.scale)) 

#' *Haul as random effect - takes 56 min*
if(F) system.time(clmm.haul <- clmm(VIABILITY ~ ASSESSMENT_TIME.s + HAUL_MT.s + TOW_DUR.s + WEIGHT_KG.s + (1|HAUL_ID), data = hlbt_dat.scale))
#' does this go through the function?
clm.brant(clmm.haul) # Nope

system.time(clm.haul <- clm(VIABILITY ~ ASSESSMENT_TIME.s + HAUL_MT.s + TOW_DUR.s + WEIGHT_KG.s, data = hlbt_dat.scale))
nominal_test(clm.haul)  # p is < 0 for all meaning all of them fail?
clm.brant(clm.haul)     # all of these fail too

# What if I do probit?
system.time(clm.toow.probit <- clm(VIABILITY ~ ASSESSMENT_TIME.s, data = hlbt_dat.scale, link = "probit"))
nominal_test(clm.toow.probit)  
clm.brant(clm.toow.probit)  

# Try using rmsb package ----
library(rmsb)
?rmsb::blrm()
if(F) toow.rmsb <- rmsb::blrm(VIABILITY ~ ASSESSMENT_TIME.s, data = hlbt_dat.scale)  # this was started 1:58 pm, fnished some time before 3:00pm
toow.rmsb  # time says 3242s, or 54 min
m1 # the coeffecients are very similar (just negative?)
# Mode Beta Mean Beta Median Beta S.E.   Lower   Upper   Pr(Beta>0) Symmetry
# y>=P              -0.3206   -0.3207   -0.3207     0.0067 -0.3334 -0.3078 0.0000     1.01    
# y>=D              -1.5236   -1.5237   -1.5235     0.0082 -1.5403 -1.5085 0.0000     0.97    
# ASSESSMENT_TIME.s  0.5828    0.5829    0.5829     0.0063  0.5714  0.5958 1.0000     1.01   

# use a second model for the ppo argument, no left had side, specifies variables where the proportional ods argument is relaxed.
if(F) {
  
  toow.rmsb.ppo <- rmsb::blrm(VIABILITY ~ ASSESSMENT_TIME.s, ppo = ~ ASSESSMENT_TIME.s, data = hlbt_dat.scale) 

  toow.rmsb.ppo  # time: 51 min
  # Coefs:
  #                        Mode Beta Mean Beta Median Beta S.E.   Lower   Upper   Pr(Beta>0) Symmetry
  # y>=P                   -0.3184   -0.3184   -0.3183     0.0064 -0.3316 -0.3066 0.0000     1.02    
  # y>=D                   -1.5161   -1.5159   -1.5160     0.0084 -1.5313 -1.4988 0.0000     1.01    
  # ASSESSMENT_TIME.s       0.5945    0.5946    0.5945     0.0071  0.5815  0.6091 1.0000     1.02    
  # ASSESSMENT_TIME.s:y>=D -0.0276   -0.0276   -0.0277     0.0072 -0.0416 -0.0134 0.0000     0.99 
  predict(toow.rmsb.ppo ) #' *error: only constrained partial PO models are implemented at present - I'd have to do this manually*
  
  
  # the rms package also has lrm() for proportional odds models
  t1 <- lrm(VIABILITY ~ ASSESSMENT_TIME.s, data = hlbt_dat.scale)
  t1
  coef(t1)  # these match
  m1
  
  # https://www.fharrell.com/post/impactpo/
  # Non-PO models will be more unbiased but pay a significant price in terms of variance of estimates.
  
  summary(t1)  # well that's annoying
  # does anova of model give you wald statistic (X2 df and p?)
  
  # Violation is not Fatal ----
  #' [https://www.fharrell.com/post/po/]
  #' PO is important when comparing treatments in a randomized study. However, I think that in a predictive
  #' setting, it doesn't seem as bad?
  
  #
  #' [https://www.quanticate.com/blog/understanding-the-proportional-odds-assumption-in-clinical-trials#:~:text=The%20proportional%20odds%20assumption%20means,of%20which%20partition%20we%20consider.]
  #' Partially Proportional Model can be uses for covariates that don't meet assumption? 
  
}




# Try without haul, just permit?
system.time(clmm.permit <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|PERMIT), data = hlbt_dat.scale))  #' *I get warnings* iteration limit reached when updating the random effects, step factor reduced below minimum when updating the random effects
system.time(clmm.haul <- clmm(VIABILITY ~ ASSESSMENT_TIME.s + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|HAUL_ID), data = hlbt_dat.scale))
system.time(clmm.obs <- clmm(VIABILITY ~ ASSESSMENT_TIME.s + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|OBS_ID), data = hlbt_dat.scale))
system.time(clmm.trip <- clmm(VIABILITY ~ ASSESSMENT_TIME.s + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|TRIP_SEQ), data = hlbt_dat.scale))
anova(clm.0, clm.4.a, clmm.permit, clmm.haul, clmm.obs, clmm.trip)

#' # Adding haul as a ranef really helps. How do the rest work with haul? This might take a while
#' Observer was next most useful. Nest Haul in Observer?


system.time(clmm.obs_haul <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| OBS_ID / HAUL_ID), data = hlbt_dat.scale))
anova(clm.4.a, clmm.haul, clmm.obs, clmm.obs_haul)
# OBS_HAUL is ~ 2100 points better


system.time(clmm.permit_obs_haul <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| PERMIT / OBS_ID / HAUL_ID), data = hlbt_dat.scale))
anova(clm.4.a, clmm.haul, clmm.obs, clmm.obs_haul, clmm.permit_obs_haul)
# Adding permit didn't give me any warnings. Improved AIC only ~500 pts, but probably makes the most sense. 
coef(clmm.obs_haul)
coef(clmm.permit_obs_haul)  # coefficients are basically the same.
summary(clmm.permit_obs_haul)


#' I still get this error *negative length vectors are not allowed*
summary(hlbt_dat.scale$TRIP_ID)
unique(hlbt_dat.scale$TRIP_ID)  # I have 441 levels
uniqueN(hlbt_dat.scale$TRIP_ID)  

# Is it because obs_id isn't actually nested in TRIP_ID?
system.time(clmm.trip_haul <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| TRIP_ID / HAUL_ID), data = hlbt_dat.scale))
anova(clm.4.a, clmm.haul, clmm.obs, clmm.obs_haul, clmm.permit_obs_haul, clmm.trip_haul)
# hmm, trip_haul is better than obs_haul, perhaps because trip also contains some trip_target variability?

# HAUL is nested in both TRIP and OBS_ID
system.time(clmm.trip_obs_haul <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| TRIP_ID / CRUISE.PERMIT.HAUL) + (1| OBS_ID / CRUISE.PERMIT.HAUL), data = hlbt_dat.scale))
anova(clm.4.a, clmm.haul, clmm.obs, clmm.obs_haul, clmm.permit_obs_haul, clmm.trip_haul, clmm.trip_obs_haul) #' *20 min*
# I get another 600 AIC lower, not sure how much better

#' `Most accurate error structure?`
system.time(clmm.permit_trip_obs_haul <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| PERMIT / TRIP_ID / HAUL_ID) + (1| OBS_ID / HAUL_ID), data = hlbt_dat.scale)) #' *60 min*
clmm.permit_trip_obs_haul
summary(clmm.permit_trip_obs_haul)  # fails?



#' `Same thing but with scaled data`
system.time(clmm.permit_trip_obs_haul.s <- clmm(VIABILITY ~ ASSESSMENT_TIME.s + HAUL_MT.s + TOW_DUR.s + WEIGHT_KG.s + (1| PERMIT / TRIP_ID / HAUL_ID) + (1| OBS_ID / HAUL_ID), data = hlbt_dat.scale))  #' *85 min, why is it so much slower? AIC is slightly lower*
summary(clmm.permit_trip_obs_haul.s) 

# I can't run summary on these though... -  I think I need to simplify the model. Let's get rid of trip?
system.time(clmm.permit_haul.obs_haul <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| PERMIT / HAUL_ID) + (1| OBS_ID / HAUL_ID), data = hlbt_dat.scale))  #' *44 min*
summary(clmm.permit_haul.obs_haul )  # still don't have var/covar
#' Can see under RE that permit has only 0.1565 variance
anova(clmm.permit_trip_obs_haul, clmm.permit_haul.obs_haul)   #' *Including TRIP_SEQ was better by 873 AIC points)


## Best model? ----

clm.time.haul.tow.weight <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat.scale)

# Does permit help?
system.time(clmm.trip_haul.obs_haul <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| TRIP_ID / HAUL_ID) + (1| OBS_ID / HAUL_ID), data = hlbt_dat.scale)) #' *33 min*, only *12 min?*
summary(clmm.trip_haul.obs_haul) #' *I GET MY VAR/COVAR*
anova(clm.time.haul.tow.weight, clmm.trip_haul.obs_haul)  #' AIC is almost 19K points lower, 10%


# is this the same? Not quite, but virtually. Fewer parameters and slightly lower AIC? (22 pts)
system.time(clmm.trip_haul.obs <-  clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| TRIP_ID / HAUL_ID) + (1| OBS_ID), data = hlbt_dat.scale))  #' * 13 min*
summary(clmm.trip_haul.obs)
anova(clm.time.haul.tow.weight, clmm.trip_haul.obs_haul, clmm.trip_haul.obs) 

# So this one, clmm.obs_haul.trip is identical to clmm.trip_haul.obs
system.time(clmm.obs_haul.trip <-  clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| OBS_ID / HAUL_ID) + (1| TRIP_ID), data = hlbt_dat.scale))  #' *14 min*
summary(clmm.obs_haul.trip)
anova(clm.time.haul.tow.weight, clmm.trip_haul.obs_haul, clmm.trip_haul.obs, clmm.obs_haul.trip)

# How similar are these? Coefficients are very similar, but thresholds change a lot. How does this affect predictions?
coef(clmm.trip_haul.obs_haul)
coef(clmm.trip_haul.obs); coef(clmm.obs_haul.trip)  # the same


# using scaled data
clm.time.haul.tow.weight_scaled <- clm(VIABILITY ~ ASSESSMENT_TIME.s + HAUL_MT.s + TOW_DUR.s + WEIGHT_KG.s, data = hlbt_dat.scale)
clm.time.haul.tow.weight_scaled  
#' Looking at the coefficients, we can see the relative utility of our covariates. 

### exploring - Classification score ----

source("functions.R")
# Let's use the ranef to see how it helps
og_predict <- predict_clm(clm.time.haul.tow.weight)

both_predict.none <- predict_clm(clmm.trip_haul.obs_haul, ran_int = "none")
both_predict.average <- predict_clm(clmm.trip_haul.obs_haul, ran_int = "average")
# Shoot, I haven't gotten this to work with nested random effects!
both_predict.actual <- predict_clm(clmm.trip_haul.obs_haul, ran_int = "actual")  # doens't work when new_data is specified

one_predict.none <- predict_clm(clmm.trip_haul.obs, ran_int = "none")
one_predict.average <- predict_clm(clmm.trip_haul.obs, ran_int = "average")

#' Here, my random effects models do slightly worse with their classifications, but my test/train is the same, and I don't care about classificaiton really.
sum(diag(table(og_predict$Class, hlbt_dat.scale$VIABILITY))) / length(hlbt_dat.scale$VIABILITY) * 100             # 59.62%
sum(diag(table(both_predict.none$Class, hlbt_dat.scale$VIABILITY))) / length(hlbt_dat.scale$VIABILITY) * 100      # 58.38
sum(diag(table(both_predict.average$Class, hlbt_dat.scale$VIABILITY))) / length(hlbt_dat.scale$VIABILITY) * 100   # 58.39
sum(diag(table(one_predict.none$Class, hlbt_dat.scale$VIABILITY))) / length(hlbt_dat.scale$VIABILITY) * 100       # 58.86
sum(diag(table(one_predict.average$Class, hlbt_dat.scale$VIABILITY))) / length(hlbt_dat.scale$VIABILITY) * 100    # 58.86

# The fixed-effects only model only classifies halibut as E or D, no P!
table(og_predict$Class, hlbt_dat.scale$VIABILITY)
# The mixed-effects model actually do classify P halibut
table(both_predict.none$Class, hlbt_dat.scale$VIABILITY)
table(both_predict.average$Class, hlbt_dat.scale$VIABILITY)

### So why are the two models so similar?
AIC(clmm.trip_haul.obs_haul, clmm.trip_haul.obs)  
# Adding OBS_ID / HAUL only improves AIC slightly versus OBS, or TRIP_ID / HAUL versus TRIP?
clmm.trip_haul.obs_haul


### how do the OCI scores fare? ----

# Technically, if you account for the ordinaity of my response variable, the mixed models do slightly better (if it's wrong, it's at least closer)
oci(table(og_predict$Class, hlbt_dat.scale$VIABILITY))            # 56.315
oci(table(both_predict.none$Class, hlbt_dat.scale$VIABILITY))     # 57.275   # Not using any random effects
oci(table(both_predict.average$Class, hlbt_dat.scale$VIABILITY))  # 57.263   # Using the average of random effects
oci(table(one_predict.none$Class, hlbt_dat.scale$VIABILITY))      # 56.792
oci(table(one_predict.average$Class, hlbt_dat.scale$VIABILITY))   # 56.796

# Is this any better than not nesting at all?
system.time(clmm.haul.obs.trip <-  clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| HAUL_ID) + (1 | OBS_ID) + (1| TRIP_ID), data = hlbt_dat.scale))  #' *14 min*
AIC(clm.time.haul.tow.weight, clmm.trip_haul.obs_haul, clmm.trip_haul.obs, clmm.haul.obs.trip)  

# Huh.... so it's the same?
clmm.haul.obs.trip
clmm.trip_haul.obs

### null vs null with random effects ----
model.null <- clm(VIABILITY ~ 1, data = hlbt_dat.scale)
system.time(model.null.re <- clmm(VIABILITY ~ 1 + (1 | TRIP_ID / HAUL_ID) + (1 | OBS_ID / HAUL_ID), data = hlbt_dat.scale))
anova(model.null, model.null.re)  # AIC is improved 22.3K 

system.time(model.null.re2 <- clmm(VIABILITY ~ 1 + (1 | HAUL_ID / TRIP_ID) + (1 | HAUL_ID / OBS_ID), data = hlbt_dat.scale))
anova(model.null, model.null.re, model.null.re2)  # This way worked, but worse AIC, as I'd expect




#' *Does observer matter? Yes, don't exclude it*
system.time(clmm.trip_haul <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| TRIP_ID / HAUL_ID), data = hlbt_dat.scale)) #' *18 min*
summary(clmm.trip_haul) #'
anova(clmm.permit_trip_obs_haul, clmm.trip_haul.obs_haul, clmm.trip_haul)  #' excluding OBS_ID/HAUL is 558 pts worse

# What if I have OBS_ID but without HAUL_ID nested under it?
system.time(clmm.trip_haul.obs <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| TRIP_ID / HAUL_ID) + (1 | OBS_ID), data = hlbt_dat.scale)) #' *30 min*
anova(clmm.permit_trip_obs_haul, clmm.trip_haul.obs_haul, clmm.trip_haul, clmm.trip_haul.obs)  
# Adding OBS_ID to TRIP_ID/HAUL_ID reduces AIC by 580, but then nesting HAUL_ID under OBS_ID increases AIC 20 points...

# What if I had Trip by itself and OBS_ID/HAUL_ID?
system.time(clmm.trip.haul_obs <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| TRIP_ID) + (1 | OBS_ID  / HAUL_ID), data = hlbt_dat.scale)) #' *30 min*
anova(clmm.permit_trip_obs_haul, clmm.trip_haul.obs_haul, clmm.trip_haul, clmm.trip_haul.obs, clmm.trip.haul_obs)  
#' *huh... this was the same? HOW??? It's because I have the same number of groups?*
#' *But when I do TRIP/HAUL and OBS/HAUL at the same time, I get different groupings?*
clmm.trip_haul.obs_haul  # both HAUL/OBS and HAUL/TRIP have 13455, same number of hauls, but AIC is worse
hlbt_dat.scale[, .(TRIP_ID, OBS_ID, HAUL_ID)]

hlbt_dat.scale[, .(HAUL_ID)] |> uniqueN()
hlbt_dat.scale[, .(TRIP_ID, HAUL_ID)] |> uniqueN()


clmm.trip_haul.obs  #' how is HAUL_ID:TRIP_ID the same as HAUL_ID:OBS_ID
clmm.trip.haul_obs

#' *If I don't use permit I can do OBS/TRIP/HAUL. Hoever, this takes a LONG time...*
#' *BUT TRIP_ID IS NOT NESTED IN OBSERVER! Don't Use this!*
if(F) {
  system.time(clmm.obs_trip_haul <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| OBS_ID / TRIP_ID / HAUL_ID), data = hlbt_dat.scale)) #'
  anova(clmm.permit_trip_obs_haul, clmm.trip_haul.obs_haul, clmm.trip_haul, clmm.trip_haul.obs, clmm.trip.haul_obs, clmm.obs_trip_haul)  
}

# Non-prportional odds ----

library(VGAM)

# proportional odds
fit.vglm <- vglm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, family = propodds, data = hlbt_dat.scale)
coef(fit.vglm)
coef(clm.time.haul.tow.weight)  # Basically the same

# now non-proportional odds
fit.vglm.np <- vglm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, family = cumulative(parallel = FALSE), data = hlbt_dat.scale,  )
fit.vglm.np  # We have different coefficents for each threshold

AIC(fit.vglm)
AIC(fit.vglm.np)  # AIC is improved only 329 points

predictvglm(fit.vglm, type = "response")  # Hmy proportions of each category
predict(fit.vglm.np, type = "response")


predict.vglm <- unname(unlist(apply(predictvglm(fit.vglm, type = "response"), 1, function(x) names(x)[which(x == max(x))], simplify = F)))
predict.vglm.np <- unname(unlist(apply(predictvglm(fit.vglm.np, type = "response"), 1, function(x) names(x)[which(x == max(x))], simplify = F)))

predict.vglm <- factor(predict.vglm, ordered = T, levels = c("E", "P", "D"))
predict.vglm.np <- factor(predict.vglm.np, ordered = T, levels = c("E", "P", "D"))


oci(table(predict.vglm, hlbt_dat.scale$VIABILITY))
oci(table(predict.vglm.np, hlbt_dat.scale$VIABILITY))  # not as good with non-proportional odds, marginally
table(predict.vglm, hlbt_dat.scale$VIABILITY)
table(predict.vglm.np, hlbt_dat.scale$VIABILITY)  # np does actually assign some P halibut, but hardly better than random chance

# Can I do mixed effects with VGAM? It does not appear so. lme4's glmer4 would do it but cant do OLR

#' The mixor package? can relax the assumption using KG option


# bayesian approach using brms package ---------------------------------------------------------------------------------

#' the brms package [tutorial: https://journals.sagepub.com/doi/pdf/10.1177/2515245918823199]
#   library(brms)

# more on cumulative models using brms:
#' [https://bookdown.org/content/3686/whats-in-this-book-read-this-first.html]




mod.brms.cumulative <- brm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG, family = "cumulative", data = hlbt_dat.scale) # logit link
# save(mod.brms.cumulative, file = "output/mod.brms.cumulative")
#' This took like 3 hours. Does it go faster if I set priors??

summary(mod.brms.cumulative)

mod.brms.cumulative.cond_eff <- conditional_effects(mod.brms.cumulative, categorical = T)   
# These are great, but I have to rebuild the plots if I want to put them together

mod.clm <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG, data = hlbt_dat.scale)

fixef(mod.brms.cumulative)
t(data.table(t(coef(mod.clm))))  # Very similar but not exact
#' *DONT USE predict() with brms. Super slow? *
brms:::predict.brmsfit
brms:::posterior_predict_ordinal
brms:::pordinal

cumulative


# OMG This is never going to finish...
if( mod.brms.cumulative.mixed <- brm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + (1|TRIP_ID) + (1|HAUL_ID) + (1|OBS_ID) + (1|PERMIT), family = "cumulative", data = hlbt_dat.scale) ) # logit link
# save(mod.brms.cumulative.mixed, file = "output/mod.brms.cumulative.mixed")


#' [https://medium.com/towards-data-science/the-truth-about-bayesian-priors-and-overfitting-84e24d3a1153]
# No priors = uniform distribution between -/+ infinity. The weaker the prior, the closer to dimulating a maximum liklihood solution

# Set some priors?
# Weakly informative priors with group level random effect.
# normal_priors <- c(prior(normal(0,1), class="Intercept"),
#                    prior(normal(0,1), class="b"),
#                    prior(gamma(2,1), "sd")) 

# Thus, to continue on with Kruschke’s minimally-informative prior approach, something like 
# prior(normal(0, 4), class = Intercept)

#---------------#

anova(clm.4.a, clmm.haul, clmm.obs, clmm.obs_haul, clmm.permit_obs_haul, clmm.trip_haul, clmm.trip_obs_haul, clmm.permit_trip_obs_haul) #' *40 min*
#' Adding PERMIT only lowered AIC by 40 points, but took forever and doesn't seem that useful
#' Here, we assume the error structure where hauls are nested in trips, and trips inside permits. We also assume that hauls
#' are nested within observers, and observers are not necessarily nested within permits 






#--- Put useful stuff above here ---

#' Here, we assume that hauls + trips are nested under observers and trips separately
#' *THIS TAKES TOO LONG and is just incorrect!*
if(F) {system.time(clmm.permit_trip_obs_haul2 <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1| PERMIT / TRIP_ID / HAUL_ID) + (1| OBS_ID / TRIP_ID / HAUL_ID), data = hlbt_dat.scale))}
anova(clm.4.a, clmm.haul, clmm.obs, clmm.obs_haul, clmm.permit_obs_haul, clmm.trip_haul, clmm.trip_obs_haul, clmm.permit_trip_obs_haul, clmm.permit_trip_obs_haul2) 

#' Hauls are nested within trips, trips within permis
#' Trips ARE NOT nested under obsrevers, as we can have multiple observers on the same trip, but observers can be assigned on different permits
#' # We should maybe only have hauls nested under observers




#' [https://m-clark.github.io/mixed-models-with-R/random_intercepts.html]
# (1|random_variable) means I'm giving each variable a random intercept.

#' I think I started this around 1:45PM, finished sometime before 3:30pm, so *1 hour 45 min?*
clmm.haul <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|CRUISE.PERMIT.HAUL), data = hlbt_dat.scale)
summary(clmm.haul)
# I didn't get any warnings!
anova(clm.4.a, clmm.permit, clmm.haul) 
# Haul takes a ton of variability out
coef(clmm.permit)
coef(clmm.haul)  # Can see that coefficients are all greater in magnitude

# Now let's nest hauls within permits
system.time(clmm.permit_haul <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|PERMIT/CRUISE.PERMIT.HAUL), data = hlbt_dat.scale))
#' *this took only 18 minutes. Maybe the ones above need to be made factors first!*
anova(clm.4.a, clmm.permit, clmm.haul, clmm.permit_haul)   #' Uhh so why is permit/haul worse AIC? It's like permit but worse?
summary(clmm.permit_haul)  #' For some reason my var/covar table is NaN
coef(clmm.permit_haul)


#' *READ THIS* [https://stats.oarc.ucla.edu/r/dae/mixed-effects-logistic-regression/]

# What about observer?
system.time(clmm.permit_obs <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|CRUISE.PERMIT.HAUL) + (1|OBS_ID), data = hlbt_dat.scale))
#' *12 minutes
summary(clmm.permit_obs)
anova(clm.4.a, clmm.permit, clmm.haul, clmm.permit_haul, clmm.permit_obs)  
#' OBS is somewhat useful... so is permit really just not useful at all? 

#' What if we do HAUL nested in Observer?
system.time(clmm.obs_haul <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|OBS_ID/CRUISE.PERMIT.HAUL), data = hlbt_dat.scale))
summary(clmm.obs_haul)
anova(clm.4.a, clmm.permit, clmm.haul, clmm.permit_haul, clmm.permit_obs, clmm.obs_haul)  
#' *15 min*
#' Again, it's still worse than just haul by itself

#' And without nesting?
system.time(clmm.obs_haul2 <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|OBS_ID) + (1|CRUISE.PERMIT.HAUL), data = hlbt_dat.scale))
summary(clmm.obs_haul2)
anova(clm.4.a, clmm.permit, clmm.haul, clmm.permit_haul, clmm.permit_obs, clmm.obs_haul, clmm.obs_haul2)  
#' *12 min*
# without nesting is worse, marginally

# obs by itself?
system.time(clmm.obs <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|OBS_ID), data = hlbt_dat.scale))
summary(clmm.obs)
anova(clm.4.a, clmm.permit, clmm.haul, clmm.permit_haul, clmm.permit_obs, clmm.obs_haul, clmm.obs_haul2, clmm.obs)  
#' *7.581 min*
#' OBS alone is somewhat useful, but only by itself apparently, like haul.


#' [https://stats.stackexchange.com/questions/298078/what-are-the-consequences-of-including-unnecessary-random-effects]
#' cites [https://pmc.ncbi.nlm.nih.gov/articles/PMC3881361/]
#' ...by allowing random slopes and intercepts you're more likely to get a better fit to the data and thus better detect
#' when variance is attributable to the fixed effects.

#' It's not a great idea to use the sample data to determine if random effects are "necessary". Just because the 
#' inclusion of random effects doesn't explain variance in your current dataset doesn't mean that it is not important
#' to the population you're making inferences about.


#' Random Slopes ? [https://m-clark.github.io/mixed-models-with-R/random_slopes.html]

clmm.haul.1 <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT * TOW_DUR + WEIGHT_KG + (1|CRUISE.PERMIT.HAUL), data = hlbt_dat.scale)
#'*I get warnings here*

clmm.haul.2 <- clmm(VIABILITY ~ ASSESSMENT_TIME * WEIGHT_KG + HAUL_MT*TOW_DUR  + (1|CRUISE.PERMIT.HAUL), data = hlbt_dat.scale)
#'*I get warnings here, no surprise*

anova(clmm.haul, clmm.haul.1, clmm.haul.2)  # Adding interactions just made things worse.


#' [https://stats.stackexchange.com/questions/189021/model-selection-for-random-effects-can-unselected-random-effects-be-used-as-fix]

#' My fixed effects are *ASSESSMENT_TIME + WEIGHT_KG + HAUL_MT + TOW_DUR*
#' My full random effects structure would be: * 1 | permit / trip / obs / cruise.permit.haul *



system.time(what <- clmm(VIABILITY ~ ASSESSMENT_TIME + WEIGHT_KG + HAUL_MT + TOW_DUR  + (1|CRUISE.PERMIT.HAUL), data = hlbt_dat.scale))
anova(what, clmm.haul)
#' *What the hell?* Why was clmm.haul so lo won AIC? Was it not on the scaled data? Or was it before I factorized?
system.time(what2 <- clmm(VIABILITY ~ ASSESSMENT_TIME + WEIGHT_KG + HAUL_MT + TOW_DUR  + (1|CRUISE.PERMIT.HAUL), data = hlbt_dat))
anova(what, what2, clmm.haul)

# Can i see what data was used to make clmm.haul?
str(clmm.haul$model$CRUISE.PERMIT.HAUL)  # Cruise PERMIT HAUL was a character here, not FACTOR!
str(what$model$CRUISE.PERMIT.HAUL)  # FUCK CRUISE PERMIT HAUL IS PERMIT!??


# How many vessels, trips, plants, OBSERVERS? ----

# 17 vessels, 441 trips, 13,462 hauls, 105 cruises with 161 total observers

# NEED TO GET SAMPLED_BY AND BADGE_ID
hlbt_dat[, .(
  VES_N = uniqueN(PERMIT),
  TRIP_N = uniqueN(interaction(CRUISE, PERMIT, TRIP_SEQ)),
  HAUL_N = uniqueN(CRUISE.PERMIT.HAUL),
  CRUISE_N = uniqueN(CRUISE),
  OBS_ID = uniqueN(OBS_ID),
  HLBT_N = .N
)]

# How many vessels X hauls did each obsever sample?
hlbt_dat[, uniqueN(CRUISE.PERMIT.HAUL), keyby = .(PERMIT, OBS_ID)] 
# 259 unique PERMIT x OBS_ID combinations
a <- hlbt_dat[, uniqueN(CRUISE.PERMIT.HAUL), keyby = .(PERMIT, OBS_ID)] 
hist(a$V1, breaks = 20)


# Decksort simulations ----

#' For decksorted hauls, we only have viabilities/covariates for a sample of the decksorted populations - usually 1 in 5
#' halibut. We will sample from the trip-level populations, truncated by decksort duration, to simulate the decksorted
#' populations. 

#' By recreating each haul's decksort, we can calculate the 'true' haul-level mortality weight and determine which 
#' models or sampling methods provide the most accurate mortality estimates.

#' TODO *include sort duration and unscaled assessment time in this dataset for us to simulate with!!*
#' *make new HAUL_ID column to replace CRUISE PERMIT HAUL*
hlbt_dat.scale

# Compile the halibut for each trip, subsetting halibut below the sort end time
#hlbt_dat.scale[, TRIP_HALIBUT := seq_len(.N), by = .(TRIP_ID)]
setkey(hlbt_dat.scale, TRIP_ID, ASSESSMENT_TIME)
hlbt_dat.scale[, HLBT_ID := .I]
hlbt_tbl <- hlbt_dat.scale[, .(HLBT_ID, VIABILITY, ASSESSMENT_TIME.s, WEIGHT_KG.s, HAUL_MT.s, TOW_DUR.s, PERMIT)]
trip_hlbt_tbl <- hlbt_dat.scale[, .(TRIP_ID, ASSESSMENT_TIME)]
haul_hlbt_tbl <- setkey(unique(hlbt_dat.scale[, .(TRIP_ID, OBS_ID, HAUL_ID, SORT_END, PRESORTED_NUMBER)]), TRIP_ID, HAUL_ID)
hlbt_presample <- trip_hlbt_tbl[haul_hlbt_tbl, on = .(TRIP_ID), allow.cartesian = T] |>
  _[ASSESSMENT_TIME <= SORT_END] |>
  setkey(TRIP_ID, HAUL_ID)
#' *If I simulate the decksorts at the trip level, then I'm mixing assessments from different observers.*
#' *Alternatively, I could only compile halibut at the trip and OBS_ID level...*
test3 <- hlbt_presample[, .N, keyby = .(TRIP_ID, OBS_ID, HAUL_ID, PRESORTED_NUMBER)]
test3[PRESORTED_NUMBER > N]  #' *I have 165 hauls (1.22%) with more halibut in the presort than at the trip level*
test3[PRESORTED_NUMBER > N, quantile(N)]

hlbt_tbl2 <- hlbt_dat.scale[, .(HLBT_ID, VIABILITY, ASSESSMENT_TIME.s, WEIGHT_KG.s, HAUL_MT.s, TOW_DUR.s, PERMIT, OBS_ID)]

trip_tbl <- hlbt_dat.scale[, .(TRIP_ID, OBS_ID, HLBT_ID, ASSESSMENT_TIME)]
haul_tbl <- setkey(unique(hlbt_dat.scale[, .(TRIP_ID, OBS_ID, HAUL_ID, SORT_END, PRESORTED_NUMBER)]), TRIP_ID, HAUL_ID)
hlbt_presample2 <- trip_tbl[haul_tbl, on = .(TRIP_ID, OBS_ID), allow.cartesian = T] |>
  _[ASSESSMENT_TIME <= SORT_END] |>
  setkey(TRIP_ID, HAUL_ID)
test <- hlbt_presample2[, .SD[sample(1:.N, size = .BY[["PRESORTED_NUMBER"]], replace = T)], keyby = .(TRIP_ID, OBS_ID, HAUL_ID, PRESORTED_NUMBER)]
haul_hlbt_tbl2[, sum(PRESORTED_NUMBER)] == nrow(test)
# how many halibut do I have per TRIP x OBS_ID x HAUL_ID?
hlbt_presample2[, .N, keyby = .(TRIP_ID, OBS_ID)]
test2 <- hlbt_presample2[, .N, keyby = .(TRIP_ID, OBS_ID, HAUL_ID, PRESORTED_NUMBER)]
test2[PRESORTED_NUMBER > N] #' *I have 1,019 hauls (7.57%) where I have more halibut in haul's decksort than at the TRIP x OBS_ID level*
hist(test2[PRESORTED_NUMBER > N, N]) 
test2[PRESORTED_NUMBER > N, quantile(N)] # still most of these hauls have more than 20 halibut to draw from

# So If I compile halibut at the trip level, I get 3.356M records. At the trip x observer level, I get 1.455M records

ggplot(rbind(cbind(level = "TRIP", test3), cbind(level = "TRIPxOBS", test2)), aes(x = PRESORTED_NUMBER, y = N)) + 
  geom_point(alpha = 0.2) + 
  facet_grid(level ~ .) + 
  geom_abline(slope = 1, intercept = 0, color = "blue") + 
  coord_cartesian(xlim = c(0, 1000))
# Omiting one super huge decksort that had over 2600 halibut...
# We can see that generally, we have more halibut than haul decksorts regardless of the level we aggregate them at.
# For many of the hauls, we still have a fairly large population to draw from
# I'm going to use the TRIP x OBS aggregates so I can avoid confounding my observer random effect.


#' *Train/Test datasets*
#' After I simulate the decksorted populations, I'll train the models on 70-80% of hauls, and test on the remainder.
#' I'll use both a fixed effect model as well as the mixed effects model using the 

#' [https://www.youtube.com/watch?v=XAncv0fdI_s] Linear Mixed Effect Models (Application)

#' [https://optimumsportsperformance.com/blog/making-predictions-from-a-mixed-model-using-r/]
#' can I use predict() with re.form = NA?
#' If we yuse allow.new.levels = TRUE (using lme) we get the fixed point estimates, which should be the same as re.form = NA, 
#' but this is flexible, in that it will use random effects if they are known, or fixed point estimates if they are not.
#' In order to get confidence intervals, have to use the bootMer() function , for parametric bootstrap

# Exploratory Data Analyses ----

# Using trimmed dataset, hauls trimmed to 35 minute duration decksorts.

hlbt_dat.scale[, uniqueN(PERMIT)]
hlbt_dat.scale[, uniqueN(OBS_ID)]
hlbt_dat.scale[, uniqueN(TRIP_ID)]
hlbt_dat.scale[, uniqueN(HAUL_ID)]
hlbt_dat.scale[, .N]


#' Count number of hauls and halibut by Trip and Observer
trip_haul_hltb_count <- hlbt_dat.scale[, .(HAUL_N = uniqueN(HAUL_ID), HLBT_N = .N), keyby = .(TRIP_ID, OBS_ID)]

#' number of observers ver trip (mostly 2-3)
trip_haul_hltb_count[, .(OBS_ID_N = .N), by = .(TRIP_ID)][, table(OBS_ID_N)]

## Histograms of Trips/Hauls/Halibut ----
png(filename = "figures/exploratory/histograms.png", width = 1500, height = 900, res = 600, pointsize = 4)
par(mfrow = c(2,3), lwd = 0.5)
hist(hlbt_dat.scale[, uniqueN(TRIP_ID), by = .(PERMIT)]$V1, main = "Trips per vessel", xlab = "# Trips", breaks = 12)
hist(hlbt_dat.scale[, uniqueN(HAUL_ID), by = .(PERMIT)]$V1, main = "Hauls per vessel", xlab = "# Hauls")
hist(trip_haul_hltb_count[, sum(HAUL_N), by = .(OBS_ID)]$V1, main = "Hauls per observer", xlab = "# Hauls", breaks = 12)
hist(hlbt_dat.scale[, .N, by = PERMIT]$N, main = "Assessed halibut per vessel", xlab = "# Halibut")
hist(trip_haul_hltb_count$HLBT_N, main = "Assessed halibut per haul", xlab = "# Halibut")
hist(trip_haul_hltb_count[, sum(HLBT_N), by = .(OBS_ID)]$V1, main = "Assessed halibut per observer", xlab = "# Halibut")
graphics.off()

## Covariates ----


table(hlbt_dat.scale$VIABILITY) # 58% E, 23% P, 19% D
table(hlbt_dat.scale$VIABILITY) / nrow(hlbt_dat.scale)

### Individual-level covariates ----
png(filename = "figures/exploratory/covars_individual.png", width = 900, height = 900, res = 600, pointsize = 3)
par(mfrow = c(2,2), lwd = 0.5)
plot(VIABILITY ~ ASSESSMENT_TIME, data = hlbt_dat.scale, xlab = "Assessment time (min)", ylab = "Viability")
plot(VIABILITY ~ WEIGHT_KG, data = hlbt_dat.scale, xlab = "Weight (kg)", ylab = "Viability")
boxplot(ASSESSMENT_TIME ~ VIABILITY, data = hlbt_dat.scale, ylab = "Assessment time (min)", xlab = "Viability")
boxplot(WEIGHT_KG ~ VIABILITY, data = hlbt_dat.scale, ylab = "Weight (kg)", xlab = "Viability")
graphics.off()

### Haul-level covariates ----
haul_covar <- hlbt_dat.scale[, .(TOW_DUR, HAUL_MT, FISHING_DEPTH), by = .(HAUL_ID)]

png(filename = "figures/exploratory/covars_haul.png", width = 1500, height = 900, res = 600, pointsize = 4)
par(mfrow = c(2,3), lwd = 0.5)
plot(VIABILITY ~ HAUL_MT, data = hlbt_dat.scale, xlab = "Tonnage (MT)", ylab = "Viability")
plot(VIABILITY ~ TOW_DUR, data = hlbt_dat.scale, xlab = "Tow duration (hrs)", ylab = "Viability")
plot(VIABILITY ~ FISHING_DEPTH, data = hlbt_dat.scale, xlab = "Fishing depth (ftm)", ylab = "Viability")
boxplot(HAUL_MT ~ VIABILITY, data = hlbt_dat.scale, ylab = "Tonnage (MT)", xlab = "Viability")
boxplot(TOW_DUR ~ VIABILITY, data = hlbt_dat.scale, ylab = "Tow duration (hrs)", xlab = "Viability")
boxplot(FISHING_DEPTH ~ VIABILITY, data = hlbt_dat.scale, ylab = "Fishing depth (ftm)", xlab = "Viability")
graphics.off()


### Auto-Correlation ----

hlbt_dat.haul[, cor(HAUL_MT, TOW_DUR)]  # Very little correlation between haul duration and tonnage
hlbt_dat.haul[, cor(HAUL_MT, FISHING_DEPTH)]  # 
hlbt_dat.haul[, cor(TOW_DUR, FISHING_DEPTH)]

#======================================================================================================================#
# Train and Testing ----
#======================================================================================================================#

#' Now we'll split the dataset into train and test subsets, sampling random hauls

##  Functions ---- 

# Split data table into train and test datasets
split_train_test <- function(x, train_prop, seed, by) {
  # x <- copy(hlbt_dat.scale); train_prop <- 0.7; seed <- 12345; by = "HAUL_ID"
  
  set.seed(seed)
  
  by.vec <- unique(x[[by]])
  train_i <- sample(seq_along(by.vec), size = round(train_prop * length(by.vec)), replace = F)
  train = subset(x, x[[by]] %in% train_i )
  
  list(
    train = train,
    test = fsetdiff(x, train)
  )
  
}

# Do K-fold cross validation

split_kfold <- function(x, K, seed, by) {
  # x <- copy(hlbt_dat.scale); K <- 20; seed <- 12345; by = "HAUL_ID"
  
  set.seed(seed)
  # Create vectof all unique 'by' groups
  by.vec <- unique(x[[by]])
  # split by.vec into K roughly equal-sized groups. Avoiding using runif().
  K.size <- diff(c(round(seq(1, length(by.vec), by = length(by.vec)/ K)), length(by.vec)))
  # Add one to the final group
  K.size[[K]] <- K.size[[K]] + 1
  K.lst <- vector(mode = "list", length = K)
  for(i in seq_along(K.lst)) {
    i.sample <- sample(by.vec, size = K.size[i], replace = F)
    by.vec <- setdiff(by.vec, i.sample)
    K.lst[[i]] <- subset(x, x[[by]] %in% i.sample)
  }
  K.lst
  
}


kfold_cv <- function(kfold_dat, mod) {
  # mod <- copy(mod.4)
  # kfold_dat <- split_kfold(hlbt_dat.scale, K = 20, seed = 19890310, by = "HAUL_ID")

  
  class(mod)    # class (function)
  formula(mod) # formula
  K <- length(kfold_dat)
  
  # Create the test and train datasets
  train_test.lst <- vector(mode = "list", length = K)
  for(i in 1:K) {
    train_test.lst[[i]] <- list(
      train = do.call(rbind, (kfold_dat[setdiff(1:K, i)])),
      test = kfold_dat[[i]]
    )
  }
  
  # For each fold, train the model, test it, and calculate evaluation metrics
  eval.lst <- vector(mode = "list", length = K)
  cat(paste0("Cross-validation for K = ", K, ":\n"))
  for(j in 1:K) {
    cat(paste0(j, ", "))
    mod.train <- match.fun(class(mod))(formula = formula(mod), data = train_test.lst[[j]]$train)
    mod.test <- calc_dmr(train_test.lst[[j]]$test, mod = mod.train)
    baseline <- calc_dmr(train_test.lst[[j]]$test)
    # Merge 'true' values in
    mod.test[, c("TRUE_DMR", "TRUE_MORT") := baseline[mod.test, .(DMR, MORT_KG), on = .(HAUL_ID)]]
    
    eval.lst[[j]] <- list(
      mod = mod.train,
      eval = mod.test
      
    )
  }
  cat("\n")
  eval.lst
  
}



kfold_cv2 <- function(kfold_dat, fun, formula) {
  # mod <- copy(mod.4)
  # kfold_dat <- split_kfold(hlbt_dat.scale, K = 5, seed = 19890310, by = "HAUL_ID")
  # fun <- "clm"; formula <- "VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG"
  
  formula <- as.formula(formula) # formula
  K <- length(kfold_dat)
  
  # Create the test and train datasets
  train_test.lst <- vector(mode = "list", length = K)
  for(i in 1:K) {
    train_test.lst[[i]] <- list(
      train = do.call(rbind, (kfold_dat[setdiff(1:K, i)])),
      test = kfold_dat[[i]]
    )
  }
  
  # For each fold, train the model, test it, and calculate evaluation metrics
  eval.lst <- vector(mode = "list", length = K)
  cat(paste0("Cross-validation for K = ", K, ":\n"))
  for(j in 1:K) {
    
    if(j == 1) start_time <- Sys.time()                   #' *FIXME* The timings aren't working - just 0 minutes?
    
    mod.train <- match.fun(fun)(formula = formula, data = train_test.lst[[j]]$train)
    mod.test <- calc_dmr(train_test.lst[[j]]$test, mod = mod.train)
    baseline <- calc_dmr(train_test.lst[[j]]$test)
    # Merge 'true' values in
    mod.test[, c("TRUE_DMR", "TRUE_MORT") := baseline[mod.test, .(DMR, MORT_KG), on = .(HAUL_ID)]]
    
    if(j == 1) {
      end_time <- Sys.time()
      diff_time <- round(as.numeric(end_time - start_time, units = "mins"), 2)
      est_finish <- start_time + diff_time*5
      cat(paste0("K=1 completed in ", diff_time, " minutes. K=5 completion ETA is ", diff_time*5, " minutes or ", est_finish, ".\n" ))
    }
    cat(paste0(j, ", "))

    eval.lst[[j]] <- list(
      mod = mod.train,
      eval = mod.test
    )
  }
  cat("\n")
  eval.lst
  
}

# Calculate haul-level DMRs
calc_dmr <- function(x, mod = NULL) {
  # x <- copy(hlbt.train_test$train); mod <- NULL
  # x <- copy(hlbt.train_test$train); mod <- mod.4
  
  # Assuming IPHC's mortality probabilities of E = 0.2, P = 0.055, and D = 0.9
  
  # if mod is not specified, use the dataset as-is. Otherwise, use the predictions from the model
  if(!is.null(mod)) {
    mod.pred <- predict_clm(mod, new_data = x)$Mort
    pre <- copy(x)
    pre$MORT <- mod.pred 
  } else {
    mort_prob <- data.table(VIABILITY = c("E", "P", "D"), MORT = c(0.2, 0.55, 0.9))
    pre <- mort_prob[x, on = .(VIABILITY)] 
  }

  # Calculate haul-level DMRs and mortality weight
  out <- pre |>
    _[, .(
      n = .N, 
      AVG_KG = mean(WEIGHT_KG), 
      DMR = weighted.mean(MORT, w = WEIGHT_KG)
      ), 
      by = .(HAUL_ID, PRESORTED_NUMBER)
    # Calculate the estimated total weight of the decksort and the mortality weight
    ][, EST_TOTAL_KG := AVG_KG * PRESORTED_NUMBER
    ][, MORT_KG := EST_TOTAL_KG * DMR][]
  
  setattr(out, "raw", pre)
  out
}







# Make a split
hlbt.train_test <- split_train_test(hlbt_dat.scale, train_prop = 0.7, seed = 12345, by = "HAUL_ID")
# Testing a different seed
# hlbt.train_test <- split_train_test(hlbt_dat.scale, train_prop = 0.7, seed = 2947883, by = "HAUL_ID")
hlbt.train_test <- split_train_test(hlbt_dat.scale, train_prop = 0.7, seed = 983240, by = "HAUL_ID")



## Train the models ----
mod.0 <- clm(VIABILITY ~ 1, data = hlbt.train_test$train)
mod.1 <- clm(VIABILITY ~ ASSESSMENT_TIME, data = hlbt.train_test$train)
mod.2 <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT , data = hlbt.train_test$train)
mod.3 <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR, data = hlbt.train_test$train)
mod.4 <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt.train_test$train)
# What if we did just ASSESSMENT_TIME and WEIGHT_KG
mod.5 <-  clm(VIABILITY ~ ASSESSMENT_TIME + WEIGHT_KG, data = hlbt.train_test$train)

# Mixed Models

# Full model with different random error structures                                                                                                                                       
mod.4.a <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|TRIP_ID / HAUL_ID) + (1|OBS_ID / HAUL_ID), data = hlbt.train_test$train)
mod.4.b <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|HAUL_ID), data = hlbt.train_test$train)

# Halibut-level covariates only
mod.5.a <- clmm(VIABILITY ~ ASSESSMENT_TIME + WEIGHT_KG + (1|TRIP_ID / HAUL_ID) + (1|OBS_ID / HAUL_ID), data = hlbt.train_test$train)
mod.5.b <- clmm(VIABILITY ~ ASSESSMENT_TIME + (1|HAUL_ID), data = hlbt.train_test$train)

mod.0.a <- clmm(VIABILITY ~ 1 + (1|TRIP_ID / HAUL_ID) + (1|OBS_ID / HAUL_ID), data = hlbt.train_test$train)
mod.1.a <- clmm(VIABILITY ~ ASSESSMENT_TIME + (1|TRIP_ID / HAUL_ID) + (1|OBS_ID / HAUL_ID), data = hlbt.train_test$train)
mod.2.a <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT  + (1|TRIP_ID / HAUL_ID) + (1|OBS_ID / HAUL_ID), data = hlbt.train_test$train)
mod.3.a <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + (1|TRIP_ID / HAUL_ID) + (1|OBS_ID / HAUL_ID), data = hlbt.train_test$train)



### K Fold Cross Validation ----

#' TODO Make kfold_cv function accept function and model

# K=10 might be better! But it took over 12 hours to get through all this at k=5...
kfold_dat <- split_kfold(hlbt_dat.scale, K = 5, seed = 19890310, by = "HAUL_ID")

m0.f <- kfold_cv2(kfold_dat, "clm", "VIABILITY ~ 1")
m0.m <- kfold_cv2(kfold_dat, "clmm", "VIABILITY ~ 1 + (1|PERMIT) + (1|TRIP_ID) + (1|HAUL_ID) + (1|OBS_ID)")

m1.f <- kfold_cv2(kfold_dat, "clm", "VIABILITY ~ ASSESSMENT_TIME")
m1.m <- kfold_cv2(kfold_dat, "clmm", "VIABILITY ~ ASSESSMENT_TIME + (1|PERMIT) + (1|TRIP_ID) + (1|HAUL_ID) + (1|OBS_ID)")

m2.f <- kfold_cv2(kfold_dat, "clm", "VIABILITY ~ ASSESSMENT_TIME + HAUL_MT")
m2.m <- kfold_cv2(kfold_dat, "clmm", "VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + (1|PERMIT) + (1|TRIP_ID) + (1|HAUL_ID) + (1|OBS_ID)")

m3.f <- kfold_cv2(kfold_dat, "clm", "VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG")
m3.m <- kfold_cv2(kfold_dat, "clmm", "VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + (1|PERMIT) + (1|TRIP_ID) + (1|HAUL_ID) + (1|OBS_ID)")

m4.f <- kfold_cv2(kfold_dat, "clm", "VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG")
m4.m <- kfold_cv2(kfold_dat, "clmm", "VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + (1|PERMIT) + (1|TRIP_ID) + (1|HAUL_ID) + (1|OBS_ID)")

m5.f <- kfold_cv2(kfold_dat, "clm", "VIABILITY ~ ASSESSMENT_TIME + WEIGHT_KG")
m5.m <- kfold_cv2(kfold_dat, "clmm", "VIABILITY ~ ASSESSMENT_TIME + WEIGHT_KG + (1|PERMIT) + (1|TRIP_ID) + (1|HAUL_ID) + (1|OBS_ID)")


if(F) {
  save(
    m0.f, m0.m, m1.f, m1.m, m2.f, m2.m, m3.f, m3.m, m4.f, m4.m, m5.f, m5.m,
    file = "output/kfold_cv.rdata")
}

# This function compiles the evaluation results from each model's kfold cv
compile_kfold_cv <- function(...) {
  results_list <- lapply(list(...), function(x) rbindlist(lapply(x, "[[", "eval"), idcol = "K"))
  names(results_list) <- as.character(substitute(list(...)))[-1L]
  rbindlist(results_list, idcol = "MOD")
}

mod_results <- compile_kfold_cv(m0.f, m0.m, m1.f, m1.m, m2.f, m2.m, m3.f, m3.m, m4.f, m4.m, m5.f, m5.m)
mod_results |>
  _[, .(
    MORT_BIAS = sum(MORT_KG) - sum(TRUE_MORT), MORT_SD = sd(MORT_KG - TRUE_MORT),
    DMR_BIAS = mean(DMR - TRUE_DMR), DMR_SD = sd(DMR - TRUE_DMR)
  ), keyby = .(MOD, K)
  ][, .(
    MORT_BIAS = mean(MORT_BIAS), MORT_SD = mean(MORT_SD),
    DMR_BIAS = mean(DMR_BIAS), DMR_SD = mean(DMR_SD)
  ), keyby = .(MOD)]
# With the null model, random effects reduces the mortality bias without affecting  much else


ggplot(
  melt(mod_results[, .(
    MORT_BIAS = sum(MORT_KG) - sum(TRUE_MORT), MORT_SD = sd(MORT_KG - TRUE_MORT),
    DMR_BIAS = sum(DMR) - sum(TRUE_DMR), DMR_SD = sd(TRUE_DMR - DMR)
  ), keyby = .(MOD, K)], id.vars = c("MOD", "K")),
  aes(x = MOD, y = value)) + 
  facet_grid(variable ~ ., scales = "free") + 
  geom_hline(data = data.table(variable = c("MORT_BIAS", "DMR_BIAS"), yintercept = 0), aes(yintercept = yintercept), linetype = 2) + 
  geom_boxplot(alpha = 0.8)  + geom_point()

# Really hard to say with K-5, just not enough to parse noise from signal
# it does seem like most mixed models have a lot more variability on bias. Should see how much the covariates differ between models






#' *BELOW IS OLD*

kfold_dat <- split_kfold(hlbt_dat.scale, K = 5, seed = 19890310, by = "HAUL_ID")

mod.0.kfcv <- kfold_cv(kfold_dat, mod = mod.0)
mod.1.kfcv <- kfold_cv(kfold_dat, mod = mod.1)
mod.2.kfcv <- kfold_cv(kfold_dat, mod = mod.2)
mod.3.kfcv <- kfold_cv(kfold_dat, mod = mod.3)
mod.4.kfcv <- kfold_cv(kfold_dat, mod = mod.4)
mod.5.kfcv <- kfold_cv(kfold_dat, mod = mod.5)

# Make some quick mixed effects models
system.time(mod.4m <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|OBS_ID / HAUL_ID) + (1|TRIP_ID / HAUL_ID), data = hlbt_dat.scale[1:10000]))
system.time(mod.6m <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + (1|OBS_ID / HAUL_ID) + (1|TRIP_ID / HAUL_ID), data = hlbt_dat.scale[1:10000]))

mod.4m.kfcv <- kfold_cv(kfold_dat, mod = mod.4m)
mod.6m.kfcv <- kfold_cv(kfold_dat, mod = mod.6m)


# What if we exclude TOW_DUR?
mod.6 <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG, data = hlbt.train_test$train)
mod.6.kfcv <- kfold_cv(kfold_dat, mod = mod.6)

eval_dt <- rbind(
  cbind(MOD = "mod.0", rbindlist(lapply(mod.0.kfcv, "[[", "eval"), idcol = "K")),
  cbind(MOD = "mod.1", rbindlist(lapply(mod.1.kfcv, "[[", "eval"), idcol = "K")),
  cbind(MOD = "mod.2", rbindlist(lapply(mod.2.kfcv, "[[", "eval"), idcol = "K")),
  cbind(MOD = "mod.3", rbindlist(lapply(mod.3.kfcv, "[[", "eval"), idcol = "K")),
  cbind(MOD = "mod.4", rbindlist(lapply(mod.4.kfcv, "[[", "eval"), idcol = "K")),
  cbind(MOD = "mod.4m", rbindlist(lapply(mod.4m.kfcv, "[[", "eval"), idcol = "K")),   # which method was used for ran_int?
  cbind(MOD = "mod.5", rbindlist(lapply(mod.5.kfcv, "[[", "eval"), idcol = "K")),
  cbind(MOD = "mod.6", rbindlist(lapply(mod.6.kfcv, "[[", "eval"), idcol = "K"))
)

# Calculate bias and sd
#' TODO What about bias/SD of haul-level DMR?
eval_dt |>
  _[, .(
    MORT_BIAS = sum(MORT_KG) - sum(TRUE_MORT), MORT_SD = sd(MORT_KG - TRUE_MORT),
    DMR_BIAS = mean(DMR - TRUE_DMR), DMR_SD = sd(DMR - TRUE_DMR)
    ), keyby = .(MOD, K)
  ][, .(
    MORT_BIAS = mean(MORT_BIAS), MORT_SD = mean(MORT_SD),
    DMR_BIAS = mean(DMR_BIAS), DMR_SD = mean(DMR_SD)
    ), keyby = .(MOD)]
# Adding TOW_DUR didn't really help bias, but SD went down. Adding WEIGHT_KG really helped both BIAS and SD.
# Using only individual-level covariates (mod.5), bias was low but sd was higher
# mod.6 (no TOW_DUR) had lower bias but higher sd than with it (mod.4). Might be good as a simpler model, esp as it hardly improves
# the model from 3 to 4.


ggplot(
  melt(eval_dt[, .(BIAS = sum(MORT_KG) - sum(TRUE_MORT), SD = var(MORT_KG - TRUE_MORT) ), keyby = .(MOD, K)], id.vars = c("MOD", "K")),
  aes(x = MOD, y = value)) + 
  facet_grid(variable ~ ., scales = "free") + 
  geom_hline(data = data.table(variable = "BIAS", yintercept = 0), aes(yintercept = yintercept), linetype = 2) + 
  geom_boxplot(alpha = 0.8) 

# 4m had an outlier, not great



## Test models, evaluating DMRs ----
true_dmr_mort <- calc_dmr(hlbt.train_test$test)
mod.0.pred <- calc_dmr(hlbt.train_test$test, mod = mod.0)
mod.1.pred <- calc_dmr(hlbt.train_test$test, mod = mod.1)
mod.2.pred <- calc_dmr(hlbt.train_test$test, mod = mod.2)
mod.3.pred <- calc_dmr(hlbt.train_test$test, mod = mod.3)
mod.4.pred <- calc_dmr(hlbt.train_test$test, mod = mod.4)
mod.5.pred <- calc_dmr(hlbt.train_test$test, mod = mod.5)

mod.4.a.pred <- calc_dmr(hlbt.train_test$test, mod = mod.4.a)
mod.4.b.pred <- calc_dmr(hlbt.train_test$test, mod = mod.4.b)
mod.5.a.pred <- calc_dmr(hlbt.train_test$test, mod = mod.5.a)
mod.5.b.pred <- calc_dmr(hlbt.train_test$test, mod = mod.5.b)
mod.0.a.pred <- calc_dmr(hlbt.train_test$test, mod = mod.0.a)
mod.1.a.pred <- calc_dmr(hlbt.train_test$test, mod = mod.1.a)
mod.2.a.pred <- calc_dmr(hlbt.train_test$test, mod = mod.2.a)
mod.3.a.pred <- calc_dmr(hlbt.train_test$test, mod = mod.3.a)



pred_tbl <- rbind(
  cbind(MOD = "mod.0", mod.0.pred),
  cbind(MOD = "mod.1", mod.1.pred),
  cbind(MOD = "mod.2", mod.2.pred),
  cbind(MOD = "mod.3", mod.3.pred),
  cbind(MOD = "mod.4", mod.4.pred),
  cbind(MOD = "mod.5", mod.5.pred),
  cbind(MOD = "mod.0.a", mod.0.a.pred),
  cbind(MOD = "mod.1.a", mod.1.a.pred),
  cbind(MOD = "mod.2.a", mod.2.a.pred),
  cbind(MOD = "mod.3.a", mod.3.a.pred),
  cbind(MOD = "mod.4.a", mod.4.a.pred),
  cbind(MOD = "mod.4.b", mod.4.b.pred),
  cbind(MOD = "mod.5.a", mod.5.a.pred),
  cbind(MOD = "mod.5.b", mod.5.b.pred)
) 
pred_tbl |>
  _[, TRUE_DMR := true_dmr_mort[pred_tbl, DMR, on = .(HAUL_ID)] 
  ][, TRUE_MORT := true_dmr_mort[pred_tbl, MORT_KG, on = .(HAUL_ID)] 
  ][, DMR_DIFF := DMR - TRUE_DMR
  ][, MORT_DIFF := MORT_KG - TRUE_MORT][]
pred_tbl.melt <- melt(pred_tbl, id.vars = c("HAUL_ID", "MOD", "PRESORTED_NUMBER", "EST_TOTAL_KG"), measure.vars = c("MORT_DIFF", "DMR_DIFF"))

ggplot(pred_tbl.melt, aes(x = MOD, y = value)) + 
  facet_grid(variable ~ ., scales = "free_y") + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_hline(yintercept = 0, color = "blue") +
  stat_summary(geom = "point", fun = mean, color = "black", shape = 4, size = 2)


# mod.4.a (full mod + RE) gets the lowest AIC, but even mod.3.a is close
AIC(mod.0, mod.0.a, mod.1, mod.1.a, mod.2, mod.2.a, mod.3, mod.3.a, mod.4, mod.4.a, mod.4.b, mod.5, mod.5.a, mod.5.b)
anova(mod.0, mod.0.a, mod.1, mod.1.a, mod.2, mod.2.a, mod.3, mod.3.a, mod.4, mod.4.a, mod.4.b, mod.5, mod.5.a, mod.5.b)


true_dmr_mort[, sum(MORT_KG)]
pred_tbl[, sum(MORT_DIFF), by = .(MOD)]                                         # Bias
pred_tbl[, sum(MORT_DIFF) / true_dmr_mort[, sum(MORT_KG)] * 100, by = .(MOD)]   # Percent bias
# including weight_kg reduces bias considerably! 
# bias actually worsens as we add HAUL_MT and TOW_DUR, but variance of DMRs and MORTALITY improve
# The random effects (4.a and 4.b did not improve bias)

#' *NOTE* With a different seed, mod5 (no haul-level covar) came out with less bias than mod 4 


pred_tbl[, median(MORT_DIFF), by = .(MOD)]     # Median diff is typically >0, except when we add weight (for seed 1235)
pred_tbl[, median(DMR_DIFF), by = .(MOD)]      # Median DMRs are >0 for all models

# random effects further reduced variance of haul-level DMRs, but with a trade-off of worse bias (underestimates 2.26% rather than overestimate 1.62%)
pred_tbl[, .(DMR_bias = mean(DMR_DIFF), MORT_bias = mean(MORT_DIFF), DMR_var = var(DMR_DIFF), MORT_var = var(MORT_DIFF)), by = .(MOD)]


ggplot(pred_tbl.melt, aes(x = PRESORTED_NUMBER, y = value)) + 
  facet_grid(variable ~ MOD, scales = "free_y") + 
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = 0, color = "blue")

ggplot(pred_tbl.melt, aes(x = EST_TOTAL_KG, y = value)) + 
  facet_grid(variable ~ MOD, scales = "free_y") + 
  geom_point(alpha = 0.1) +
  geom_hline(yintercept = 0, color = "blue")


# Compare individual mortality estimates

test <- copy(attr(mod.0.pred, "raw"))
test[, OG_MORT := fcase(VIABILITY == "E", 0.2, VIABILITY == "P", 0.55, VIABILITY == "D", 0.9)]
test[, MORT_DIFF := MORT - OG_MORT]
hist(test$MORT_DIFF)
test.melt <- melt(test, measure.vars = c("MORT", "OG_MORT"))
# Histogram of observer-based mortality estimates (blue) vs model-based (red)
ggplot(test.melt, aes(x = value, fill = variable)) + geom_histogram(position = "identity", bins = 40, alpha = 0.8) + theme(legend.position = "bottom")
# Model 0 assigns the same mortality estimate to all halibut, 0.4180, which is the global mean from the dataset
test[, mean(MORT)]

test <- copy(attr(mod.1.pred, "raw"))
test[, OG_MORT := fcase(VIABILITY == "E", 0.2, VIABILITY == "P", 0.55, VIABILITY == "D", 0.9)]
test[, MORT_DIFF := MORT - OG_MORT]
hist(test$MORT_DIFF)
test.melt <- melt(test, measure.vars = c("MORT", "OG_MORT"))
# Histogram of observer-based mortality estimates (blue) vs model-based (red)
ggplot(test.melt, aes(x = value, fill = variable)) + geom_histogram(position = "identity", bins = 40, alpha = 0.8) + theme(legend.position = "bottom")
# Model 0 assigns the same mortality estimate to all halibut, 0.4180, which is the global mean from the dataset
test[, mean(MORT)]


test <- copy(attr(mod.4.pred, "raw"))
test[, OG_MORT := fcase(VIABILITY == "E", 0.2, VIABILITY == "P", 0.55, VIABILITY == "D", 0.9)]
test[, MORT_DIFF := MORT - OG_MORT]
hist(test$MORT_DIFF)
test.melt <- melt(test, measure.vars = c("MORT", "OG_MORT"))
# Histogram of observer-based mortality estimates (blue) vs model-based (red)
ggplot(test.melt, aes(x = value, fill = variable)) + geom_histogram(position = "identity", bins = 40, alpha = 0.8) + theme(legend.position = "bottom")
# Model 0 assigns the same mortality estimate to all halibut, 0.4180, which is the global mean from the dataset
test[, mean(MORT)]


test2 <- copy(attr(mod.4.a.pred, "raw"))
test2[, OG_MORT := fcase(VIABILITY == "E", 0.2, VIABILITY == "P", 0.55, VIABILITY == "D", 0.9)]
test2[, MORT_DIFF := MORT - OG_MORT]
hist(test2$MORT_DIFF)
test.melt2 <- melt(test, measure.vars = c("MORT", "OG_MORT"))
# Histogram of observer-based mortality estimates (blue) vs model-based (red)
ggplot(test.melt2, aes(x = value, fill = variable)) + geom_histogram(position = "identity", bins = 40, alpha = 0.8) + theme(legend.position = "bottom")
# Model 0 assigns the same mortality estimate to all halibut, 0.4180, which is the global mean from the dataset
test2[, mean(MORT)]  # Relative to 4.a, the distribution flattens out

test3 <- rbind(
  cbind(test, MOD = "mod.4"),
  cbind(test2, MOD = "mod.4a")
)
ggplot(test3, aes(x = MORT, fill = MOD)) + geom_histogram(position = "identity", bins = 40, alpha = 0.8) + theme(legend.position = "bottom")
# Adding mixed effects flattened the distribution and pushed it lower
test[, mean(OG_MORT)]  # 0.4108
test[, mean(MORT)]     # 0.4145
test2[, mean(MORT)]    # 0.4065

# DMRs from mixed model are generally lower
ggplot(pred_tbl[MOD %in% c("mod.4", "mod.4.a")], aes(x = DMR_DIFF, fill = MOD)) + geom_histogram(position = "identity", alpha = 0.5)
ggplot(pred_tbl[MOD %in% c("mod.4", "mod.4.a")], aes(x = MORT_DIFF, fill = MOD)) + geom_histogram(position = "identity", alpha = 0.5)
pred_tbl[, DMR_DIFF_PERC := (DMR - TRUE_DMR)/TRUE_DMR * 100]
# Percent difference in haul-level DMR. Again, mixed model is slightly lower. Bimodal distribution?
ggplot(pred_tbl[MOD %in% c("mod.4", "mod.4.a")], aes(x = DMR_DIFF_PERC, fill = MOD)) + geom_histogram(position = "identity", alpha = 0.5)
ggplot(pred_tbl[MOD %in% c("mod.4", "mod.4.a")], aes(x = EST_TOTAL_KG, y = DMR_DIFF_PERC, color = MOD)) + geom_point(alpha = 0.5)

mod.4$coefficients
mod.4.a$coefficients

summary(mod.4)
summary(mod.4.a)

### Random Effects ----
names(ranef(mod.4.a))  # So when I nest HAUL_ID under trip and observer, I don't get intercepts for HAUL_ID by themselves.
ranef(mod.4.a)$OBS_ID
hist(ranef(mod.4.a)$OBS_ID$`(Intercept)`)
hist(ranef(mod.4.a)$`HAUL_ID:OBS_ID`$`(Intercept)`)
hist(ranef(mod.4.a)$`TRIP_ID`$`(Intercept)`)

hist(ranef(mod.0.a)$OBS_ID$`(Intercept)`)

ranef(mod.0.a)$OBS_ID$`(Intercept)`
ranef(mod.4.a)$OBS_ID$`(Intercept)`

# Generally the random effects are similar in mod.0 and as in mod.4.a
plot(ranef(mod.0.a)$OBS_ID$`(Intercept)` ~ ranef(mod.4.a)$OBS_ID$`(Intercept)`); abline(a = 0, b = 1)




### Model Summaries ----
#' Null model would give us an 8.3% positive bias
#' A model trained on just assessment time (west coast model) would give us a 5% positive bias.
#' Adding Haul-level covariates reduce variance slightly and increase positive bias slightly

# Excluding haul-level covariates (model 5) worsened bias and variance! 



# TODO ----

# - re-familiarize with the predict_clm function, how I calculate probabilities from log odds
# - interpret coefficients
# - viability by observer_ID?
obs_viab <- hlbt_dat.scale[, .N, keyby = .(OBS_ID, VIABILITY)]
obs_viab[, TOTAL := sum(N), by = .(OBS_ID)]
setorder(obs_viab, TOTAL, VIABILITY)
obs_viab[, I := .GRP, by = .(OBS_ID)]
obs_viab[, TOTAL := formatC(TOTAL, width = max(nchar(TOTAL)))]
ggplot(obs_viab, aes(x = I, y = N, fill = factor(VIABILITY, levels = rev(levels(obs_viab$VIABILITY))))) + geom_col(position = "fill", width = 1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5), legend.position = "bottom") +
  geom_text(aes(label = TOTAL,y = 1), angle = 90, hjust = 1, vjust = 0.5, size = 2, check_overlap = T, nudge_y = 0.04) +
  guides(fill = guide_legend(reverse = T))

# What if we ordered by proportion 
obs_viab2 <- copy(obs_viab)
obs_viab2[, PROP := N/sum(N), by = .(OBS_ID)]
obs_viab2[, E_PROP := PROP[VIABILITY == "E"], by = .(OBS_ID)]
obs_viab2[, D_PROP := PROP[VIABILITY == "D"], by = .(OBS_ID)]
setorder(obs_viab2, E_PROP, -D_PROP)
obs_viab2[, I := .GRP, by = OBS_ID]
ggplot(obs_viab2, aes(x = I, y = N, fill = factor(VIABILITY, levels = rev(levels(obs_viab$VIABILITY))))) + geom_col(position = "fill", width = 1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5), legend.position = "bottom") +
  geom_text(aes(label = TOTAL,y = 1), angle = 90, hjust = 1, vjust = 0.5, size = 2, check_overlap = T, nudge_y = 0.04) +
  guides(fill = guide_legend(reverse = T))
# Can see a fair amount of variability here. If we subset only observers with at least 1000 viabilitie, same pattern
ggplot(obs_viab2[TOTAL >= 1000], aes(x = as.factor(I), y = N, fill = factor(VIABILITY, levels = rev(levels(obs_viab$VIABILITY))))) + 
  geom_col(position = "fill", width = 1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5), legend.position = "bottom") +
  geom_text(aes(label = TOTAL,y = 1), angle = 90, hjust = 1, vjust = 0.5, size = 2, check_overlap = T, nudge_y = 0.04) +
  guides(fill = guide_legend(reverse = T)) + 
  scale_x_discrete(drop = T)


# Permits do seem to differ too.. but it could be explained by covariates too? Some permits might take longer tows or bigger hauls?
# What if I took my model outputs
permit_viab <- hlbt_dat.scale[, .N, keyby = .(PERMIT, VIABILITY)]
permit_viab[, TOTAL := sum(N), by = .(PERMIT)]
permit_viab[, PROP := N/sum(N), by = .(PERMIT)]
permit_viab[, E_PROP := PROP[VIABILITY == "E"], by = .(PERMIT)]
permit_viab[, D_PROP := PROP[VIABILITY == "D"], by = .(PERMIT)]
setorder(permit_viab, E_PROP, -D_PROP, VIABILITY)
permit_viab[, I := .GRP, by = .(PERMIT)]
permit_viab[, TOTAL := formatC(TOTAL, width = max(nchar(TOTAL)))]
ggplot(permit_viab, aes(x = I, y = N, fill = factor(VIABILITY, levels = rev(levels(obs_viab$VIABILITY))))) + geom_col(position = "fill", width = 1) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5), legend.position = "bottom") +
  geom_text(aes(label = TOTAL, y = 1), angle = 90, hjust = 1, vjust = 0.5, size = 2, check_overlap = T, nudge_y = 0.04) +
  geom_text(aes(label = PERMIT, y = 0), angle = 90, hjust = 1, vjust = 0.5, size = 2, check_overlap = T, nudge_y = 0.1) +
  guides(fill = guide_legend(reverse = T))

hlbt_dat.haul[, .(HAUL_MT = mean(HAUL_MT), TOW_DUR = mean(TOW_DUR)), by = PERMIT][order(HAUL_MT)] # HAUL_MT differes more by vessel than TOW_DUR
hlbt_dat.haul[, MEAN_HAUL_MT := mean(HAUL_MT), by = PERMIT]
setorder(hlbt_dat.haul, MEAN_HAUL_MT)
hlbt_dat.haul[, I := .GRP, by = PERMIT]
ggplot(hlbt_dat.haul, aes(x = HAUL_MT)) + geom_histogram() + facet_grid(I ~ ., scales = "free_y") + 
  stat_summary(aes(x = 0, y = HAUL_MT), fun.data = function(x) data.frame(xintercept = mean(x)), geom = "vline")
# Can I plot this next to proportions of halibut viabilities?


# Proportions of viability by vessel also appear to differ
viab_by_permit <- hlbt_dat.scale[, .N, by = .(PERMIT, VIABILITY)]
viab_by_permit[, TOTAL := sum(N), by = .(PERMIT)]
ggplot(viab_by_permit, aes(x = N, y = PERMIT,  fill = factor(VIABILITY, levels = rev(levels(obs_viab$VIABILITY))))) + 
  geom_col(position = "fill") +
  geom_text(aes(label = TOTAL), x = 1, hjust = 0, check_overlap = T, size = 2, nudge_x = 0.05) + 
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse = T))

#' TODO I wonder if these are similar to what we see now??

viab_by_permit_obs <- hlbt_dat.scale[, .N, by = .(PERMIT, VIABILITY, OBS_ID)]
viab_by_permit_obs[, TOTAL := sum(N), by = .(PERMIT, OBS_ID)]
ggplot(viab_by_permit_obs, aes(x = N, y = OBS_ID,  fill = factor(VIABILITY, levels = rev(levels(obs_viab$VIABILITY))))) + 
  facet_wrap(PERMIT ~ ., scales = "free") + 
  geom_col(position = "fill") +
  geom_text(aes(label = TOTAL), x = 1, hjust = 0, check_overlap = T, size = 2, nudge_x = 0.05) + 
  labs(fill = "Viability", y = "Badge", x = "Proportion") + 
  theme(legend.position = "bottom") + 
  guides(fill = guide_legend(reverse = T))


# Observers varied a lot by vessel as well...
#'* Plot by random intercept*

viab_by_obs <- hlbt_dat.scale[, .N, by = .(VIABILITY, OBS_ID)]
viab_by_obs[, TOTAL := sum(N), by = .(OBS_ID)]
mod.4.a.obs_id <- setnames(as.data.table(ranef(mod.4.a)$OBS_ID, keep.rownames = T), c("OBS_ID", "INTERCEPT"))
viab_by_obs <- viab_by_obs[mod.4.a.obs_id, on = .(OBS_ID)]
setorder(viab_by_obs, INTERCEPT)
viab_by_obs[, I := .GRP, by = .(OBS_ID)]

hist(ranef(mod.4.a)$OBS_ID$`(Intercept)`)

# Observers on the left have a lower intercept than those on the right (more likely to assign E than D)
ggplot(viab_by_obs, aes(x = I, y =  N, fill = factor(VIABILITY, levels = rev(levels(obs_viab$VIABILITY))))) + 
  #facet_wrap(PERMIT ~ ., scales = "free") + 
  geom_col(position = "fill", width = 1) +
  geom_text(aes(label = TOTAL), y = 1, hjust = 0, check_overlap = T, size = 2, nudge_x = 0.05) + 
  labs(fill = "Viability", x = "Badge", y = "Proportion") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 1)) + 
  guides(fill = guide_legend(reverse = T))
# Is there a way we can use these random effects to adjust the 'TRUE' values?


#' *Permit as a fixed effect?*
test1 <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat.scale)
test2 <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + PERMIT, data = hlbt_dat.scale)
summary(test2)  # It does look like PERMIT always comes out with a significant fixed effect...?
anova(test1, test2)  # AIC is reduced 3441.4 points, but how much does this help with predictions?
test1$fitted.values

### K-fold Cross Validation ----

# Using 10 groups? Can also repeat the sampling.
# What about Stratified K-fold? Typically stratified K-fold tries to split the data so that the responses are 
# I'm using group K-fold, stratifying by haul
# distributed among groups similarly (like if you had a binary response variable).
#' [https://machinelearningmastery.com/k-fold-cross-validation/]

# Is this better than just repeated train/test sampling?


# ML, using python
#' [https://www.youtube.com/watch?v=-8s9KuNo5SA] 



# Here is where some old stuff is here:
#' [C:\Users\geoff.mayhew\Work\GMayhew files\Documents\Halibut Deck Sorting EFP]
#' with `halibut_decksort_v3.R` the most recent file.
#' 
#' 


## Tutorial on CLMM ----

#' [https://user2021.r-project.org/participation/technical_notes/t186/technote/]


# Random Effects ----

dat_split <- split_train_test(hlbt_dat.scale, 0.5, seed = 12312908, by = "HAUL_ID")

m1.f <- clm(VIABILITY ~ ASSESSMENT_TIME, data = dat_split$train)
m1.m <- clmm(VIABILITY ~ ASSESSMENT_TIME + (1|OBS_ID/HAUL_ID) + (1|TRIP_ID/HAUL_ID), data = dat_split$train)

ranef(m1.m)  # this is a list, one for each random effect, with a value for each group
m1.m$ranef  # this is suppposed to be a list but only has the random effects from the first random term

length(m1.m$ranef)  # 14047, is this the number of unique groups?
uniqueN(dat_split$train[, .(HAUL_ID, OBS_ID, TRIP_ID)])  # No, I have 6725 groups
m1.m$ranef

ranef(m1.m)[[1]][, 1] == m1.m$ranef  # is this supposed to be a list but is unlisted?
lapply(ranef(m1.m), nrow)
sum(unlist(lapply(ranef(m1.m), nrow)))  # Ahhh. here we are...

table(m1.m$ranef ==  unname(unlist(ranef(m1.m))))  # so mod$ranef should be a list but its not, basically unlisted!


mean(m1.m$ranef)  # Mean across all - this is what is currently being used?
sapply(ranef(m1.m), function(x) mean(unlist(x)))  # Mean of each random effect term. Can see that TRIP_ID and OBS_ID are skewed high
mean(sapply(ranef(m1.m), function(x) mean(unlist(x))))  # Mean of the means
sum(sapply(ranef(m1.m), function(x) mean(unlist(x)))) 

hist(unlist(ranef(m1.m)[[3]])); abline(v = mean(unlist(ranef(m1.m)[[3]])), col = "red")  # trip_id
hist(unlist(ranef(m1.m)[[4]])); abline(v = mean(unlist(ranef(m1.m)[[4]])), col = "red")  # observer

# So should global mean be the means across all random effects - or by i?


library(lme4)

glmer(VIABILITY ~ ASSESSMENT_TIME + (1|OBS_ID/HAUL_ID) + (1|TRIP_ID/HAUL_ID),  )

library(mixor)

# UPDATE ORDINAL ----

# Can't install?
# devtools::install_github('runehaubo/ordinal')  # Downloading a more recent version than the cran version
library(ordinal)
?ordinal::clmm



## Using multiple models instead of proportional odds ----
#' [https://pubmed.ncbi.nlm.nih.gov/9762873/]
#' `polytomous logistic regression`, howver, it doesn't make use of the ordinality of the response. no cumulative odds ratios

# Instead use partial proportional odds model? But this doens't help when ALL of my covariates fail the test



# Testing Proportional ODDS Assumption -----

#' [https://www.bookdown.org/rwnahhas/RMPH/blr-ordinal.html#blr-po]
#' In general, however, such goodness-of-fit tests can lack power in small sample sizes and in *large sample sizes can*
#' *detect practically non-meaningful deviations from the assumption*

## Binary models

hlbt_dat.scale[, VIAB_BIN1 := factor(fcase(
  VIABILITY == "E", "E",
  VIABILITY %in% c("P", "D"), "PD"), 
  levels = c("E", "PD")
)][, VIAB_BIN2 := factor(fcase(
  VIABILITY %in% c("E", "P"), "EP",
  VIABILITY == "D", "D"), 
  levels = c("EP", "D")
)]

# fit proportional odds model
mod.polr <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat.scale)
# fit two binary models
mod.glm1 <- glm(VIAB_BIN1 ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat.scale, family = "binomial")
mod.glm2 <- glm(VIAB_BIN2 ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat.scale, family = "binomial")

mod.polr$beta
mod.glm1$coefficients[-1]   # Can see coefficients differ
mod.glm2$coefficients[-1]

# so how would you use this model? 



# TODO Also do this - using coefficients from separate models? ----

#' [https://www.restore.ac.uk/srme/www/fac/soc/wie/research-new/srme/modules/mod5/9/index.html]



# Can I get rmsb to work? ----

library(rmsb)

# installing cmdstanr
install.packages('cmdstanr', repos='https://mc-stan.org/r-packages',lib='/usr/local/lib/R/site-library')
#' *use personal library? Yes*
cmdstanr::check_cmdstan_toolchain(fix = TRUE)
cmdstanr::install_cmdstan(cores=10)
# NOTE: Please add C:/Users/geoff.mayhew/.cmdstan/cmdstan-2.36.0/stan/lib/stan_math/lib/tbb to your PATH variable.


#' [https://hbiostat.org/r/examples/blrm/blrm]
options(mc.cores = parallel::detectCores() - 1) 

wstan <- c('cmdstan', 'rstan')[2]
rfile <- function(f) {
  require(rmsb)
  paste0(as.character(substitute(f)), if(wstan == 'cmdstan') 'c', '.rds')
}

psigma <- function(r, a, inline=FALSE, pr=! inline) {
  sigma <- abs(log(r)) / qnorm(1 - a)
  dir <- if(r > 1.) '>' else '<'
  x <- if(inline) paste0('$\\Pr(\\text{OR}', dir, r, ') =', a,
                         ' \\Rightarrow \\sigma=', round(sigma, 3), '$')
  else paste0('Pr(OR ', dir, ' ', r, ') = ', a, ' ⇒ σ=', round(sigma, 3))
  if(inline) return(x)
  if(pr) {
    cat('\n', x, '\n\n', sep='')
    return(invisible(sigma))
  }
  sigma
}
. <- function(...) list(...)


set.seed(1)
n <- 500
x1 <- runif(n, -1, 1)
x2 <- runif(n, -1, 1)
x3 <- sample(0 : 1, n, TRUE)
y <- x1 + 0.5 * x2 + x3 + rnorm(n)
y <- as.integer(cut2(y, g=10))
dd <- datadist(x1, x2, x3); options(datadist='dd')
f <- lrm(y ~ x1 + pol(x2, 2) + x3, eps=1e-7) # eps to check against Stan        # By default, data looks at global environment 
f



# Define a function that creates a `pcontrast` for `blrm`
# Skepticism of prior is specified by making changes in Y be small
# as x2 goes from -1 to 0 to 1
# Pr(OR > 2) = p
con <- function(p)
  list(sd=psigma(2, p),
       c1=.(x2=0), c2=.(x2=-1), c3=.(x2=1), c4=.(x2=0),
       contrast=expression(c1-c2, c3-c4))
k <- NULL
for(p in c(.01, .05, .1, .2)) {
  g <- blrm(y ~ x1 + pol(x2, 2) + x3, method='optimizing',
            pcontrast=con(p))
  cat('-2 log likelihood:', g$deviance, '\n')
  k <- rbind(k, g$coefficients)
}

k


options(rmsb.backend='cmdstan')
bs <- blrm(y ~ x1 + pol(x2, 2) + x3, file=rfile(bs))


blrmStats(bs, pl=TRUE)
stanDxplot(bs)
stanDx(bs)

# wow, that was a ton of work for basically the same answer -__-
cbind(MLE=coef(f), t(bs$param))

plot(MLE ~ mean, data = cbind(MLE=coef(f), t(bs$param))); abline(a = 0, b = 1)

round(diag(vcov(f)) / diag(vcov(bs)), 2)
contrast(f,  list(x1=0, x3=1), list(x1=.25, x3=0))
k <- contrast(bs, list(x1=0:1, x3=1), list(x1=.25, x3=0))
k



# fit using frequentist POLR
sdfs <- lrm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = dat_split$train)




## Trying this on my data... ----
dat_split <- split_train_test(hlbt_dat.scale, 0.5, seed = 12312908, by = "HAUL_ID")

# started around 2:15pm
heregoesnothing <- blrm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, file=rfile(hlbt_blrm), data = dat_split$train)
# It ran in 1187 seconds (19.78 min) without random effects, but is it the saving that is taking a while? 564 mb

coef(heregoesnothing)
heregoesnothing
#' What does Pr(Beta>0) mean and why is it 1 for time, haul_mt, and tow_dur?

# Started at 2:59pm. *Relacting parallel odds assumption for all covariates. 1564 seconds to finish chains. Saved at 3:34
heregoesnothing.partial <- blrm(
  VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG,
  ppo = ~ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG,
  file=rfile(hlbt_blrm), data = dat_split$train)


heregoesnothing  # p-value is 1 for time, haul, and tow
heregoesnothing.partial  # p-value is < 0.05 for time where y>=D

coef(heregoesnothing)
matrix(coef(heregoesnothing.partial )[-(1:2)], ncol = 2) #' *Weird, so time out of water goes from positive to negative...*
# How does that make any sense when the data strongly suggests otherwise?

plot(VIABILITY ~ ASSESSMENT_TIME, data = dat_split$train)
plot(VIABILITY ~ ASSESSMENT_TIME, data = dat_split$train[VIABILITY != "E"]) # Removing 'E'... the effect is less apparent but wouldn't sugest the opposite direction...


# Testing PO assumption via graphing rather than test? ----

#' [https://groups.google.com/g/medstats/c/y_94cReelQg]

#' Frank Harrell
#' I saw a paper on this but could not find it in my bibliographic database.  In general, the only time grouping will 
#' help is if there are levels of Y that are in the wrong order.
#' Note that the test for PO in SAS (invented by a former PhD student of mine Bercedis Peterson) was shown by Peterson 
#' to be anti-conservative.  A better way to assess PO is through partial residuals plots using different Y cutoffs, or 
#' fitting a sequence of binary models for all cutoffs of Y and plotting the log odds ratios against the cutoff.  For 
#' the latter here's an example using R:
  
require(rms)
y <- as.factor(mydata$y)
Y <- as.numeric(y) - 1
ncut <- length(unique(Y)) - 1
p <- ...  # total no. of coefficients less intercepts
Coef <- matrix(NA, ncol=p, nrow=ncut,
               dimnames=list(paste('>=', levels(y)[-1],sep=''),
                             NULL))
for(k in 1:ncut) {
  f <- lrm(Y >= k ~ x1 + x2 + ..., data=mydata)
  Coef[k,] <- coef(f)[-1]
}
colnames(Coef) <- names(coef(f))[-1]
round(Coef, 3)


# pomcheckr: Graphical Check for Proportional Odds Assumption
#' [https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/]
#' One of the assumptions underlying ordinal logistic (and ordinal probit) regression is that the relationship between 
#' each pair of outcome groups is the same. In other words, ordinal logistic regression assumes that the coefficients 
#' that describe the relationship between, say, the lowest versus all higher categories of the response variable are the 
#' same as those that describe the relationship between the next lowest category and all higher categories, etc. This is
#' called the proportional odds assumption or the parallel regression assumption. Because the relationship between all 
#' pairs of groups is the same, there is only one set of coefficients.
#' [https://cran.r-project.org/web/packages/pomcheckr/pomcheckr.pdf]



library(pomcheckr)  # This packages kind of blows

po_results <- pomcheck(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat.scale)
plot(po_results)

# These arent' scaled to zero though
ggplot(melt(as.data.table(po_results[[1]]), id.vars = c("ASSESSMENT_TIME")), aes(y = ASSESSMENT_TIME, x = value, color = variable)) + 
  geom_point() + geom_point()

pomcheckr:::plot.pomcheck


x <- copy(po_results); legend.position = "none"; idx <- 1
# re-writing it the way I think it was intended
new_fun <- function (x, legend.position = "none", ...) 
{
  assertthat::assert_that(inherits(x, "pomcheck"), msg = "x must be a pomcheck object")
  for (idx in seq_along(x)) {
    res1 <- x[[idx]]
    tmp <- attr(res1, "variable")
    nc <- ncol(res1)
    if (any(rowSums(sapply(res1[, 2:nc], is.finite)) >= 2)) {
      res2 <- cbind(res1[, 1], res1[, 3:nc])   # cbind(res1[, 1], res1[, 3:nc] - res1[, 3:(nc - 1)])
      to_zero <- res1[, 3]
      res3 <- cbind(res1[, 1], res1[, 3:nc] - unlist(rep(to_zero, times = 2)))
      print(res3 %>% tidyr::pivot_longer(-c(.data[[tmp]]), 
                                         names_to = "label") %>% dplyr::filter(is.finite(.data$value)) %>% 
              ggplot2::ggplot() + ggplot2::geom_point(mapping = ggplot2::aes(x = .data$value, 
                                                                             y = .data[[tmp]], color = .data$label)) + ggplot2::labs(y = tmp, 
                                                                                                                                     x = "logit") + ggplot2::scale_colour_discrete(labels = function(x) stringr::str_wrap(x, 
                                                                                                                                                                                                                          width = 10, whitespace_only = FALSE)) + ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, 
                                                                                                                                                                                                                                                                                                                                   width = 10)) + ggplot2::xlim(NA, 0) + ggplot2::theme(legend.position = legend.position))
    }
    else {
      message(paste0("Unable to generate plot for ", tmp, 
                     ". Counts must be > 0 in at least 3 categories in\n                     order to calculate proportional odds."))
    }
  }
}
# Yea, these don't look that bad to me...
new_fun(po_results)




# how does glmer handle random intercepts in predictions? ----
hlbt_dat.scale

library(lme4)

system.time(test <- glmer(VIAB_BIN1 ~ ASSESSMENT_TIME.s + (1|HAUL_ID), data = hlbt_dat.scale, family = "binomial"))

predict(test)

?lme4:::predict.merMod()

a1 <- predict(test, re.form = ~0)
a2 <- predict(test)

test.r <- ranef(test)
nrow(test.r$HAUL_ID)  # One for each HAUL_ID

a2

coef(test)
fixef(test)

predict(test, type = "response")  # probability


hlbt_dat.scale[1, .(ASSESSMENT_TIME, HAUL_ID)]
fixef(test)

hist(ranef(test)[["HAUL_ID"]])

head(unname(a1), 1)
head(unname(a2), 1)
predict(test, type = "response")[[1]]                # 0.1086581   # probability
predict(test, re.form = ~0, type = "response")[[1]]  # 0.224601    # probability

hlbt_dat.scale[1, ASSESSMENT_TIME * fixef(test)[["ASSESSMENT_TIME.s"]]]

isthisit <- fixef(test)[["(Intercept)"]] + (hlbt_dat.scale[1, ASSESSMENT_TIME] * fixef(test)[["ASSESSMENT_TIME.s"]])

exp(isthisit / (1 - isthisit))

ranef(test)[['HAUL_ID']][[1]][[1]]   #  -0.8654691

isthisit2 <- fixef(test)[["(Intercept)"]] + (hlbt_dat.scale[1, ASSESSMENT_TIME] * fixef(test)[["ASSESSMENT_TIME.s"]])  - 0.8654691
exp(isthisit2 / (1 - isthisit2))


plogis(predict(test, re.form = ~0)[[1]])

p_fun <- function(x) exp(x) / (1 + exp(x))  # this is just plogis

p_fun( exp( fixef(test)[["(Intercept)"]] ))
p_fun( exp( ranef(test)[['HAUL_ID']][[1]][[1]]   ))

p_fun( exp( fixef(test)[["(Intercept)"]] * (hlbt_dat.scale[1, ASSESSMENT_TIME] * fixef(test)[["ASSESSMENT_TIME.s"]])  ))

p_fun( exp( fixef(test)[["(Intercept)"]] * (hlbt_dat.scale[1, ASSESSMENT_TIME] * fixef(test)[["ASSESSMENT_TIME.s"]]) * -0.8654691 ))


p_fun( fixef(test)[["(Intercept)"]] ) * p_fun( (hlbt_dat.scale[1, ASSESSMENT_TIME] * fixef(test)[["ASSESSMENT_TIME.s"]]) )

# intercept only
p_fun( fixef(test)[["(Intercept)"]] )
# intercept and coef * ASSESSMENT_TIME
p_fun( fixef(test)[["(Intercept)"]] ) * p_fun( (hlbt_dat.scale[1, ASSESSMENT_TIME] * fixef(test)[["ASSESSMENT_TIME.s"]]) )

fixef(test)[["(Intercept)"]] * (hlbt_dat.scale[1, ASSESSMENT_TIME] * fixef(test)[["ASSESSMENT_TIME.s"]])

exp(fixef(test))


coefs <- fixef(test)

odds_i <- exp(fixef(test)[["(Intercept)"]])
odds_i / (1 + odds_i)
# convert to probability
odds_p <- exp(coefs[[1]]) * exp(coefs[[2]])
odds_p / (1 + odds_p)



plogis(coefs[[1]])
p_fun(coefs[[1]])

mean(predict(test, type = "response", re.form = NA)) == plogis(fixef(test)[[1]])


# Confirmed here
predict(test, type = "response", re.form = ~0)[[1]]
plogis(predict(test, re.form = ~0)[[1]])

# Logit with no random effects
predict(test, re.form = ~0)[[1]]

fixef(test)


toow <- hlbt_dat.scale[1, ASSESSMENT_TIME.s]

fixef(test)[[1]]

shit <- fixef(test)[[2]] * toow


head(unname(predict(test, type = "response", re.form = ~0)))
plogis(head(unname(predict(test, re.form = ~0))))

fixef(test)[[1]] + fixef(test)[[2]] * toow

# I just don't see how you get 0.224 from my coefficients!!!
fixef(test)[[1]]


head(unname(predict(test, type = "response")))
head(unname(fitted(test)))   #' `fitted` gets you the same as `response`, the probabilities 


hist(plogis(coefs[[1]] + coefs[[2]] * hlbt_dat.scale$ASSESSMENT_TIME.s) - predict(test, type = "response", re.form = ~0))


# re.form = ~0 just does the predictions witout any sort of random effects (uses the coefficients by themselves!)


# Observer Effects ----

full.ranef <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + (1|HAUL_ID) + (1|OBS_ID) + (1|TRIP_ID), data = hlbt_dat.scale)

names(ranef(full.ranef))

obs_ranef <- data.table(ranef(full.ranef)$OBS_ID, keep.rownames = "OBS_ID")
obs_ranef_dt <- data.table(full.ranef$model)[, .(Count = as.numeric(.N)), by = .(OBS_ID, VIABILITY)][obs_ranef, on = .(OBS_ID)]
obs_ranef_dt[, Proportion := Count / sum(Count), by = .(OBS_ID)]
setorder(obs_ranef_dt, OBS_ID, VIABILITY)

obs_effect_no_permit <- ggplot(
  melt(obs_ranef_dt, id.vars = c("OBS_ID", "VIABILITY", "(Intercept)"), value.vars = c("Count", "Proportion")),
  aes(x = `(Intercept)`, y = value, fill = VIABILITY, group = OBS_ID)
) + facet_grid(variable ~ ., scales = "free_y") + 
  geom_col(width = 0.005) + scale_fill_viridis_d(direction = -1) + 
  theme_bw() + theme(legend.position = "bottom") + labs(fill = "Viability", x = "(1|OBS_ID) Random intercept")
obs_effect_no_permit
# Which observer graded things 'E' more heavily?


# Can we see how observers differ on the same permit? and trip?

hlbt_dat[, uniqueN(HAUL_ID), by = .(TRIP_ID)][order(-V1)]

hlbt_dat |>
  _[TRIP_ID == 138, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
# The observers had roughly similar proportions of E, but one assessed 50% more P than D halibut
ggplot(hlbt_dat[TRIP_ID == 138], aes(x = HAUL_ID, fill = VIABILITY)) + facet_grid(OBS_ID ~ .) + geom_bar(color = "black") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

hlbt_dat |>
  _[TRIP_ID == 125, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
# So even though mean TOOW was higher for one observer, proportion of E was much higher than for other observers
ggplot(hlbt_dat[TRIP_ID == 125], aes(x = HAUL_ID, fill = VIABILITY)) + facet_grid(OBS_ID ~ .) + geom_bar(color = "black") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# Some of these patterns could be driven by large hauls with lots of large excellent halibut sorted quickly...?
# Based on the model prediction (i.e., using covariates, we could see what we would 'expect' and how much observer might differ?

hlbt_dat |>
  _[TRIP_ID == 236, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
# Pretty similar here, just a slight difference in P and D.
ggplot(hlbt_dat[TRIP_ID == 236], aes(x = HAUL_ID, fill = VIABILITY)) + facet_grid(OBS_ID ~ .) + geom_bar(color = "black") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

hlbt_dat |>
  _[TRIP_ID == 132, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
# TOOW was much higher for one observer with higher D proportion, all seemed to come from hauls with huge halibut bycatch
ggplot(hlbt_dat[TRIP_ID == 132], aes(x = HAUL_ID, fill = VIABILITY)) + facet_grid(OBS_ID ~ .) + geom_bar(color = "black") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



a <- hlbt_dat[, .(HAUL_N = uniqueN(HAUL_ID)), keyby = .(PERMIT, TRIP_ID, HAUL_ID, OBS_ID)]
# Can we find PERMIT and OBS_ID matches across trips (multiple trips with the same set of observers?)
setorder(a, PERMIT, TRIP_ID, HAUL_ID, OBS_ID)
a[, OBS_SET := paste0(unique(OBS_ID), collapse = "."), by = .(PERMIT, TRIP_ID)]
a[, OBS_GRP := .GRP, by = .(OBS_SET)]

a_smry <- a[, .(HAUL_N = .N), by = .(OBS_GRP)]
head(a_smry[order(-HAUL_N)], 10)

# These two observers each did ~ 100 hauls on the same vessel, with ~770 halibut each, but differing proportions. One
# Observer did generally have a higher mean TOOW
grp_254 <- hlbt_dat[HAUL_ID %in% a[OBS_GRP == 254, unique(HAUL_ID)]]
grp_254 |>
  _[, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
grp_254[, MEAN_TOOW := round(mean(ASSESSMENT_TIME)), by = .(HAUL_ID)]
ggplot(grp_254, aes(x = HAUL_ID)) + facet_grid(OBS_ID ~ .) + geom_bar(color = "black", aes( fill = VIABILITY)) +
  geom_text(aes(y = after_stat(count), label = MEAN_TOOW), stat = "count", position = "stack", angle = 90, size = 3, color = "gray40") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# It looks like all the D halibut with one observer tended to show up at the start during some larger tows., but the other observer didn't see
# the same high proportions of D. It This looks like most of the differences can be attributed to toow?


# Also pretty similar

grp_114 <- hlbt_dat[HAUL_ID %in% a[OBS_GRP == 114, unique(HAUL_ID)]]
grp_114 |>
  _[, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
grp_114[, MEAN_TOOW := round(mean(ASSESSMENT_TIME)), by = .(HAUL_ID)]
grp_114[, HAUL_n := .N, by = .(HAUL_ID)]
ggplot(grp_114, aes(x = HAUL_ID)) + facet_grid(OBS_ID ~ .) + geom_bar(color = "black", aes( fill = VIABILITY)) +
  geom_text(aes(y = after_stat(count), label = MEAN_TOOW), stat = "count", position = "stack", angle = 90, size = 3, color = "gray40", hjust = -1) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(subtitle = paste0("Permit:", unique(grp_114$PERMIT), ". Annoations = mean time-out-of-water"))

## Example differences between observers on the same vessel and trips ----

# Looks like some larger differences here, not easily explained by TOOW
grp_184 <- hlbt_dat[HAUL_ID %in% a[OBS_GRP == 184, unique(HAUL_ID)]]
grp_184 |>
  _[, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
grp_184[, MEAN_TOOW := round(mean(ASSESSMENT_TIME)), by = .(HAUL_ID)]
grp_184[, HAUL_n := .N, by = .(HAUL_ID)]
ggplot(grp_184, aes(x = HAUL_ID)) + facet_grid(OBS_ID ~ .) + geom_bar(color = "black", aes( fill = VIABILITY)) +
  geom_text(aes(y = after_stat(count), label = MEAN_TOOW), stat = "count", position = "stack", angle = 90, size = 3, color = "gray40", hjust = -1) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(subtitle = paste0("Permit:", unique(grp_184$PERMIT), ". Annoations = mean time-out-of-water"))
# It looks like all the D halibut with one observer tended to show up at the start during some larger tows., but the other observer didn't see
# the same high proportions of D. It This looks like most of the differences can be attributed to toow?

grp_278 <- hlbt_dat[HAUL_ID %in% a[OBS_GRP == 278, unique(HAUL_ID)]]
grp_278 |>
  _[, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
grp_278[, MEAN_TOOW := round(mean(ASSESSMENT_TIME)), by = .(HAUL_ID)]
grp_278[, HAUL_n := .N, by = .(HAUL_ID)]
# Not as obvious, but one observer had over twice the proportion of D
ggplot(grp_278, aes(x = HAUL_ID)) + facet_grid(OBS_ID ~ .) + geom_bar(color = "black", aes( fill = VIABILITY)) +
  geom_text(aes(y = after_stat(count), label = MEAN_TOOW), stat = "count", position = "stack", angle = 90, size = 3, color = "gray40", hjust = -1) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(subtitle = paste0("Permit:", unique(grp_278$PERMIT), ". Annoations = mean time-out-of-water"))



#======================================================================================================================#
# How the model uses the E|P and P|D thresholds with the logits to get the probabilities ----

predict_clm
mod.4.a$alpha  # Here are my thresholds
# For each threshold, I subtract my coefficient sums from the thresholds when I run plogis
# for(j in 1:length(mod$alpha)){
#   res[, j] <- exp(mod$alpha[[j]] - coef_sum) / (1 + exp(mod$alpha[[j]] - coef_sum)) - rowSums(res, na.rm=T) 
# }

check <- data.table(mod.4.a$model)[1]
check  # I don't have intercepts in my models because instead I have thresholds

col_names <- names(mod.4.a$beta)
coef_sum <- sum(mod.4.a$beta * unlist(check[, ..col_names]))

# nice, I did it
c(
  E = plogis(mod.4.a$alpha[[1]] - coef_sum ),
  P = plogis(mod.4.a$alpha[[2]] - coef_sum ) - plogis(mod.4.a$alpha[[1]] - coef_sum ),
  D = 1 - (plogis(mod.4.a$alpha[[1]] - coef_sum ) + plogis(mod.4.a$alpha[[2]] - coef_sum ) - plogis(mod.4.a$alpha[[1]] - coef_sum ))
)
predict_clm(mod.4.a)$Prob[1,]




# Error Structure? ----

# Basically 1:1? Does this error structure make sense or can it be simplified?
plot(ranef(mod.4.a)[["HAUL_ID:OBS_ID"]][[1]], ranef(mod.4.a)[["HAUL_ID:TRIP_ID"]][[1]])
# These are VERY similar but not identical...
head( cbind(ranef(mod.4.a)[["HAUL_ID:OBS_ID"]], ranef(mod.4.a)[["HAUL_ID:TRIP_ID"]]), 10 )

dat_50 <- split_train_test(hlbt_dat.scale, train_prop = 0.5, seed = 12345, by = "HAUL_ID")

t1 <- clmm(VIABILITY ~ ASSESSMENT_TIME + (1|HAUL_ID) + (1|OBS_ID) + (1|TRIP_ID), data = dat_50$train)
t2 <- clmm(VIABILITY ~ ASSESSMENT_TIME + (1|OBS_ID/HAUL_ID) + (1|TRIP_ID), data = dat_50$train)
t3 <- clmm(VIABILITY ~ ASSESSMENT_TIME + (1|OBS_ID/HAUL_ID) + (1|TRIP_ID/HAUL_ID), data = dat_50$train)

AIC(t1, t2, t3)   # AIC is identical between the first two
anova(t1, t2, t3)  # AIC is actually worse just based on having another parameter (hence the diff of 2, LR.stat of 0)

names(ranef(t1))
names(ranef(t2))  # Nesting HAUL in OBS didn't change the model, relative to having HAUL_ID alreade present
names(ranef(t3))

plot(ranef(t1)[["HAUL_ID"]][[1]], ranef(t2)[["HAUL_ID:OBS_ID"]][[1]])
table(ranef(t1)[["HAUL_ID"]][[1]] - ranef(t2)[["HAUL_ID:OBS_ID"]][[1]])  # identical


# Is TRIP_ID and OBS_ID the same? virtually equal, just rounding error
plot(ranef(t1)[["TRIP_ID"]][[1]], ranef(t3)[["TRIP_ID"]][[1]])
plot(ranef(t1)[["OBS_ID"]][[1]], ranef(t3)[["OBS_ID"]][[1]]) 


head(ranef(t1)[["HAUL_ID"]][[1]], 20)
head(ranef(t3)[["HAUL_ID:TRIP_ID"]][[1]], 20)
head(ranef(t3)[["HAUL_ID:OBS_ID"]][[1]], 20)

# These terms are basically identical
plot(ranef(t3)[["HAUL_ID:TRIP_ID"]][[1]], ranef(t3)[["HAUL_ID:OBS_ID"]][[1]])
# And just split in half compared to HAUL_ID by itself
plot(ranef(t1)[["HAUL_ID"]][[1]], ranef(t3)[["HAUL_ID:TRIP_ID"]][[1]] + ranef(t3)[["HAUL_ID:OBS_ID"]][[1]])




# TODO check for time between retrieval and sort time - was there shortwiring?  ----


# Temperature ----

hlbt_dat.scale

m1 <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat.scale)
m2 <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG + TMP_2M.s, data = hlbt_dat.scale)
anova(m1, m2)  # Temperature contributed nothing - what about interaction with time?

# Lowered AIC but barely... Try ABS from 
m3 <- clm(VIABILITY ~ ASSESSMENT_TIME * TMP_2M.s + HAUL_MT + TOW_DUR + WEIGHT_KG , data = hlbt_dat.scale)
anova(m1, m3)  # Temperature contributed nothing - what about interaction with time?

range(hlbt_dat.scale$TMP_2M)  # Try abs from 5 C

hlbt_dat.scale[, TMP_ABS5 := abs(5 - TMP_2M)]
m4 <- clm(VIABILITY ~ ASSESSMENT_TIME + TMP_ABS5 + HAUL_MT + TOW_DUR + WEIGHT_KG , data = hlbt_dat.scale)
anova(m1, m4)  # Hmm, slightly more useful, 675 point difference


hlbt_dat.scale[, TMP_ABS5.s := scale(TMP_ABS5)]
m5 <- clm(VIABILITY ~ ASSESSMENT_TIME * TMP_ABS5.s + HAUL_MT + TOW_DUR + WEIGHT_KG , data = hlbt_dat.scale)
anova(m1, m4, m5)  # Interaction is a little better but not much


c0 <-  clm(VIABILITY ~ 1 , data = hlbt_dat.scale)
c1 <- clm(VIABILITY ~ ASSESSMENT_TIME , data = hlbt_dat.scale)
c2 <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT, data = hlbt_dat.scale)
c3a <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG , data = hlbt_dat.scale)
c3b <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR , data = hlbt_dat.scale)
c4a <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat.scale)
c4b <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + TMP_ABS5, data = hlbt_dat.scale)
c5a <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR + TMP_ABS5 + WEIGHT_KG + TMP_ABS5, data = hlbt_dat.scale)
c5b <- clm(VIABILITY ~ ASSESSMENT_TIME * TMP_ABS5 + HAUL_MT + TOW_DUR + WEIGHT_KG, data = hlbt_dat.scale)
anova(c0, c1, c2, c3a, c3b, c4a, c4b, c5a, c5b)  # Yea, temperature is less useful than 

# TMP_ABS5 actually lowers AIC more than WEIGHT_KG... is more useful than TOW_DUR!
anova(c3a, c3b)
# Between adding WEIGHT_KG and TMP_ABS5, temperature was actually MORE useful?
anova(c3b, c4a, c4b)
# Adding Weight, then Weight + interaction of temperature and time
anova(c4b, c5a, c5b)
      
# Once we add random intercepts, mow much do things get better as covariates are added?


# TODO Once a model is made train on year 1 to test on year 2, and vice versa   -----



# Permit as a fixed effect   -----

c5o <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG, data = hlbt_dat.scale)
c5p <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + PERMIT , data = hlbt_dat.scale)
anova(c5o, c5p)  # Permit actually has a HUGE effect! Less than assessment time but more than HAUL_MT

# Now the mixed version
c5p.m <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + PERMIT  + (1|HAUL_ID) + (1|OBS_ID) + (1|TRIP_ID), data = hlbt_dat.scale)
anova(c5o, c5p, c5p.m)
# Random effects reduced AIC by another 16,820!

cw   <-  clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG, data = hlbt_dat.scale)
cw.m <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + (1|HAUL_ID) + (1|OBS_ID) + (1|TRIP_ID), data = hlbt_dat.scale)
anova(cw, cw.m)  # reduced by 20K here
anova(cw.m, c5p.m) # Adding PERMIT adds a bunch of df, so AIC is actually reduced only marginally.

# How much do predictions improve though?
oci(table(predict_clm(cw.m)$Class, hlbt_dat.scale$VIABILITY))
oci(table(predict_clm(c5p.m)$Class, hlbt_dat.scale$VIABILITY))   #' @TODO predict_clm doesn't function correctly with categorical FE!


# Can I add Permit as a RE?
cw.mp <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + (1|PERMIT) +  (1|HAUL_ID) + (1|OBS_ID) + (1|TRIP_ID), data = hlbt_dat.scale)
anova(cw.m, c5p.m, cw.mp)

c5p$beta[names(c5p$beta) %like% "PERMIT"]
points(y = unname(c5p$beta[names(c5p$beta) %like% "PERMIT"]), x = rep(0, times = 16)) ; abline(c5p$beta[names(c5p$beta) %like% "PERMIT"])
boxplot(c5p$beta[names(c5p$beta) %like% "PERMIT"], add = F) 


setdiff(
  rownames(ranef(cw.mp)[['PERMIT']]),
  sub("PERMIT", "", names(c5p$beta[names(c5p$beta) %like% "PERMIT"]))
)  # 1610 is what was set as 0


setNames(as.data.frame(ranef(cw.mp)[['PERMIT']], row.names = T), c("PERMIT"))

test <- rbind(
  data.frame(
    x = "RANEF",
    PERMIT = rownames(as.data.frame(ranef(cw.mp)[['PERMIT']])),
    y = ranef(cw.mp)[['PERMIT']][["(Intercept)"]]
  ),
  rbind(
    data.frame(
      x = "FIXEF",
      PERMIT = sub("PERMIT", "", names(c5p$beta[names(c5p$beta) %like% "PERMIT"])),
      y = unname(c5p$beta[names(c5p$beta) %like% "PERMIT"])
    ),
    data.frame(x = "FIXEF", PERMIT = 1610, y = 0)
  )
)
boxplot(y ~ x, data = test)



test <- unique(hlbt_dat.scale[, .(PERMIT, OBS_ID, HAUL_ID)]) |>
  _[, .N, by = .(PERMIT, OBS_ID)]
ggplot(test, aes(x = OBS_ID, y = PERMIT, fill = N)) + geom_tile() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
# Histogram of number of vessels assigned to each observer
hist(test[, .(VESSEL_N = uniqueN(PERMIT)), by = OBS_ID]$VESSEL_N, breaks = 6)
table(test[, .(VESSEL_N = uniqueN(PERMIT)), by = OBS_ID]$VESSEL_N)
test[, .(VESSEL_N = uniqueN(PERMIT)), by = OBS_ID][, sum(VESSEL_N > 1)/.N]  # 40% of observers were assigned to more than one boat.



# How many obseververs did eac permit hve?
test[, .(OBS_N = uniqueN(OBS_ID)), by = .(PERMIT)][order(-OBS_N)]  # 12/17 vessels had at least 15 observers


# How do RANEF of OBS_ID change when PERMIT is added?
hist(ranef(c5p.m)[["OBS_ID"]][[1]])
hist(ranef(cw.m)[["OBS_ID"]][[1]])
hist(ranef(cw.mp)[["OBS_ID"]][[1]])



test2 <- rbind(
  cbind("MOD" = "c5p.m", data.frame(OBS_ID = rownames(ranef(c5p.m)[["OBS_ID"]])), y = ranef(c5p.m)[["OBS_ID"]][[1]]),
  cbind("MOD" = "cw.m", data.frame(OBS_ID = rownames(ranef(cw.m)[["OBS_ID"]])), y = ranef(cw.m)[["OBS_ID"]][[1]]),
  cbind("MOD" = "cw.mp", data.frame(OBS_ID = rownames(ranef(cw.mp)[["OBS_ID"]])), y = ranef(cw.mp)[["OBS_ID"]][[1]])
)
ggplot(test2, aes(x = OBS_ID, y = y, fill = MOD)) + geom_col(position = "dodge") 


ggplot(test2, aes(x = MOD, y = y, fill = MOD)) + geom_col() + facet_wrap(~OBS_ID) + geom_hline(yintercept = 0)
# Random effects of OBS_ID don't change much when PERMIT is added. Effects are similar when PERMIT used as a fixed vs random effect.
# Biggest difference is when PERMIT is excluded entirely.

#' *how much did permit reduce the amount of variability accounted for by observers?*
cw.m
cw.mp  # Adding permit makes OBS_ID account for more variability and TRIP_ID account for less


# This took 20 and 45 minutes, respectively. Don't include tow duration!
system.time(mod.m.permit <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + (1|PERMIT) + (1|TRIP_ID) + (1|HAUL_ID) + (1|OBS_ID), data = hlbt_dat.scale))
system.time(mod.m.permit_towdur <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + TOW_DUR +  WEIGHT_KG + (1|PERMIT) + (1|TRIP_ID) + (1|HAUL_ID) + (1|OBS_ID), data = hlbt_dat.scale))

anova(mod.m.permit, mod.m.permit_towdur)  # tow duration only reduces AIC by 357

oci(table(predict_clm(mod.m.permit)$Class, hlbt_dat.scale$VIABILITY))
oci(table(predict_clm(mod.m.permit_towdur)$Class, hlbt_dat.scale$VIABILITY))   # tow dur actually makes preditions significant worse

mod.m.permit
mod.m.permit_towdur

# What if we nested things 
system.time(mod.nested <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + (1 | PERMIT / TRIP_ID / HAUL_ID) + (1 | OBS_ID), data = hlbt_dat.scale))
anova(c5p.m, cw.m, cw.mp, mod.nested)
# The nested model is the same as individual ones because I used unique identifiers.




# TODO make the same obs effects plot but with the random effects of PERMIT!


obs_ranef2 <- data.table(ranef(cw.mp)$OBS_ID, keep.rownames = "OBS_ID")
obs_ranef_dt2 <- data.table(cw.mp$model)[, .(Count = as.numeric(.N)), by = .(OBS_ID, VIABILITY)][obs_ranef2, on = .(OBS_ID)]
obs_ranef_dt2[, Proportion := Count / sum(Count), by = .(OBS_ID)]
setorder(obs_ranef_dt2, OBS_ID, VIABILITY)

obs_effect_no_permit
obs_effect_w_permit <- ggplot(
  melt(obs_ranef_dt2, id.vars = c("OBS_ID", "VIABILITY", "(Intercept)"), value.vars = c("Count", "Proportion")),
  aes(x = `(Intercept)`, y = value, fill = VIABILITY, group = OBS_ID)
) + facet_grid(variable ~ ., scales = "free_y") + 
  geom_col(width = 0.005) + scale_fill_viridis_d(direction = -1) + 
  theme_bw() + theme(legend.position = "bottom") + labs(fill = "Viability", x = "(1|OBS_ID) Random intercept")

# Still looks very similar
obs_effect_no_permit
obs_effect_w_permit

### new  predict_clm ----

mod.no_permit   <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG, data = hlbt_dat.scale)
mod.with_permit <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + PERMIT, data = hlbt_dat.scale)


pred.no_permit <- predict_clm(mod.no_permit)
pred.with_permit <- predict_clm(mod.with_permit)

oci(table(pred.no_permit$Class, hlbt_dat.scale$VIABILITY))
oci(table(pred.with_permit$Class, hlbt_dat.scale$VIABILITY))  # with permit, it goes up a bit but not much
oci(table(predict_clm(cw.m)$Class, hlbt_dat.scale$VIABILITY)) # mixed without permit, does better
oci(table(predict_clm(cw.mp)$Class, hlbt_dat.scale$VIABILITY)) # mixed with permit does slightly worse with classification but not much

table(pred.no_permit$Class, hlbt_dat.scale$VIABILITY)  # The models still don't assign ANY P condition halibut
table(pred.with_permit$Class, hlbt_dat.scale$VIABILITY) # With permit, it actually does worse with Dead halibut. It 
table(predict_clm(cw.m)$Class, hlbt_dat.scale$VIABILITY) # mixed models do assign some P halibut
table(predict_clm(cw.mp)$Class, hlbt_dat.scale$VIABILITY) # Slightly better with E, slightly worse with P and D





# TODO Short-wiring? Difference in time between retrieval and start of decksort? Need to QAQC the data?  ----

retrv_sort <- unique(hlbt_dat[, .(HAUL_ID, RETRV, SORTING_BEGIN_TIME)])
retrv_sort[, DIFF := as.numeric(SORTING_BEGIN_TIME - RETRV, units = "mins")]
retrv_sort  # SORTING_BEGIN_TIME IS MISSING SOMETIMES??
summary(retrv_sort)
retrv_sort[, table(year(RETRV), is.na(SORTING_BEGIN_TIME))]    # Argh.. no sort time start time for 2017

# At least in cases where we do have data, what do we have?
retrv_sort[!is.na(SORTING_BEGIN_TIME), hist(DIFF)]  # yea, some of this data is bad, wrong days. Not QAQC'd.
retrv_sort[!is.na(SORTING_BEGIN_TIME) & DIFF > 0, hist(DIFF) ] # Most are soon after, some are hours later
retrv_sort[DIFF > 600]   # is this true or just bad data?
hlbt_dat[HAUL_ID == 468]  # 6 E and 1 D, so probably bad data
hlbt_dat[HAUL_ID == 12543]  # all 9 E, so bad data
retrv_sort[DIFF > 180 & DIFF < 600] 
hlbt_dat[HAUL_ID == 1658]   # 3 P, 1 D
hlbt_dat[HAUL_ID == 1672]   # 1 E, 1 P
hlbt_dat[HAUL_ID == 5007]   # 2 E
hlbt_dat[HAUL_ID == 8345]   # 4 E, 1 P, 1 D



# In predictions, if we assume no error in the response variable, mixed models applied without their random effects will
# not necessarily perform better. However, I think that their predictions are more reliable than fixed models because 
# the covariates are more finely tuned to the relationship with viablity, absent of the effects of permits and vessels.



# Removing variablity to get 'true' modeled mortality ----

#' TODO Can I used mixed models to remove variability due to PERMIT and OBS_ID? and get model-based mortalities
#' that I can use as my 'TRUE' y-value instead of observer's viability-based DMR??

# took 34.7 minutes
system.time(ran_mod <- clmm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + (1|PERMIT) + (1|OBS_ID) + (1|TRIP_ID) + (1|HAUL_ID), data = hlbt_dat.scale))

ran_mod

# so can I exclude the random effects from PERMIT and OBS_ID?
ran_mod_c <- copy(ran_mod)
ran_mod_c

# would need to remove names from ran_mod_c$ST and from ranef()
ordinal:::ranef.clmm  # here is what gets ranef from the models


object <- copy(ran_mod_c)

asgn <- attributes(object$gfList)$assign   # Ivector of integer identifiers of random effects
gflevs <- lapply(object$gfList, levels)    # levels of the random effects
reind <- with(object$dims, factor(rep.int(seq_len(nretrms),  nlev.re * qi)))     # With all the random effects concatenated, this makes a vector of all RE Ids
relist <- split(object$ranef, reind)        # grabs the ranef object and splits by the index

# Have to edit ran_mod_c$gfList and ranfef, and dims?
object$ranef

attributes(object$gfList)  # OBS_ID is 3 and PERMIT is 4

names(object$gfList)

# Update gfList
new_gfList <- object$gfList[c("HAUL_ID", "TRIP_ID")]
setattr(new_gfList, "assign", 1:2L)
object$gfList <- new_gfList
# Update ranef
object$ranef <- object$ranef[(reind %in% c("1","2"))]
# Update dims
object$dims$nlev.re <- object$dims$nlev.re[1:2]
object$dims$nlev.gf <- object$dims$nlev.gf[1:2]
object$dims$qi <- object$dims$qi[1:2]
object$dims$nretrms <- 2
object$dims$ngf <- 2
object$dims$q <- length(object$ranef)
object$dims$nSTpar <- 2
object$ST <- object$ST[1:2]

ranef(object)   # I still see things listed under random effects, and the formula is unchanged, but at least ranef() works now


# look at formatRanef* function that gets gflevs

pred_no_permit_obsid <- predict_clm(object, ran_int = "actual")

table(pred_no_permit_obsid$Class) # I don't have a ton of P but better than any of the fixed models!

table(pred_no_permit_obsid$Class, hlbt_dat.scale$VIABILITY)  # huh... this is quite different from the original guesses..., very bad with D
oci(table(pred_no_permit_obsid$Class, hlbt_dat.scale$VIABILITY))  # 0.49, worse ?
sum(diag(table(pred_no_permit_obsid$Class, hlbt_dat.scale$VIABILITY))) / nrow(hlbt_dat.scale)  # 0.64 predicted correctly

system.time(fix_mod <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG, data = hlbt_dat.scale))
pred_fixed <- predict_clm(object)
table(pred_fixed$Class, hlbt_dat.scale$VIABILITY)  # huh... this is quite different from the original guesses..., very bad with D
oci(table(pred_fixed$Class, hlbt_dat.scale$VIABILITY))   # 0.57  # This has fewer perfect matches, but better?
sum(diag(table(pred_fixed$Class, hlbt_dat.scale$VIABILITY))) / nrow(hlbt_dat.scale)   # 0.586 predicted correct, much worse

anova(ran_mod, fix_mod) # AIC is 20K lower, so it SHOULD be better since haul_id and trip_id took most of the variation


# So my mixed model, when excluding PERMIT and OBSERVER intercepts, actually predicts class more frequently (but mistakes are also worse? What about haul-level DMRS and mortality?
# Neither model gets that close to predicting D class, which is probably also difficult to do

true_dmr_mort <- calc_dmr(hlbt_dat.scale)
fix.dmr <- calc_dmr(hlbt_dat.scale, fix_mod)
ran.dmr <- calc_dmr(hlbt_dat.scale, ran_mod)

fix.dmr
ran.dmr
pred_tbl <- rbind(
  cbind(MOD = "fix", fix.dmr),
  cbind(MOD = "ran", ran.dmr)
)

pred_tbl |>
  _[, TRUE_DMR := true_dmr_mort[pred_tbl, DMR, on = .(HAUL_ID)] 
  ][, TRUE_MORT := true_dmr_mort[pred_tbl, MORT_KG, on = .(HAUL_ID)] 
  ][, DMR_DIFF := DMR - TRUE_DMR
  ][, MORT_DIFF := MORT_KG - TRUE_MORT][]
pred_tbl.melt <- melt(pred_tbl, id.vars = c("HAUL_ID", "MOD", "PRESORTED_NUMBER", "EST_TOTAL_KG"), measure.vars = c("MORT_DIFF", "DMR_DIFF"))
ggplot(pred_tbl.melt, aes(x = MOD, y = value)) + 
  facet_grid(variable ~ ., scales = "free_y") + 
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_hline(yintercept = 0, color = "blue") +
  stat_summary(geom = "point", fun = mean, color = "black", shape = 4, size = 2)

pred_tbl[, sum(MORT_DIFF), by = .(MOD)]  # fixed is a lot closer in mortality
pred_tbl[, sd(MORT_DIFF), by = .(MOD)]  # fixed is a lot closer in mortality
pred_tbl[, mean(DMR_DIFF), by = .(MOD)]  # fixed is a lot closer in mortality   #' But I'm e
pred_tbl[, sd(DMR_DIFF), by = .(MOD)]  # fixed is a lot closer in mortality   #' But I'm e

# So, if I use these as my 'new true', how well do random models perform?
table(pred_no_permit_obsid$Class)  # predictions have a lot more excellent, but these classes are different from 
table(hlbt_dat.scale$VIABILITY)    # 

a <- copy(hlbt_dat.scale)
a[, MORT := fcase(VIABILITY == "E", 0.2, VIABILITY == "P", 0.55, VIABILITY == "D", 0.9)]
mean(a$MORT)
mean(pred_no_permit_obsid$Mort)  # Overall, predictions have mortality estimated as slightly less, 0.416 down to 0.401,  (0.401 - 0.416) / 0.416, 3% less
a[, NO_PERMIT_OBSID := pred_no_permit_obsid$Mort]
a[, MORT_DIFF := NO_PERMIT_OBSID - MORT]

hist(a[, .(MEAN_MORT_DIFF = mean(MORT_DIFF)), keyby = .(PERMIT)]$MEAN_MORT_DIFF) # By permit, looks pretty unbiased?
hist(a[, .(MEAN_MORT_DIFF = mean(MORT_DIFF)), keyby = .(OBS_ID)]$MEAN_MORT_DIFF) # By observer, distribution is a bit skewed
a[, .(N = .N, MEAN_MORT_DIFF = mean(MORT_DIFF)), keyby = .(OBS_ID)][order(MEAN_MORT_DIFF)]  # Bias ranges from -16.6% to + 0.93%


### Remvoving OBS_ID only. Make this a seaprate script for observer variabiltiy!! ----

# what my prediction gives me is the predicted mortality prob of each halibut taking into account permit, haul and trip,
# i.e., the prediction without each observer's effect. If I compare this to the actual data, my delta should show me 
# how each observer differed compared to the 'average' observer

# This was taking into account both permit and OBS_ID. What we we included permit effect as well?

object <- copy(ran_mod_c)

asgn <- attributes(object$gfList)$assign   # Ivector of integer identifiers of random effects
gflevs <- lapply(object$gfList, levels)    # levels of the random effects
reind <- with(object$dims, factor(rep.int(seq_len(nretrms),  nlev.re * qi)))     # With all the random effects concatenated, this makes a vector of all RE Ids
relist <- split(object$ranef, reind)        # grabs the ranef object and splits by the index

# Have to edit ran_mod_c$gfList and ranfef, and dims?
object$ranef

attributes(object$gfList)  # OBS_ID is 3 and PERMIT is 4

names(object$gfList)

# Update gfList
new_gfList <- object$gfList[c("HAUL_ID", "TRIP_ID", "PERMIT")]
setattr(new_gfList, "assign", c(1, 2, 4))
object$gfList <- new_gfList
# Update ranef
object$ranef <- object$ranef[(reind %in% c("1","2", "4"))]
# Update dims
object$dims$nlev.re <- object$dims$nlev.re[c(1,2,4)]
object$dims$nlev.gf <- object$dims$nlev.gf[c(1,2,4)]
object$dims$qi <- object$dims$qi[c(1,2,4)]
object$dims$nretrms <- 3
object$dims$ngf <- 3
object$dims$q <- length(object$ranef)
object$dims$nSTpar <- 3
object$ST <- object$ST[c(1,2,4)]

ranef(object)   # I still see things listed under random effects, and the formula is unchanged, but at least ranef() works now


# look at formatRanef* function that gets gflevs

pred_no_permit <- predict_clm(object, ran_int = "actual")

a <- copy(hlbt_dat.scale)
a[, MORT := fcase(VIABILITY == "E", 0.2, VIABILITY == "P", 0.55, VIABILITY == "D", 0.9)]
mean(a$MORT)
mean(pred_no_permit$Mort)  # Pretty close when permit is kept
a[, NO_PERMIT_OBSID := pred_no_permit$Mort]
a[, MORT_DIFF := NO_PERMIT_OBSID - MORT]

hist(a[, .(MEAN_MORT_DIFF = mean(MORT_DIFF)), keyby = .(PERMIT)]$MEAN_MORT_DIFF) # By permit, looks pretty unbiased?
hist(a[, .(MEAN_MORT_DIFF = mean(MORT_DIFF)), keyby = .(OBS_ID)]$MEAN_MORT_DIFF) # By observer, distribution is a bit skewed
a[, .(N = .N, MEAN_MORT_DIFF = mean(MORT_DIFF)), keyby = .(OBS_ID)][order(MEAN_MORT_DIFF)]  # Bias in est mortality weight ranges from -14.5% to + 10.8%
# Calc haul-level DMRs

a1 <- a[, .(DMR_DIFF = weighted.mean(MORT_DIFF, w = WEIGHT_KG)), by = .(OBS_ID, HAUL_ID)]
a1[, OBS_ID_MEAN := mean(DMR_DIFF), by = .(OBS_ID)]
ggplot(a1, aes(x = DMR_DIFF)) + facet_wrap(~OBS_ID, scales = "free_y") + geom_histogram() + geom_vline(xintercept = 0, color = "blue") + 
  theme_bw() + theme(panel.grid = element_blank()) + 
  geom_vline(aes(xintercept = OBS_ID_MEAN, color = OBS_ID_MEAN) , linewidth = 1) + 
  scale_color_gradient2(low = "red", high = "darkgreen", mid = "white", midpoint = 0) 
unique(a1[, .(OBS_ID, OBS_ID_MEAN)])[order(OBS_ID_MEAN)]  # Kind of like before, DMR bias should line up with mortality bias
# this is mean haul dmr 

# how about raw diff in mort prob?
a2 <- a[, .(MEAN_MORT_DIFF = mean(MORT_DIFF)), by = OBS_ID][order(MEAN_MORT_DIFF)]  # -0.145 to +0.109. more observers on the lower tail (more likely to classify D)
hist(a2$MEAN_MORT_DIFF); abline(v = 0)
ggplot(a, aes(x = MORT_DIFF)) + facet_wrap(~OBS_ID, scales = "free_y") + geom_histogram() + geom_vline(xintercept = 0, color = "blue") + 
  theme_bw() + theme(panel.grid = element_blank()) + 
  geom_vline(data = a2, aes(xintercept = MEAN_MORT_DIFF, color = MEAN_MORT_DIFF) , linewidth = 1) + 
  scale_color_gradient2(low = "red", high = "darkgreen", mid = "white", midpoint = 0) 


# I should be able to combine this with the vessel x observer combo list to see which ones have the biggest discrepancies (or use the ranef too)

ranef.obs_id <- setnames(as.data.table(ranef(ran_mod)$'OBS_ID', keep.rownames = T), c("OBS_ID", "Int"))
ranef.obs_id[, OBS_ID := as.factor(OBS_ID)]

permit_obs_combo <- hlbt_dat.scale |>
  _[, .(HLBT_N = .N), keyby = .(PERMIT, OBS_ID, TRIP_ID, HAUL_ID)               #' I need a list of hauls by group...
  ][, .(HAUL_N = .N, HLBT_N = sum(HLBT_N)), keyby = .(PERMIT, TRIP_ID, OBS_ID)
  ][, COMBO_ID := paste0(unique(OBS_ID), collapse = "."), keyby = .(PERMIT, TRIP_ID)
  ][, COMBO_GRP := .GRP, by = .(COMBO_ID)][]
permit_obs_combo |>
  # Merge in random effect
  _[, Int := ranef.obs_id[permit_obs_combo, Int, on = .(OBS_ID)]
    # find the largest differences in Int
  ][, MAX_DIFF := max(Int) - min(Int),  by = .(COMBO_GRP)
    # Total number of hauls in group
  ][, GRP_HAUL := sum(HAUL_N), by = .(COMBO_GRP)]
head(unique(permit_obs_combo[, .(COMBO_GRP, GRP_HAUL, MAX_DIFF)])[order(-MAX_DIFF)], 20)


# COMBO_GRP 122 had obvious large differences
permit_obs_combo[COMBO_GRP == 122]  # Yup, this is the one that I found already
obs_grp_dat <- hlbt_dat.scale[TRIP_ID %in% permit_obs_combo[COMBO_GRP == 122, unique(TRIP_ID)]]
obs_grp_dat |>
  _[, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
obs_grp_dat |>
  _[, MEAN_TOOW := round(mean(ASSESSMENT_TIME)), by = .(HAUL_ID)
  ][, HAUL_n := .N, by = .(HAUL_ID)][]
obs_grp_dat |>
  _[, Int := ranef.obs_id[obs_grp_dat, Int, on = .(OBS_ID)]]
obs_grp_dat[, Int2 := paste0(ifelse(Int > 0, paste0("+", round(Int, 4)), round(Int, 4 )))
][, OBS_ID_RANEF := paste0(OBS_ID, "  :  ", Int2)]
ggplot(obs_grp_dat, aes(x = HAUL_ID)) + facet_grid(OBS_ID_RANEF ~ .) + geom_bar(color = "black", aes( fill = VIABILITY)) +
  geom_text(aes(y = after_stat(count), label = MEAN_TOOW), stat = "count", position = "stack", angle = 90, size = 3, color = "gray40", hjust = -1) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(subtitle = paste0("Permit:", unique(obs_grp_dat$PERMIT), ". Annotations = mean time-out-of-water"))

# COMBO_GRP 129 also had large differences with many hauls
permit_obs_combo[COMBO_GRP == 129]  

obs_grp_dat <- hlbt_dat.scale[TRIP_ID %in% permit_obs_combo[COMBO_GRP == 129, unique(TRIP_ID)]]
obs_grp_dat |>
  _[, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
obs_grp_dat |>
  _[, MEAN_TOOW := round(mean(ASSESSMENT_TIME)), by = .(HAUL_ID)
  ][, HAUL_n := .N, by = .(HAUL_ID)][]
obs_grp_dat |>
  _[, Int := ranef.obs_id[obs_grp_dat, Int, on = .(OBS_ID)]]
obs_grp_dat[, Int2 := paste0(ifelse(Int > 0, paste0("+", round(Int, 4)), round(Int, 4 )))
  ][, OBS_ID_RANEF := paste0(OBS_ID, "  :  ", Int2)]
ggplot(obs_grp_dat, aes(x = HAUL_ID)) + facet_grid(OBS_ID_RANEF ~ .) + geom_bar(color = "black", aes( fill = VIABILITY)) +
  geom_text(aes(y = after_stat(count), label = MEAN_TOOW), stat = "count", position = "stack", angle = 90, size = 3, color = "gray40", hjust = -1) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(subtitle = paste0("Permit:", unique(obs_grp_dat$PERMIT), ". Annotations = mean time-out-of-water"))




permit_obs_combo[COMBO_GRP == 62]  
obs_grp_dat <- hlbt_dat.scale[TRIP_ID %in% permit_obs_combo[COMBO_GRP == 62, unique(TRIP_ID)]]
obs_grp_dat |>
  _[, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
obs_grp_dat |>
  _[, MEAN_TOOW := round(mean(ASSESSMENT_TIME)), by = .(HAUL_ID)
  ][, HAUL_n := .N, by = .(HAUL_ID)][]
obs_grp_dat |>
  _[, Int := ranef.obs_id[obs_grp_dat, Int, on = .(OBS_ID)]]
obs_grp_dat[, Int2 := paste0(ifelse(Int > 0, paste0("+", round(Int, 4)), round(Int, 4 )))
][, OBS_ID_RANEF := paste0(OBS_ID, "  :  ", Int2)]
ggplot(obs_grp_dat, aes(x = HAUL_ID)) + facet_grid(OBS_ID_RANEF ~ .) + geom_bar(color = "black", aes( fill = VIABILITY)) +
  geom_text(aes(y = after_stat(count), label = MEAN_TOOW), stat = "count", position = "stack", angle = 90, size = 3, color = "gray40", hjust = -1) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(subtitle = paste0("Permit:", unique(obs_grp_dat$PERMIT), ". Annotations = mean time-out-of-water"))
# Hmm, 12117 has a -0.985? Doesn't look like they had that many more E halibut
hlbt_dat.scale[OBS_ID == 12117, table(VIABILITY)]  # This observer does have a ton of E halibut though...
obs_grp_dat[, table(VIABILITY) / .N, by = OBS_ID]



permit_obs_combo[COMBO_GRP == 6]  
obs_grp_dat <- hlbt_dat.scale[TRIP_ID %in% permit_obs_combo[COMBO_GRP == 6, unique(TRIP_ID)]]
obs_grp_dat |>
  _[, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
obs_grp_dat |>
  _[, MEAN_TOOW := round(mean(ASSESSMENT_TIME)), by = .(HAUL_ID)
  ][, HAUL_n := .N, by = .(HAUL_ID)][]
obs_grp_dat |>
  _[, Int := ranef.obs_id[obs_grp_dat, Int, on = .(OBS_ID)]]
obs_grp_dat[, Int2 := paste0(ifelse(Int > 0, paste0("+", round(Int, 4)), round(Int, 4 )))
][, OBS_ID_RANEF := paste0(OBS_ID, "  :  ", Int2)]
ggplot(obs_grp_dat, aes(x = HAUL_ID)) + facet_grid(OBS_ID_RANEF ~ .) + geom_bar(color = "black", aes( fill = VIABILITY)) +
  geom_text(aes(y = after_stat(count), label = MEAN_TOOW), stat = "count", position = "stack", angle = 90, size = 3, color = "gray40", hjust = -1) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(subtitle = paste0("Permit:", unique(obs_grp_dat$PERMIT), ". Annotations = mean time-out-of-water"))
# Hmm, 12117 has a -0.985? Doesn't look like they had that many more E halibut
hlbt_dat.scale[OBS_ID == 12117, table(VIABILITY)]  # This observer does have a ton of E halibut though...
obs_grp_dat[, table(VIABILITY) / .N, by = OBS_ID]




 
obs_grp_dat <- hlbt_dat.scale[TRIP_ID %in% permit_obs_combo[COMBO_GRP == 19, unique(TRIP_ID)]]
obs_grp_dat |>
  _[, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
obs_grp_dat |>
  _[, MEAN_TOOW := round(mean(ASSESSMENT_TIME)), by = .(HAUL_ID)
  ][, HAUL_n := .N, by = .(HAUL_ID)][]
obs_grp_dat |>
  _[, Int := ranef.obs_id[obs_grp_dat, Int, on = .(OBS_ID)]]
obs_grp_dat[, Int2 := paste0(ifelse(Int > 0, paste0("+", round(Int, 4)), round(Int, 4 )))
][, OBS_ID_RANEF := paste0(OBS_ID, "  :  ", Int2)]
ggplot(obs_grp_dat, aes(x = HAUL_ID)) + facet_grid(OBS_ID_RANEF ~ .) + geom_bar(color = "black", aes( fill = VIABILITY)) +
  geom_text(aes(y = after_stat(count), label = MEAN_TOOW), stat = "count", position = "stack", angle = 90, size = 3, color = "gray40", hjust = -1) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(subtitle = paste0("Permit:", unique(obs_grp_dat$PERMIT), ". Annotations = mean time-out-of-water"))
# Looks pretty different here


obs_grp_dat <- hlbt_dat.scale[TRIP_ID %in% permit_obs_combo[COMBO_GRP == 131, unique(TRIP_ID)]]
obs_grp_dat |>
  _[, .(HAUL_N = uniqueN(HAUL_ID), VIAB_N = .N,  VIABILITY, MEAN_TOOW = mean(ASSESSMENT_TIME)), by = .(OBS_ID)
  ][, .N, keyby = .(OBS_ID, HAUL_N, VIAB_N, VIABILITY, MEAN_TOOW )
  ][, PROP := N / sum(N), by = .(OBS_ID, HAUL_N, VIAB_N, MEAN_TOOW )] |>
  dcast(OBS_ID + HAUL_N + VIAB_N + MEAN_TOOW  ~ VIABILITY, value.var = c("N", "PROP")) 
obs_grp_dat |>
  _[, MEAN_TOOW := round(mean(ASSESSMENT_TIME)), by = .(HAUL_ID)
  ][, HAUL_n := .N, by = .(HAUL_ID)][]
obs_grp_dat |>
  _[, Int := ranef.obs_id[obs_grp_dat, Int, on = .(OBS_ID)]]
obs_grp_dat[, Int2 := paste0(ifelse(Int > 0, paste0("+", round(Int, 4)), round(Int, 4 )))
][, OBS_ID_RANEF := paste0(OBS_ID, "  :  ", Int2)]
ggplot(obs_grp_dat, aes(x = HAUL_ID)) + facet_grid(OBS_ID_RANEF ~ .) + geom_bar(color = "black", aes( fill = VIABILITY)) +
  geom_text(aes(y = after_stat(count), label = MEAN_TOOW), stat = "count", position = "stack", angle = 90, size = 3, color = "gray40", hjust = -1) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) + 
  labs(subtitle = paste0("Permit:", unique(obs_grp_dat$PERMIT), ". Annotations = mean time-out-of-water"))
# 9200 has almost no D halibut, 11661 has lots




# Effect of permit? ----

hist(ranef(ran_mod)$`OBS_ID`$`(Intercept)`)
hist(ranef(ran_mod)$`PERMIT`$`(Intercept)`)  # Not as big of a range as OBS_ID

setNames(as.data.table(ranef(ran_mod)$`PERMIT`, keep.rownames = T), c("PERMIT", "Int"))



ranef_dt <- rbindlist(
  lapply(ranef(ran_mod), function(x) setNames(as.data.table(x, keep.rownames = T), c("ID", "Int"))), 
  idcol = "Ranef")
range(ranef_dt$Int)  # Crazy - some hauls have HUGE intercepts
ggplot(ranef_dt, aes(x = Int)) + facet_wrap(~ Ranef, scales = "free_y", ncol = 1) + geom_histogram() + geom_vline(xintercept = 0)
# Wow, so some hauls and trips have huge intercepts compared to OBS_ID and PERMIT


# Can I use this to get a 'true unbiased mort prob' for each halibut?
# I train my models using the observer's viability class, but I test using modeled mortality probality from the full model?
# I'd probably have to use the same model to get the 'true prob' because I can't evaluate across models if they have different baselines...


#======================================================================================================================#

# Resource on measurement error in dependent variable ----

#' [https://stats.stackexchange.com/questions/129991/why-doesnt-measurement-error-in-the-dependent-variable-bias-the-results]

