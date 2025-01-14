#' *data analysis*

#' @SEE GitLab repos, I have some analysis in there that may be useful. I don't think I had the decksort simulation though.

library(data.table)
library(ordinal)  # for clm(), cumulative link models and clmm(), the mixed model version
library(ggplot2)

# Load the dataset
(load("data/hlbt_dat.rdata"))



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

hlbt_dat.scale <- hlbt_dat[, .(VIABILITY, ASSESSMENT_TIME, SORT_DUR, PRESORTED_NUMBER, LAST_HAL, HAUL_MT, TOW_DUR, FISHING_DEPTH, WEIGHT_KG, PERMIT, TRIP_SEQ, OBS_ID, TRIP_ID, HAUL_ID)]
hlbt_dat.scale[, ':=' (PERMIT = as.factor(PERMIT), OBS_ID = as.factor(OBS_ID))]
#' Use SORT_DUR + 1 or LAST_HAL to determine the duration of each haul's sorting operation
hlbt_dat.scale[, SORT_END := pmax(SORT_DUR + 1, LAST_HAL)]
# Only 2,080 our of 106,563 halibut, or 1.95%were assessed after 35 minutes. We should truncate our dataset to 35 minutes
hlbt_dat.scale[SORT_END > 35, SORT_END := 35]
hlbt_dat.scale <- hlbt_dat.scale[ASSESSMENT_TIME <= 35 ]

# Scale the numeric variables so models converge more easily
#' TODO scale the values separately - individual vs haul-level metrics!
hlbt_dat.scale.haul <- unique(hlbt_dat.scale[, .(HAUL_ID, HAUL_MT, TOW_DUR, FISHING_DEPTH)])
hlbt_dat.scale.haul[, c("HAUL_MT.s", "TOW_DUR.s", "FISHING_DEPTH.s") := lapply(.SD, scale), .SDcols = c("HAUL_MT", "TOW_DUR", "FISHING_DEPTH")]
hlbt_dat.scale[, c("HAUL_MT.s", "TOW_DUR.s") := hlbt_dat.scale.haul[hlbt_dat.scale, .(HAUL_MT.s, TOW_DUR.s), on = .(HAUL_ID)]]
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

#' the brms package [tutorial: https://osf.io/preprints/psyarxiv/x8swp]
#   library(brms)

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


viab_by_obs <- hlbt_dat.scale[, .N, by = .(VIABILITY, OBS_ID)]
viab_by_obs[, TOTAL := sum(N), by = .(OBS_ID)]
mod.4.a.obs_id <- setnames(as.data.table(ranef(mod.4.a)$OBS_ID, keep.rownames = T), c("OBS_ID", "INTERCEPT"))
viab_by_obs <- viab_by_obs[mod.4.a.obs_id, on = .(OBS_ID)]
setorder(viab_by_obs, INTERCEPT)
viab_by_obs[, I := .GRP, by = .(OBS_ID)]

# Observers on the left have a lower intercept than those on the right (more likely to assign E than D)
ggplot(viab_by_obs, aes(x = I, y =  N, fill = factor(VIABILITY, levels = rev(levels(obs_viab$VIABILITY))))) + 
  #facet_wrap(PERMIT ~ ., scales = "free") + 
  geom_col(position = "fill", width = 1) +
  geom_text(aes(label = TOTAL), y = 1, hjust = 0, check_overlap = T, size = 2, nudge_x = 0.05) + 
  labs(fill = "Viability", x = "Badge", y = "Proportion") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 1)) + 
  guides(fill = guide_legend(reverse = T))
# Is there a way we can use these random effects to adjust the 'TRUE' values?



# TODO It would be interesting to see how these proportions would change according to our predictions? Do they level off?

