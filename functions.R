#' This was pulled from my `hcc_trawl_analysis.R` script from the `Halibut Covariate Studies` Gitlab repo

# require(data.table)   # for data wrangling
# require(ggplot2)      # for plotting
# require(GGally)       # for ggpairs function
# require(ordinal)      # for clm and clmm functions (cumalative linked models including proportional odds logistic regression)
#require(randomForest) # for randomForest models

#==============#
# FUNCTIONS ####

oci <- function(CM){
  # CM is the confusion matrix
  # K is the number of groups (should just be dim of CM?)
  K <- dim(CM)[1]  # number of groups 
  N <- sum(CM)     # total number of observations
  ggamma <- 1
  bbeta <- 0.75/(N*(K-1)^ggamma)
  
  helperM2 <- matrix(0, ncol=K, nrow=K)   #matrix of zeroes with dimensions K, K
  for(r in 1:K){
    for( c in 1:K){
      helperM2[r, c] <- CM[r, c] * ((abs(r-c))^ggamma)
    }
  }
  
  TotalDispersion = sum(helperM2)^(1/ggamma)
  helperM1 <- CM / (TotalDispersion+N)
  
  errMatrix <- matrix(0, nrow=K, ncol=K)
  errMatrix[1,1] <- 1 - helperM1[1,1] + bbeta*helperM2[1,1]
  for(r in 2:K){
    c=1
    errMatrix[r,c] =  errMatrix[r-1, c] - helperM1[r,c] + bbeta*helperM2[r,c]
  }
  for(c in 2:K){
    r=1
    errMatrix[r,c] <- errMatrix[r, c-1] - helperM1[r,c] + bbeta*helperM2[r,c]
  }
  
  for(c in 2:K){
    for(r in 2:K){
      costup <- errMatrix[r-1, c]
      costleft <- errMatrix[r, c-1]
      lefttopcost <- errMatrix[r-1, c-1]
      aux <- min(costup, costleft, lefttopcost)
      errMatrix[r,c] <- aux - helperM1[r,c] + bbeta*helperM2[r,c]
    }
  }
  return(round(errMatrix[K,K], 5))
} 

# Weighted OCI. Be sure to use table(PREDICT, ACTUAL) for the confusion matrices!
woci <- function(x) {
  return(oci(x / rep(colSums(x), each=ncol(x))))
}

# Like class score but operates on the CM only. For random forest evaluations
CM_score <- function(x) {
  res <- c(oci(x), woci(x), sum(diag(x)/sum(x)), mean(diag(x) / colSums(x)), diag(x) / colSums(x))
  names(res) <-  c("oci", "woci", "acc", "wacc", "E", "P", "D")
  res <- data.table(t(res))
  return(res)
}

#=============#
# PREDICT CLM #

# The current predict methods for 'clm' objects does well for the class, but doesn't give the probabilties for each class
# Currently, there are not any predict() methods for 'clmm' objects. We can igore the random intercept for each haul and striclty use the model coefficients to get predicted classes 

predict_clm <- function(mod, ran_int = "none", new_data = NULL){
  # mod <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG + PERMIT, data = hlbt_dat.scale); new_data <- NULL; ran_int <- "none"
  # mod <- clm(VIABILITY ~ ASSESSMENT_TIME + HAUL_MT + WEIGHT_KG, data = hlbt_dat.scale); new_data <- NULL; ran_int <- "none"
  
  if(!is.null(new_data) & ran_int=="actual") stop("Cannot apply random intercepts to a new dataset")
  
  # Get predictions for original dataset in the model object or for a new dataset?
  if(is.null(new_data)){
    dat <- data.table(mod$model)            # Combine data and thresholds
  } else {
    dat <- new_data[, colnames(mod$model), with=F]
  }
  
  outcomes <- mod$y.levels                # Get the levels of the outcomes     
  coefs <- names(mod$beta)                # Get names of all coefficients
  
  #  Combine data with model coefficients

  ## First, identify the covariates with the categorical fixed effects
  mod_terms <- attr(mod$terms, "dataClasses")
  mod_terms <- mod_terms[names(mod_terms) %in% labels(terms(mod))]
  terms.covar <- names(mod_terms)[mod_terms == "numeric"]
  terms.cat   <- names(mod_terms)[mod_terms %in% c("factor", "character")]
  
  ## Handle the covariates, if present
  if(length(terms.covar) < 1){
    coef_sum <- rep(0, nrow(dat))             # If there are no coefficients (null model), make coef_sum = 0
  } else {
    # Combine data with coefficients
    coef_tbl <- list()                        # Initialize outputs
    for(i in 1:length(terms.covar)){
      focus_coef <- terms.covar[i]
      if(focus_coef %like% ":"){
        # if the term is an interaction, multiply data across rows and multiply by coefficient
        coef_tbl[[focus_coef]] <- apply(dat[, unlist(strsplit(focus_coef, split=":")), with=F] , MARGIN = 1, FUN=prod) * mod$beta[[focus_coef]]
      }  else {
        # if the term is a main effect, simply multiply data by coefficient
        coef_tbl[[focus_coef]] <- dat[[focus_coef]] * mod$beta[[focus_coef]]
      }
    }
    coef_sum <- rowSums(as.data.table(coef_tbl))   # Sum up model terms
  }
  
  ## Handle the categorical predictors if present
  if(length(terms.cat) > 1) {
    cat_tbl <- list()
    for(i in 1:length(terms.cat)){
      focus_coef <- terms.cat[i]
      focus_cat_coefs <- as.data.table(mod$beta[grepl(paste0("^", focus_coef), "*" , x = names(mod$beta))], keep.rownames = T)
      focus_cat_coefs[, (focus_coef) := sub(paste0("^", focus_coef), "", V1)]
      # Merge in coefficients for each level, and assign 0 to the base level
      cat_coef_dt <- dat[, ..focus_coef]
      cat_coef_dt[, value := focus_cat_coefs[cat_coef_dt, V2, on = .(PERMIT)]][is.na(value), value := 0]
      cat_tbl[[focus_coef]] <- cat_coef_dt$value
    }
    # Add to the covariate sums
    coef_sum <- coef_sum + rowSums(as.data.table(cat_tbl))
  }

  # For clmm objects, include random intercepts? By default predictions are made without them, but change ran_int if you want to use actuals or global averages of random intercepts
  if(class(mod)=="clmm"){
    if(ran_int == "actual"){
      ran_name <- names(mod$ST)
      ran <- unique(dat[, ..ran_name])
      
      ran[, INT := 0]   # Initialize intercept
      for(i in 1:length(ran_name)){
        ran[, INT := INT + setNames(as.data.table(ranef(mod)[ran_name[[i]]], keep.rownames=T), c(ran_name[i], "INT"))[ran, INT, on=ran_name[i]]  ]
      }
      
      coef_sum <- coef_sum + dat[ran, on=ran_name]$INT
    } else if(ran_int == "average"){
      coef_sum <- coef_sum + mean(mod$ranef)   # Use global mean of random effects
    }
  }
  
  # Calculate probabilities using coef_sum and thresholds
  res <- matrix(nrow=length(coef_sum), ncol=length(outcomes))                   # Initialize outputs
  for(j in 1:length(mod$alpha)){
    res[, j] <- exp(mod$alpha[[j]] - coef_sum) / (1 + exp(mod$alpha[[j]] - coef_sum)) - rowSums(res, na.rm=T) 
  }
  res[, length(outcomes)] <- 1-rowSums(res, na.rm=T)   # Calculate remaining probability for final class
  colnames(res) <- paste0("p_", outcomes)              # Rename columns with class names
  
  # Predict class using outcome with highest probability
  classes <- factor(apply(res, 1, function(x) outcomes[as.vector(which(x == max(x)))]), levels=outcomes, ordered=T)
  # Get Modeled DMR
  mortality <- rowSums(sweep(res, MARGIN=2, FUN='*', c(0.2, 0.55, 0.9)))        # Sweep is kind of like apply, but we don't have to do any transposing to 
  # Format probabilities to be like fitted.values for polr objects
  probs <- copy(res)
  colnames(probs) <- outcomes
  
  return(list(Class = classes, Mort = mortality, Prob = probs))
  
}


# Calculates classification scores as well as invidual-level bias and variance. Works with both clm and randomForest models
mod_score <- function(x, new_data) {
  # x <- olr_mod; new_data <- test_dat; f <- TRUE;
  # x <- rf_mod; new_data <- test_dat; f <- TRUE;
  
  # CLASSIFICATION SCORES
  dat <- copy(new_data)[, .(HAUL_ID, WEIGHT_KG, VIAB, OG_DMR)]
  dat[, PRED_CLASS := predict(x, newdata=new_data, type="class")]
  CM <- table(dat$PRED_CLASS, dat$VIAB)
  pred_class_score <- CM_score(CM)
  
  # BIAS & VAR FROM BOTH CLASS AND PROB
  dat[, PRED_CLASS_MORT := ifelse(PRED_CLASS == "A", 0.2, ifelse(PRED_CLASS == "B", 0.55, 0.9))]
  if("clm" %in% class(x)) {
    dat[, PRED_PROB_MORT := predict_clm(x, new_data=new_data)$Mort] 
    row_id <- data.table("Formula" = deparse(x$formula), aic = AIC(x))
  }          
  if("randomForest" %in% class(x)) {
    dat[, PRED_PROB_MORT := rowSums(predict(x, newdata=new_data, type="prob") * rep(c(0.2, 0.55, 0.9), each=nrow(new_data)))] 
    row_id <- data.table("Formula" = "randomForest", aic = NA_real_)
  } 
  
  # Individual-level mortalities from Class and Prob
  class_res <- dat[, .(bias = mean(PRED_CLASS_MORT - OG_DMR),
                       var = var(PRED_CLASS_MORT - OG_DMR),
                       wbias = mean(PRED_CLASS_MORT*WEIGHT_KG - OG_DMR*WEIGHT_KG),
                       wvar = var(PRED_CLASS_MORT*WEIGHT_KG - OG_DMR*WEIGHT_KG))]
  names(class_res) <- paste("C", names(class_res), sep="_")
  prob_res <- dat[, .(bias = mean(PRED_PROB_MORT - OG_DMR),
                      var = var(PRED_PROB_MORT - OG_DMR),
                      wbias = mean(PRED_PROB_MORT*WEIGHT_KG - OG_DMR*WEIGHT_KG),
                      wvar = var(PRED_PROB_MORT*WEIGHT_KG - OG_DMR*WEIGHT_KG))]
  names(prob_res) <- paste("P", names(prob_res), sep="_")
  ind_out <- cbind(row_id, pred_class_score, cbind(class_res, prob_res))
  
  # Haul-level DMRs from both Class and Prob. Here, w_mean is weighted by number of halibut, not weigt_kg
  dat_haul <- dat[, .(HAL_N = .N, 
                      OG_DMR = weighted.mean(OG_DMR, WEIGHT_KG), 
                      PRED_C_DMR = weighted.mean(PRED_CLASS_MORT, WEIGHT_KG), 
                      PRED_P_DMR = weighted.mean(PRED_PROB_MORT, WEIGHT_KG)), by=.(HAUL_ID)]
  dat_haul_melt <- melt(dat_haul, id.vars=c("HAUL_ID", "HAL_N", "OG_DMR"))
  dat_haul_melt_smry <- dat_haul_melt[, .(MEAN = mean(value-OG_DMR), VAR = var(value-OG_DMR), W_MEAN = weighted.mean(value-OG_DMR, HAL_N)), by=variable]
  haul_out <- cbind(row_id[, -2], dcast(dat_haul_melt_smry, formula="" ~ variable, value.var=c("MEAN", "VAR", "W_MEAN"))[, -1])
  
  return(list(IND = ind_out, HAUL = haul_out, RAW = cbind(row_id[, -2], dat_haul)))
}
