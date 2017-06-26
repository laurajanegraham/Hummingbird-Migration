mod_fit <- function(input_dat, sp, ssn, mig_months) {
  mod_out <- list()
  # migration start and end dates vary by species - select from the input file
  start_mon <- mig_months[mig_months$species == sp & mig_months$season == ssn, 'start']
  end_mon <- mig_months[mig_months$species == sp & mig_months$season == ssn, 'end']
  
  # select the correct data for the species and season
  if(sp == 'all') {
    dat <- filter(input_dat, month >=start_mon, month<=end_mon)
    dat$resp <- cbind(dat$rich,4-dat$rich)
  } else {
    dat <- filter(input_dat, month >=start_mon, month<=end_mon, spname == sp)
    dat$resp <- dat$presence
  }
  
  #null
  mod_out[[1]] <- glmer(resp ~ (1|month), family=binomial, data = dat)
  
  # plant richness
  mod_out[[2]] <- glmer(resp~ plant_rich + (1|month),family=binomial, data = dat)
  
  # plant richness + elevation
  mod_out[[3]] <- glmer(resp~ plant_rich + scelev + (1|month), family=binomial, data = dat)
  
  # evi
  mod_out[[4]] <- glmer(resp~ evi + (1|month), family=binomial, data = dat)
  
  # evi + elevation
  mod_out[[5]] <- glmer(resp~ evi + scelev + (1|month), family=binomial, data = dat)
  
  # plant richness + evi
  mod_out[[6]] <- glmer(resp ~ plant_rich + evi + (1|month), family = binomial, data = dat)
  
  # plant richness * evi
  mod_out[[7]] <- glmer(resp ~ plant_rich + evi + plant_rich*evi + (1|month), family = binomial, data = dat)
  
  # plant richness + evi + elevation
  mod_out[[8]] <- glmer(resp ~ plant_rich + evi + scelev + (1|month), family = binomial, data = dat)
  
  # plant richness * elevation + evi
  mod_out[[9]] <- glmer(resp ~ plant_rich + evi + scelev + plant_rich*scelev + (1|month), family = binomial, data = dat)
  
  # plant richness + evi * elevation
  mod_out[[10]] <- glmer(resp ~ plant_rich + evi + scelev + evi*scelev + (1|month), family = binomial, data = dat)
  
  # plant richness * evi + elevation 
  mod_out[[11]] <- glmer(resp ~ plant_rich + evi + scelev + plant_rich*evi + (1|month), family = binomial, data = dat)
  
  # plant richness * evi + plant richness * elevation
  mod_out[[12]] <- glmer(resp ~ plant_rich + evi + scelev + plant_rich*evi + plant_rich*scelev + (1|month), family = binomial, data = dat)
  
  # plant richness * evi + evi * elevation 
  mod_out[[13]] <- glmer(resp ~ plant_rich + evi + scelev + plant_rich*evi + evi*scelev + (1|month), family = binomial, data = dat)
  
  # plant richnes * evi + plant richness * elevation + evi * elevation
  mod_out[[14]] <- glmer(resp ~ plant_rich + evi + scelev + plant_rich*evi + plant_rich*scelev + evi*scelev + (1|month), family = binomial, data = dat)
  
  rsq <- lapply(mod_out, function(x) r.squaredGLMM(x))
  
  save(mod_out, file=paste0("results/mod_", sp, "_", ssn, ".Rdata"))
  save(rsq, file=paste0("results/rsq_", sp, "_", ssn, ".Rdata"))
  return(list(mod_out = mod_out, rsq = rsq))
}

table_magic <- function(x){
  x <- summary(x)
  mod <- data.frame(coef(x))
  mod$sig <- ifelse(mod$Pr...z.. < 0.05, "*", "")
  mod <- mod[c("Estimate","Std..Error","sig")]
  mod$Estimate <- round(mod$Estimate,2)
  mod$Std..Error <- round(mod$Std..Error,2)
  
  mod_uni <- unite_(mod,"Totalres",c("Estimate","Std..Error"), sep = " [")
  mod_uni <- unite_(mod_uni,"out",c("Totalres","sig"), sep = "]")
  
  mod_t <- t(mod_uni)
  mod_t <- data.frame(mod_t)
  
  mod_t$call <- as.character(x$call)[2]
  mod_t$AIC <- round(x$AIC[1],0)
  return(mod_t)
}