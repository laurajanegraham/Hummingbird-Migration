library(tidyr)
library(plyr)

table_magic <- function(x){
  mod <- data.frame(coef(x))
  mod$sig <- ifelse(mod$Pr...z.. < 0.05, "*", "")
  mod <- mod[c("Estimate","Std..Error","sig")]
  mod$Estimate <- round(mod$Estimate,2)
  mod$Std..Error <- round(mod$Std..Error,2)
  
  mod_uni <- unite_(mod,"Totalres",c("Estimate","Std..Error"), sep = " [")
  mod_uni <- unite_(mod_uni,"Totalres",c("Totalres","sig"), sep = "]")
  
  mod_t <- t(mod_uni)
  mod_t <- data.frame(mod_t)
  if(ncol(mod_t) == 1) colnames(mod_t) <- c("Intercept")
  if(ncol(mod_t) == 2) colnames(mod_t) <- c("Intercept","Plant Richness")
  if(ncol(mod_t) == 3) colnames(mod_t) <- c("Intercept","Plant Richness", "Elevation")
  if(ncol(mod_t) == 4) colnames(mod_t) <- c("Intercept", "Plant Richness", "Elevation", "Interaction")
 
  mod_t$call <- as.character(x$call)[2]
  mod_t$AIC <- round(x$AIC[1],0)
  return(mod_t)
}

alex <- lapply(alex_sum, table_magic)
alex <- ldply(alex)
alex_r <- read.csv("alex_rsq.csv")
alex <- cbind(alex,round(alex_r,2))
alex <- alex[,-7]

alex <- sapply(alex, as.character)
alex[is.na(alex)] <- "-"
write.csv(alex,"alex_int.csv")

bird <- lapply(bird_sum, table_magic)
bird <- ldply(bird)
bird_r <- read.csv("bird_rsq.csv")
bird <- cbind(bird,round(bird_r,2))
bird <- bird[,-7]

bird <- sapply(bird, as.character)
bird[is.na(bird)] <- "-"
write.csv(bird,"bird_int.csv")

cal <- lapply(cal_sum,table_magic)
cal <- ldply(cal)
cal_r <- read.csv("cal_rsq.csv")
cal <- cbind(cal,round(cal_r,2))
cal <- cal[,-7]

cal <- sapply(cal,as.character)
cal[is.na(cal)] <- "-"
write.csv(cal,"cal_int.csv")

plat <- lapply(plat_sum,table_magic)
plat <- ldply(plat)
plat_r <- read.csv("plat_rsq.csv")
plat <- cbind(plat,round(plat_r,2))
plat <- plat[,-7]

plat <- sapply(plat,as.character)
plat[is.na(plat)] <- "-"
write.csv(plat,"plat_int.csv")

ruf <- lapply(ruf_sum,table_magic)
ruf <- ldply(ruf)
ruf_r <- read.csv("ruf_rsq.csv")
ruf <- cbind(ruf,round(ruf_r,2))
ruf <- ruf[,-7]

ruf <- sapply(ruf,as.character)
ruf[is.na(ruf)] <- "-"
write.csv(ruf,"ruf_int.csv")

ref_call <- read.csv("model_call.csv")
all_models <- rbind(alex,bird,cal,plat,ruf)
all_models <- merge(ref_call,all_models, by="call",all.x=T)

all_models$call <- factor(all_models$call,levels = c("whole","cbind(pb_rich_evi$rich, 4 - pb_rich_evi$rich) ~ (1 | pb_rich_evi$month)",
"cbind(pb_rich_evi$rich, 4 - pb_rich_evi$rich) ~ pb_rich_evi$plant_rich + (1 | pb_rich_evi$month)",
"cbind(pb_rich_evi$rich, 4 - pb_rich_evi$rich) ~ pb_rich_evi$plant_rich + pb_rich_evi$scelev + (1 | pb_rich_evi$month)",
"cbind(pb_rich$rich, 4 - pb_rich$rich) ~ sqrt(pb_rich$plant_rich) + pb_rich$scelev + sqrt(pb_rich$plant_rich) * pb_rich$scelev + (1 | pb_rich$month)",
"alex_data$presence ~ (1 | alex_data$month)",
"alex_data$presence ~ alex_data$plant_rich + (1 | alex_data$month)",
"alex_data$presence ~ alex_data$plant_rich + alex_data$scelev + (1 | alex_data$month)",
"alex_data$presence ~ alex_data$plant_rich + alex_data$scelev + alex_data$plant_rich * alex_data$scelev + (1 | alex_data$month)",
"cal_data$presence ~ (1 | cal_data$month)",
"cal_data$presence ~ cal_data$plant_rich + (1 | cal_data$month)",
"cal_data$presence ~ cal_data$plant_rich + cal_data$scelev + (1 | cal_data$month)",
"cal_data$presence ~ cal_data$plant_rich + cal_data$scelev + cal_data$plant_rich * cal_data$scelev + (1 | cal_data$month)",
"plat_data$presence ~ (1 | plat_data$month)",
"plat_data$presence ~ plat_data$plant_rich + (1 | plat_data$month)",
"plat_data$presence ~ plat_data$plant_rich + plat_data$scelev + (1 | plat_data$month)",
"plat_data$presence ~ plat_data$plant_rich + plat_data$scelev + plat_data$plant_rich * plat_data$scelev + (1 | plat_data$month)",
"ruf_data$presence ~ (1 | ruf_data$month)",
"ruf_data$presence ~ ruf_data$plant_rich + (1 | ruf_data$month)",
"ruf_data$presence ~ ruf_data$plant_rich + ruf_data$scelev + (1 | ruf_data$month)",
"ruf_data$presence ~ ruf_data$plant_rich + ruf_data$scelev + ruf_data$plant_rich * ruf_data$scelev + (1 | ruf_data$month)",
"spring","cbind(spring_rich$rich, 4 - spring_rich$rich) ~ (1 | spring_rich$month)",
"cbind(spring_rich$rich, 4 - spring_rich$rich) ~ spring_rich$plant_rich + (1 | spring_rich$month)",
"cbind(spring_rich$rich, 4 - spring_rich$rich) ~ spring_rich$plant_rich + spring_rich$scelev + (1 | spring_rich$month)",
"cbind(spring_rich$rich, 4 - spring_rich$rich) ~ spring_rich$plant_rich + spring_rich$scelev + spring_rich$plant_rich * spring_rich$scelev + (1 | spring_rich$month)",
"alex_spring$presence ~ (1 | alex_spring$month)",
"alex_spring$presence ~ alex_spring$plant_rich + (1 | alex_spring$month)",
"alex_spring$presence ~ alex_spring$plant_rich + alex_spring$scelev + (1 | alex_spring$month)",
"alex_spring$presence ~ alex_spring$plant_rich + alex_spring$scelev + alex_spring$plant_rich * alex_spring$scelev + (1 | alex_spring$month)",
"cal_spring$presence ~ (1 | cal_spring$month)",
"cal_spring$presence ~ cal_spring$plant_rich + (1 | cal_spring$month)",
"cal_spring$presence ~ cal_spring$plant_rich + cal_spring$scelev + (1 | cal_spring$month)",
"cal_spring$presence ~ cal_spring$plant_rich + cal_spring$scelev + cal_spring$plant_rich * cal_spring$scelev + (1 | cal_spring$month)",
"plat_spring$presence ~ (1 | plat_spring$month)",
"plat_spring$presence ~ plat_spring$plant_rich + (1 | plat_spring$month)",
"plat_spring$presence ~ plat_spring$plant_rich + plat_spring$scelev + (1 | plat_spring$month)",
"plat_spring$presence ~ plat_spring$plant_rich + plat_spring$scelev + plat_spring$plant_rich * plat_spring$scelev + (1 | plat_spring$month)",
"ruf_spring$presence ~ (1 | ruf_spring$month)",
"ruf_spring$presence ~ ruf_spring$plant_rich + (1 | ruf_spring$month)",
"ruf_spring$presence ~ ruf_spring$plant_rich + ruf_spring$scelev + (1 | ruf_spring$month)",
"ruf_spring$presence ~ ruf_spring$plant_rich + ruf_spring$scelev + ruf_spring$plant_rich * ruf_spring$scelev + (1 | ruf_spring$month)",
"autumn","cbind(fall_rich$rich, 4 - fall_rich$rich) ~ (1 | fall_rich$month)",
"cbind(fall_rich$rich, 4 - fall_rich$rich) ~ fall_rich$plant_rich + (1 | fall_rich$month)",
"cbind(fall_rich$rich, 4 - fall_rich$rich) ~ fall_rich$plant_rich + fall_rich$scelev + (1 | fall_rich$month)",
"cbind(fall_rich$rich, 4 - fall_rich$rich) ~ fall_rich$plant_rich + fall_rich$scelev + fall_rich$plant_rich * fall_rich$scelev + (1 | fall_rich$month)","alex_fall$presence ~ (1 | alex_fall$month)",
"alex_fall$presence ~ (1 | alex_fall$month)",
"alex_fall$presence ~ alex_fall$plant_rich + (1 | alex_fall$month)",
"alex_fall$presence ~ alex_fall$plant_rich + alex_fall$scelev + (1 | alex_fall$month)",
"alex_fall$presence ~ alex_fall$plant_rich + alex_fall$scelev + alex_fall$plant_rich * alex_fall$scelev + (1 | alex_fall$month)",
"cal_fall$presence ~ (1 | cal_fall$month)",
"cal_fall$presence ~ cal_fall$plant_rich + (1 | cal_fall$month)",
"cal_fall$presence ~ cal_fall$plant_rich + cal_fall$scelev + (1 | cal_fall$month)",
"cal_fall$presence ~ cal_fall$plant_rich + cal_fall$scelev + cal_fall$plant_rich * cal_fall$scelev + (1 | cal_fall$month)",
"plat_fall$presence ~ (1 | plat_fall$month)",
"plat_fall$presence ~ plat_fall$plant_rich + (1 | plat_fall$month)",
"plat_fall$presence ~ plat_fall$plant_rich + plat_fall$scelev + (1 | plat_fall$month)",
"plat_fall$presence ~ plat_fall$plant_rich + plat_fall$scelev + plat_fall$plant_rich * plat_fall$scelev + (1 | plat_fall$month)",
"ruf_fall$presence ~ (1 | ruf_fall$month)",
"ruf_fall$presence ~ ruf_fall$plant_rich + (1 | ruf_fall$month)",
"ruf_fall$presence ~ ruf_fall$plant_rich + ruf_fall$scelev + (1 | ruf_fall$month)",
"ruf_fall$presence ~ ruf_fall$plant_rich + ruf_fall$scelev + ruf_fall$plant_rich * ruf_fall$scelev + (1 | ruf_fall$month)"
))

correct <- all_models[order(all_models$call),]
write.csv(correct, "plantrich_models.csv")

#EVI table
setwd("~/masters/Hummingbirds/R for masters/2BIEN3/model results")

bird_evi <- lapply(birdevi_sum, table_magic)
bird_evi <- ldply(bird_evi)
birdevi_r <- read.csv("birdevi_rsq.csv")
bird_evi <- cbind(bird_evi,round(birdevi_r,2))
bird_evi <- bird_evi[,-7]

bird_evi <- sapply(bird_evi, as.character)
bird_evi[is.na(bird_evi)] <- "-"
write.csv(bird_evi,"birdevi_int.csv")

cal_evi <- lapply(calevi_sum, table_magic)
cal_evi <- ldply(cal_evi)
calevi_r <- read.csv("cal_evirsq.csv")
cal_evi <- cbind(cal_evi,round(calevi_r,2))
cal_evi <- cal_evi[,-7]

cal_evi <- sapply(cal_evi,as.character)
cal_evi[is.na(cal_evi)] <- "-"
write.csv(cal_evi,"calevi_int.csv")

plat_evi <- lapply(platevi_sum,table_magic)
plat_evi <- ldply(plat_evi)
platevi_r <- read.csv("plat_evirsq.csv")
plat_evi <- cbind(plat_evi,round(platevi_r,2))
plat_evi <- plat_evi[,-7]

plat_evi <- sapply(plat_evi,as.character)
plat_evi[is.na(plat_evi)] <- "-"
write.csv(plat_evi,"platevi_int.csv")

ruf_evi <- lapply(rufevi_sum,table_magic)
ruf_evi <- ldply(ruf_evi)
rufevi_r <- read.csv("ruf_evirsq.csv")
ruf_evi <- cbind(ruf_evi,round(rufevi_r,2))
ruf_evi <- ruf_evi[,-7]

ruf_evi <- sapply(ruf_evi,as.character)
ruf_evi[is.na(ruf_evi)] <- "-"
write.csv(ruf_evi,"rufevi_int.csv")

alex_evi <- lapply(alexevi_sum,table_magic)
alex_evi <- ldply(alex_evi)
alexevi_r <- read.csv("alex_evirsq.csv")
alex_evi <- cbind(alex_evi,round(alexevi_r,2))
alex_evi <- alex_evi[,-7]

alex_evi <- sapply(alex_evi,as.character)
alex_evi[is.na(alex_evi)] <- "-"
write.csv(alex_evi,"alexevi_int.csv")

evi_call <- read.csv("evimodel_call.csv")
evi_models <- rbind(alex_evi,bird_evi,cal_evi,plat_evi,ruf_evi)
evi_models <- merge(evi_call,evi_models, by="call",all.x=T)

evi_models$call <- factor(evi_models$call,levels = c("whole","cbind(pb_rich_evi$rich, 4 - pb_rich_evi$rich) ~ (1 | pb_rich_evi$month)",
                                                    "cbind(pb_rich_evi$rich, 4 - pb_rich_evi$rich) ~ pb_rich_evi$evi + (1 | pb_rich_evi$month)",
                                                    "cbind(pb_rich_evi$rich, 4 - pb_rich_evi$rich) ~ pb_rich_evi$evi + pb_rich_evi$scelev + (1 | pb_rich_evi$month)",
                                                    "cbind(pb_rich_evi$rich, 4 - pb_rich_evi$rich) ~ pb_rich_evi$evi + pb_rich_evi$scelev + pb_rich_evi$evi * pb_rich_evi$scelev + (1 | pb_rich_evi$month)",
                                                    "alex_data_evi$presence ~ (1 | alex_data_evi$month)",
                                                    "alex_data_evi$presence ~ alex_data_evi$evi + (1 | alex_data_evi$month)",
                                                    "alex_data_evi$presence ~ alex_data_evi$evi + alex_data_evi$scelev + (1 | alex_data_evi$month)",
                                                    "alex_data_evi$presence ~ alex_data_evi$evi + alex_data_evi$scelev + alex_data_evi$evi * alex_data_evi$scelev + (1 | alex_data_evi$month)",
                                                    "cal_data_evi$presence ~ (1 | cal_data_evi$month)",
                                                    "cal_data_evi$presence ~ cal_data_evi$evi + (1 | cal_data_evi$month)",
                                                    "cal_data_evi$presence ~ cal_data_evi$evi + cal_data_evi$scelev + (1 | cal_data_evi$month)",
                                                    "cal_data_evi$presence ~ cal_data_evi$evi + cal_data_evi$scelev + cal_data_evi$evi * cal_data_evi$scelev + (1 | cal_data_evi$month)",
                                                    "plat_data_evi$presence ~ (1 | plat_data_evi$month)",
                                                    "plat_data_evi$presence ~ plat_data_evi$evi + (1 | plat_data_evi$month)",
                                                    "plat_data_evi$presence ~ plat_data_evi$evi + plat_data_evi$scelev + (1 | plat_data_evi$month)",
                                                    "plat_data_evi$presence ~ plat_data_evi$evi + plat_data_evi$scelev + plat_data_evi$evi * plat_data_evi$scelev + (1 | plat_data_evi$month)",
                                                    "ruf_data_evi$presence ~ (1 | ruf_data_evi$month)",
                                                    "ruf_data_evi$presence ~ ruf_data_evi$evi + (1 | ruf_data_evi$month)",
                                                    "ruf_data_evi$presence ~ ruf_data_evi$evi + ruf_data_evi$scelev + (1 | ruf_data_evi$month)",
                                                    "ruf_data_evi$presence ~ ruf_data_evi$evi + ruf_data_evi$scelev + ruf_data_evi$evi * ruf_data_evi$scelev + (1 | ruf_data_evi$month)",
                                                    "spring","cbind(spring_rich_evi$rich, 4 - spring_rich_evi$rich) ~ (1 | spring_rich_evi$month)",
                                                    "cbind(spring_rich_evi$rich, 4 - spring_rich_evi$rich) ~ spring_rich_evi$evi + (1 | spring_rich_evi$month)",
                                                    "cbind(spring_rich_evi$rich, 4 - spring_rich_evi$rich) ~ spring_rich_evi$evi + spring_rich_evi$scelev + (1 | spring_rich_evi$month)",
                                                    "cbind(spring_rich_evi$rich, 4 - spring_rich_evi$rich) ~ spring_rich_evi$evi + spring_rich_evi$scelev + spring_rich_evi$evi * spring_rich_evi$scelev + (1 | spring_rich_evi$month)",
                                                    "alex_spring_evi$presence ~ (1 | alex_spring_evi$month)",
                                                    "alex_spring_evi$presence ~ alex_spring_evi$evi + (1 | alex_spring_evi$month)",
                                                    "alex_spring_evi$presence ~ alex_spring_evi$evi + alex_spring_evi$scelev + (1 | alex_spring_evi$month)",
                                                    "alex_spring_evi$presence ~ alex_spring_evi$evi + alex_spring_evi$scelev + alex_spring_evi$evi * alex_spring_evi$scelev + (1 | alex_spring_evi$month)",
                                                    "cal_spring_evi$presence ~ (1 | cal_spring_evi$month)",
                                                    "cal_spring_evi$presence ~ cal_spring_evi$evi + (1 | cal_spring_evi$month)",
                                                    "cal_spring_evi$presence ~ cal_spring_evi$evi + cal_spring_evi$scelev + (1 | cal_spring_evi$month)",
                                                    "cal_spring_evi$presence ~ cal_spring_evi$evi + cal_spring_evi$scelev + cal_spring_evi$evi * cal_spring_evi$scelev + (1 | cal_spring_evi$month)",
                                                    "plat_spring_evi$presence ~ (1 | plat_spring_evi$month)",
                                                    "plat_spring_evi$presence ~ plat_spring_evi$evi + (1 | plat_spring_evi$month)",
                                                    "plat_spring_evi$presence ~ plat_spring_evi$evi + plat_spring_evi$scelev + (1 | plat_spring_evi$month)",
                                                    "plat_spring_evi$presence ~ plat_spring_evi$evi + plat_spring_evi$scelev + plat_spring_evi$evi * plat_spring_evi$scelev + (1 | plat_spring_evi$month)",
                                                    "ruf_spring_evi$presence ~ (1 | ruf_spring_evi$month)",
                                                    "ruf_spring_evi$presence ~ ruf_spring_evi$evi + (1 | ruf_spring_evi$month)",
                                                    "ruf_spring_evi$presence ~ ruf_spring_evi$evi + ruf_spring_evi$scelev + (1 | ruf_spring_evi$month)",
                                                    "ruf_spring_evi$presence ~ ruf_spring_evi$evi + ruf_spring_evi$scelev + ruf_spring_evi$evi * ruf_spring_evi$scelev + (1 | ruf_spring_evi$month)",
                                                    "autumn","cbind(fall_rich_evi$rich, 4 - fall_rich_evi$rich) ~ (1 | fall_rich_evi$month)",
                                                    "cbind(fall_rich_evi$rich, 4 - fall_rich_evi$rich) ~ fall_rich_evi$evi + (1 | fall_rich_evi$month)",
                                                    "cbind(fall_rich_evi$rich, 4 - fall_rich_evi$rich) ~ fall_rich_evi$evi + fall_rich_evi$scelev + (1 | fall_rich_evi$month)",
                                                    "cbind(fall_rich_evi$rich, 4 - fall_rich_evi$rich) ~ fall_rich_evi$evi + fall_rich_evi$scelev + fall_rich_evi$evi * fall_rich_evi$scelev + (1 | fall_rich_evi$month)",
                                                    "alex_fall_evi$presence ~ (1 | alex_fall_evi$month)",
                                                    "alex_fall_evi$presence ~ alex_fall_evi$evi + (1 | alex_fall_evi$month)",
                                                    "alex_fall_evi$presence ~ alex_fall_evi$evi + alex_fall_evi$scelev + (1 | alex_fall_evi$month)",
                                                    "alex_fall_evi$presence ~ alex_fall_evi$evi + alex_fall_evi$scelev + alex_fall_evi$evi * alex_fall_evi$scelev + (1 | alex_fall_evi$month)",
                                                    "cal_fall_evi$presence ~ (1 | cal_fall_evi$month)",
                                                    "cal_fall_evi$presence ~ cal_fall_evi$evi + (1 | cal_fall_evi$month)",
                                                    "cal_fall_evi$presence ~ cal_fall_evi$evi + cal_fall_evi$scelev + (1 | cal_fall_evi$month)",
                                                    "cal_fall_evi$presence ~ cal_fall_evi$evi + cal_fall_evi$scelev + cal_fall_evi$evi * cal_fall_evi$scelev + (1 | cal_fall_evi$month)",
                                                    "plat_fall_evi$presence ~ (1 | plat_fall_evi$month)",
                                                    "plat_fall_evi$presence ~ plat_fall_evi$evi + (1 | plat_fall_evi$month)",
                                                    "plat_fall_evi$presence ~ plat_fall_evi$evi + plat_fall_evi$scelev + (1 | plat_fall_evi$month)",
                                                    "plat_fall_evi$presence ~ plat_fall_evi$evi + plat_fall_evi$scelev + plat_fall_evi$evi * plat_fall_evi$scelev + (1 | plat_fall_evi$month)",
                                                    "ruf_fall_evi$presence ~ (1 | ruf_fall_evi$month)",
                                                    "ruf_fall_evi$presence ~ ruf_fall_evi$evi + (1 | ruf_fall_evi$month)",
                                                    "ruf_fall_evi$presence ~ ruf_fall_evi$evi + ruf_fall_evi$scelev + (1 | ruf_fall_evi$month)",
                                                    "ruf_fall_evi$presence ~ ruf_fall_evi$evi + ruf_fall_evi$scelev + ruf_fall_evi$evi * ruf_fall_evi$scelev + (1 | ruf_fall_evi$month)"
                                                     )) 
correct_evi <- evi_models[order(evi_models$call),]
write.csv(correct_evi, "evi_models.csv")
