#fitting models
bird_rich_evi<-list()
bird_rich_evi[[1]]<-glmer(cbind(pb_rich_evi$rich,4-pb_rich_evi$rich)~ (1|pb_rich_evi$month),
                             family=binomial)
bird_rich_evi[[2]]<-glmer(cbind(pb_rich_evi$rich,4-pb_rich_evi$rich)~ pb_rich_evi$evi + (1|pb_rich_evi$month),
                             family=binomial)
#bird_rich_models[[3]]<-glmer(cbind(pb_rich$rich,4-pb_rich$rich)~ pb_rich$scelev + (1|pb_rich$month),
#                            family=binomial)
bird_rich_evi[[3]]<-glmer(cbind(pb_rich_evi$rich,4-pb_rich_evi$rich)~ pb_rich_evi$evi + pb_rich_evi$scelev + (1|pb_rich_evi$month),
                             family=binomial)

spring_rich_evi<-pb_rich_evi %>%
  filter(month >= 2 & month <= 5)
bird_rich_evi[[4]]<-glmer(cbind(spring_rich_evi$rich,4-spring_rich_evi$rich)~ (1|spring_rich_evi$month),
                             family=binomial)
bird_rich_evi[[5]]<-glmer(cbind(spring_rich_evi$rich,4-spring_rich_evi$rich)~ spring_rich_evi$evi + (1|spring_rich_evi$month),
                             family=binomial)
#bird_rich_models[[7]]<-glmer(cbind(spring_rich$rich,4-spring_rich$rich)~ spring_rich$scelev + (1|spring_rich$month),
#                            family=binomial)
bird_rich_evi[[6]]<-glmer(cbind(spring_rich_evi$rich,4-spring_rich_evi$rich)~ spring_rich_evi$evi + spring_rich_evi$scelev + (1|spring_rich_evi$month),
                             family=binomial)


fall_rich_evi<-pb_rich_evi %>%
  filter(month >= 5 & month <= 11)
bird_rich_evi[[7]]<-glmer(cbind(fall_rich_evi$rich,4-fall_rich_evi$rich)~ (1|fall_rich_evi$month),
                             family=binomial)
bird_rich_evi[[8]]<-glmer(cbind(fall_rich_evi$rich,4-fall_rich_evi$rich)~ fall_rich_evi$evi + (1|fall_rich_evi$month),
                              family=binomial)
#bird_rich_models[[11]]<-glmer(cbind(fall_rich$rich,4-fall_rich$rich)~ fall_rich$scelev + (1|fall_rich$month),
#                            family=binomial)
bird_rich_evi[[9]]<-glmer(cbind(fall_rich_evi$rich,4-fall_rich_evi$rich)~ fall_rich_evi$evi + fall_rich_evi$scelev + (1|fall_rich_evi$month),
                              family=binomial)
bird_rich_evi[[10]]<-glmer(cbind(pb_rich_evi$rich,4-pb_rich_evi$rich)~ pb_rich_evi$evi + pb_rich_evi$scelev + 
                             pb_rich_evi$evi*pb_rich_evi$scelev + (1|pb_rich_evi$month), family=binomial)
bird_rich_evi[[11]]<-glmer(cbind(spring_rich_evi$rich,4-spring_rich_evi$rich)~ spring_rich_evi$evi + spring_rich_evi$scelev + 
                             spring_rich_evi$evi*spring_rich_evi$scelev + (1|spring_rich_evi$month), family=binomial)
bird_rich_evi[[12]]<-glmer(cbind(fall_rich_evi$rich,4-fall_rich_evi$rich)~ fall_rich_evi$evi + fall_rich_evi$scelev + 
                             fall_rich_evi$evi*fall_rich_evi$scelev + (1|fall_rich_evi$month), family=binomial)

bird_rich__evirsq<-list()
bird_rich_evirsq<-lapply(bird_rich_evi, function(x) r.squaredGLMM(x))

#individual species models
alex_evi<-list()

alex_data_evi<-pb_sp_evi %>%
  filter(spname == 'Archilochusalexandri')
alex_evi[[1]]<-glmer(alex_data_evi$presence~ (1|alex_data_evi$month),
                        family=binomial)
alex_evi[[2]]<-glmer(alex_data_evi$presence~ alex_data_evi$evi + (1|alex_data_evi$month),
                        family=binomial)
#alex_models[[3]]<-glmer(alex_data_evi$presence~ alex_data$scelev + (1|alex_data$month),
#   family=binomial)
alex_evi[[3]]<-glmer(alex_data_evi$presence~ alex_data_evi$evi + alex_data_evi$scelev + (1|alex_data_evi$month),
                        family=binomial)

alex_spring_evi<-alex_data_evi %>% filter(month >= 2 & month <= 6)
alex_evi[[4]]<-glmer(alex_spring_evi$presence~ (1|alex_spring_evi$month),
                        family=binomial)
alex_evi[[5]]<-glmer(alex_spring_evi$presence~ alex_spring_evi$evi + (1|alex_spring_evi$month),
                        family=binomial)
#alex_models[[7]]<-glmer(alex_spring_evi$presence~ alex_spring$scelev + (1|alex_spring$month),
#             family=binomial)
alex_evi[[6]]<-glmer(alex_spring_evi$presence~ alex_spring_evi$evi + alex_spring_evi$scelev + (1|alex_spring_evi$month),
                        family=binomial)

alex_fall_evi<-alex_data_evi %>% filter(month >= 6 & month <= 12)
alex_evi[[7]]<-glmer(alex_fall_evi$presence~ (1|alex_fall_evi$month),
                        family=binomial)
alex_evi[[8]]<-glmer(alex_fall_evi$presence~ alex_fall_evi$evi + (1|alex_fall_evi$month),
                         family=binomial)
#alex_models[[11]]<-glmer(alex_fall_evi$presence~ alex_fall$scelev + (1|alex_fall$month),
#                      family=binomial)
alex_evi[[9]]<-glmer(alex_fall_evi$presence~ alex_fall_evi$evi + alex_fall_evi$scelev + (1|alex_fall_evi$month),
                         family=binomial)
alex_evi[[10]]<-glmer(alex_data_evi$presence~ alex_data_evi$evi + alex_data_evi$scelev + 
                        alex_data_evi$evi*alex_data_evi$scelev + (1|alex_data_evi$month), family=binomial)
alex_evi[[11]]<-glmer(alex_spring_evi$presence~ alex_spring_evi$evi + alex_spring_evi$scelev + 
                        alex_spring_evi$evi*alex_spring_evi$scelev + (1|alex_spring_evi$month), family=binomial)
alex_evi[[12]]<-glmer(alex_fall_evi$presence~ alex_fall_evi$evi + alex_fall_evi$scelev + 
                        alex_fall_evi$evi*alex_fall_evi$scelev + (1|alex_fall_evi$month), family=binomial)
alex_evirsq<-lapply(alex_evi, function(x) r.squaredGLMM(x))

cal_evi<-list()
cal_data_evi<-pb_sp_evi %>% filter(spname == 'Selasphoruscalliope')

cal_evi[[1]]<-glmer(cal_data_evi$presence~ (1|cal_data_evi$month),
                       family=binomial)
cal_evi[[2]]<-glmer(cal_data_evi$presence~ cal_data_evi$evi + (1|cal_data_evi$month),
                       family=binomial)
#cal_models[[3]]<-glmer(cal_data_evi$presence~ cal_data$scelev + (1|cal_data$month),
#                 family=binomial)
cal_evi[[3]]<-glmer(cal_data_evi$presence~ cal_data_evi$evi + cal_data_evi$scelev + (1|cal_data_evi$month),
                       family=binomial)

cal_spring_evi<-cal_data_evi %>% filter(month >= 3 & month <= 5)
cal_evi[[4]]<-glmer(cal_spring_evi$presence~ (1|cal_spring_evi$month),
                       family=binomial)
cal_evi[[5]]<-glmer(cal_spring_evi$presence~ cal_spring_evi$evi + (1|cal_spring_evi$month),
                       family=binomial)
#cal_models[[6]]<-glmer(cal_spring_evi$presence~ cal_spring$scelev + (1|cal_spring$month),
#             family=binomial)
cal_evi[[6]]<-glmer(cal_spring_evi$presence~ cal_spring_evi$evi + cal_spring_evi$scelev + (1|cal_spring_evi$month),
                       family=binomial)

cal_fall_evi<-cal_data_evi %>% filter(month >= 5 & month <= 11)
cal_evi[[7]]<-glmer(cal_fall_evi$presence~ (1|cal_fall_evi$month),
                       family=binomial)
cal_evi[[8]]<-glmer(cal_fall_evi$presence~ cal_fall_evi$evi + (1|cal_fall_evi$month),
                        family=binomial)
#cal_models[[11]]<-glmer(cal_fall_evi$presence~ cal_fall$scelev + (1|cal_fall$month),
#                      family=binomial)
cal_evi[[9]]<-glmer(cal_fall_evi$presence~ cal_fall_evi$evi + cal_fall_evi$scelev + (1|cal_fall_evi$month),
                        family=binomial)
cal_evi[[10]]<-glmer(cal_data_evi$presence~ cal_data_evi$evi + cal_data_evi$scelev + 
                       cal_data_evi$evi*cal_data_evi$scelev + (1|cal_data_evi$month), family=binomial)
cal_evi[[11]]<-glmer(cal_spring_evi$presence~ cal_spring_evi$evi + cal_spring_evi$scelev + 
                       cal_spring_evi$evi*cal_spring_evi$scelev + (1|cal_spring_evi$month), family=binomial)
cal_evi[[12]]<-glmer(cal_fall_evi$presence~ cal_fall_evi$evi + cal_fall_evi$scelev + 
                       cal_fall_evi$evi*cal_fall_evi$scelev + (1|cal_fall_evi$month), family=binomial)

cal_evirsq<-lapply(cal_evi, function(x) r.squaredGLMM(x))

plat_evi<-list()
plat_data_evi<-pb_sp_evi %>% filter(spname == 'Selasphorusplatycercus')

plat_evi[[1]]<-glmer(plat_data_evi$presence~ (1|plat_data_evi$month),
                        family=binomial)
plat_evi[[2]]<-glmer(plat_data_evi$presence~ plat_data_evi$evi + (1|plat_data_evi$month),
                        family=binomial)
#plat_models[[3]]<-glmer(plat_data_evi$presence~ plat_data$scelev + (1|plat_data$month),
#                       family=binomial)
plat_evi[[3]]<-glmer(plat_data_evi$presence~ plat_data_evi$evi + plat_data_evi$scelev + (1|plat_data_evi$month),
                        family=binomial)

plat_spring_evi<-plat_data_evi %>% filter(month >= 2 & month <= 7)

plat_evi[[4]]<-glmer(plat_spring_evi$presence~ (1|plat_spring_evi$month),
                        family=binomial)
plat_evi[[5]]<-glmer(plat_spring_evi$presence~ plat_spring_evi$evi + (1|plat_spring_evi$month),
                        family=binomial)
#plat_models[[6]]<-glmer(plat_spring_evi$presence~ plat_spring$scelev + (1|plat_spring$month),
#                   family=binomial)
plat_evi[[6]]<-glmer(plat_spring_evi$presence~ plat_spring_evi$evi + plat_spring_evi$scelev + (1|plat_spring_evi$month),
                        family=binomial)

plat_fall_evi<-plat_data_evi %>% filter(month >= 7 & month <= 12)
plat_evi[[7]]<-glmer(plat_fall_evi$presence~ (1|plat_fall_evi$month),
                        family=binomial)
plat_evi[[8]]<-glmer(plat_fall_evi$presence~ plat_fall_evi$evi + (1|plat_fall_evi$month),
                         family=binomial)
#plat_models[[11]]<-glmer(plat_fall$presence~ plat_fall$scelev + (1|plat_fall$month),
#                      family=binomial)
plat_evi[[9]]<-glmer(plat_fall_evi$presence~ plat_fall_evi$evi + plat_fall_evi$scelev + (1|plat_fall_evi$month),
                         family=binomial)
plat_evi[[10]]<-glmer(plat_data_evi$presence~ plat_data_evi$evi + plat_data_evi$scelev + 
                        plat_data_evi$evi*plat_data_evi$scelev + (1|plat_data_evi$month), family=binomial)
plat_evi[[11]]<-glmer(plat_spring_evi$presence~ plat_spring_evi$evi + plat_spring_evi$scelev + 
                        plat_spring_evi$evi*plat_spring_evi$scelev + (1|plat_spring_evi$month), family=binomial)
plat_evi[[12]]<-glmer(plat_fall_evi$presence~ plat_fall_evi$evi + plat_fall_evi$scelev + 
                        plat_fall_evi$evi*plat_fall_evi$scelev + (1|plat_fall_evi$month), family=binomial)

plat_evirsq<-lapply(plat_evi, function(x) r.squaredGLMM(x))

ruf_evi<-list()
ruf_data_evi<-pb_sp_evi %>% filter(spname == 'Selasphorusrufus')

ruf_evi[[1]]<-glmer(ruf_data_evi$presence~ (1|ruf_data_evi$month),
                       family=binomial)
ruf_evi[[2]]<-glmer(ruf_data_evi$presence~ ruf_data_evi$evi + (1|ruf_data_evi$month),
                       family=binomial)
#ruf_models[[3]]<-glmer(ruf_data_evi$presence~ ruf_data$scelev + (1|ruf_data$month),
#                      family=binomial)
ruf_evi[[3]]<-glmer(ruf_data_evi$presence~ ruf_data_evi$evi + ruf_data_evi$scelev + (1|ruf_data_evi$month),
                       family=binomial)

ruf_spring_evi<-ruf_data_evi %>% filter(month >= 1 & month <= 5)
ruf_evi[[4]]<-glmer(ruf_spring_evi$presence~ (1|ruf_spring_evi$month),
                       family=binomial)
ruf_evi[[5]]<-glmer(ruf_spring_evi$presence~ ruf_spring_evi$evi + (1|ruf_spring_evi$month),
                       family=binomial)
#ruf_models[[6]]<-glmer(ruf_spring_evi$presence~ ruf_spring$scelev + (1|ruf_spring$month),
#                      family=binomial)
ruf_evi[[6]]<-glmer(ruf_spring_evi$presence~ ruf_spring_evi$evi + ruf_spring_evi$scelev + (1|ruf_spring_evi$month),
                       family=binomial)

ruf_fall_evi<-ruf_data_evi %>% filter(month >= 5 & month <= 11)
ruf_evi[[7]]<-glmer(ruf_fall_evi$presence~ (1|ruf_fall_evi$month),
                       family=binomial)
ruf_evi[[8]]<-glmer(ruf_fall_evi$presence~ ruf_fall_evi$evi + (1|ruf_fall_evi$month),
                        family=binomial)
#ruf_models[[11]]<-glmer(ruf_fall_evi$presence~ ruf_fall$scelev + (1|ruf_fall$month),
#                       family=binomial)
ruf_evi[[9]]<-glmer(ruf_fall_evi$presence~ ruf_fall_evi$evi + ruf_fall_evi$scelev + (1|ruf_fall_evi$month),
                        family=binomial)
ruf_evi[[10]]<-glmer(ruf_data_evi$presence~ ruf_data_evi$evi + ruf_data_evi$scelev + 
                       ruf_data_evi$evi*ruf_data_evi$scelev + (1|ruf_data_evi$month), family=binomial)
ruf_evi[[11]]<-glmer(ruf_spring_evi$presence~ ruf_spring_evi$evi + ruf_spring_evi$scelev + 
                       ruf_spring_evi$evi*ruf_spring_evi$scelev + (1|ruf_spring_evi$month), family=binomial)
ruf_evi[[12]]<-glmer(ruf_fall_evi$presence~ ruf_fall_evi$evi + ruf_fall_evi$scelev + 
                       ruf_fall_evi$evi*ruf_fall_evi$scelev + (1|ruf_fall_evi$month), family=binomial)

ruf_evirsq<-lapply(ruf_evi, function(x) r.squaredGLMM(x))

#AIC tests
bird_eviyr<-AIC(bird_rich_evi[[1]],bird_rich_evi[[2]],bird_rich_evi[[3]])
bird_evisp<-AIC(bird_rich_evi[[4]],bird_rich_evi[[5]],bird_rich_evi[[6]])
bird_evifa<-AIC(bird_rich_evi[[7]],bird_rich_evi[[8]],bird_rich_evi[[9]])

alex_eviyr<-AIC(alex_evi[[1]],alex_evi[[2]],alex_evi[[3]])
alex_evisp<-AIC(alex_evi[[4]],alex_evi[[5]],alex_evi[[6]])
alex_evifa<-AIC(alex_evi[[7]],alex_evi[[8]],alex_evi[[9]])

cal_eviyr<-AIC(cal_evi[[1]],cal_evi[[2]],cal_evi[[3]])
cal_evisp<-AIC(cal_evi[[4]],cal_evi[[5]],cal_evi[[6]])
cal_evifa<-AIC(cal_evi[[7]],cal_evi[[8]],cal_evi[[9]])

plat_eviyr<-AIC(plat_evi[[1]],plat_evi[[2]],plat_evi[[3]])
plat_evisp<-AIC(plat_evi[[4]],plat_evi[[5]],plat_evi[[6]])
plat_evifa<-AIC(plat_evi[[7]],plat_evi[[8]],plat_evi[[9]])

ruf_eviyr<-AIC(ruf_evi[[1]],ruf_evi[[2]],ruf_evi[[3]])
ruf_evisp<-AIC(ruf_evi[[4]],ruf_evi[[5]],ruf_evi[[6]])
ruf_evifa<-AIC(ruf_evi[[7]],ruf_evi[[8]],ruf_evi[[9]])

#save
save(bird_rich_evirsq,file="bird_rich_evirsq.Rdata")
save(bird_eviyr,file="bird_eviyr.Rdata")
save(bird_evisp,file="bird_evisp.Rdata")
save(bird_evifa,file="bird_evifa.Rdata")
save(alex_evirsq,file="alex_rsq.Rdata")
save(cal_evirsq,file="cal_rsq.Rdata")
save(plat_evirsq,file="plat_rsq.Rdata")
save(ruf_evirsq,file="ruf_rsq.Rdata")
save(alex_eviyr,file="alex_yr.Rdata")
save(alex_evisp,file="alex_sp.Rdata")
save(alex_evifa,file="alex_fa.Rdata")
save(cal_eviyr,file="cal_yr.Rdata")
save(cal_evisp,file="cal_sp.Rdata")
save(cal_evifa,file="cal_fa.Rdata")
save(plat_eviyr,file="plat_yr.Rdata")
save(plat_evisp,file="plat_sp.Rdata")
save(plat_evifa,file="plat_fa.Rdata")
save(ruf_eviyr,file="ruf_yr.Rdata")
save(ruf_evisp,file="ruf_sp.Rdata")
save(ruf_evifa,file="ruf_fa.Rdata")