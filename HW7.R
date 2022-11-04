model_logit1 <- glm(vaxx ~ EEDUC,
family = binomial, data = Household_Pulse_data)

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA") 

model_logit1 <- glm(vaxx ~ EEDUC
                    family = binomial, data = Household_Pulse_data)

table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)

summary(as.numeric(Household_Pulse_data$vaxx))

summary(Household_Pulse_data$vaxx)

vaxx_factor <- as.factor(Household_Pulse_data$vaxx)
levels(vaxx_factor)
levels(vaxx_factor) <- c("no","yes")

glm(RECVDVACC ~ EEDUC,family = binomial)

pick_use1 <- (Household_Pulse_data$TBIRTH_YEAR < 2000) 
dat_use1 <- subset(Household_Pulse_data, pick_use1)

# and to be finicky, might want to use this for factors after subsetting in case some get lost
dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 

model_logit1 <- glm(vaxx ~ TBIRTH_YEAR + EEDUC + MS + RRACE + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use1)
summary(model_logit1)

pick_use1 <- (Household_Pulse_data$TBIRTH_YEAR < 2000) 
dat_use1 <- subset(Household_Pulse_data, pick_use1)

# and to be finicky, might want to use this for factors after subsetting in case some get lost
dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC) 

model_logit1 <- glm(vaxx ~ TBIRTH_YEAR + RRACE + RHISPANIC + GENID_DESCRIBE,
                    family = binomial, data = dat_use1)
summary(model_logit1)

new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 1990,
                                       EEDUC = factor("bach deg", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("never",levels = levels(dat_use1$MS)),
                                       RRACE = factor("Black",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("male", levels = levels(dat_use1$GENID_DESCRIBE))
)
predict(model_logit1,new_data_to_be_predicted)
