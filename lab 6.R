# Group names :

# Muhibul Islam , Zakaria Sule , Liam O'Neill , Victoria K , Amira Elmakawy.

attach(Household_Pulse_data)
# we do summary of the people who received the vaccine in the household pulse data first .

summary(Household_Pulse_data$RECVDVACC)
#we craeate the data to use :

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
is.na(Household_Pulse_data$vaxx) <- which(Household_Pulse_data$RECVDVACC == "NA") 

# we do the generic format for logit model :

model_logit1 <- glm(RECVDVACC ~ EEDUC, family = binomial, data = Household_Pulse_data)
summary(model_logit1)
summary(as.numeric(Household_Pulse_data$RECVDVACC))
table(Household_Pulse_data$vaxx,Household_Pulse_data$EEDUC)



#since we got that RECVACC has 3 levels and we're doing only 2 levels ,
# so we remove the "NA's" by :

Household_Pulse_data$vaxx <- (Household_Pulse_data$RECVDVACC == "yes got vaxx")
summary(Household_Pulse_data$vaxx)
# now we did it as two levels.
# We want to subset :
pick_use1 <- (Household_Pulse_data$TBIRTH_YEAR < 2000)
dat_use1 <- subset(Household_Pulse_data, pick_use1)
dat_use1$RECVDVACC <- droplevels(dat_use1$RECVDVACC)

# So start from this baseline model and launch ahead , 

model_logit2 <- glm(vaxx ~ EEDUC + MS + RRACE +  GENID_DESCRIBE,
                    family = binomial, data = dat_use1)
summary(model_logit2)

to_be_predicted2 <- data.frame(EEDUC = "some coll", MS = "divorced", RRACE = "White", GENID_DESCRIBE = "female", data = dat_use1)
to_be_predicted$yhat <- predict(model_logit2, to_be_predicted, type = "response")

summary(to_be_predicted$yhat)
# The predicted probability of the divorced white females with some college who got vaxxed is 0.828.
# Almost all of the variables are statistically significant with the vaccination status.



new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 1990,
                                       EEDUC = factor("bach deg", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("never",levels = levels(dat_use1$MS)),
                                       RRACE = factor("Black",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("male", levels = levels(dat_use1$GENID_DESCRIBE))
)
predict(model_logit1,new_data_to_be_predicted)

# Result : 4.423

new_data_to_be_predicted <- data.frame(TBIRTH_YEAR = 1996,
                                       EEDUC = factor("some coll", levels = levels(dat_use1$EEDUC)),
                                       MS = factor("married",levels = levels(dat_use1$MS)),
                                       RRACE = factor("white",levels = levels(dat_use1$RRACE)),
                                       RHISPANIC = factor("Not Hispanic",levels = levels(dat_use1$RHISPANIC)),
                                       GENID_DESCRIBE = factor("Female", levels = levels(dat_use1$GENID_DESCRIBE))
)
predict(model_logit1,new_data_to_be_predicted)

# Result : 4.394




# Probit model : 

model_probit1 <- glm(vaxx ~ EEDUC + MS + RRACE  + GENID_DESCRIBE + ANYWORK*INCOME,
                     family = binomial (link = 'probit'), data = dat_use1)
summary(model_probit1)
to_be_predicted3<- data.frame(EEDUC = "some coll", MS = "separated", RRACE = "Asian", GENID_DESCRIBE = "female",
                              ANYWORK = "no employment in last 7 days", INCOME= "HH income $150 - 199

",  data = dat_use1)
to_be_predicted3$yhat<-predict(model_probit1, to_be_predicted3, type="response")
summary(to_be_predicted3$yhat)



