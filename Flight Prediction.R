data1 <-read.csv(file.choose(), header = T , sep = ",")

#DEPARTURE DELAY PLOTS WITH DIFFERENT CAUSES

bi_model <- lm(DEP_DELAY_NEW ~ SECURITY_DELAY, data=data1)
bi_model
summary(bi_model)
plot(data1$SECURITY_DELAY, data1$DEP_DELAY_NEW)
abline(bi_model)

bi_model <- lm(DEP_DELAY_NEW ~ NAS_DELAY, data=data1)
bi_model
summary(bi_model)
plot(data1$NAS_DELAY, data1$DEP_DELAY_NEW)
abline(bi_model)

bi_model <- lm(DEP_DELAY_NEW ~ LATE_AIRCRAFT_DELAY, data=data1)
bi_model
summary(bi_model)
plot(data1$LATE_AIRCRAFT_DELAY, data1$DEP_DELAY_NEW)
abline(bi_model)

bi_model <- lm(DEP_DELAY_NEW ~ WEATHER_DELAY, data=data1)
bi_model
summary(bi_model)
plot(data1$WEATHER_DELAY, data1$DEP_DELAY_NEW)
abline(bi_model)

bi_model <- lm(DEP_DELAY_NEW ~ CARRIER_DELAY, data=data1)
bi_model
summary(bi_model)
plot(data1$CARRIER_DELAY, data1$DEP_DELAY_NEW)
abline(bi_model)


#ARRIVAL DELAY PLOTS WITH DIFFERENT CAUSES

bi_model <- lm(ARR_DELAY_NEW ~ SECURITY_DELAY, data=data1)
bi_model
summary(bi_model)
plot(data1$SECURITY_DELAY, data1$ARR_DELAY_NEW)
abline(bi_model)

bi_model <- lm(ARR_DELAY_NEW ~ NAS_DELAY, data=data1)
bi_model
summary(bi_model)
plot(data1$NAS_DELAY, data1$ARR_DELAY_NEW)
abline(bi_model)

bi_model <- lm(ARR_DELAY_NEW ~ LATE_AIRCRAFT_DELAY, data=data1)
bi_model
summary(bi_model)
plot(data1$LATE_AIRCRAFT_DELAY, data1$ARR_DELAY_NEW)
abline(bi_model)

bi_model <- lm(ARR_DELAY_NEW ~ WEATHER_DELAY, data=data1)
bi_model
summary(bi_model)
plot(data1$WEATHER_DELAY, data1$ARR_DELAY_NEW)
abline(bi_model)

bi_model <- lm(ARR_DELAY_NEW ~ CARRIER_DELAY, data=data1)
bi_model
summary(bi_model)
plot(data1$CARRIER_DELAY, data1$ARR_DELAY_NEW)
abline(bi_model)

#MULTI-REGRESSION MODEL WITH ALL FIVE CAUSES

multi_model <- lm(DEP_DELAY_NEW ~ SECURITY_DELAY + NAS_DELAY +
                                  LATE_AIRCRAFT_DELAY + 
                                  WEATHER_DELAY + CARRIER_DELAY, 
                  data=data1)
multi_model
summary(multi_model)

delay_vars = c("SECURITY_DELAY", "NAS_DELAY",
               "LATE_AIRCRAFT_DELAY", "WEATHER_DELAY", 
               "CARRIER_DELAY", "DEP_DELAY_NEW", "ARR_DELAY_NEW")
delay_data = data1[delay_vars]
plot(delay_data)

#data1[data1$SECURITY_DELAY != 0,]

#data_pruned <- data1[data1$SECURITY_DELAY != 0 &
#                     data1$NAS_DELAY != 0,
#                     data1$WEATHER_DELAY,]
