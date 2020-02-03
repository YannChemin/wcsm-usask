model <- function(t, Y, parameters, daylengths,crownTemps,...) {
  Time <<- t # Current day
  vernDays <- Y['vernDays'] # Vernalization days so far
  dehardAmt <- Y['dehardAmt'] # Accumulated amount of dehardening due to crownTemp > threshold
  dehardAmtStress <- Y['dehardAmtStress'] # Accumulated amount of dehardening due to low temperature stress
  accAmt <- Y['accAmt'] # Accumulated acclimation
  respProg <- Y['respProg'] # Accumulated amount of dehardening due to respiration
  LT50raw <- Y['LT50raw'] # LT50 response
  photoReqFraction <- Y['photoReqFraction'] # Progress towards photoperiod saturation
  minLT50 <- Y['minLT50'] # Minimum LT50 so far
  mflnFraction <- Y['mflnFraction'] # Progress towards minimum final leaf number
  minDD <- parameters['minDD'] # Initial value: minimum degree day requirement
  vernProg <- Y['vernProg'] # Progress towards vernalization saturation
  photoCoeff <- parameters['photoCoeff'] # Intitial value: photoperiod coefficient
  photoCritical <- parameters['photoCritical'] # Initial value: critical photoperiod
  vernReq <- parameters['vernReq'] # Initial value: vernalization requirement
#  LTDprog <- Y['LTDprog']
#  LTDdays <- Y['LTDdays']
#  LTDreq <- parameters['LTDreq'] # Initial value: Low Temperature Delay requirement
  fiveDayTemp <- DELAY(t,10,crownTemps) # array of crown temperatures from previous 5 days
  fiveDayTempMean <- mean(fiveDayTemp) # 5 day mean crown temperature
  fiveDayTempSD <- sd(fiveDayTemp) # 5 day SDEV of crown temperature
  initLT50 <- parameters['initLT50'] # Inital value: LT50
  LT50c <- parameters['LT50c'] # Initial value: LT50C
  daylength <- inputData(t,data.frame(1:length(daylengths),daylengths)) # Initial Values: Daylengths
  crownTemp <- inputData(t,crownTemps) # Initial Values: Crown Temperatures
  photoProg <- ifelse(photoCoeff > 0, min(photoReqFraction, 1), 0)
  
  LT50 <- min(initLT50, LT50raw)
  vernSaturation <-  ifelse(vernReq > 0, min((vernDays) / vernReq , 1), 1)
  thresholdTemp <- 3.7214 - 0.4011 * LT50c # Calculate threshold Temperature based on LT50c [1]
  LT50DamageAdj <- LT50c - dehardAmtStress
#  LTDSaturation <- ifelse(LTDreq > 0, min((LTDdays) / LTDreq , 1), 1)
#  LTDdays <- ifelse(crownTemp < thresholdTemp & LTDreq > 0,LTDdays + 1,LTDdays)
  LT50MinFlow <- ifelse(
    LT50 < minLT50, (LT50 - minLT50), 0
  )
  #Minimum Degree Days to VRT
  DDReqCurrentTemp <-
    max(minDD, ((0.95 * minDD - 340) * (crownTemp - 2) + minDD)) # Formula [5]
  
  mflnFlow <- max(crownTemp, 0) / DDReqCurrentTemp # Formula [6]
#  mflnFlow <- max(crownTemp, 0)
	# Vernalization
	vernRate <- ifelse(
    crownTemp > -1.3 & crownTemp < 10, 
      1,
      ifelse(
        crownTemp >= 10 & crownTemp < 12,
        0.364*(3.313*(crownTemp+1.3)^0.423-(crownTemp+1.3)^0.846), 0 # Formula [7a/7b] (Updated)
      )
  )

	VRProg <- min(min(min(1,mflnFraction), photoProg), vernSaturation)
  VRFactor <- 1 / (1 + exp(80 * (VRProg - 0.9))) # VRT Factor [4]
	# Respiration Stress
  respFlow <- ifelse(
    fiveDayTempMean < 1.5 & fiveDayTempMean > -1 & fiveDayTempSD < 0.75,
    0.54 * (exp(0.84 + 0.051 * crownTemp) - 2) / 1.85, 0 # Formula [11]
  )
	# Dehardening
  dehardRate <- 5.05 / (1 + exp(4.35 - 0.28 * min(crownTemp, thresholdTemp))) # Formula [10]
  dehardFlow <- ifelse(
    respFlow > 0, 0, ifelse(
      crownTemp > thresholdTemp & LT50 < initLT50, dehardRate, ifelse(
        crownTemp > initLT50 & LT50 < initLT50, dehardRate * (1 - VRFactor), 0
      )
    )
  )
  # Low Temperature Stress
  LTStressFlow <-  ifelse(
    (LT50 < crownTemp) & ((minLT50 / 2) > crownTemp) &
      (LT50 - dehardAmtStress < initLT50) & (crownTemp < initLT50),
      abs((minLT50 - crownTemp) / exp(-0.654 * (minLT50 - crownTemp) - 3.74)),0 # Formula [12]
  )
	# Photoperiod Requirements	
  photoFactor <- ifelse(
    crownTemp > 0 & respFlow == 0, abs((3.5 / (1 + exp(0.504 * (daylength - photoCritical) - 0.321 * (crownTemp - 13.242)))) - 3.5), 0 # Formula [8]
  )
  photoFlow <- photoFactor / (3.25 * photoCoeff)
#  photoFlow <- photoFactor / (140 * photoCoeff)
  # Threshold Induction Temperatures and Acclimation
  accRate <- max(0, 0.014*(thresholdTemp - crownTemp) * (LT50 - LT50DamageAdj)) # Formula [2]
  accFlow <- ifelse(
    respFlow > 0,0,
    ifelse((LTStressFlow == 0), (VRFactor) * accRate ,0)
  )
  dLT50raw = respFlow  + LTStressFlow  + dehardFlow  - accFlow
  dminLT50 = LT50MinFlow
  ddehardAmt = -dehardFlow
  ddehardAmtStress = -respFlow  - LTStressFlow
  dphotoReqFraction = photoFlow
  dmflnFraction = mflnFlow
  daccAmt = accFlow
  dvernDays = vernRate
  dvernProg = vernRate/vernReq
  drespProg = respFlow
#  dLTDdays = LTDdays
#  dLTDprog = LTDSaturation
  list(
    c(
      dLT50raw,
      dminLT50,
      ddehardAmt,
      ddehardAmtStress,
      dmflnFraction,
      dphotoReqFraction,
      daccAmt,
      dvernDays,
      dvernProg,
      drespProg
#      dLTDdays,
#      dLTDprog
    ),
    c(vernSaturation=vernSaturation,respiration=respFlow,daylength=daylength,temperature=crownTemp)
  )
}
