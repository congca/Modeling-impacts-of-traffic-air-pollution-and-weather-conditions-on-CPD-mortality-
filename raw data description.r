#### Visualization
mortal <- read.table("mortal raw.csv",header=T,sep=";",dec = "." )
Oslomortal <- mortal[which(mortal$municipality_name %in% "1"),]
Bergemmortal <- mortal[which(mortal$municipality_name %in% "2"),]
Trondheimmortal <- mortal[which(mortal$municipality_name %in% "3"),]
Tromsomortal <- mortal[which(mortal$municipality_name %in% "4"),]
par(mfcol=c(2,2)) 
plot(Oslomortal$month.of.the.year,Oslomortal$Mean.air.temperature, type = "p",xlab= "From January to December",
     ylab="Daily mean air temperature (°C)", lwd = 1, lend = 2,col="light blue"
     ,main = "Daily mean air temperature of Oslo ")
plot(Bergemmortal$month.of.the.year,Bergemmortal$Mean.air.temperature, type = "p",xlab= "From January to December",
     ylab="Daily mean air temperature (°C)", lwd = 1, lend = 2,col="light blue"
     ,main = "Daily mean air temperature of Bergen ")
plot(Trondheimmortal$month.of.the.year,Trondheimmortal$Mean.air.temperature, type = "p",xlab= "From January to December",
     ylab="Daily mean air temperature (°C)", lwd = 1, lend = 2,col="light blue"
     ,main = "Daily mean air temperature of Trondheim ")
plot(Tromsomortal$month.of.the.year,Tromsomortal$Mean.air.temperature, type = "p",xlab= "From January to December",
     ylab="Daily mean air temperature (°C)", lwd = 1, lend = 2,col="light blue"
     ,main = "Daily mean air temperature of Tromsø ")
par(mfcol=c(2,2)) 
plot(Oslomortal$year,Oslomortal$Mean.air.temperature, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily mean air temperature (°C)", lwd = 1, lend = 2,col="light blue"
     ,main = "Daily mean air temperature of Oslo ")
plot(Bergemmortal$year,Bergemmortal$Mean.air.temperature, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily mean air temperature (°C)", lwd = 1, lend = 2,col="light blue"
     ,main = "Daily mean air temperature of Bergen ")
plot(Trondheimmortal$year,Trondheimmortal$Mean.air.temperature, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily mean air temperature (°C)", lwd = 1, lend = 2,col="light blue"
     ,main = "Daily mean air temperature of Trondheim ")
plot(Tromsomortal$year,Tromsomortal$Mean.air.temperature, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily mean air temperature (°C)", lwd = 1, lend = 2,col="light blue"
     ,main = "Daily mean air temperature of Tromsø ")

par(mfcol=c(2,2)) 
plot(Oslomortal$day.of.the.week,Oslomortal$Mean.air.temperature, type = "p",xlab= "From Monday to Friday",
     ylab="Daily mean air temperature (°C)", lwd = 1, lend = 2,col="light blue"
     ,main = "Daily mean air temperature of Oslo ")
plot(Bergemmortal$day.of.the.week,Bergemmortal$Mean.air.temperature, type = "p",xlab= "From Monday to Friday",
     ylab="Daily mean air temperature (°C)", lwd = 1, lend = 2,col="light blue"
     ,main = "Daily mean air temperature of Bergen ")
plot(Trondheimmortal$day.of.the.week,Trondheimmortal$Mean.air.temperature, type = "p",xlab= "From Monday to Friday",
     ylab="Daily mean air temperature (°C)", lwd = 1, lend = 2,col="light blue"
     ,main = "Daily mean air temperature of Trondheim ")
plot(Tromsomortal$day.of.the.week,Tromsomortal$Mean.air.temperature, type = "p",xlab= "From Monday to Friday",
     ylab="Daily mean air temperature (°C)", lwd = 1, lend = 2,col="light blue"
     ,main = "Daily mean air temperature of Tromsø ")


mm1 <- mm[,c(6:34,36,38,40,46,50,55:57,64:68)]
par(mfcol=c(2,1)) 
plot(mm$year,mm$patient_number_GP, type = "p",xlab= "From 2009 to 2018",
     ylab="Number of patients visiting GPs per day", lwd = 1, lend = 2,col=" blue"
     ,main = "Number of patients visiting GPs per day in Oslo for ten years ")
plot(mm$year,mm$patient_emergency, type = "p",xlab= "From 2009 to 2018",
     ylab="Number of patients visiting ER per day", lwd = 1, lend = 2,col=" blue"
     ,main = "Number of patients visiting ER per day in Oslo for ten years ")

par(mfcol=c(2,2)) 
plot(mm$year,mm$deductible_GP, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily deductible paid at GP (unit:KR)", lwd = 1, lend = 2,col=" blue"
     ,main = "Ten years daily deductible at GP")
plot(mm$year,mm$deductible_emergency, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily deductible paid at ER (unit:KR)", lwd = 1, lend = 2,
     col=" blue"
     ,main = "Ten years daily deductible at ER")
plot(mm$year,mm$government_GP, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily government spending at GP (unit:KR)", lwd = 1, lend = 2,col=" blue"
     ,main = "Daily government spending at GP for ten years")

plot(mm$year,mm$government_emergency, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily government spending at ER (unit:KR)", lwd = 1, lend = 2,
     col="blue"
     ,main = "Daily government spending at ER for ten years")


m <- read.table("mortal raw.csv",header=T,sep=";",dec = "." )
m <- m[1:14606,]
Osl <- m[which(m$municipality_name %in% 1),]
Ber <- m[which(m$municipality_name %in% 2),]
Tro <- m[which(m$municipality_name %in% 3),]
Tmso <- m[which(m$municipality_name %in% 4),]

par(mfcol=c(2,2)) 
plot(Osl$year,Osl$CVD_total_mortality, type = "p",xlab= "From 2009 to 2018",
     ylab="CPD total mortality per day(unit:per person)", lwd = 2, lend = 2,
     col=" blue",main = "Oslo daily CPD total mortality for ten years")
plot(Ber$year , Ber$CVD_total_mortality, type = "p",xlab= "From 2009 to 2018",
     ylab="CPD total mortality per day(unit:per person)", lwd = 2, lend = 2,
     col=" blue",main = "Bergen daily CPD total mortality for ten years")
plot(Tro$year,Tro$CVD_total_mortality, type = "p",xlab= "From 2009 to 2018",
     ylab="CPD total mortality per day(unit:per person)", lwd = 2, lend = 2,
     col=" blue",main = "Trondheim daily CPD total mortality for ten years")
plot(Tmso$year , Tmso$CVD_total_mortality, type = "p",xlab= "From 2009 to 2018",
     ylab="CPD total mortality per day(unit:per person)", lwd = 2, lend = 2,
     col=" blue",main = "Tromsø daily CPD total mortality for ten years")




par(mfcol=c(2,1)) 
plot(mm$month.of.the.year,mm$patient_number_GP, type = "h",xlab= "From January to December",
     ylab="Number of patients visiting GPs per day", lwd = 15, lend = 2,col="light blue"
     ,main = "Number of patients visiting GPs per day in Oslo,in different month")
plot(mm$month.of.the,mm$patient_emergency, type = "h",xlab= "From January to December",
     ylab="Number of patients visiting ER per day", lwd = 15, lend = 2,col="light blue"
     ,main = "Number of patients visiting ER per day in Oslo,in different month ")

par(mfcol=c(2,2)) 
plot(mm$month.of.the.year,mm$deductible_GP, type = "h",xlab= "From January to December",
     ylab="Daily deductible paid at GP (unit:KR)", lwd = 15, lend = 2,col="light blue"
     ,main = "Daily deductible at GP in different month")
plot(mm$month.of.the.year,mm$deductible_emergency, type = "h",xlab= "From January to December",
     ylab="Daily deductible paid at ER (unit:KR)", lwd = 15, lend = 2,
     col="light blue"
     ,main = "Daily deductible at ER in different month")
plot(mm$month.of.the.year,mm$government_GP, type = "h",xlab= "From January to December",
     ylab="Daily government spending at GP (unit:KR)", lwd = 15, lend = 2,col="light blue"
     ,main = "Daily government spending at GP in different month")
plot(mm$month.of.the.year,mm$government_emergency, type = "h",xlab= "From January to December",
     ylab="Daily government spending at ER (unit:KR)", lwd = 15, lend = 2,
     col="light blue"
     ,main = "Daily government spending at ER in different month")


par(mfcol=c(2,2)) 
plot(Osl$month.of.the.year,Osl$CVD_total_mortality, type = "h",xlab= "From January to December",
     ylab="CPD total mortality per day(unit:per person)", lwd = 15, lend = 2,
     col="light blue",main = "Oslo daily CPD total mortality in different month")
plot(Ber$month.of.the.year , Ber$CVD_total_mortality, type = "h",xlab= "From January to December",
     ylab="CPD total mortality per day(unit:per person)", lwd = 15, lend = 2,
     col="light blue",main = "Bergen daily CPD total mortality in different month")
plot(Tro$month.of.the.year,Tro$CVD_total_mortality, type = "h",xlab= "From January to December",
     ylab="CPD total mortality per day(unit:per person)", lwd = 15, lend = 2,
     col="light blue",main = "Trondheim daily CPD total mortality in different month")
plot(Tmso$month.of.the.year , Tmso$CVD_total_mortality, type = "h",xlab= "From January to December",
     ylab="CPD total mortality per day(unit:per person)", lwd = 15, lend = 2,
     col="light blue",main = "Tromsø daily CPD total mortality in different month")


par(mfcol=c(2,1)) 
plot(mm$day.of.the.week,mm$patient_number_GP, type = "h",xlab= "From Monday to Sunday",
     ylab="Number of patients visiting GPs per day", lwd = 15, lend = 2,col="light blue"
     ,main = "Number of patients visiting GPs per day in Oslo")
plot(mm$day.of.the.week,mm$patient_emergency, type = "h",xlab= "From Monday to Sunday",
     ylab="Number of patients visiting ER per day", lwd = 15, lend = 2,col="light blue"
     ,main = "Number of patients visiting ER per day in Oslo")

par(mfcol=c(2,2)) 
plot(mm$day.of.the.week,mm$deductible_GP, type = "h",xlab= "From Monday to Sunday",
     ylab="Daily deductible paid at GP (unit:KR)", lwd = 15, lend = 2,col="light blue"
     ,main = "Daily deductible at GP")
plot(mm$day.of.the.week,mm$deductible_emergency, type = "h",xlab= "From Monday to Sunday",
     ylab="Daily deductible paid at ER (unit:KR)", lwd = 15, lend = 2,
     col="light blue"
     ,main = "Daily deductible at ER")
plot(mm$day.of.the.week,mm$government_GP, type = "h",xlab= "From Monday to Sunday",
     ylab="Daily government spending at GP (unit:KR)", lwd = 15, lend = 2,col="light blue"
     ,main = "Daily government spending at GP")
plot(mm$day.of.the.week,mm$government_emergency, type = "h",xlab= "From Monday to Sunday",
     ylab="Daily government spending at ER (unit:KR)", lwd = 15, lend = 2,
     col="light blue"
     ,main = "Daily government spending at ER")



Osl <- mortal[which(mortal$municipality_name %in% "1"),]
Ber <- mortal[which(mortal$municipality_name %in% "2"),]
Tro <- mortal[which(mortal$municipality_name %in% "3"),]
Tmso <- mortal[which(mortal$municipality_name %in% "4"),]

par(mfcol=c(2,2)) 
plot(Osl$day.of.the.week,Osl$CVD_total_mortality, type = "h",xlab= "From Monday to Sunday",
     ylab="CPD total mortality per day(unit:per person)", lwd = 15, lend = 2,
     col="light blue",main = "Oslo daily CPD total mortality ")
plot(Ber$day.of.the.week , Ber$CVD_total_mortality, type = "h",xlab= "From Monday to Sunday",
     ylab="CPD total mortality per day(unit:per person)", lwd = 15, lend = 2,
     col="light blue",main = "Bergen daily CPD total mortality")
plot(Tro$day.of.the.week,Tro$CVD_total_mortality, type = "h",xlab= "From Monday to Sunday",
     ylab="CPD total mortality per day(unit:per person)", lwd = 15, lend = 2,
     col="light blue",main = "Trondheim daily CPD total mortality")
plot(Tmso$day.of.the.week , Tmso$CVD_total_mortality, type = "h",xlab= "From Monday to Sunday",
     ylab="CPD total mortality per day(unit:per person)", lwd = 15, lend = 2,
     col="light blue",main = "Tromsø daily CPD total mortality")

par(mfcol=c(2,2)) 
plot(Osl$day.of.the.week,Osl$PM2.5, type = "p",xlab= "From Monday to Sunday",
     ylab="Daily PM2.5 (unit:µg/m³)", lwd = 1, lend = 3,
     col="blue",main = "Daily Oslo PM2.5 value")
plot(Ber$day.of.the.week , Ber$PM2.5, type = "p",xlab= "From Monday to Sunday",
     ylab="Daily PM2.5 (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Bergen  PM2.5 value")
plot(Tro$day.of.the.week,Tro$PM2.5, type = "p",xlab= "From Monday to Sunday",
     ylab="Daily PM2.5 (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Trondheim PM2.5 value")
plot(Tmso$day.of.the.week , Tmso$PM2.5, type = "p",xlab= "From Monday to Sunday",
     ylab="Daily PM2.5 (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Tromsø PM2.5 value")

par(mfcol=c(2,2)) 
plot(Osl$month.of.the.year,Osl$PM2.5, type = "p",xlab= "From January to December",
     ylab="Daily PM2.5 (unit:µg/m³)", lwd = 1, lend = 3,
     col="blue",main = "Daily Oslo PM2.5 value in different month")
plot(Ber$month.of.the.year , Ber$PM2.5, type = "p",xlab= "From January to December",
     ylab="Daily PM2.5 (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Bergen  PM2.5 value in different month")
plot(Tro$month.of.the.year,Tro$PM2.5, type = "p",xlab= "From January to December",
     ylab="Daily PM2.5 (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Trondheim PM2.5 value in different month")
plot(Tmso$month.of.the.year , Tmso$PM2.5, type = "p",xlab= "From January to December",
     ylab="Daily PM2.5 (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Tromsø PM2.5 value in different month")

par(mfcol=c(2,2)) 
plot(Osl$year,Osl$PM2.5, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily PM2.5 (unit:µg/m³)", lwd = 1, lend = 3,
     col="blue",main = "Daily Oslo PM2.5 value for ten years")
plot(Ber$year , Ber$PM2.5, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily PM2.5 (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Bergen  PM2.5 value for ten years")
plot(Tro$year,Tro$PM2.5, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily PM2.5 (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Trondheim PM2.5 value for ten years")
plot(Tmso$year , Tmso$PM2.5, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily PM2.5 (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Tromsø PM2.5 value for ten years")







par(mfcol=c(2,2)) 
plot(Osl$day.of.the.week,Osl$NOx, type = "p",xlab= "From Monday to Sunday",
     ylab="Daily NOx (unit:µg/m³)", lwd = 1, lend = 3,
     col="blue",main = "Daily Oslo NOx value")
plot(Ber$day.of.the.week , Ber$NOx, type = "p",xlab= "From Monday to Sunday",
     ylab="Daily NOx (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Bergen  NOx value")
plot(Tro$day.of.the.week,Tro$NOx, type = "p",xlab= "From Monday to Sunday",
     ylab="Daily NOx (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Trondheim NOx value")
plot(Tmso$day.of.the.week , Tmso$NOx, type = "p",xlab= "From Monday to Sunday",
     ylab="Daily NOx (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Tromsø NOx value")

par(mfcol=c(2,2)) 
plot(Osl$month.of.the.year,Osl$NOx, type = "p",xlab= "From January to December",
     ylab="Daily NOx (unit:µg/m³)", lwd = 1, lend = 3,
     col="blue",main = "Daily Oslo NOx value in different month")
plot(Ber$month.of.the.year , Ber$NOx, type = "p",xlab= "From January to December",
     ylab="Daily NOx (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Bergen  NOx value in different month")
plot(Tro$month.of.the.year,Tro$NOx, type = "p",xlab= "From January to December",
     ylab="Daily NOx (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Trondheim NOx value in different month")
plot(Tmso$month.of.the.year , Tmso$NOx, type = "p",xlab= "From January to December",
     ylab="Daily NOx (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Tromsø NOx value in different month")

par(mfcol=c(2,2)) 
plot(Osl$year,Osl$NOx, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily NOx (unit:µg/m³)", lwd = 1, lend = 3,
     col="blue",main = "Daily Oslo NOx value for ten years")
plot(Ber$year , Ber$NOx, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily NOx (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Bergen  NOx value for ten years")
plot(Tro$year,Tro$NOx, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily NOx (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Trondheim NOx value for ten years")
plot(Tmso$year , Tmso$PM2.5, type = "p",xlab= "From 2009 to 2018",
     ylab="Daily NOx (unit:µg/m³)", lwd = 1, lend = 3,
     col=" blue",main = "Daily Tromsø NOx value for ten years")


 