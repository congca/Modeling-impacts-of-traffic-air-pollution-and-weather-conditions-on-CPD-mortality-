
** create variables
destring t municipality_name county_name region_name dayoftheweek dayofmonth monthoftheyear year day CVD_total_mortality Trafficvolume NOx PM25 Meanairtemperature Maximumairtemperature Minimumairtemperature Heatingdegreedays Vapourpressure Averageofmeanwindspeedfrom Meanwindgust, replace

**declaration is a panel data structure
summarize
xtset municipality_name t
**generating new variables
**holiday variable (includes new year, winter holiday, easter week, labour day and national day)
gen holiday=0
replace holiday=1 if monthoftheyear==1 & dayofmonth==1
replace holiday=1 if year==1 & monthoftheyear==4 & dayofmonth==9
replace holiday=1 if year==1 & monthoftheyear==4 & dayofmonth==10
replace holiday=1 if year==1 & monthoftheyear==4 & dayofmonth==12
replace holiday=1 if year==1 & monthoftheyear==4 & dayofmonth==13

replace holiday=1 if year==2 & monthoftheyear==4 & dayofmonth==1
replace holiday=1 if year==2 & monthoftheyear==4 & dayofmonth==2
replace holiday=1 if year==2 & monthoftheyear==4 & dayofmonth==4
replace holiday=1 if year==2 & monthoftheyear==4 & dayofmonth==5

replace holiday=1 if year==3 & monthoftheyear==4 & dayofmonth==21
replace holiday=1 if year==3 & monthoftheyear==4 & dayofmonth==22
replace holiday=1 if year==3 & monthoftheyear==4 & dayofmonth==24
replace holiday=1 if year==3 & monthoftheyear==4 & dayofmonth==25

replace holiday=1 if year==4 & monthoftheyear==4 & dayofmonth==5
replace holiday=1 if year==4 & monthoftheyear==4 & dayofmonth==6
replace holiday=1 if year==4 & monthoftheyear==4 & dayofmonth==8
replace holiday=1 if year==4 & monthoftheyear==4 & dayofmonth==9

replace holiday=1 if year==5 & monthoftheyear==3 & dayofmonth==28
replace holiday=1 if year==5 & monthoftheyear==3 & dayofmonth==29
replace holiday=1 if year==5 & monthoftheyear==3 & dayofmonth==31
replace holiday=1 if year==5 & monthoftheyear==4 & dayofmonth==1

replace holiday=1 if year==6 & monthoftheyear==4 & dayofmonth==17
replace holiday=1 if year==6 & monthoftheyear==4 & dayofmonth==18
replace holiday=1 if year==6 & monthoftheyear==4 & dayofmonth==20
replace holiday=1 if year==6 & monthoftheyear==4 & dayofmonth==21

replace holiday=1 if year==7 & monthoftheyear==4 & dayofmonth==2
replace holiday=1 if year==7 & monthoftheyear==4 & dayofmonth==3
replace holiday=1 if year==7 & monthoftheyear==4 & dayofmonth==5
replace holiday=1 if year==7 & monthoftheyear==4 & dayofmonth==6

replace holiday=1 if year==8 & monthoftheyear==3 & dayofmonth==24
replace holiday=1 if year==8 & monthoftheyear==3 & dayofmonth==25
replace holiday=1 if year==8 & monthoftheyear==3 & dayofmonth==27
replace holiday=1 if year==8 & monthoftheyear==3 & dayofmonth==28

replace holiday=1 if year==9 & monthoftheyear==4 & dayofmonth==13
replace holiday=1 if year==9 & monthoftheyear==4 & dayofmonth==14
replace holiday=1 if year==9 & monthoftheyear==4 & dayofmonth==16
replace holiday=1 if year==9 & monthoftheyear==4 & dayofmonth==17

replace holiday=1 if year==10 & monthoftheyear==3 & dayofmonth==29
replace holiday=1 if year==10 & monthoftheyear==3 & dayofmonth==30
replace holiday=1 if year==10 & monthoftheyear==4 & dayofmonth==1
replace holiday=1 if year==10 & monthoftheyear==4 & dayofmonth==2

replace holiday=1 if monthoftheyear==5 & dayofmonth==1
replace holiday=1 if monthoftheyear==5 & dayofmonth==17
replace holiday=1 if monthoftheyear==5 & dayofmonth==30
replace holiday=1 if monthoftheyear==6 & dayofmonth==10
replace holiday=1 if monthoftheyear==12 & dayofmonth==25
replace holiday=1 if monthoftheyear==12 & dayofmonth==26
**Xmas varible
gen xmas=0
replace xmas=1 if monthoftheyear==12 & dayofmonth==25
replace xmas=1 if monthoftheyear==12 & dayofmonth==26
**Generate new variable 
gen NOx2=NOx * NOx
gen PM252=PM25 * PM25
gen Meanairtemperature2=Meanairtemperature * Meanairtemperature
gen Maximumairtemperature2=Maximumairtemperature * Maximumairtemperature
gen Minimumairtemperature2=Minimumairtemperature * Minimumairtemperature
gen Heatingdegreedays2=Heatingdegreedays * Heatingdegreedays
gen Vapourpressure2=Vapourpressure * Vapourpressure
gen Averageofmeanwindspeedfrom2=Averageofmeanwindspeedfrom * Averageofmeanwindspeedfrom
gen Meanwindgust2=Meanwindgust*Meanwindgust
gen Meanwindgust3=Meanwindgust*Meanwindgust*Meanwindgust
gen NOx3=NOx * NOx* NOx
gen PM253=PM25 * PM25* PM25
gen Meanairtemperature3=Meanairtemperature * Meanairtemperature * Meanairtemperature
gen Maximumairtemperature3=Maximumairtemperature * Maximumairtemperature * Maximumairtemperature
gen Minimumairtemperature3=Minimumairtemperature * Minimumairtemperature * Minimumairtemperature
gen Heatingdegreedays3=Heatingdegreedays * Heatingdegreedays * Heatingdegreedays
gen Vapourpressure3=Vapourpressure * Vapourpressure * Vapourpressure
gen Averageofmeanwindspeedfrom3=Averageofmeanwindspeedfrom * Averageofmeanwindspeedfrom * Averageofmeanwindspeedfrom




/// CVD total mortality as dependent variable-with full model
xtreg CVD_total_mortality l1.CVD_total_mortality l2.CVD_total_mortality l3.CVD_total_mortality l4.CVD_total_mortality l5.CVD_total_mortality l6.CVD_total_mortality Trafficvolume l1.Trafficvolume l2.Trafficvolume l3.Trafficvolume l4.Trafficvolume l5.Trafficvolume l6.Trafficvolume l7.Trafficvolume l8.Trafficvolume NOx l1.NOx l2.NOx l3.NOx l4.NOx l5.NOx l6.NOx l7.NOx l8.NOx PM25 l1.PM25 l2.PM25 l3.PM25 l4.PM25 l5.PM25 l6.PM25 l7.PM25 l8.PM25 Meanairtemperature l1.Meanairtemperature l2.Meanairtemperature l3.Meanairtemperature l4.Meanairtemperature l5.Meanairtemperature l6.Meanairtemperature l7.Meanairtemperature l8.Meanairtemperature Maximumairtemperature l1.Maximumairtemperature l2.Maximumairtemperature l3.Maximumairtemperature l4.Maximumairtemperature l5.Maximumairtemperature l6.Maximumairtemperature l7.Maximumairtemperature l8.Maximumairtemperature Minimumairtemperature l1.Minimumairtemperature l2.Minimumairtemperature  l3.Minimumairtemperature l4.Minimumairtemperature l5.Minimumairtemperature l6.Minimumairtemperature l7.Minimumairtemperature l8.Minimumairtemperature Heatingdegreedays l1.Heatingdegreedays l2.Heatingdegreedays l3.Heatingdegreedays l4.Heatingdegreedays l5.Heatingdegreedays l6.Heatingdegreedays l7.Heatingdegreedays l8.Heatingdegreedays Vapourpressure l1.Vapourpressure l2.Vapourpressure l3.Vapourpressure l4.Vapourpressure l5.Vapourpressure l6.Vapourpressure   l7.Vapourpressure l8.Vapourpressure Averageofmeanwindspeedfrom l1.Averageofmeanwindspeedfrom l2.Averageofmeanwindspeedfrom l3.Averageofmeanwindspeedfrom l4.Averageofmeanwindspeedfrom l5.Averageofmeanwindspeedfrom l6.Averageofmeanwindspeedfrom l7.Averageofmeanwindspeedfrom l8.Averageofmeanwindspeedfrom Meanwindgust l1.Meanwindgust l2.Meanwindgust l3.Meanwindgust l4.Meanwindgust l5.Meanwindgust l6.Meanwindgust l7.Meanwindgust l8.Meanwindgust c.NOx##c.Meanairtemperature l1.c.NOx##c.Meanairtemperature l2.c.NOx##c.Meanairtemperature l3.c.NOx##c.Meanairtemperature l4.c.NOx##c.Meanairtemperature l5.c.NOx##c.Meanairtemperature l6.c.NOx##c.Meanairtemperature l7.c.NOx##c.Meanairtemperature l8.c.NOx##c.Meanairtemperature c.NOx##c.Maximumairtemperature l.c.NOx##c.Maximumairtemperature l2.c.NOx##c.Maximumairtemperature l3.c.NOx##c.Maximumairtemperature l4.c.NOx##c.Maximumairtemperature l5.c.NOx##c.Maximumairtemperature l6.c.NOx##c.Maximumairtemperature l7.c.NOx##c.Maximumairtemperature l8.c.NOx##c.Maximumairtemperature c.NOx##c.Minimumairtemperature l1.c.NOx##c.Minimumairtemperature l2.c.NOx##c.Minimumairtemperature l3.c.NOx##c.Minimumairtemperature l4.c.NOx##c.Minimumairtemperature  l5.c.NOx##c.Minimumairtemperature l6.c.NOx##c.Minimumairtemperature l7.c.NOx##c.Minimumairtemperature l8.c.NOx##c.Minimumairtemperature c.NOx##c.Heatingdegreedays l1.c.NOx##c.Heatingdegreedays l2.c.NOx##c.Heatingdegreedays l3.c.NOx##c.Heatingdegreedays l4.c.NOx##c.Heatingdegreedays l5.c.NOx##c.Heatingdegreedays l6.c.NOx##c.Heatingdegreedays l7.c.NOx##c.Heatingdegreedays l8.c.NOx##c.Heatingdegreedays c.NOx##c.Vapourpressure l1.c.NOx##c.Vapourpressure l2.c.NOx##c.Vapourpressure l3.c.NOx##c.Vapourpressure l4.c.NOx##c.Vapourpressure l5.c.NOx##c.Vapourpressure l6.c.NOx##c.Vapourpressure l7.c.NOx##c.Vapourpressure l8.c.NOx##c.Vapourpressure c.NOx##c.Averageofmeanwindspeedfrom l1.c.NOx##c.Averageofmeanwindspeedfrom l2.c.NOx##c.Averageofmeanwindspeedfrom l3.c.NOx##c.Averageofmeanwindspeedfrom l4.c.NOx##c.Averageofmeanwindspeedfrom l5.c.NOx##c.Averageofmeanwindspeedfrom l6.c.NOx##c.Averageofmeanwindspeedfrom l7.c.NOx##c.Averageofmeanwindspeedfrom l8.c.NOx##c.Averageofmeanwindspeedfrom c.NOx##c.Meanwindgust l1.c.NOx##c.Meanwindgust l2.c.NOx##c.Meanwindgust l3.c.NOx##c.Meanwindgust l4.c.NOx##c.Meanwindgust l5.c.NOx##c.Meanwindgust l6.c.NOx##c.Meanwindgust l7.c.NOx##c.Meanwindgust l8.c.NOx##c.Meanwindgust c.NOx##c. Trafficvolume l1.c.NOx##c. Trafficvolume l2.c.NOx##c. Trafficvolume l3.c.NOx##c. Trafficvolume l4.c.NOx##c. Trafficvolume l5.c.NOx##c. Trafficvolume  l6.c.NOx##c. Trafficvolume l7.c.NOx##c. Trafficvolume  l8.c.NOx##c. Trafficvolume c.PM25##c.Meanairtemperature l1.c.PM25##c.Meanairtemperature l2.c.PM25##c.Meanairtemperature l3.c.PM25##c.Meanairtemperature l4.c.PM25##c.Meanairtemperature l5.c.PM25##c.Meanairtemperature l6.c.PM25##c.Meanairtemperature l7.c.PM25##c.Meanairtemperature  l8.c.PM25##c.Meanairtemperature c.PM25##c.Maximumairtemperature l.c.PM25##c.Maximumairtemperature l2.c.PM25##c.Maximumairtemperature l3.c.PM25##c.Maximumairtemperature l4.c.PM25##c.Maximumairtemperature l5.c.PM25##c.Maximumairtemperature l6.c.PM25##c.Maximumairtemperature l7.c.PM25##c.Maximumairtemperature l8.c.PM25##c.Maximumairtemperature c.PM25##c.Minimumairtemperature l.c.PM25##c.Minimumairtemperature l2.c.PM25##c.Minimumairtemperature l3.c.PM25##c.Minimumairtemperature l4.c.PM25##c.Minimumairtemperature l5.c.PM25##c.Minimumairtemperature l6.c.PM25##c.Minimumairtemperature l7.c.PM25##c.Minimumairtemperature l8.c.PM25##c.Minimumairtemperature c.PM25##c.Heatingdegreedays l1.c.PM25##c.Heatingdegreedays l2.c.PM25##c.Heatingdegreedays l3.c.PM25##c.Heatingdegreedays l4.c.PM25##c.Heatingdegreedays l5.c.PM25##c.Heatingdegreedays l6.c.PM25##c.Heatingdegreedays l7.c.PM25##c.Heatingdegreedays l8.c.PM25##c.Heatingdegreedays c.PM25##c.Vapourpressure l1.c.PM25##c.Vapourpressure l2.c.PM25##c.Vapourpressure l3.c.PM25##c.Vapourpressure l4.c.PM25##c.Vapourpressure l5.c.PM25##c.Vapourpressure  l6.c.PM25##c.Vapourpressure l7.c.PM25##c.Vapourpressure l8.c.PM25##c.Vapourpressure c.PM25##c.Averageofmeanwindspeedfrom l1.c.PM25##c.Averageofmeanwindspeedfrom l2.c.PM25##c.Averageofmeanwindspeedfrom l3.c.PM25##c.Averageofmeanwindspeedfrom l4.c.PM25##c.Averageofmeanwindspeedfrom l5.c.PM25##c.Averageofmeanwindspeedfrom l6.c.PM25##c.Averageofmeanwindspeedfrom l7.c.PM25##c.Averageofmeanwindspeedfrom l8.c.PM25##c.Averageofmeanwindspeedfrom c.PM25##c.Meanwindgust l1.c.PM25##c.Meanwindgust l2.c.PM25##c.Meanwindgust l3.c.PM25##c.Meanwindgust l4.c.PM25##c.Meanwindgust l5.c.PM25##c.Meanwindgust l6.c.PM25##c.Meanwindgust l7.c.PM25##c.Meanwindgust l8.c.PM25##c.Meanwindgust  c.PM25##c. Trafficvolume  l1.c.PM25##c. Trafficvolume l2.c.PM25##c. Trafficvolume  l3.c.PM25##c. Trafficvolume l4.c.PM25##c. Trafficvolume l5.c.PM25##c. Trafficvolume l6.c.PM25##c. Trafficvolume l7.c.PM25##c. Trafficvolume l8.c.PM25##c. Trafficvolume i.holiday i.xmas i.dayoftheweek i.dayofmonth i.monthoftheyear i.year, fe

estat ic
scalar ll = e(ll)
scalar n = _N
scalar pseudo_r2 = 1 - exp(ll / n)^(2 / n)
di "Pseudo R-squared: " pseudo_r2



 
*****Middle model
xtreg CVD_total_mortality l1.CVD_total_mortality l2.CVD_total_mortality l3.CVD_total_mortality l4.CVD_total_mortality l5.CVD_total_mortality l6.CVD_total_mortality Trafficvolume NOx PM25 Meanairtemperature Maximumairtemperature Minimumairtemperature Heatingdegreedays Vapourpressure Vapourpressure Averageofmeanwindspeedfrom Meanwindgust c.NOx##c.Meanairtemperature c.NOx##c.Maximumairtemperature c.NOx##c.Minimumairtemperature c.NOx##c.Heatingdegreedays c.NOx##c.Vapourpressure c.NOx##c.Averageofmeanwindspeedfrom c.NOx##c.Meanwindgust c.NOx##c. Trafficvolume  c.PM25##c.Meanairtemperature c.PM25##c.Maximumairtemperature c.PM25##c.Minimumairtemperature c.PM25##c.Heatingdegreedays c.PM25##c.Vapourpressure c.PM25##c.Averageofmeanwindspeedfrom c.PM25##c.Meanwindgust c.PM25##c. Trafficvolume  i.holiday i.xmas i.dayoftheweek i.dayofmonth i.monthoftheyear i.year,fe
estat ic
scalar ll = e(ll)
scalar n = _N
scalar pseudo_r2 = 1 - exp(ll / n)^(2 / n)
di "Pseudo R-squared: " pseudo_r2


est store fe_middle
estimates table, star(.05 .01 .001)
estimates table fe_middle, b(%10.3f) star
putdocx begin
putdocx table tbl1 = etable
putdocx save fe_middle
*****Lag model
 
xtreg CVD_total_mortality l1.CVD_total_mortality l2.CVD_total_mortality l3.CVD_total_mortality l4.CVD_total_mortality l5.CVD_total_mortality l6.CVD_total_mortality Trafficvolume l1.Trafficvolume l2.Trafficvolume l3.Trafficvolume l4.Trafficvolume l5.Trafficvolume l6.Trafficvolume l7.Trafficvolume l8.Trafficvolume NOx l1.NOx l2.NOx l3.NOx l4.NOx l5.NOx l6.NOx l7.NOx l8.NOx PM25 l1.PM25 l2.PM25 l3.PM25 l4.PM25 l5.PM25 l6.PM25 l7.PM25 l8.PM25 Meanairtemperature l1.Meanairtemperature l2.Meanairtemperature l3.Meanairtemperature l4.Meanairtemperature l5.Meanairtemperature l6.Meanairtemperature l7.Meanairtemperature l8.Meanairtemperature Maximumairtemperature l1.Maximumairtemperature l2.Maximumairtemperature l3.Maximumairtemperature l4.Maximumairtemperature l5.Maximumairtemperature l6.Maximumairtemperature l7.Maximumairtemperature l8.Maximumairtemperature Minimumairtemperature l1.Minimumairtemperature l2.Minimumairtemperature  l3.Minimumairtemperature l4.Minimumairtemperature l5.Minimumairtemperature l6.Minimumairtemperature l7.Minimumairtemperature l8.Minimumairtemperature Heatingdegreedays l1.Heatingdegreedays l2.Heatingdegreedays l3.Heatingdegreedays l4.Heatingdegreedays l5.Heatingdegreedays l6.Heatingdegreedays l7.Heatingdegreedays l8.Heatingdegreedays Vapourpressure l1.Vapourpressure l2.Vapourpressure l3.Vapourpressure l4.Vapourpressure l5.Vapourpressure l6.Vapourpressure   l7.Vapourpressure l8.Vapourpressure Averageofmeanwindspeedfrom l1.Averageofmeanwindspeedfrom l2.Averageofmeanwindspeedfrom l3.Averageofmeanwindspeedfrom l4.Averageofmeanwindspeedfrom l5.Averageofmeanwindspeedfrom l6.Averageofmeanwindspeedfrom l7.Averageofmeanwindspeedfrom l8.Averageofmeanwindspeedfrom Meanwindgust l1.Meanwindgust l2.Meanwindgust l3.Meanwindgust l4.Meanwindgust l5.Meanwindgust l6.Meanwindgust l7.Meanwindgust l8.Meanwindgust i.holiday i.xmas i.dayoftheweek i.dayofmonth i.monthoftheyear i.year , fe
estat ic
scalar ll = e(ll)
scalar n = _N
scalar pseudo_r2 = 1 - exp(ll / n)^(2 / n)
di "Pseudo R-squared: " pseudo_r2


est store fe_Lag
estimates table, star(.05 .01 .001)
estimates table fe_Lag, b(%10.3f) star
putdocx begin
putdocx table tbl1 = etable
putdocx save fe_Lag


 
*****simple Model
xtreg CVD_total_mortality l1.CVD_total_mortality l2.CVD_total_mortality l3.CVD_total_mortality l4.CVD_total_mortality l5.CVD_total_mortality l6.CVD_total_mortality Trafficvolume NOx PM25 Meanairtemperature Maximumairtemperature Minimumairtemperature Heatingdegreedays Vapourpressure Vapourpressure Averageofmeanwindspeedfrom Meanwindgust  i.holiday i.xmas i.dayoftheweek i.dayofmonth i.monthoftheyear i.year,fe
estat ic
scalar ll = e(ll)
scalar n = _N
scalar pseudo_r2 = 1 - exp(ll / n)^(2 / n)
di "Pseudo R-squared: " pseudo_r2


est store fe_simple
estimates table, star(.05 .01 .001)
estimates table fe_simple, b(%10.3f) star
putdocx begin
putdocx table tbl1 = etable
putdocx save fe_simple



///Done
