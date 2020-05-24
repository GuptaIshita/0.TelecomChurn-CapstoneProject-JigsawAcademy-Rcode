library(dataQualityR)
library(dplyr)
setwd("C:\\DS Full stack\\Graded Assignments\\09 - Capstone Project  and Certitication")
telecom=read.csv("telecomfinal.csv",stringsAsFactors = FALSE)
names(telecom)
#total - 66297 * 81

#Data Quality Report
checkDataQuality(telecom,out.file.num="telecom_numeric.csv",out.file.cat = "telecom_char.csv")
#telecom_numeric.csv : 60 variables
#telecom_char.csv : 21 variables

#ignoring variables where missing values >= 15% or not important: 36 variables
telecom=select(telecom,-da_Mean,-da_Range,-avg3mou,-avg3qty,-avg6mou,-avg6qty,
               -retdays,-eqpdays,-blck_dat_Mean,-drop_dat_Mean,
               -drop_vce_Mean,
               -car_buy,-cartype,-children,-div_type,-dwllsize,-dwlltype,
               -ethnic,-hnd_webcap,-mailordr,
               -mailresp,-marital,-occu1,-prizm_social_one,-proptype,
               -solflag,-wrkwoman,-age1,-age2,
               -forgntvl,-hnd_price,-income,-models,-mtrcycle,-numbcars,-truck)

#Derived variables
telecom$complete_Mean=telecom$comp_dat_Mean+telecom$comp_vce_Mean+
  telecom$plcd_dat_Mean+telecom$plcd_vce_Mean

telecom=select(telecom,-comp_dat_Mean,-comp_vce_Mean,-plcd_dat_Mean,-plcd_vce_Mean)

names(telecom)
#total variables - 42

#Decile Analysis for categorical variable : area
unique(telecom$area)
telecom%>%count(churn,levels=area)%>%filter(churn==1)->dc_area
dc_area$N<-unclass(telecom%>%filter(area%in%dc_area$levels)%>%count(area))[[2]]
dc_area$churn_per<-dc_area$n/dc_area$N
dc_area$varname<-rep("Geographic area",nrow(dc_area))
dc_area
# many levels

#Decile Analysis for categorical variable : asl_flag(asl)
unique(telecom$asl_flag)
telecom%>%count(churn,levels=asl_flag)%>%filter(churn==1)->dc_asl
dc_asl$N<-unclass(telecom%>%filter(asl_flag%in%dc_asl$levels)%>%count(asl_flag))[[2]]
dc_asl$churn_per<-dc_asl$n/dc_asl$N
dc_asl$varname<-rep("Account spending limit",nrow(dc_asl))
dc_asl
#only 2 levels, keeping as of now. will check p-value after first iteration & take decision.

#Decile Analysis for categorical variable : crclscod (sco)
unique(telecom$crclscod)
telecom%>%count(churn,levels=crclscod)%>%filter(churn==1)->dc_sco
dc_sco$N<-unclass(telecom%>%filter(crclscod%in%dc_sco$levels)%>%count(crclscod))[[2]]
dc_sco$churn_per<-dc_sco$n/dc_sco$N
dc_sco$varname<-rep("Credit class code",nrow(dc_sco))
dc_sco
#many levels

#Decile Analysis for categorical variable : csa
unique(telecom$csa)
telecom%>%count(churn,levels=csa)%>%filter(churn==1)->dc_csa
dc_csa$N<-unclass(telecom%>%filter(csa%in%dc_csa$levels)%>%count(csa))[[2]]
dc_csa$churn_per<-dc_csa$n/dc_csa$N
dc_csa$varname<-rep("Communications local service area",nrow(dc_csa))
dc_csa
#there are many levels, & increasing/decreasing trend both. so, ignoring 

#Decile Analysis for categorical variable : refurb_new
unique(telecom$refurb_new)
telecom%>%count(churn,levels=refurb_new)%>%filter(churn==1)->dc_rn
dc_rn$N<-unclass(telecom%>%filter(refurb_new%in%dc_rn$levels)%>%count(refurb_new))[[2]]
dc_rn$churn_per<-dc_rn$n/dc_rn$N
dc_rn$varname<-rep("Handset: refurbished or new",nrow(dc_rn))
dc_rn
##only 2 levels, keeping as of now. will check p-value after first iteration & take decision.

#Decile Analysis for continuous variable : total calls (totcalls)
unique(telecom$totcalls)
telecom%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_cal
dc_cal$N<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec))[[2]]
dc_cal$churn_per<-dc_cal$n/dc_cal$N
dc_cal$min<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
dc_cal$max<-unclass(telecom%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
dc_cal$varname<-rep("Total number of calls over the life of the customer",nrow(dc_cal))
dc_cal
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : total revenue (totrev)
unique(telecom$totrev)
telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_rev
dc_rev$N<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%count(dec))[[2]]
dc_rev$churn_per<-dc_rev$n/dc_rev$N
dc_rev$min<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
dc_rev$max<-unclass(telecom%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
dc_rev$varname<-rep("Total revenue",nrow(dc_rev))
dc_rev
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : months
unique(telecom$months)
telecom%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_months
dc_months$N<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%count(dec))[[2]]
dc_months$churn_per<-dc_months$n/dc_months$N
dc_months$min<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
dc_months$max<-unclass(telecom%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
dc_months$varname<-rep("Total number of months in service",nrow(dc_months))
dc_months
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : totmrc_Mean
unique(telecom$totmrc_Mean)
telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_totmrcM
dc_totmrcM$N<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
dc_totmrcM$churn_per<-dc_totmrcM$n/dc_totmrcM$N
dc_totmrcM$min<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
dc_totmrcM$max<-unclass(telecom%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
dc_totmrcM$varname<-rep("Mean total monthly recurring charge",nrow(dc_totmrcM))
dc_totmrcM
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : rev_Mean 
unique(telecom$rev_Mean)
telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_rev
dc_rev$N<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec))[[2]]
dc_rev$churn_per<-dc_rev$n/dc_rev$N
dc_rev$min<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
dc_rev$max<-unclass(telecom%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
dc_rev$varname<-rep("Mean monthly revenue (charge amount)",nrow(dc_rev))
dc_rev
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : mou_Mean 
unique(telecom$mou_Mean)
telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_mou
dc_mou$N<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec))[[2]]  
dc_mou$churn_per<-dc_mou$n/dc_mou$N
dc_mou$min<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
dc_mou$max<-unclass(telecom%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
dc_mou$varname<-rep("Mean number of monthly minutes of use",nrow(dc_mou))
dc_mou
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : rev_Range 
unique(telecom$rev_Range)
telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_revR
dc_revR$N<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec))[[2]]  
dc_revR$churn_per<-dc_revR$n/dc_revR$N
dc_revR$min<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
dc_revR$max<-unclass(telecom%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
dc_revR$varname<-rep("Range of Revenue (charge amount)",nrow(dc_revR))
dc_revR
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : mou_Range 
unique(telecom$mou_Range)
telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_mouR
dc_mouR$N<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec))[[2]]  
dc_mouR$churn_per<-dc_mouR$n/dc_mouR$N
dc_mouR$min<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
dc_mouR$max<-unclass(telecom%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
dc_mouR$varname<-rep("Range of number of minutes of use",nrow(dc_mouR))
dc_mouR
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : avgqty
unique(telecom$avgqty)
telecom%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_mnthL
dc_mnthL$N<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec))[[2]]
dc_mnthL$churn_per<-dc_mnthL$n/dc_mnthL$N
dc_mnthL$min<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
dc_mnthL$max<-unclass(telecom%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
dc_mnthL$varname<-rep("Average monthly number of calls over the life of the customer",nrow(dc_mnthL))
dc_mnthL
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : avgmou
unique(telecom$avgmou)
telecom%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_mnthL
dc_mnthL$N<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec))[[2]]
dc_mnthL$churn_per<-dc_mnthL$n/dc_mnthL$N
dc_mnthL$min<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
dc_mnthL$max<-unclass(telecom%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
dc_mnthL$varname<-rep("Average monthly minutes of use over the life of the customer",nrow(dc_mnthL))
dc_mnthL
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : avgrev
unique(telecom$avgrev)
telecom%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_avgrev
dc_avgrev$N<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec))[[2]]
dc_avgrev$churn_per<-dc_avgrev$n/dc_avgrev$N
dc_avgrev$min<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
dc_avgrev$max<-unclass(telecom%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
dc_avgrev$varname<-rep("Average monthly revenue over the life of the customer",nrow(dc_avgrev))
dc_avgrev
#increasing & decreasing trend, however event rates are closer to each other
#so keeping

#Decile Analysis for continuous variable : adjrev
unique(telecom$adjrev)
telecom%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_adjrev
dc_adjrev$N<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec))[[2]]
dc_adjrev$churn_per<-dc_adjrev$n/dc_adjrev$N
dc_adjrev$min<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
dc_adjrev$max<-unclass(telecom%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
dc_adjrev$varname<-rep("Billing adjusted total revenue over the life of the customer",nrow(dc_adjrev))
dc_adjrev
#increasing & decreasing trend, however event rates are closer to each other
#so keeping

#Decile Analysis for continuous variable : adjqty
unique(telecom$adjqty)
telecom%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_adjQ
dc_adjQ$N<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec))[[2]]
dc_adjQ$churn_per<-dc_adjQ$n/dc_adjQ$N
dc_adjQ$min<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
dc_adjQ$max<-unclass(telecom%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
dc_adjQ$varname<-rep("Billing adjusted total number of calls over the life of the customer",nrow(dc_adjQ))
dc_adjQ
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : adjmou
unique(telecom$adjmou)
telecom%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_adjmou
dc_adjmou$N<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec))[[2]]
dc_adjmou$churn_per<-dc_adjmou$n/dc_adjmou$N
dc_adjmou$min<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
dc_adjmou$max<-unclass(telecom%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
dc_adjmou$varname<-rep("Billing adjusted total minutes of use over the life of the customer",nrow(dc_adjmou))
dc_adjmou
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : ovrrev_Mean 
unique(telecom$ovrrev_Mean)
telecom%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_ovrrev
dc_ovrrev$N<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%count(dec))[[2]][-11][-10]  
dc_ovrrev$churn_per<-dc_ovrrev$n/dc_ovrrev$N
dc_ovrrev$min<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]][-11][-10]
dc_ovrrev$max<-unclass(telecom%>%mutate(dec=ntile(ovrrev_Mean,n=10))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]][-11][-10]
dc_ovrrev$varname<-rep("Mean overage revenue",nrow(dc_ovrrev))
dc_ovrrev
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : ovrmou_Mean 
unique(telecom$ovrmou_Mean)
telecom%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_ovrmou
dc_ovrmou$N<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%count(dec))[[2]][-11][-10]  
dc_ovrmou$churn_per<-dc_ovrmou$n/dc_ovrmou$N
dc_ovrmou$min<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]][-11][-10]
dc_ovrmou$max<-unclass(telecom%>%mutate(dec=ntile(ovrmou_Mean,n=10))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]][-11][-10]
dc_ovrmou$varname<-rep("Mean overage minutes of use",nrow(dc_ovrmou))
dc_ovrmou
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : datovr_Mean 
unique(telecom$datovr_Mean)
telecom%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_datovrM
dc_datovrM$N<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=10))%>%count(dec))[[2]][-11][-10][-9][-8][-7]  
dc_datovrM$churn_per<-dc_datovrM$n/dc_datovrM$N
dc_datovrM$min<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=10))%>%group_by(dec)%>%summarise(min(datovr_Mean)))[[2]][-11][-10][-9][-8][-7]
dc_datovrM$max<-unclass(telecom%>%mutate(dec=ntile(datovr_Mean,n=10))%>%group_by(dec)%>%summarise(max(datovr_Mean)))[[2]][-11][-10][-9][-8][-7]
dc_datovrM$varname<-rep("Mean revenue of data overage",nrow(dc_datovrM))
dc_datovrM
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : datovr_Range
unique(telecom$datovr_Range)
telecom%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_datovrR
dc_datovrR$N<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=10))%>%count(dec))[[2]][-11][-10][-9][-8][-7]
dc_datovrR$churn_per<-dc_datovrR$n/dc_datovrR$N
dc_datovrR$min<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=10))%>%group_by(dec)%>%summarise(min(datovr_Range)))[[2]][-11][-10][-9][-8][-7]
dc_datovrR$max<-unclass(telecom%>%mutate(dec=ntile(datovr_Range,n=10))%>%group_by(dec)%>%summarise(max(datovr_Range)))[[2]][-11][-10][-9][-8][-7]
dc_datovrR$varname<-rep("Range of revenue of data overage",nrow(dc_datovrR))
dc_datovrR
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : comp_dat_Mean
unique(telecom$comp_dat_Mean)
telecom%>%mutate(dec=ntile(comp_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_compM
dc_compM$N<-unclass(telecom%>%mutate(dec=ntile(comp_dat_Mean,n=10))%>%count(dec))[[2]][-10][-9][-8][-7][-6]
dc_compM$churn_per<-dc_compM$n/dc_compM$N
dc_compM$min<-unclass(telecom%>%mutate(dec=ntile(comp_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_dat_Mean)))[[2]][-10][-9][-8][-7][-6]
dc_compM$max<-unclass(telecom%>%mutate(dec=ntile(comp_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_dat_Mean)))[[2]][-10][-9][-8][-7][-6]
dc_compM$varname<-rep("Mean number of completed data calls",nrow(dc_compM))
dc_compM
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : comp_vce_Mean
unique(telecom$comp_vce_Mean)
telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_cmpM
dc_cmpM$N<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec))[[2]]
dc_cmpM$churn_per<-dc_cmpM$n/dc_cmpM$N
dc_cmpM$min<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
dc_cmpM$max<-unclass(telecom%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
dc_cmpM$varname<-rep("Mean number of completed voice calls",nrow(dc_cmpM))
dc_cmpM
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : plcd_dat_Mean
unique(telecom$plcd_dat_Mean)
telecom%>%mutate(dec=ntile(plcd_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_plcdM
dc_plcdM$N<-unclass(telecom%>%mutate(dec=ntile(plcd_dat_Mean,n=10))%>%count(dec))[[2]][-10][-9][-8][-7][-6]
dc_plcdM$churn_per<-dc_plcdM$n/dc_plcdM$N
dc_plcdM$min<-unclass(telecom%>%mutate(dec=ntile(plcd_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_dat_Mean)))[[2]][-10][-9][-8][-7][-6]
dc_plcdM$max<-unclass(telecom%>%mutate(dec=ntile(plcd_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_dat_Mean)))[[2]][-10][-9][-8][-7][-6]
dc_plcdM$varname<-rep("Mean number of attempted data calls placed",nrow(dc_plcdM))
dc_plcdM
#increasing & decreasing trend however, event rates close to each other
#so keeping

#Decile Analysis for continuous variable : plcd_vce_Mean
unique(telecom$plcd_vce_Mean)
telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_plcM
dc_plcM$N<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec))[[2]]
dc_plcM$churn_per<-dc_plcM$n/dc_plcM$N
dc_plcM$min<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
dc_plcM$max<-unclass(telecom%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
dc_plcM$varname<-rep("Mean number of attempted voice calls placed",nrow(dc_plcM))
dc_plcM
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : complete_Mean
unique(telecom$complete_Mean)
telecom%>%mutate(dec=ntile(complete_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_completeM
dc_completeM$N<-unclass(telecom%>%mutate(dec=ntile(complete_Mean,n=10))%>%count(dec))[[2]]
dc_completeM$churn_per<-dc_completeM$n/dc_completeM$N
dc_completeM$min<-unclass(telecom%>%mutate(dec=ntile(complete_Mean,n=10))%>%group_by(dec)%>%summarise(min(complete_Mean)))[[2]]
dc_completeM$max<-unclass(telecom%>%mutate(dec=ntile(complete_Mean,n=10))%>%group_by(dec)%>%summarise(max(complete_Mean)))[[2]]
dc_completeM$varname<-rep("Mean number of attempted & placed data and voice calls",nrow(dc_completeM))
dc_completeM
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : opk_dat_Mean
unique(telecom$opk_dat_Mean)
telecom%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_opkM
dc_opkM$N<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%count(dec))[[2]][-10][-9][-8][-7][-6][-5]
dc_opkM$churn_per<-dc_opkM$n/dc_opkM$N
dc_opkM$min<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%group_by(dec)%>%summarise(min(opk_dat_Mean)))[[2]][-10][-9][-8][-7][-6][-5]
dc_opkM$max<-unclass(telecom%>%mutate(dec=ntile(opk_dat_Mean,n=10))%>%group_by(dec)%>%summarise(max(opk_dat_Mean)))[[2]][-10][-9][-8][-7][-6][-5]
dc_opkM$varname<-rep("Mean number of off-peak data calls",nrow(dc_opkM))
dc_opkM
#decreasing trend, so keeping

#Decile Analysis for continuous variable : mou_pead_Mean
unique(telecom$mou_pead_Mean)
telecom%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_peadM
dc_peadM$N<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%count(dec))[[2]][-10][-9][-8][-7][-6][-5]
dc_peadM$churn_per<-dc_peadM$n/dc_peadM$N
dc_peadM$min<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_pead_Mean)))[[2]][-10][-9][-8][-7][-6][-5]
dc_peadM$max<-unclass(telecom%>%mutate(dec=ntile(mou_pead_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_pead_Mean)))[[2]][-10][-9][-8][-7][-6][-5]
dc_peadM$varname<-rep("Mean unrounded minutes of use of peak data calls",nrow(dc_peadM))
dc_peadM
#decreasing trend only, so keeping this variable

#Decile Analysis for continuous variable : mou_opkv_Range
unique(telecom$mou_opkv_Range)
telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_ofpkR
dc_ofpkR$N<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec))[[2]]
dc_ofpkR$churn_per<-dc_ofpkR$n/dc_ofpkR$N
dc_ofpkR$min<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
dc_ofpkR$max<-unclass(telecom%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
dc_ofpkR$varname<-rep("Range of unrounded minutes of use of off-peak voice calls",nrow(dc_ofpkR))
dc_ofpkR
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : iwylis_vce_Mean
unique(telecom$iwylis_vce_Mean)
telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_inbM
dc_inbM$N<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]][-10]
dc_inbM$churn_per<-dc_inbM$n/dc_inbM$N
dc_inbM$min<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]][-10]
dc_inbM$max<-unclass(telecom%>%mutate(dec=ntile(iwylis_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]][-10]
dc_inbM$varname<-rep("Mean number of inbound wireless to wireless voice calls",nrow(dc_inbM))
dc_inbM
#event rates are very close to each other, so keeping


#Decile Analysis for continuous variable : owylis_vce_Range
unique(telecom$owylis_vce_Range)
telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_outR
dc_outR$N<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec))[[2]]
dc_outR$churn_per<-dc_outR$n/dc_outR$N
dc_outR$min<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
dc_outR$max<-unclass(telecom%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
dc_outR$varname<-rep("Range of number of outbound wireless to wireless voice calls",nrow(dc_outR))
dc_outR
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : callwait_Mean
unique(telecom$callwait_Mean)
telecom%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_waitM
dc_waitM$N<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean,n=10))%>%count(dec)%>%unname())[[2]][-10][-9]
dc_waitM$churn_per<-dc_waitM$n/dc_waitM$N
dc_waitM$min<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean,n=10))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]][-10][-9]
dc_waitM$max<-unclass(telecom%>%mutate(dec=ntile(callwait_Mean,n=10))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]][-10][-9]
dc_waitM$varname<-rep("Mean number of call waiting calls",nrow(dc_waitM))
dc_waitM
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : recv_sms_Mean
unique(telecom$recv_sms_Mean)
telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_smsM
dc_smsM$N<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%count(dec))[[2]][-10][-9][-8][-7][-6][-5]
dc_smsM$churn_per<-dc_smsM$n/dc_smsM$N
dc_smsM$min<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%group_by(dec)%>%summarise(min(recv_sms_Mean)))[[2]][-10][-9][-8][-7][-6][-5]
dc_smsM$max<-unclass(telecom%>%mutate(dec=ntile(recv_sms_Mean,n=10))%>%group_by(dec)%>%summarise(max(recv_sms_Mean)))[[2]][-10][-9][-8][-7][-6][-5]
dc_smsM$varname<-rep("Mean number of received SMS calls",nrow(dc_smsM))
dc_smsM
#decreasing trend only, so keeping this variable

#Decile Analysis for continuous variable : callwait_Range
unique(telecom$callwait_Range)
telecom%>%mutate(dec=ntile(callwait_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_waitR
dc_waitR$N<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=10))%>%count(dec)%>%unname())[[2]][-10][-9][-8]
dc_waitR$churn_per<-dc_waitR$n/dc_waitR$N
dc_waitR$min<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=10))%>%group_by(dec)%>%summarise(min(callwait_Range)))[[2]][-10][-9][-8]
dc_waitR$max<-unclass(telecom%>%mutate(dec=ntile(callwait_Range,n=10))%>%group_by(dec)%>%summarise(max(callwait_Range)))[[2]][-10][-9][-8]
dc_waitR$varname<-rep("Range of number of call waiting calls",nrow(dc_waitR))
dc_waitR
#increasing & decreasing, so ingoring

#Decile Analysis for continuous variable : roam_Mean
unique(telecom$roam_Mean)
telecom%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_roamM
dc_roamM$N<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=10))%>%count(dec)%>%unname())[[2]][-11][-10][-9][-8]
dc_roamM$churn_per<-dc_roamM$n/dc_roamM$N
dc_roamM$min<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=10))%>%group_by(dec)%>%summarise(min(roam_Mean)))[[2]][-11][-10][-9][-8]
dc_roamM$max<-unclass(telecom%>%mutate(dec=ntile(roam_Mean,n=10))%>%group_by(dec)%>%summarise(max(roam_Mean)))[[2]][-11][-10][-9][-8]
dc_roamM$varname<-rep("Mean number of roaming waiting calls",nrow(dc_roamM))
dc_roamM
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : custcare_Mean
unique(telecom$custcare_Mean)
telecom%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_custM
dc_custM$N<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=10))%>%count(dec)%>%unname())[[2]][-10][-9][-8]
dc_custM$churn_per<-dc_custM$n/dc_custM$N
dc_custM$min<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%summarise(min(custcare_Mean)))[[2]][-10][-9][-8]
dc_custM$max<-unclass(telecom%>%mutate(dec=ntile(custcare_Mean,n=10))%>%group_by(dec)%>%summarise(max(custcare_Mean)))[[2]][-10][-9][-8]
dc_custM$varname<-rep("Mean number of customer care calls",nrow(dc_custM))
dc_custM
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : ccrndmou_Range
unique(telecom$ccrndmou_Range)
telecom%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_rndR
dc_rndR$N<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%count(dec)%>%unname())[[2]][-10][-9][-8]
dc_rndR$churn_per<-dc_rndR$n/dc_rndR$N
dc_rndR$min<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%group_by(dec)%>%summarise(min(ccrndmou_Range)))[[2]][-10][-9][-8]
dc_rndR$max<-unclass(telecom%>%mutate(dec=ntile(ccrndmou_Range,n=10))%>%group_by(dec)%>%summarise(max(ccrndmou_Range)))[[2]][-10][-9][-8]
dc_rndR$varname<-rep("Range of rounded minutes of use of customer care calls",nrow(dc_rndR))
dc_rndR
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : change_mou
unique(telecom$change_mou)
telecom%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_change_mou
dc_change_mou$N<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec))[[2]]
dc_change_mou$churn_per<-dc_change_mou$n/dc_change_mou$N
dc_change_mou$min<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou))) [[2]]
dc_change_mou$max<-unclass(telecom%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
dc_change_mou$varname<-rep("percent change in monthly minutes of use vs previous three months average",nrow(dc_change_mou))
dc_change_mou
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : drop_blk_Mean
unique(telecom$drop_blk_Mean)
telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_dropM
dc_dropM$N<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec))[[2]]
dc_dropM$churn_per<-dc_dropM$n/dc_dropM$N
dc_dropM$min<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
dc_dropM$max<-unclass(telecom%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
dc_dropM$varname<-rep("Mean number of dropped or blocked calls",nrow(dc_dropM))
dc_dropM
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : drop_vce_Range
unique(telecom$drop_vce_Range)
telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_dropRF
dc_dropRF$N<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec))[[2]]
dc_dropRF$churn_per<-dc_dropRF$n/dc_dropRF$N
dc_dropRF$min<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
dc_dropRF$max<-unclass(telecom%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
dc_dropRF$varname<-rep("Range of number of dropped (failed) voice calls",nrow(dc_dropRF))
dc_dropRF
#event rates are very close to each other, so keeping

#Decile Analysis for continuous variable : uniqsubs
unique(telecom$uniqsubs)
telecom%>%mutate(dec=ntile(uniqsubs,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_uniqsubs
dc_uniqsubs$N<-unclass(telecom%>%mutate(dec=ntile(uniqsubs,n=10))%>%count(dec))[[2]][-10][-9][-8][-7][-6]
dc_uniqsubs$churn_per<-dc_uniqsubs$n/dc_uniqsubs$N
dc_uniqsubs$min<-unclass(telecom%>%mutate(dec=ntile(uniqsubs,n=10))%>%group_by(dec)%>%summarise(min(uniqsubs)))[[2]][-10][-9][-8][-7][-6]
dc_uniqsubs$max<-unclass(telecom%>%mutate(dec=ntile(uniqsubs,n=10))%>%group_by(dec)%>%summarise(max(uniqsubs)))[[2]][-10][-9][-8][-7][-6]
dc_uniqsubs$varname<-rep("Number of unique subscribers in the household",nrow(dc_uniqsubs))
dc_uniqsubs
#increasing & decreasing, so ingoring

#Decile Analysis for continuous variable : actvsubs
unique(telecom$actvsubs)
telecom%>%mutate(dec=ntile(actvsubs,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_actvsubs
dc_actvsubs$N<-unclass(telecom%>%mutate(dec=ntile(actvsubs,n=10))%>%count(dec))[[2]][-10][-9][-8][-7][-6][-5]
dc_actvsubs$churn_per<-dc_actvsubs$n/dc_actvsubs$N
dc_actvsubs$min<-unclass(telecom%>%mutate(dec=ntile(actvsubs,n=10))%>%group_by(dec)%>%summarise(min(actvsubs)))[[2]][-10][-9][-8][-7][-6][-5]
dc_actvsubs$max<-unclass(telecom%>%mutate(dec=ntile(actvsubs,n=10))%>%group_by(dec)%>%summarise(max(actvsubs)))[[2]][-10][-9][-8][-7][-6][-5]
dc_actvsubs$varname<-rep("Number of active subscribers in household",nrow(dc_actvsubs))
dc_actvsubs
#only decreasing trend so keeping this variable


#Decile Analysis for continuous variable : custid
unique(telecom$Customer_ID)
telecom%>%mutate(dec=ntile(Customer_ID,n=10))%>%count(churn,dec)%>%filter(churn==1)->dc_custid
dc_custid$N<-unclass(telecom%>%mutate(dec=ntile(Customer_ID,n=10))%>%count(dec))[[2]]
dc_custid$churn_per<-dc_custid$n/dc_custid$N
dc_custid$min<-unclass(telecom%>%mutate(dec=ntile(Customer_ID,n=10))%>%group_by(dec)%>%summarise(min(Customer_ID))) [[2]]
dc_custid$max<-unclass(telecom%>%mutate(dec=ntile(Customer_ID,n=10))%>%group_by(dec)%>%summarise(max(Customer_ID)))[[2]]
dc_custid$varname<-rep("unique tournament specific customer id for scoring purposes",nrow(dc_custid))
dc_custid
#event rates are very close to each other, so keeping

#Decile analysis outcome :
#Following are the variables not to include in analysis -
#callwait_Range,uniqsubs,csa

telecom=select(telecom,-callwait_Range,-uniqsubs,-csa)

names(telecom)               
#total - 39 variables 

#treatement of missing value 
#sum(is.na(telecom$refurb_new))
#head(telecom$refurb_new)
#table(telecom$refurb_new)
telecom$refurb_new[is.na(telecom$refurb_new)]<-'R'

#head(telecom$area)
#sum(is.na(telecom$area))
#unique(telecom$area)
#table(telecom$area)
telecom$area[is.na(telecom$area)]<-'Others'

sum(is.na(telecom$totmrc_Mean)) #181 missing values
sum(is.na(telecom$rev_Mean)) #181 missing values
sum(is.na(telecom$mou_Mean)) #181 missing values
sum(is.na(telecom$rev_Range)) #181 missing values
sum(is.na(telecom$mou_Range)) #181 missing values
sum(is.na(telecom$ovrrev_Mean)) #181 missing values
sum(is.na(telecom$ovrmou_Mean)) #181 missing values
sum(is.na(telecom$datovr_Mean)) #181 missing values
sum(is.na(telecom$datovr_Range)) #181 missing values
sum(is.na(telecom$roam_Mean)) #181 missing values
sum(is.na(telecom$change_mou)) #414 missing values

mean(telecom$totmrc_Mean,na.rm=TRUE)  #47
mean(telecom$rev_Mean,na.rm=TRUE)  #59
mean(telecom$mou_Mean,na.rm=TRUE)  #529
mean(telecom$rev_Range,na.rm=TRUE)  #44
mean(telecom$mou_Range,na.rm=TRUE)  #376
mean(telecom$ovrrev_Mean,na.rm=TRUE)  #13
mean(telecom$ovrmou_Mean,na.rm=TRUE)  #40
mean(telecom$datovr_Mean,na.rm=TRUE)  #0.25
mean(telecom$datovr_Range,na.rm=TRUE)  #0.72
mean(telecom$roam_Mean,na.rm=TRUE)  #1.2
mean(telecom$change_mou,na.rm=TRUE)  #-9

telecom$totmrc_Mean[is.na(telecom$totmrc_Mean)]<-47
telecom$rev_Mean[is.na(telecom$rev_Mean)]<-59
telecom$mou_Mean[is.na(telecom$mou_Mean)]<-529
telecom$rev_Range[is.na(telecom$rev_Range)]<-44
telecom$mou_Range[is.na(telecom$mou_Range)]<-376
telecom$ovrrev_Mean[is.na(telecom$ovrrev_Mean)]<-13
telecom$ovrmou_Mean[is.na(telecom$ovrmou_Mean)]<-40
telecom$datovr_Mean[is.na(telecom$datovr_Mean)]<-0.25
telecom$datovr_Range[is.na(telecom$datovr_Range)]<-0.72
telecom$roam_Mean[is.na(telecom$roam_Mean)]<-1.2
telecom$change_mou[is.na(telecom$change_mou)]<--9

sum(is.na(telecom))
summary(telecom)

#Converting continuous variables to categorical
#drop_blk_Mean
min(telecom$drop_blk_Mean)  #0
max(telecom$drop_blk_Mean)  #489.6
mean(telecom$drop_blk_Mean)  #10.18
head(telecom$drop_blk_Mean)
telecom$drop_blk_Mean<-ifelse(telecom$drop_blk_Mean>=0 & telecom$drop_blk_Mean<=10,"lesser than avg", "Greater than avg")
head(telecom$drop_blk_Mean)
telecom$drop_blk_Mean<-as.factor(telecom$drop_blk_Mean)

#reducing levels for categorical variables 
#area
data_area=data.frame(levels=dc_area$levels,churn_per=dc_area$churn_per)
head(data_area)
unique(data_area)
j<-1
for (i in telecom$area)
{
  #print(i)
  if (i=="TENNESSEE AREA" || i=="MIDWEST AREA" || i=="CENTRAL/SOUTH TEXAS AREA" || 
      i=="HOUSTON AREA" || i=="OHIO AREA" || i=="DC/MARYLAND/VIRGINIA AREA" ||
      i=="GREAT LAKES AREA" || i=="ATLANTIC SOUTH AREA" || i=="DALLAS AREA" || 
      i=="Others") 
  {  telecom[j,"area_Level"]<-1}
  else
  {telecom[j,"area_Level"]<-2}
  j=j+1
}

#reducing levels for 'crclscod'
data_sco=data.frame(levels=dc_sco$levels,churn_per=dc_sco$churn_per)
library(rpart)
mod_sco<-rpart(levels~churn_per,data=data_sco,method="class")
unique(mod_sco$where)  #4,6,7,8,9
data_sco$nodes<-mod_sco$where
head(data_sco)
order_sco<-arrange(data_sco,churn_per)

filter(data_sco,nodes==4) #B2,C,CA,CC,D,I,O,U,ZY :0.2142-0.2439
filter(data_sco,nodes==6) #A,AA,BA,D2,G,JF,Z,Z2,ZA:0.2449-0.2600
filter(data_sco,nodes==7) #A2,A3,B,EF,EM,GA,GY,IF,K,M,P1,TP,Z1:0.2655-0.6666
filter(data_sco,nodes==8) #C2,CY,DA,E,EA,J,L,U1,W,Z4:0.1617-0.2047
filter(data_sco,nodes==9) #C5,D4,D5,E2,E4,EC,V1,Y,Z5:0.0312-0.1600

j<-1
for (i in telecom$crclscod)
{
  #print(i)
  if (i=="A" || i=="AA" || i=="BA" || i=="D2" || i=="G" || i=="JF" || i=="Z" ||
      i=="Z2" || i=="ZA" || i=="B2" || i=="C" || i=="CA" || i=="CC" || i=="D" ||
      i=="I" || i=="O" || i=="U" || i=="ZY" || i=="A2" || i=="A3" || i=="B" ||
      i=="EF" || i=="EM" || i=="GA" || i=="GY" || i=="IF" || i=="K" || i=="M" ||
      i=="P1" || i=="TP" || i=="Z1") 
  {  telecom[j,"crclscod_Level"]<-1}
  else if (i=="C2"|| i=="CY"|| i=="DA" || i=="E" || i=="EA" || i=="J" || i=="L" ||
           i=="U1" || i=="W" || i=="Z4")
  {telecom[j,"crclscod_Level"]<-2}
  else
  {telecom[j,"crclscod_Level"]<-3}
  j=j+1
}

#Building the model
#converting all variables into numeric form
str(telecom)
telecom$asl_flag<-ifelse(telecom$asl_flag=="Y",1,0)
telecom$refurb_new<-ifelse(telecom$refurb_new=="R",1,0)
telecom$drop_blk_Mean<-ifelse(telecom$drop_blk_Mean=="Greater than avg",1,0)
telecom$area<-ifelse(telecom$area_Level==1,1,0)
telecom$crclscod<-ifelse(telecom$crclscod_Level==1,1,0)

#removing some dummy variables variables now
telecom=select(telecom,-crclscod_Level,-area_Level)
names(telecom)
#39 variables

#split the data into train & test
set.seed(123)
index<-sort(sample(nrow(telecom),nrow(telecom)*0.8))
train<-telecom[index,]
test<-telecom[-index,]
#train - 53037 * 39
# test - 13260 * 39

#glm model

names(telecom)
model=glm(churn ~ ., family="binomial",data=train)
summary(model)
#AIC-57212

#removing insignificant variables
#owylis_vce_Range,months,custcare_Mean,callwait_Mean,
#ccrndmou_Range,ovrmou_Mean,avgqty,actvsubs,opk_dat_Mean,roam_Mean,
#recv_sms_Mean,mou_pead_Mean,datovr_Mean,adjmou,adjrev,avgrev,totrev
model=glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+
            drop_blk_Mean+drop_vce_Range+mou_opkv_Range+
            totcalls+iwylis_vce_Mean+
            adjqty+ovrrev_Mean+rev_Mean+avgmou+
            crclscod+asl_flag+area+refurb_new+
            datovr_Range+Customer_ID+
            complete_Mean,
          family="binomial",data=train)
summary(model)
#AIC-57239

#removing insignificant variable
#change_mou,iwylis_vce_Mean,
model=glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+
            drop_blk_Mean+drop_vce_Range+mou_opkv_Range+
            totcalls+
            adjqty+ovrrev_Mean+rev_Mean+avgmou+
            crclscod+asl_flag+area+refurb_new+
            datovr_Range+Customer_ID+
            complete_Mean,
          family="binomial",data=train)
summary(model)
#AIC-57240

#removing variables which are not important for analysis
#datovr_Range,Customer_ID,
#asl_flag,refurb_new,crclscod,
#rev_Range,mou_Range,drop_vce_Range,mou_opkv_Range,
model=glm(churn~mou_Mean+totmrc_Mean+
            drop_blk_Mean+
            totcalls+
            adjqty+ovrrev_Mean+rev_Mean+avgmou+
            area+
            complete_Mean,
          family="binomial",data=train)
summary(model)
#AIC-57691
#All 10 variables are significant now
#dof calc:
53037-1  #53036
53036-10  #53026

#using step
model1=glm(churn~.,family="binomial",data=train)
step(model1,direction="both")
#AIC - 57200, 28 variables significant

final=glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+
            drop_blk_Mean+drop_vce_Range+mou_opkv_Range+
            months+totcalls+custcare_Mean+iwylis_vce_Mean+
            adjqty+ovrrev_Mean+rev_Mean+avgmou+
            crclscod+asl_flag+area+refurb_new+actvsubs+
            roam_Mean+recv_sms_Mean+mou_pead_Mean+
            datovr_Range+adjrev+Customer_ID+
            complete_Mean,
          family="binomial",data=train)
summary(final)
#AIC - 57200

#removing insignificant variables
#custcare_Mean,actvsubs,roam_Mean,recv_sms_Mean,mou_pead_Mean
final=glm(churn~mou_Mean+totmrc_Mean+rev_Range+mou_Range+change_mou+
            drop_blk_Mean+drop_vce_Range+mou_opkv_Range+
            months+totcalls+iwylis_vce_Mean+
            adjqty+ovrrev_Mean+rev_Mean+avgmou+
            crclscod+asl_flag+area+refurb_new+
            datovr_Range+adjrev+Customer_ID+
            complete_Mean,
          family="binomial",data=train)
summary(final)
#AIC - 57203

#removing variables which are not important for analysis
#asl_flag,refurb_new,crclscod,
#rev_Range,mou_Range,drop_vce_Range,mou_opkv_Range,months,
#datovr_Range,Customer_ID
final=glm(churn~mou_Mean+totmrc_Mean+change_mou+
            drop_blk_Mean+
            totcalls+iwylis_vce_Mean+
            adjqty+ovrrev_Mean+rev_Mean+avgmou+
            area+
            adjrev+
            complete_Mean,
          family="binomial",data=train)
summary(final)
AIC-57685

#removing variables which are not significant
#adjrev, iwylis_vce_Mean
final=glm(churn~mou_Mean+totmrc_Mean+change_mou+
            drop_blk_Mean+
            totcalls+
            adjqty+ovrrev_Mean+rev_Mean+avgmou+
            area+
            complete_Mean,
          family="binomial",data=train)
summary(final)
AIC-57683
#11 variables significant
53036-11  #53025
#the best model as here AIC is the lowest & all variables significant/important

#Model Validations
#1) vif
library(car)
vif(final)
#As it should be <5, some of the variables are highly co-related
#variables >10, totcalls, adjqty

#removing variables which have high co-relation
final=glm(churn~mou_Mean+totmrc_Mean+change_mou+
            drop_blk_Mean+ovrrev_Mean+rev_Mean+avgmou+area+
            complete_Mean,
          family="binomial",data=train)
summary(final)
AIC-57704
#9 variables significant
53036-9  #53027

vif(final)
#the best model as here AIC is the lowest & all variables significant/important
# & no variable is highly co-related 


#2) confusion matrix & Area under curve (AUC)
prob=predict(final,newdata=test,type="response")
head(prob)
library(ROCR)
names(test)
#Plotting ROCR curve
pred=prediction(prob,test$churn)
roc=performance(pred,'tpr',"fpr")
plot(roc)
abline(0,1)

#confusion matrix
class=ifelse(prob>=0.6,1,0)
t=table(class,test$churn)
t
sum(diag(t))/nrow(test)
#76% accuracy at 0.6 cut-off

#checking AUC value (the hightest one will be a good model)
auc=performance(pred,"auc")
auc
auc=unlist(slot(auc,"y.values"))
auc    #58% accurate

#####Customer Segments##################
names(test)
Cust_Sgmnt=data.frame(Revenue=test$totrev,
                      Prob_Churn=predict(final,newdata=test,type="response"),
                      CustomerID=test$Customer_ID)

head(Cust_Sgmnt)
summary(Cust_Sgmnt$Revenue)
#min - 20
#1st Quartile-510
#2nd Quartile-806
#3rd Quartile-1276
#4th Quartile-13427
# ------700-----10500-----14000

#high/med/low revenue
#high/med/low probability of churn
head(Cust_Sgmnt)

Cust_Sgmnt$Rev_Bucket[Cust_Sgmnt$Revenue<=700]<-"Low_Rev"
Cust_Sgmnt$Rev_Bucket[Cust_Sgmnt$Revenue>700 & Cust_Sgmnt$Revenue<10500]<-"Med_Rev"
Cust_Sgmnt$Rev_Bucket[Cust_Sgmnt$Revenue>10500]<-"High_Rev"

summary(Cust_Sgmnt$Prob_Churn)
#min-0.00957
#1st Quartile-0.20
#2nd Quartile-0.24
#3rd Quartile-0.28
#4th Quartile-0.84
# ------0.22-----0.65-----0.90

Cust_Sgmnt$Prob_Bucket[Cust_Sgmnt$Prob_Churn<=0.22]<-"Low_Prob"
Cust_Sgmnt$Prob_Bucket[Cust_Sgmnt$Prob_Churn>0.22 & Cust_Sgmnt$Prob_Churn<0.65]<-"Med_Prob"
Cust_Sgmnt$Prob_Bucket[Cust_Sgmnt$Prob_Churn>0.65]<-"High_Prob"

head(Cust_Sgmnt)

####No of Customers in each customer segment####
table(Cust_Sgmnt$Rev_Bucket,Cust_Sgmnt$Prob_Bucket)

Cust_Sgmnt%>%filter(Rev_Bucket=="High_Rev"&Prob_Bucket=="High_Prob")%>%select(CustomerID)
#High_Prob, High_Rev = 0

Cust_Sgmnt%>%filter(Rev_Bucket=="Med_Rev"&Prob_Bucket=="High_Prob")%>%select(CustomerID)
#High_Prob, Med_Rev  = 3 Customers
#CustomerID - 1078686, 1076359, 1093990

Cust_Sgmnt%>%filter(Rev_Bucket=="High_Rev"&Prob_Bucket=="Med_Prob")%>%select(CustomerID)
#Med_Prob, High_Rev = 5 Customers
#CustomerID- 1010652, 1000930, 1001662, 1001990, 1009400

###############End of the Code###########################