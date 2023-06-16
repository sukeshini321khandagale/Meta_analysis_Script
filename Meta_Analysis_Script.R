
##Script to meta-analysis for different parameters
####MetS vs Non MetS##################################
getwd()
setwd("D:/Systematic_Review_paper1/CSV_results")

#################################################################################################################
library(BiocManager)
library(metafor)
library(esc)
library(esvis)
library(plyr)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(esc)
library(tidyverse)
data<-read.csv("D:/Systematic_Review_paper1/MetS_vs_no_MetS.csv")

#################################################################################################################
##data for duration of diabetes

df_metS<-data[, c(1,4:9),] # dataframe for diabetes duration
hedges_g<-esc_mean_sd(grp1m = df_metS$metS_duration_mean, grp1sd = df_metS$mets_duration_sd, grp1n = df_metS$Met_Sample_Size,
                      grp2m = df_metS$NS_duration_mean, grp2sd = df_metS$NS_duration_sd, grp2n = df_metS$No_Met_Sample_Size, 
                      es.type = "g", study = df_metS$Author)

#object to subject for forest plot
res<- rma(yi = hedges_g$es,     # The d-column of the df, which contains Cohen's d
              vi = hedges_g$var) 

#forest plot
forest.rma(res, xlim=c(-6,5), cex=1.5, header="Author(s) and Year",
       slab = hedges_g$study, xlab = "Hedge's g", showweights = TRUE)  #randomised forest plot


### add text for stats for heterogeneity
text(-6,-1, pos=4, cex=1.5, bquote(paste("RE Model(Q = ",
                                         .(formatC(res$QE, digits=2, format="f")),", df = ",.(res$k - res$p),
                                         ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                         .(formatC(res$I2, digits=1, format="f")), "%)")))

### add text for test of overall effect
text(-6, -1.5, pos=4, cex=1.5, bquote(paste("Test for overall effect: Z=",
                                          .(formatC(res$zval, digits=2, format="f")),
                                          ", P", .(ifelse(res$pval<.001, "<0.001",
                                                          paste0("=",formatC(res$pval, digits=2, format="f")))))))

funnel(res, main="duration of diabetes")
##results I2 = 0%, pval=0.3592
##se=0.164  , pval=0.20

#################################################################################################################
##data insulin units # homogenous data so no removal of studies
df_INS_units<-data[, c(1,4,5,10:13), ] # dataframe for BMI
hedges_g<-esc_mean_sd(grp1m = df_INS_units$MetS_insulin_mean, grp1sd = df_INS_units$MetS_insulin_sd, grp1n = df_INS_units$Met_Sample_Size ,
                      grp2m = df_INS_units$NS_insulin_mean, grp2sd = df_INS_units$NS_insulin_sd, grp2n = df_INS_units$No_Met_Sample_Size, 
                      es.type = "g", study = df_INS_units$Author)

res<- rma(yi = hedges_g$es,     # The d-column of the df, which contains Cohen's d
          vi = hedges_g$var) 

#Forest plot
forest(res, xlim=c(-6,5), cex=1.5, header="Author(s) and Year",
       slab = hedges_g$study, xlab = "Hedge's g", showweights = TRUE) #randomised forest plot

### add text for stats for heterogeneity
text(-6,-1, pos=4, cex=1.5, bquote(paste("RE Model(Q = ",
                                       .(formatC(res$QE, digits=2, format="f")),", df = ",.(res$k - res$p),
                                       ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                       .(formatC(res$I2, digits=1, format="f")), "%)")))

### add text for test of overall effect
text(-6, -1.5, pos=4, cex=1.5, bquote(paste("Test for overall effect: Z=",
                                            .(formatC(res$zval, digits=2, format="f")),
                                            ", P", .(ifelse(res$pval<.001, "<0.001",
                                                            paste0("=",formatC(res$pval, digits=2, format="f")))))))

funnel(res, main="Units of insulin")
##results I2=0%, pval=0.15

##################################################################################################################
##data for HbA1C 
df_HbA1C<- data[, c(1,4,5,14:17), ]
#df_HbA1C<-df_HbA1C[-c(4), ]#to remove one study that shows max heterogeneity
hedges_g<-esc_mean_sd(grp1m = df_HbA1C$metS_HbA1c_mean, grp1sd = df_HbA1C$metS_HbA1c_sd, grp1n = df_HbA1C$Met_Sample_Size ,
                      grp2m = df_HbA1C$NS_HbA1c_mean, grp2sd = df_HbA1C$NS_HbA1c_sd, grp2n = df_HbA1C$No_Met_Sample_Size, 
                      es.type = "g", study = df_HbA1C$Author)

res<- rma(yi = hedges_g$es, # The d-column of the df, which contains Cohen's d
          vi = hedges_g$var) 
#forest plot
forest(res, xlim=c(-6,5), cex=1.5, header="Author(s) and Year",
       slab = hedges_g$study, xlab = "Hedge's g", showweights = TRUE) #randomised forest plot

## add text for heterogeneity significance
text(-6,-1, pos=4, cex=1.5, bquote(paste("RE Model(Q = ",
                                       .(formatC(res$QE, digits=2, format="f")),", df = ",.(res$k - res$p),
                                       ", p = ", .(formatC(res$QEp, digits=3, format="f")), "; ", I^2, " = ",
                                       .(formatC(res$I2, digits=1, format="f")), "%)")))
### add text for test of overall effect
text(-6, -1.5, pos=4, cex=1.5, bquote(paste("Test for overall effect: Z=",
                                            .(formatC(res$zval, digits=2, format="f")),
                                            ", P", .(ifelse(res$pval<.001, "<0.001",
                                                            paste0("=",formatC(res$pval, digits=2, format="f")))))))
funnel(res, main="HbA1C")
##Results  I2=94.3%, pval=0.12
##outlier is the Study from monika grabria may be because we coonverted median and IQR to mean and sd

###################################################################################################################
##data for waist circumference
df_WC<- data[, c(1,4,5,18:21), ]
df_WC<- df_WC[-c(2), ]#to remove one study that shows max heterogeneity
#df_WC<- df_WC[-c(4), ]

hedges_g<-esc_mean_sd(grp1m = df_WC$metS_WC_mean, grp1sd = df_WC$metS_WC_sd, grp1n = df_WC$Met_Sample_Size ,
                      grp2m = df_WC$NS_WC_mean, grp2sd = df_WC$NS_WC_sd, grp2n = df_WC$No_Met_Sample_Size, 
                      es.type = "g", study = df_WC$Author)
res<- rma(yi = hedges_g$es,     # The d-column of the df, which contains Cohen's d
              vi = hedges_g$var) 
#Forest plot
forest(res, xlim=c(-6,6), cex=1.5, header="Author(s) and Year",
       slab = hedges_g$study, xlab = "Hedge's g", showweights = TRUE) #randomised forest plot

text(-6,-1, pos=4, cex=1.5, bquote(paste("RE Model(Q = ",
                                       .(formatC(res$QE, digits=2, format="f")),", df = ",.(res$k - res$p),
                                       ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                       .(formatC(res$I2, digits=1, format="f")), "%)")))
### add text for test of overall effect
text(-6, -1.5, pos=4, cex=1.5, bquote(paste("Test for overall effect: Z=",
                                            .(formatC(res$zval, digits=2, format="f")),
                                            ", P", .(ifelse(res$pval<.001, "<0.001",
                                                            paste0("=",formatC(res$pval, digits=2, format="f")))))))
funnel(res, main="Waist circunference")
baujat(res)
##Result I2=82.6%, pval=0.00
## heterogeneity reduced by removing soliman 2019 ## I2 = 0.0%, pval=0.000000

###################################################################################################################
##data for TG
df_TG<- data[, c(1,4,5,22:25), ]
df_TG<- df_TG[-c(4), ]#to remove one study that shows max heterogeneity
hedges_g<-esc_mean_sd(grp1m = df_TG$metS_TG_mean, grp1sd = df_TG$metS_TG_sd, grp1n = df_TG$Met_Sample_Size,
                      grp2m = df_TG$NS_TG_mean, grp2sd = df_TG$NS_TG_sd, grp2n = df_TG$No_Met_Sample_Size, 
                      es.type = "g", study = df_TG$Author)
res<- rma(yi = hedges_g$es,     # The d-column of the df, which contains Cohen's d
         vi = hedges_g$var) 
#Forest plot
forest(res, xlim=c(-6,6), cex=1.5, header="Author(s) and Year",
       slab = hedges_g$study, xlab = "Hedge's g", showweights = TRUE) #randomised forest plot
#text for heterogeneity
text(-6,-1, pos=4, cex=1.5, bquote(paste("RE Model(Q = ",
                                       .(formatC(res$QE, digits=2, format="f")),", df = ",.(res$k - res$p),
                                       ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                       .(formatC(res$I2, digits=1, format="f")), "%)")))
### add text for test of overall effect
text(-6, -1.5, pos=4, cex=1.5, bquote(paste("Test for overall effect: Z=",
                                            .(formatC(res$zval, digits=2, format="f")),
                                            ", P", .(ifelse(res$pval<.001, "<0.001",
                                                            paste0("=",formatC(res$pval, digits=2, format="f")))))))

funnel(res, main="Triglyceride")
##Result I2= 83.5%, pval=2.63
## heterogenirty removed by excluding monika gabria study

###################################################################################################################
##data for HDL
df_HDL<- data[, c(1,4,5,26:29), ]#homogenous data so no removal of datasets
hedges_g<-esc_mean_sd(grp1m = df_HDL$metS_HDL_mean, grp1sd = df_HDL$metS_HDL_sd, grp1n = df_HDL$Met_Sample_Size,
                      grp2m = df_HDL$NS_HDL_mean, grp2sd = df_HDL$NS_HDL_sd, grp2n = df_HDL$No_Met_Sample_Size, 
                      es.type = "g", study = df_HDL$Author)
res<- rma(yi = hedges_g$es,     # The d-column of the df, which contains Cohen's d
            vi = hedges_g$var) 
#Forest Plot
forest(res, xlim=c(-6,4), cex=1.5, header="Author(s) and Year",
       slab = hedges_g$study, xlab = "Hedge's g", showweights = TRUE) #randomised forest plot
#text for heterogeneity significance
text(-6,-1, pos=4, cex=1.5, bquote(paste("RE Model(Q = ",
                                       .(formatC(res$QE, digits=2, format="f")),", df = ",.(res$k - res$p),
                                       ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                       .(formatC(res$I2, digits=1, format="f")), "%)")))
### add text for test of overall effect
text(-6, -1.5, pos=4, cex=1.5, bquote(paste("Test for overall effect: Z=",
                                            .(formatC(res$zval, digits=2, format="f")),
                                            ", P", .(ifelse(res$pval<.001, "<0.001",
                                                            paste0("=",formatC(res$pval, digits=2, format="f")))))))
funnel(res, main="HDL")
##Result I2=0%, pval= 0.01

###################################################################################################################
##data for LDL
df_LDL<- data[, c(1,4,5,30:33), ]#to remove one study that shows max heterogeneity
df_LDL<-df_LDL[-c(4), ]
hedges_g<-esc_mean_sd(grp1m = df_LDL$metS_LDL_mean, grp1sd = df_LDL$metS_LDL_sd, grp1n = df_LDL$Met_Sample_Size,
                      grp2m = df_LDL$NS_LDL_mean, grp2sd = df_LDL$NS_LDL_sd, grp2n = df_LDL$No_Met_Sample_Size, 
                      es.type = "g", study = df_LDL$Author)
res<- rma(yi = hedges_g$es,     # The d-column of the df, which contains Cohen's d
                     vi = hedges_g$var) 

#Forest Plot
forest(res, xlim=c(-6,5), cex=1.5, header="Author(s) and Year",
       slab = hedges_g$study, xlab = "Hedge's g", showweights = TRUE) #randomised forest plot
#Text for heterogeneity
text(-6,-1, pos=4, cex=1.5, bquote(paste("RE Model(Q = ",
                                       .(formatC(res$QE, digits=2, format="f")),", df = ",.(res$k - res$p),
                                       ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                       .(formatC(res$I2, digits=1, format="f")), "%)")))
### add text for test of overall effect
text(-6, -1.5, pos=4, cex=1.5, bquote(paste("Test for overall effect: Z=",
                                            .(formatC(res$zval, digits=2, format="f")),
                                            ", P", .(ifelse(res$pval<.001, "<0.001",
                                                            paste0("=",formatC(res$pval, digits=2, format="f")))))))
funnel(res, main="LDL")

##Result I2=76.7%%, pval=0.01
# removal of monika gabria reduced heterogeneity to I2=23.4%, p=0.01
##################################################################################################################

