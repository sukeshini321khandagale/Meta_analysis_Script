install.packages("BiocManager")
install.packages("metafor")
install.packages("esc")
install.packages("esvis")
library(BiocManager)
library(metafor)
library(esc)
library(esvis)

##########################################################################################################

data<-read.csv("D:/Systematic_Review_paper1/Gender_heterogenity.csv")## MASTER DATA

df_BMI<-data[-c(5:14), ] # dataframe for BMI
df_HbA1C<- data[-c(1:5, 10:14), ] #dataframe for HbA1C
df_insulinUnits_kg_day<- data[-c(1:10), ] #dataframe for insulin

df_smd<-escalc(measure = "SMD" , m1i=mean,
               m2i = mean.1,
               sd1i = sd,
               sd2i = sd.1,
               n1i = sample.size,
               n2i = sample.size.1,
               data = data)
hg<-hedges_g(df_smd$yi, df_smd$Total.sample) #converts cohens to hedges
df_smd$hedges_g<- with(df_smd,hg) #bind the result of hg to data frame of cohens d. 

#calculating standard mean difference for BMI
df_smd_BMI<-escalc(measure = "SMD", 
               m1i = mean,
               m2i = mean.1,
               sd1i = sd,
               sd2i = sd.1,
               n1i = sample.size,
               n2i = sample.size.1,
               data = df_BMI)
hg<-hedges_g(df_smd_BMI$yi, df_smd_BMI$Total.sample) #converts cohens to hedges
df_smd_BMI$hedges_g<- with(df_smd_BMI,hg) #bind the result of hg to data frame of cohens d. 

#calculating standard mean difference for HbA1C
df_smd_HbA1C<-escalc(measure = "SMD", m1i = mean,
               m2i = mean.1,
               sd1i = sd,
               sd2i = sd.1,
               n1i = sample.size,
               n2i = sample.size.1,
               data = df_HbA1C)
hg<-hedges_g(df_smd_HbA1C$yi, df_smd_HbA1C$Total.sample) #converts cohens to hedges
df_smd_HbA1C$hedges_g<- with(df_smd_HbA1C,hg) #bind the result of hg to data frame of cohens d. 

#calculating standard mean difference for insulin units
df_smd_insulin<-escalc(measure = "SMD", m1i = mean,
               m2i = mean.1,
               sd1i = sd,
               sd2i = sd.1,
               n1i = sample.size,
               n2i = sample.size.1,
               data = df_insulinUnits_kg_day)
hg<-hedges_g(df_smd_insulin$yi, df_smd_insulin$Total.sample) #converts cohens to hedges
df_smd_insulin$hedges_g<- with(df_smd_insulin,hg) #bind the result of hg to data frame of cohens d. 

####################################################################################################

#below code for randomised model###################################################################
m_re_df_smd <- rma(yi = df_smd$hedges_g,     # The d-column of the df, which contains Cohen's d
                vi = df_smd$vi)           # The vi-column of the df, which contains the variances
m_re_df_smd


m_re_BMI <- rma(yi = df_smd_BMI,     # The d-column of the df, which contains Cohen's d
         vi = df_smd_BMI$vi)           # The vi-column of the df, which contains the variances
m_re_BMI

m_re_HbA1C <- rma(yi = df_smd_HbA1C$yi,     # The d-column of the df, which contains Cohen's d
            vi = df_smd_HbA1C$vi)    # The vi-column of the df, which contains the variances
m_re_HbA1C

m_re_insulin <- rma(yi = df_smd_insulin$yi,     # The d-column of the df, which contains Cohen's d
            vi = df_smd_insulin$vi)    # The vi-column of the df, which contains the variances
m_re_insulin

####################################################################################################

#creating a forest plot
#forest(m, slab=df_smd$study)# fixed effect forest plot
forest(m_re_df_smd, slab = df_smd$study..BMI.) #randomised forest plot
forest(m_re_BMI, slab = df_smd_BMI$study..BMI., addcred = TRUE)
pdf(file='forestplot_BMI.pdf') # Open PDF device with specific file name
forest(m_re_BMI, slab = df_smd_BMI$study)# Plot the forest
dev.off()

forest(m_re_HbA1C, slab = df_smd_HbA1C$study) #randomised forest plot
forest(m_re_HbA1C, slab = df_smd_HbA1C$study, addcred = TRUE)
pdf(file='forestplot_HbA1C.pdf')
forest(m_re_HbA1C, slab = df_smd_HbA1C$study)
dev.off()

forest(m_re_insulin, slab = df_smd_insulin$study) #randomised forest plot
forest(m_re_insulin, slab = df_smd_insulin$study, addcred = TRUE)
pdf(file='forestplot_insulin.pdf')
forest(m_re_insulin, slab = df_smd_insulin$study)
##saving th forest plot as pdf#######################################################################

dev.off() # Turn the PDF device off


#Below code for fixed effect model
#m <- rma(yi = df_smd$yi,     # The d-column of the df, which contains Cohen's d
#         vi = df_smd$vi,    # The vi-column of the df, which contains the variances
#         method = "FE") # Run a fixed-effect model
#m
#m$I2
