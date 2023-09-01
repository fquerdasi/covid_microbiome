setwd("~/Library/CloudStorage/Box-Box/Sarah and Fran Microbiome Projects/Covid Microbiome Paper") #laptop

library(psych)
library(car)
library(jtools)
library(lm.beta)
library(lavaan)
library(ggplot2)

df <- read.csv("COVIDpapervars.csv")

#comparing pre vs. during pandemic groups

#comparing alpha diversity metrics between pre- and during pandemic groups
#shannon
t.test(df$Shannon ~ df$pandemic)
#ns. t=1.6, p=0.11

#chao
t.test(df$Chao1 ~ df$pandemic)
#significant, t=6.06, p<0.0001

#split sample into pre- and post pandemic start

df_pre <- df[df$pandemic == 0,]
df_post <- df[df$pandemic == 1,]

#look at relationship between collection date (dist322 var) and Alpha diversity metrics in just post sample
cor.test(df_post$dist322, df_post$Shannon) #ns
cor.test(df_post$dist322, df_post$Chao1) #ns

#basic plot to visually inspect
plot(df_post$dist322, df_post$Shannon)
plot(df_post$dist322, df_post$Chao1)

#comparing pre vs. during on sociodemographic characteristics

#basic SES/economic strain measures
t.test(df$Mom_ED ~ df$pandemic) #ns
describe(df_pre$Mom_ED)
describe(df_post$Mom_ED)
t.test(df$Dad_ED ~ df$pandemic) #ns
describe(df_pre$Dad_ED)
describe(df_post$Dad_ED)
t.test(df$New.ITN ~ df$pandemic) #ns
describe(df_pre$New.ITN)
describe(df_post$New.ITN)
t.test(df$Problems_Composite_Score_3 ~ df$pandemic) #trend
describe(df_pre$Problems_Composite_Score_3)
describe(df_post$Problems_Composite_Score_3)

#no significant differences in pre vs. during groups on any measures of socioeconomic or material status

#three month caregiver MH measures, all collected before the start of the pandemic
t.test(df$Perceived_Stress_3 ~ df$pandemic)#n.s.
describe(df_pre$Perceived_Stress_3)
describe(df_post$Perceived_Stress_3)
t.test(df$Edinburgh_3 ~ df$pandemic)#n.s.
describe(df_pre$Edinburgh_3)
describe(df_post$Edinburgh_3)
t.test(df$STAI.Y.1_state_3 ~ df$pandemic)#ns
describe(df_pre$STAI.Y.1_state_3)
describe(df_post$STAI.Y.1_state_3)
t.test(df$STAI.Y.2_trait_3 ~ df$pandemic)#ns
describe(df_pre$STAI.Y.2_trait_3)
describe(df_post$STAI.Y.2_trait_3)

#maternal MH at time of stool sample collection
t.test(df$PSS12 ~ df$pandemic)#ns
describe(df_pre$PSS12)
describe(df_post$PSS12)
t.test(df$PHQ.9 ~ df$pandemic)#n.s.
describe(df_pre$PHQ.9)
describe(df_post$PHQ.9)

#nutrition, lifestyle, household characteristics
t.test(df$avg_carbs ~ df$pandemic)#ns
describe(df_pre$avg_carbs)
describe(df_post$avg_carbs)
t.test(df$avg_proteins ~ df$pandemic)#ns
describe(df_pre$avg_proteins)
describe(df_post$avg_proteins)
t.test(df$avg_fats ~ df$pandemic) #higher post-pandemic 
describe(df_pre$avg_fats)
describe(df_post$avg_fats)
t.test(df$People_Household3 ~ df$pandemic)#n.s.
describe(df_pre$People_Household3)
describe(df_post$People_Household3)


t.test(df$Siblings ~ df$pandemic) #n.s.
chisq.test(df$Siblings, df$pandemic) #n.s. 
table(df_pre$method_delivery)
table(df_post$method_delivery)
chisq.test(df$method_delivery, df$pandemic)#n.s.
table(df_pre$pets)
table(df_post$pets)
chisq.test(df$pets, df$pandemic)#n.s.
table(df_pre$still_breastfeed)
table(df_post$still_breastfeed)
chisq.test(df$still_breastfeed, df$pandemic)#n.s.
chisq.test(df$Antibiotic.exposure, df$pandemic)#n.s.
chisq.test(df$Antifungal.exposure, df$pandemic)#n.s.
chisq.test(df$Probiotic.exposure, df$pandemic)#n.s.

#demographics
chisq.test(df$Child_Race, df$pandemic) #ns
table(df_pre$Child_Race)
table(df_post$Child_Race)
df$Child_Ethnicity <- as.factor(df$Child_Ethnicity.1)
chisq.test(df$Child_Ethnicity.1, df$pandemic) #ns
table(df_pre$Child_Ethnicity.1)
table(df_post$Child_Ethnicity.1)
chisq.test(df$sex, df$pandemic)
table(df_pre$sex)
table(df_post$sex)

#follow up on significant difference in fat intake between pandemic groups
#correlations between fat intake and alpha diversity metrics
cor.test(df$avg_fats, df$Shannon)#n.s.
#r=-0.24, p= .14
cor.test(df$avg_fats, df$Chao1)#significant
#r=-0.41, p= .007. chao1 diversity significantly lower in infants with greater fat intake
#will include fat intake as a covariate in subsequent analyses

#alpha diversity/pandemic regressions including covariates
#need to recode method of delivery to use in lavaan models
df$method_delivery_num <- as.numeric(df$method_delivery)
table(df$method_delivery_num)
str(df$method_delivery)
df$method_delivery_num <- ifelse(df$method_delivery == "Vaginal_birth", 1, 
                                 ifelse(df$method_delivery == "C-section", 0, df$method_delivery))

table(df$method_delivery_num)

modelc <- '
Chao1 ~ pandemic + avg_fats + sex + still_breastfeed + method_delivery_num

'
fitc <- sem(modelc, fixed.x=F,missing="FIML", data = df)#estimator = "MLR", std.lv = TRUE
summary(fitc, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE, estimates = TRUE, ci = TRUE)

models <- '
Shannon ~ pandemic + avg_fats + sex + still_breastfeed + method_delivery_num

'
fits <- sem(models, fixed.x=F,missing="FIML", data = df)#estimator = "MLR", std.lv = TRUE
summary(fits, fit.measures = TRUE, standardized = TRUE, rsquare=TRUE, estimates = TRUE, ci = TRUE)


#generating plots

#make sure pandemic var is coded as factor to ggplot reads it correctly
df$pandemic <- as.factor(df$pandemic)
str(df$pandemic)
table(df$pandemic)

#alpha diversity pre vs. during plots

#chao1 pre vs. during
plot1 <- ggplot(df, aes(x=as.factor(pandemic), y =Chao1, color = as.factor(pandemic))) + 
  geom_point(position=position_jitter(w=0.3, h=0), na.rm = TRUE)  +
  geom_boxplot(alpha=0.3, outlier.shape=NA, na.rm=TRUE) + 
  xlab("Pandemic Group") + ylab("Chao1 Diversity") + 
  scale_x_discrete("Pandemic Group", labels = c("0" = "Pre-Pandemic", "1" = "Pandemic")) +
  scale_color_brewer(palette = "Dark2") + #specify colorblind friendly color palette for color
  theme_minimal() +
  theme( 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
  ) +
  theme(legend.position = "none")
plot1
ggsave("figures/chao_timepoint.jpg", height=4, width=6, dpi=1000)

#shannon pre vs. during
plot2 <- ggplot(df, aes(x=as.factor(pandemic), y =Shannon, color = as.factor(pandemic))) + 
  geom_point(position=position_jitter(w=0.3, h=0), na.rm = TRUE)  +
  geom_boxplot(alpha=0.3, outlier.shape=NA, na.rm=TRUE) + 
  xlab("Pandemic Group") + ylab("Shannon Diversity") + 
  scale_x_discrete("Pandemic Group", labels = c("0" = "Pre-Pandemic", "1" = "Pandemic")) +
  scale_color_brewer(palette = "Dark2") + #specify colorblind friendly color palette for color
  theme_minimal() +
  theme( 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
  ) +
  theme(legend.position = "none")
plot2
ggsave("figures/shannon_timepoint.jpg", height = 4, width = 6, dpi=1000)

#alpha diversity by days since pandemic onset 

#shannon diversity by days since pandemic onset
plot3 <- ggplot(df_post, aes(y = Shannon, x = dist322)) +
  geom_point(alpha=1, position = position_jitter(height = .002, width = .1), na.rm = TRUE, color='#7570B3') + geom_smooth(method="lm", color='#E7298A') +
  xlab("Days Since Pandemic Start") + ylab("Shannon Diversity") +
  scale_color_brewer(palette = "Dark2") + #specify colorblind friendly color palette for color
  theme_minimal() +
  #theme( 
  #  panel.grid.major.x = element_blank(),
  #  panel.grid.minor.x = element_blank(),
  #  panel.grid.major.y = element_blank(),
  #  panel.grid.minor.y = element_blank(),
  #) +
  theme(legend.position = "none")

plot3
ggsave("figures/shannondays.jpg", height = 4, width=6, dpi=1000)

#chao1 diversity by days since pandemic onset
plot4 <- ggplot(df_post, aes(y = Chao1, x = dist322)) +
  geom_point(alpha=1, position = position_jitter(height = .002, width = .1), na.rm = TRUE, color='#7570B3') + geom_smooth(method="lm", color='#E7298A') +
  xlab("Days Since Pandemic Start") + ylab("Chao1 Diversity") +
  scale_color_brewer(palette = "Dark2") + #specify colorblind friendly color palette for color
  theme_minimal() +
  #theme( 
  #  panel.grid.major.x = element_blank(),
  #  panel.grid.minor.x = element_blank(),
  #  panel.grid.major.y = element_blank(),
  #  panel.grid.minor.y = element_blank(),
  #) +
  theme(legend.position = "none")

plot4
ggsave("figures/chao1days.jpg", height = 4, width=6, dpi=1000)






