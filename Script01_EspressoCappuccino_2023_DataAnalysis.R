### JERONIMO & VARASSIN, 2023 ###

#### Library ####
pacman::p_load(DHARMa, gam, ggcorrplot, mgcv, MuMIn, rstatix, tidyverse, vegan, 
               visreg)

#### Data ####
# Farm level data
data.prop <- read.csv('Data/Data_Local.csv', 
                      sep = ';', 
                      stringsAsFactors = F) %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate(ENNF_MN_1KM = as.numeric(ENNF_MN_1KM)) %>% 
  glimpse()

View(data.prop)

# Municipal level data
data.muni <- read.csv('Data/Data_Regional.csv') %>% 
  mutate_if(is.character, as.factor) %>% 
  glimpse()

#### Mantel test for spatial self-correlation (Farm level only) ####
# Coordinates matrix
data.pcoo <- data.prop %>%
  select(Latitude, Longitude)

str(data.pcoo)

pcoo.std <- decostand(data.pcoo, 
                      method='standardize')

pcoo.ecl <- vegdist(pcoo.std, 
                    method='euclidean')

str(pcoo.ecl)

# Production (kg) matrix
data.ppkg <- data.prop %>%
  select(Prod_kg)

str(data.ppkg)

ppkg.std <- decostand(data.ppkg, 
                      method='standardize')

ppkg.ecl <- vegdist(ppkg.std, 
                    method='euclidean')

str(ppkg.ecl)

# Landscape metrics matrix
data.pmtr <- data.prop %>%
  select(PLANDF_5KM, PDF_5KM, PROXF_MN_5KM, ENNF_MN_5KM, CONNECTF_5KM, 
         SHEI_5KM, II_5KM)

str(data.pmtr)

pmtr.std <- decostand(data.pmtr, 
                      method='standardize')

pmtr.ecl <- vegdist(pmtr.std, 
                    method='euclidean')

str(pmtr.ecl)

# Mantel test production
test.MTL01 <- mantel(pcoo.ecl, ppkg.ecl, 
                     method='pearson', 
                     permutations=1000)

test.MTL01

data.MTL01 <- cbind(pcoo.ecl, ppkg.ecl)

str(data.MTL01)

write.table(data.MTL01, 
            'Data/data.MTL01.csv')

# Mantel test landscape metrics
test.MTL02 <- mantel(pcoo.ecl, pmtr.ecl, 
                     method='pearson', 
                     permutations=1000)

test.MTL02

# Mantel plot landscape metrics
data.MTL02 <- cbind(pcoo.ecl, pmtr.ecl)

write.table(data.MTL02, 
            'Data/data.MTL02.csv')



#### Correlation test of predictor variables ####
# Farm level
data.COR01 <- data.prop %>% 
  select(PLANDF_5KM, PDF_5KM, PROXF_MN_5KM, ENNF_MN_5KM, CONNECTF_5KM, 
         SHEI_5KM, II_5KM) %>% 
  rename('F. Cover Percentage'=PLANDF_5KM,
         'F. Patches Density'=PDF_5KM,
         'F. Proximity'=PROXF_MN_5KM,
         'F. Isolation'=ENNF_MN_5KM,
         'F. Connectance'=CONNECTF_5KM,
         'L. Diversity'=SHEI_5KM,
         'L. Intensity'=II_5KM)

test.COR01 <- cor(data.COR01)

p.mat <- cor_pmat(data.COR01)

test.COR01

# Municipality level
data.COR02 <- data.muni %>% 
  select(PLANDF, PDF, PROXF_MN, ENNF_MN, CONNECTF, SHEI, II) %>% 
  rename('F. Cover Percentage'=PLANDF,
         'F. Patches Density'=PDF,
         'F. Proximity'=PROXF_MN,
         'F. Isolation'=ENNF_MN,
         'F. Connectance'=CONNECTF,
         'L. Diversity'=SHEI,
         'L. Intensity'=II)

test.COR02 <- cor(data.COR02)

p.mat2 <- cor_pmat(data.COR02)

test.COR02



#### Scale of Effect - Farm only #### 

# FOREST PERCENTAGE OF LANDSCAPE 
v1.1KM <- glm(data=data.prop, 
              formula=ProdArea_kg~PLANDF_1KM,
              family = Gamma(link=inverse))
summary(v1.1KM) # 393.04

v1.2KM <- glm(data=data.prop, 
              formula=ProdArea_kg~PLANDF_2KM,
              family = Gamma(link=inverse))
summary(v1.2KM) # 392.92 

v1.3KM <- glm(data=data.prop,
              formula=ProdArea_kg~PLANDF_3KM,
              family = Gamma(link=inverse))
summary(v1.3KM) # 392.85 ***

v1.4KM <- glm(data=data.prop, 
              formula=ProdArea_kg~PLANDF_4KM,
              family = Gamma(link=inverse))
summary(v1.4KM) # 393.33

v1.5KM <- glm(data=data.prop, 
              formula=ProdArea_kg~PLANDF_5KM,
              family = Gamma(link=inverse))
summary(v1.5KM) # 393.68

# FOREST PATCH DENSITY
v2.1KM <- glm(data=data.prop, 
              formula=ProdArea_kg~PDF_1KM, 
              family = Gamma(link=inverse))
summary(v2.1KM) # 393.04

v2.2KM <- glm(data=data.prop, 
              formula=ProdArea_kg~PDF_2KM, 
              family = Gamma(link=inverse))
summary(v2.2KM) # 389.00 ***

v2.3KM <- glm(data=data.prop, 
              formula=ProdArea_kg~PDF_3KM, 
              family = Gamma(link=inverse))
summary(v2.3KM) # 390.00

v2.4KM <- glm(data=data.prop, 
              formula=ProdArea_kg~PDF_4KM, 
              family = Gamma(link=inverse))
summary(v2.4KM) # 391.69

v2.5KM <- glm(data=data.prop,
              formula=ProdArea_kg~PDF_5KM, 
              family = Gamma(link=inverse))
summary(v2.5KM) # 391.27

# FOREST PROXIMITY INDEX
v3.1KM <- glm(data=data.prop, 
              formula=ProdArea_kg~PROXF_MN_1KM,
              family = Gamma(link=inverse))
summary(v3.1KM) # 394.69 ***

v3.2KM <- glm(data=data.prop, 
              formula=ProdArea_kg~PROXF_MN_2KM, 
              family = Gamma(link=inverse))
summary(v3.2KM) # 395.26

v3.3KM <- glm(data=data.prop, 
              formula=ProdArea_kg~PROXF_MN_3KM, 
              family = Gamma(link=inverse))
summary(v3.3KM) # 395.5

v3.4KM <- glm(data=data.prop,
              formula=ProdArea_kg~PROXF_MN_4KM,
              family = Gamma(link=inverse))
summary(v3.4KM) # 395.39

v3.5KM <- glm(data=data.prop, 
              formula=ProdArea_kg~PROXF_MN_5KM, 
              family = Gamma(link=inverse))
summary(v3.5KM) # 395.31

# FOREST EUCLIDEAN NEXT NEIGHBOR
v4.1KM <- glm(data=data.prop, 
              formula=ProdArea_kg~ENNF_MN_1KM, 
              family = Gamma(link=inverse))
summary(v4.1KM) # 395.04

v4.2KM <- glm(data=data.prop, 
              formula=ProdArea_kg~ENNF_MN_2KM,
              family = Gamma(link=inverse))
summary(v4.2KM) # 379.29

v4.3KM <- glm(data=data.prop,
              formula=ProdArea_kg~ENNF_MN_3KM,
              family = Gamma(link=inverse))
summary(v4.3KM) # 381.94

v4.4KM <- glm(data=data.prop, 
              formula=ProdArea_kg~ENNF_MN_4KM, 
              family = Gamma(link=inverse))
summary(v4.4KM) # 380.25 ***

v4.5KM <- glm(data=data.prop, 
              formula=ProdArea_kg~ENNF_MN_5KM, 
              family = Gamma(link=inverse))
summary(v4.5KM) # 382.03

# FOREST CONNECTIVITY INDEX 
v5.1KM <- glm(data=data.prop,
              formula=ProdArea_kg~CONNECTF_1KM, 
              family = Gamma(link=inverse))
summary(v5.1KM) # 388.15 ***

v5.2KM <- glm(data=data.prop, 
              formula=ProdArea_kg~CONNECTF_2KM,
              family = Gamma(link=inverse))
summary(v5.2KM) # 395.44

v5.3KM <- glm(data=data.prop, 
              formula=ProdArea_kg~CONNECTF_3KM,
              family = Gamma(link=inverse))
summary(v5.3KM) # 393.52

v5.4KM <- glm(data=data.prop,
              formula=ProdArea_kg~CONNECTF_4KM,
              family = Gamma(link=inverse))
summary(v5.4KM) # 392.74

v5.5KM <- glm(data=data.prop, 
              formula=ProdArea_kg~CONNECTF_5KM,
              family = Gamma(link=inverse))
summary(v5.5KM) # 394.72

# LANDSCAPE SHANNON EVENNESS INDEX
v6.1KM <- glm(data=data.prop,
              formula=ProdArea_kg~SHEI_1KM, 
              family = Gamma(link=inverse))
summary(v6.1KM) # 390.39

v6.2KM <- glm(data=data.prop, 
              formula=ProdArea_kg~SHEI_2KM, 
              family = Gamma(link=inverse))
summary(v6.2KM) # 390.04 ***


v6.3KM <- glm(data=data.prop,
              formula=ProdArea_kg~SHEI_3KM, 
              family = Gamma(link=inverse))
summary(v6.3KM) # 390.51

v6.4KM <- glm(data=data.prop, 
              formula=ProdArea_kg~SHEI_4KM,
              family = Gamma(link=inverse))
summary(v6.4KM) # 390.49

v6.5KM <- glm(data=data.prop, 
              formula=ProdArea_kg~SHEI_5KM, 
              family = Gamma(link=inverse))
summary(v6.5KM) # 390.87

# LANDSCAPE INTENSITY INDEX
v7.1KM <- glm(data=data.prop, 
              formula=ProdArea_kg~II_1KM,
              family = Gamma(link=inverse))
summary(v7.1KM) # 376.6

v7.2KM <- glm(data=data.prop,
              formula=ProdArea_kg~II_2KM,
              family = Gamma(link=inverse))
summary(v7.2KM) # 374.45 ***

v7.3KM <- glm(data=data.prop,
              formula=ProdArea_kg~II_3KM,
              family = Gamma(link=inverse))
summary(v7.3KM) # 375.24

v7.4KM <- glm(data=data.prop,
              formula=ProdArea_kg~II_4KM,
              family = Gamma(link=inverse))
summary(v7.4KM) # 378.68

v7.5KM <- glm(data=data.prop,
              formula=ProdArea_kg~II_5KM,
              family = Gamma(link=inverse))
summary(v7.5KM) # 381.69

#### GLM MODEL SELECTION FARM LEVEL ####
# Analysis data frame
data.GLM01 <- data.prop %>% 
  select(ProdArea_kg, PLANDF_3KM, PDF_2KM, ENNF_MN_4KM, PROXF_MN_1KM, 
         CONNECTF_4KM, SHEI_2KM, II_2KM) %>% 
  rename('PROD'=ProdArea_kg,
         'CVRG'=PLANDF_3KM,
         'DNST'=PDF_2KM,
         'ISLT'=ENNF_MN_4KM,
         'PRXM'=PROXF_MN_1KM,
         'CNNC'=CONNECTF_4KM, 
         'SHEI'=SHEI_2KM,
         'IIND'=II_2KM) %>% 
  glimpse()

# MODELS
# NULL MODEL
M0 <- glm(data=data.GLM01, 
          formula=PROD~1, 
          family=Gamma(link=inverse))
summary(M0)
# UNIV MODELS
M1 <- glm(data=data.GLM01,
          formula=PROD~CVRG,
          family=Gamma(link=inverse))
summary(M1)

M2 <- glm(data=data.GLM01,
          formula=PROD~DNST, 
          family=Gamma(link=inverse))
summary(M2)

# M3 <- glm(data=data.GLM01, 
#           formula=PROD~ISLT,
#           family=Gamma(link=inverse))
# summary(M3)
# 
# M4 <- glm(data=data.GLM01,
#           formula=PROD~PRXM,
#           family=Gamma(link=inverse))
# summary(M4)

M5 <- glm(data=data.GLM01,
          formula=PROD~CNNC, 
          family=Gamma(link=inverse))
summary(M5)

M6 <- glm(data=data.GLM01,
          formula=PROD~SHEI,
          family=Gamma(link=inverse))
summary(M6)

M7 <- glm(data=data.GLM01,
          formula=PROD~IIND,
          family=Gamma(link=inverse))
summary(M7)

  # Univ models selection
UVAR.SLCT <- model.sel(M0, M1, M2, M5, M6, M7)

UVAR.SLCT

write.table(as.data.frame(UVAR.SLCT), 
            'Results/UnivModels.csv', 
            sep = ',')

# Best fitted model evaluation
testResiduals(M7)
simulateResiduals(M7, n = 1000, plot = T)


# MULTIV MODELS
MA <- glm(data=data.GLM01, 
          formula=PROD~IIND*CVRG, 
          family=Gamma(link=inverse))
summary(MA)

MB <- glm(data=data.GLM01,
          formula=PROD~IIND*DNST, 
          family=Gamma(link=inverse))
summary(MB)

MC <- glm(data=data.GLM01,
          formula=PROD~IIND*CNNC,
          family=Gamma(link=inverse))
summary(MC)

MD <- glm(data=data.GLM01,
          formula=PROD~IIND*SHEI,
          family=Gamma(link=inverse))
summary(MD)
         
# Univ models selection
MVAR.SLCT <- model.sel(M0, MA, MB, MC, MD)

MVAR.SLCT

write.table(as.data.frame(MVAR.SLCT), 
            'Results/MulvModels.csv',
            sep = ',')

# Best fitted model evaluation
# Best fitted model evaluation
testResiduals(MD)
simulateResiduals(MD, n = 1000, plot = T)

testResiduals(MB)
simulateResiduals(MB, n = 1000, plot = T)

testResiduals(MC)
simulateResiduals(MC, n = 1000, plot = T)


#### GLM MODEL SELECTION MUNICIPALITY LEVEL ####

# Data set GLM City Level
data.GLM02 <- data.muni %>% 
  select(ProdArea_kg, PLANDF, PDF, ENNF_MN, PROXF_MN, CONNECTF, SHEI, 
         SHDI, II) %>% 
  rename('PROD'=ProdArea_kg,
         'CVRG'=PLANDF,
         'DNST'=PDF,
         'ISLT'=ENNF_MN,
         'PRXM'=PROXF_MN,
         'CNNC'=CONNECTF, 
         'SHEI'=SHEI,
         'SHDI'=SHDI,
         'IIND'=II)

# MODELS

# NULL MODEL
C0 <- glm(data=data.GLM02, 
          formula=PROD~1, 
          family=Gamma(link=inverse))
summary(C0)

# UNIV MODELS
C1 <- glm(data=data.GLM02,
          formula=PROD~CVRG, 
          family=Gamma(link=inverse))
summary(C1)

C2 <- glm(data=data.GLM02,
          formula=PROD~DNST, 
          family=Gamma(link=inverse))
summary(C2)

# C3 <- glm(data=data.GLM02,
#           formula=PROD~ISLT, 
#           family=Gamma(link=inverse))
# summary(C3)
# 
# C4 <- glm(data=data.GLM02,
#           formula=PROD~PRXM,
#           family=Gamma(link=inverse))
# summary(C4)

C5 <- glm(data=data.GLM02,
          formula=PROD~CNNC,
          family=Gamma(link=inverse))
summary(C5)

C6 <- glm(data=data.GLM02,
          formula=PROD~SHEI, 
          family=Gamma(link=inverse))
summary(C6)

C7 <- glm(data=data.GLM02,
          formula=PROD~IIND,
          family=Gamma(link=inverse))
summary(C7)

# MODEL SELECTION

# Univ models selection
UVAR.SLCT2 <- model.sel(C0, C1, C2, C5, C6, C7)

UVAR.SLCT2 

write.table(as.data.frame(UVAR.SLCT2), 
            'Results/UnivModelsCity.csv',
            sep = ',')

### T-test national mean ###
t.test(data.prop$ProdArea_kg, mu=1932)
t.test(data.muni$ProdArea_kg, mu=1932)

mean(data.prop$ProdArea_kg)
median(data.prop$ProdArea_kg)
sd(data.prop$ProdArea_kg)

mean(data.muni$ProdArea_kg)
median(data.muni$ProdArea_kg)
sd(data.muni$ProdArea_kg)

### Fitting selection ####
S1 <- glm(data=data.GLM01,
          formula=PROD~IIND,
          family=gaussian(link=identity))
summary(S1)

S2 <- glm(data=data.GLM01,
          formula=PROD~IIND,
          family=gaussian(link=log))
summary(S2)

S3 <- glm(data=data.GLM01,
          formula=PROD~IIND,
          family=gaussian(link=inverse))
summary(S3)

S4 <- glm(data=data.GLM01,
          formula=PROD~IIND,
          family=Gamma(link=identity))
summary(S4)

S5 <- glm(data=data.GLM01,
          formula=PROD~IIND,
          family=Gamma(link=log))
summary(S5)

S6 <- glm(data=data.GLM01,
          formula=PROD~IIND,
          family=Gamma(link=inverse))
summary(S6)

S7 <- mgcv::gam(formula=PROD~s(IIND),
          data=data.GLM01,
          family=Gamma(link = inverse),
          method = "REML")
summary(S7)

S8 <- mgcv::gam(formula=PROD~s(IIND),
          data=data.GLM01,
          family = gaussian(link=inverse),
          method = "REML")
summary(S8)

S8 <- mgcv::gam(formula=PROD~s(IIND),
                data=data.GLM01,
                family = gaussian(link=inverse),
                method = "REML")
summary(S8)


model.sel(S1, S2, S3, S4, S5, S6, S7, S8)

