# Libraries ----
library(tidyverse)
library(haven)
library(fastDummies)
library(psych)

# Modulo De Movilidad Social Intergeneracional 2016 (MMSI) ----
mmsi <- read_csv("./Data/MMSI 2016/MMSI_2016.csv")
mmsi <- mmsi %>% mutate(
  P11_1_1 = ifelse(P11_1_1 == 1, 1, 0),
  P11_1_2 = ifelse(P11_1_2 == 1, 1, 0),
  P11_1_3 = ifelse(P11_1_3 == 1, 1, 0),
  P11_1_4 = ifelse(P11_1_4 == 1, 1, 0)
)

## Parental Information
mmsi_parental <- mmsi %>% transmute(
  # Assets and Services in the household
  plumbing          = P2_9_1,
  stove             = P2_9_2,
  electricity       = P2_9_3,
  tv                = P2_10_1,
  fridge            = P2_10_2,
  washing_machine   = P2_10_3,
  landline          = P2_10_7,
  dvd_vcr           = P2_10_9,
  microwave         = P2_10_10,
  cable_tv          = P2_10_11,
  
  # Assets from Residents 
  other_housing     = P2_11_1,
  bank_account      = P2_11_9,
  credit_card       = P2_11_10,
  other_land        = P2_11_4,
  automobile        = P2_11_5,
  
  # Agricultural Activities
  premises          = P2_11_2,
  working_parcels   = P2_11_3,
  working_machinery = P2_11_6,
  working_animals   = P2_11_7,
  livestock         = P2_11_8,

  
  # Occupation 
  father = DivOcu_Pad,
  mother = DivOcu_Mad, 
  
  # Age of individual
  age               = P1_2
)

mmsi_parental <- dummy_cols(mmsi_parental, select_columns = 'father') # Create Dummies for Fathers Occupation 
mmsi_parental <- dummy_cols(mmsi_parental, select_columns = 'mother') # Create Dummies for Mothers Occupation 

mmsi_parental <- mmsi_parental %>% select(
  -c(father, mother, father_99, father_NA, mother_99, mother_NA) # Drop Occupation variable
)

mmsi_parental[mmsi_parental == 2]   <- 0   # Replace 2 = 0 
mmsi_parental[mmsi_parental == 9]   <- NA  # Replace 9 = NA
mmsi_parental[is.na(mmsi_parental)] <- 0   # Replace NA = 0

mmsi_parental$father_educ <- as.double(mmsi$EscAcu_Pad) # Years of schooling father 
mmsi_parental$mother_educ <- as.double(mmsi$EscAcu_Mad) # Years of schooling mother 
mmsi_parental[mmsi_parental == 99]  <- NA # Replace 9 = NA
mmsi_parental[is.na(mmsi_parental)] <- 0  # Replace NA = 0

mmsi_parental$ID <- paste0("mmsi-", 1:nrow(mmsi_parental))

### Generate SEI
#### Wealth 
x_wealth <- mmsi_parental[, 1:20]
poly     <- polychoric(x_wealth)

pca      <- prcomp(poly$rho, rank. = 5, center = T, scale = T)
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_parents_wealth_mmsi.csv")

if (pca$rotation[1, 1] < 0) {
  mmsi_parental$wealth <- predict(pca, newdata = x_wealth)[, 1]*-1
} else {
  mmsi_parental$wealth <- predict(pca, newdata = x_wealth)[, 1]
}

mmsi_parental$wealth_age <- predict(lm(wealth~poly(age, 2), data = mmsi_parental)) # Get life-cycle Bias
mmsi_parental$wealth_parental <- mmsi_parental$wealth - mmsi_parental$wealth_age   # Correct life-cycle Bias

#### Ocupation 
x_ocup   <- mmsi_parental %>% select(starts_with("father_0"))
poly     <- polychoric(x_ocup)

pca      <- prcomp(poly$rho, rank. = 5, center = T, scale = T)
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_parents_father_ocup_mmsi.csv")

if (pca$rotation[1, 1] < 0) {
  mmsi_parental$father_ocup <- predict(pca, newdata = x_ocup)[, 1]*-1
} else {
  mmsi_parental$father_ocup <- predict(pca, newdata = x_ocup)[, 1]
}

mmsi_parental$father_ocup_age <- predict(lm(father_ocup~poly(age, 2), data = mmsi_parental)) # Get life-cycle Bias
mmsi_parental$father_ocup     <- mmsi_parental$father_ocup - mmsi_parental$father_ocup_age   # Correct life-cycle Bias

x_ocup   <- mmsi_parental %>% select(starts_with("mother_0"))
poly     <- polychoric(x_ocup)

pca      <- prcomp(poly$rho, rank. = 5, center = T, scale = T)
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_parents_mother_ocup_mmsi.csv")

if (pca$rotation[1, 1] < 0) {
  mmsi_parental$mother_ocup <- predict(pca, newdata = x_ocup)[, 1]*-1
} else {
  mmsi_parental$mother_ocup <- predict(pca, newdata = x_ocup)[, 1]
}

mmsi_parental$mother_ocup_age <- predict(lm(mother_ocup~poly(age, 2), data = mmsi_parental)) # Get life-cycle Bias
mmsi_parental$mother_ocup     <- mmsi_parental$mother_ocup - mmsi_parental$mother_ocup_age   # Correct life-cycle Bias

### SEI
x_pca <- mmsi_parental %>%
  select(
    c(wealth_parental, father_ocup, mother_ocup, father_educ, mother_educ)  # Select variables for PCA
  )

pca <- prcomp(x_pca, center = T, scale. = T) # Estimate PCA
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_parents_mmsi.csv")

if (pca$rotation[1, 1] < 0) {
  mmsi_parental$y_hat <- predict(pca)[, 1]*-1
} else {
  mmsi_parental$y_hat <- predict(pca)[, 1]
}

mmsi_parental$y_age <- predict(lm(y_hat~poly(age, 2), data = mmsi_parental)) # Get life-cycle Bias
mmsi_parental$y_parental <- mmsi_parental$y_hat - mmsi_parental$y_age        # Correct life-cycle Bias

### Ranks
mmsi_parental <- mmsi_parental %>% mutate(
  wealth_parental   = ntile(wealth_parental, 100),
  r_wealth_parental = ntile(wealth_parental, 50), 
  Q_wealth_parental = ntile(wealth_parental, 5),
  D_wealth_parental = ntile(wealth_parental, 10),
  
  SEI_parental = ntile(y_parental, 100),
  r_parental   = ntile(y_parental, 50),
  Q_parental   = ntile(y_parental, 5), 
  D_parental   = ntile(y_parental, 10)
)

## Individual Data 
mmsi_individual <- mmsi %>% transmute(
  # Assets and Services in the household
  plumbing          = ifelse(disp_agua_ENH == 1 | disp_agua_ENH == 2, 1, 2),
  stove             = P11_7_12,
  electricity       = ifelse(disp_elect_ENH == 1 | disp_elect_ENH == 2 | disp_elect_ENH == 4, 1,
                             ifelse(disp_elect_ENH == 3, 2, 0)),
  tv                = P11_7_6,
  fridge            = P11_7_11,
  washing_machine   = P11_7_13,
  landline          = P11_7_1,
  dvd_bluray        = P11_7_5,
  microwave         = P11_7_10,
  cable_tv          = P11_7_3,
  internet          = P11_7_4,
  cellphone         = P11_7_2,
  computer          = P11_7_14,
  
  # Assets from Residents 
  other_housing     = P11_4_1,
  other_land        = P11_4_4,
  automobile        = P11_1_1 + P11_1_2 + P11_1_3,
  automobile        = ifelse(automobile > 0, 1, 0),
  bank_account      = P11_5_1,
  credit_card       = P11_5_2,
  
  # Agricultural Activities
  premises          = P11_4_2,
  working_parcels   = P11_4_3,
  working_machinery = P11_1_4,
  working_animals   = P11_7_21,
  livestock         = P11_7_22,
  
  # Occupation
  ocup = DivOcu_Act,
  
  # Age of respondent 
  age               = P1_2
)

mmsi_individual <- dummy_cols(mmsi_individual, select_columns = 'ocup') # Create occupational dummies
mmsi_individual <- mmsi_individual %>% select(
  -c(ocup, ocup_99, ocup_NA) # Drop non-relevant dummies
)

mmsi_individual[mmsi_individual == 2]   <- 0  # Replace 2 = 0
mmsi_individual[mmsi_individual == 9]   <- NA # Replace 9 = NA
mmsi_individual[is.na(mmsi_individual)] <- 0  # Replace NA = 0

mmsi_individual$educ <- as.double(mmsi$EscAcu_Inf) # Respondents years of schooling
mmsi_individual[mmsi_individual == 99]  <- NA # Replace 99 = NA
mmsi_individual[is.na(mmsi_individual)] <- 0  # Replace NA = 0

mmsi_individual$ID <- paste0("mmsi-", 1:nrow(mmsi_individual))

### Generate SEI
#### Wealth
x_wealth <- mmsi_individual[, 1:20]
poly     <- polychoric(x_wealth)

pca      <- prcomp(poly$rho, rank. = 5, center = T, scale = T)
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_individual_wealth_mmsi.csv")

if (pca$rotation[1, 1] < 0) {
  mmsi_individual$wealth <- predict(pca, newdata = x_wealth)[, 1]*-1
} else {
  mmsi_individual$wealth <- predict(pca, newdata = x_wealth)[, 1]
}

mmsi_individual$wealth_age <- predict(lm(wealth~poly(age, 2), data = mmsi_individual)) # Get life-cycle Bias
mmsi_individual$wealth_individual <- mmsi_individual$wealth - mmsi_individual$wealth_age   # Correct life-cycle Bias

#### Ocupation 
x_ocup   <- mmsi_individual %>% select(starts_with("ocup"))
poly     <- polychoric(x_ocup)

pca      <- prcomp(poly$rho, rank. = 5, center = T, scale = T)
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_infdividual_ocup_mmsi.csv")

if (pca$rotation[1, 1] < 0) {
  mmsi_individual$ocup <- predict(pca, newdata = x_ocup)[, 1]*-1
} else {
  mmsi_individual$ocup <- predict(pca, newdata = x_ocup)[, 1]
}

mmsi_individual$ocup_age <- predict(lm(ocup~poly(age, 2), data = mmsi_individual)) # Get life-cycle Bias
mmsi_individual$ocup     <- mmsi_individual$ocup - mmsi_individual$ocup_age   # Correct life-cycle Bias

### SEI
x_pca <- mmsi_individual %>%
  select(
    c(wealth, ocup, educ)  # Select variables for PCA
  )

pca <- prcomp(x_pca, center = T, scale. = T) # Estimate PCA
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_individual_mmsi.csv")

if (pca$rotation[1, 1] < 0) {
  mmsi_individual$y_hat <- predict(pca)[, 1]*-1
} else {
  mmsi_individual$y_hat <- predict(pca)[, 1]
}

mmsi_individual$y_age <- predict(lm(y_hat~poly(age, 2), data = mmsi_individual)) # Get life-cycle Bias
mmsi_individual$y_individual <- mmsi_individual$y_hat - mmsi_individual$y_age        # Correct life-cycle Bias

### Ranks
mmsi_individual <- mmsi_individual %>% mutate(
  wealth_individual   = ntile(wealth_individual, 100),
  r_wealth_individual = ntile(wealth_individual, 50), 
  Q_wealth_individual = ntile(wealth_individual, 5),
  D_wealth_individual = ntile(wealth_individual, 10),
  
  SEI_individual = ntile(y_individual, 100),
  r_individual   = ntile(y_individual, 50),
  Q_individual   = ntile(y_individual, 5), 
  D_individual   = ntile(y_individual, 10)
)

# ESRU EMOVI 2017 ----
emovi <- read_dta("./Data/EMOVI 2017/ESRU-EMOVI 2017 Entrevistado.dta")

## Parental Information
emovi_parental <- emovi %>% transmute(
  # Assets and Services in the household
  plumbing          = p30_a,
  stove             = p33_a,
  electricity       = p30_b,
  tv                = p33_e,
  fridge            = p33_c,
  washing_machine   = p33_b,
  landline          = p33_d,
  dvd_vcr           = p33_n,
  microwave         = p33_i,
  cable_tv          = p33_h,
  
  # Assets from Residents 
  other_housing     = p34_a,
  bank_account      = p32_b,
  credit_card       = p32_c,
  other_land        = p34_d,
  automobile        = p34_e,
  
  # Agricultural Activities
  premises          = p34_b,
  working_parcels   = p34_c,
  working_machinery = p34_f,
  working_animals   = p34_g,
  livestock         = p34_h,
  
  
  # Occupation 
  father = substr(SINCO1, 1, 2),
  father = ifelse(father == 99, father, paste0(0, substr(father, 1, 1))),
  mother = substr(SINCO2, 1, 2), 
  mother = ifelse(mother == 99, mother, paste0(0, substr(mother, 1, 1))),
  
  # Age of individual
  age = p05,
)

emovi_parental <- dummy_cols(emovi_parental, select_columns = 'father') # Create Dummies for Fathers Occupation 
emovi_parental <- dummy_cols(emovi_parental, select_columns = 'mother') # Create Dummies for Mothers Occupation 

emovi_parental <- emovi_parental %>% select(
  -c(father, mother, father_99, father_NA, mother_99, mother_NA) # Drop Occupation variable
)

# Recode 2 = 0 and 9 = NA
emovi_parental[emovi_parental == 2]   <- 0   # Replace 2 = 0 
emovi_parental[emovi_parental == 8]   <- NA  # Replace 8 = NA
emovi_parental[is.na(emovi_parental)] <- 0   # Replace NA = 0

emovi_parental$father_educ <- ifelse(emovi$p42 == 2 | emovi$p42m == 8, 0,       # Years of schooling father 
                                     ifelse(emovi$p43 == 1 | emovi$p43 == 98, 0,
                                            ifelse(emovi$p43 == 2, emovi$p44,
                                                   ifelse(emovi$p43 == 3 | emovi$p43 == 4 | emovi$p43 == 7, emovi$p44 + 6,
                                                          ifelse(emovi$p43 == 5 | emovi$p43 == 6 | emovi$p43 == 8, emovi$p44 + 9,
                                                                 ifelse(emovi$p43 == 9 | emovi$p43 == 10 | emovi$p43 == 11, emovi$p44 + 12,
                                                                        ifelse(emovi$p43 == 12, emovi$p44 + 16 , 0)
                                                                 )
                                                          )
                                                   )
                                            )
                                     )
)
emovi_parental$father_educ[is.na(emovi_parental$father_educ)] <- 0

emovi_parental$mother_educ <- ifelse(emovi$p42m == 2 | emovi$p42m == 8, 0,      # Years of schooling mother 
                                     ifelse(emovi$p43m == 1 | emovi$p43m == 98, 0,
                                            ifelse(emovi$p43m == 2, emovi$p44m,
                                                   ifelse(emovi$p43m == 3 | emovi$p43m == 4 | emovi$p43m == 7, emovi$p44m + 6,
                                                          ifelse(emovi$p43m == 5 | emovi$p43m == 6 | emovi$p43m == 8, emovi$p44m + 9,
                                                                 ifelse(emovi$p43m == 9 | emovi$p43m == 10 | emovi$p43m == 11, emovi$p44m + 12,
                                                                        ifelse(emovi$p43m == 12, emovi$p44m + 16 , 0)
                                                                 )
                                                          )
                                                   )
                                            )
                                     )
)
emovi_parental$mother_educ[is.na(emovi_parental$mother_educ)] <- 0

emovi_parental$ID <- paste0("emovi-", 1:nrow(emovi_parental))

### Generate SEI
#### Wealth 
x_wealth <- emovi_parental[, 1:20]
poly     <- polychoric(x_wealth)

pca      <- prcomp(poly$rho, rank. = 5, center = T, scale = T)
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_parents_wealth_emovi.csv")

if (pca$rotation[1, 1] < 0) {
  emovi_parental$wealth <- predict(pca, newdata = x_wealth)[, 1]*-1
} else {
  emovi_parental$wealth <- predict(pca, newdata = x_wealth)[, 1]
}

emovi_parental$wealth_age <- predict(lm(wealth~poly(age, 2), data = emovi_parental)) # Get life-cycle Bias
emovi_parental$wealth_parental <- emovi_parental$wealth - emovi_parental$wealth_age   # Correct life-cycle Bias

#### Ocupation 
x_ocup   <- emovi_parental %>% select(starts_with("father_0"))
poly     <- polychoric(x_ocup)

pca      <- prcomp(poly$rho, rank. = 5, center = T, scale = T)
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_parents_father_ocup_emovi.csv")

if (pca$rotation[1, 1] < 0) {
  emovi_parental$father_ocup <- predict(pca, newdata = x_ocup)[, 1]*-1
} else {
  emovi_parental$father_ocup <- predict(pca, newdata = x_ocup)[, 1]
}

emovi_parental$father_ocup_age <- predict(lm(father_ocup~poly(age, 2), data = emovi_parental)) # Get life-cycle Bias
emovi_parental$father_ocup     <- emovi_parental$father_ocup - emovi_parental$father_ocup_age   # Correct life-cycle Bias

x_ocup   <- emovi_parental %>% select(starts_with("mother_0"))
poly     <- polychoric(x_ocup)

pca      <- prcomp(poly$rho, rank. = 5, center = T, scale = T)
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_parents_mother_ocup_emovi.csv")

if (pca$rotation[1, 1] < 0) {
  emovi_parental$mother_ocup <- predict(pca, newdata = x_ocup)[, 1]*-1
} else {
  emovi_parental$mother_ocup <- predict(pca, newdata = x_ocup)[, 1]
}

emovi_parental$mother_ocup_age <- predict(lm(mother_ocup~poly(age, 2), data = emovi_parental)) # Get life-cycle Bias
emovi_parental$mother_ocup     <- emovi_parental$mother_ocup - emovi_parental$mother_ocup_age   # Correct life-cycle Bias

### SEI
x_pca <- emovi_parental %>%
  select(
    c(wealth_parental, father_ocup, mother_ocup, father_educ, mother_educ)  # Select variables for PCA
  )

pca <- prcomp(x_pca, center = T, scale. = T) # Estimate PCA
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_parents_emovi.csv")

if (pca$rotation[1, 1] < 0) {
  emovi_parental$y_hat <- predict(pca)[, 1]*-1
} else {
  emovi_parental$y_hat <- predict(pca)[, 1]
}

emovi_parental$y_age <- predict(lm(y_hat~poly(age, 2), data = emovi_parental)) # Get life-cycle Bias
emovi_parental$y_parental <- emovi_parental$y_hat - emovi_parental$y_age        # Correct life-cycle Bias

### Ranks
emovi_parental <- emovi_parental %>% mutate(
  wealth_parental   = ntile(wealth_parental, 100),
  r_wealth_parental = ntile(wealth_parental, 50), 
  Q_wealth_parental = ntile(wealth_parental, 5),
  D_wealth_parental = ntile(wealth_parental, 10),
  
  SEI_parental = ntile(y_parental, 100),
  r_parental   = ntile(y_parental, 50),
  Q_parental   = ntile(y_parental, 5), 
  D_parental   = ntile(y_parental, 10)
)

## Individual Data 
emovi_individual <- emovi %>% transmute(
  # Assets and Services in the household
  plumbing          = p125a,
  stove             = p126a,
  electricity       = p125b,
  tv                = p126e,
  fridge            = p126c,
  washing_machine   = p126b,
  landline          = p126k,
  dvd_bluray        = p126h,
  microwave         = p126d,
  cable_tv          = p126j,
  internet          = p126m,
  cellphone         = p126i,
  computer          = p126o,
  
  # Assets from Residents 
  other_housing     = p129a,
  other_land        = p129e,
  automobile        = ifelse(p131 > 1, 1, 0),
  bank_account      = p128c,
  credit_card       = p128d,
  
  # Agricultural Activities
  premises          = p129b,
  working_parcels   = p129c,
  working_machinery = p126r,
  working_animals   = p126p,
  livestock         = p126q,
  
  # Occupation
  ocup = substr(SINCO3, 1, 2),
  ocup = ifelse(ocup == 99, ocup, paste0(0, substr(ocup, 1, 1))), 
  
  # Age of respondent 
  age = p05
)

emovi_individual <- dummy_cols(emovi_individual, select_columns = 'ocup') # Create occupational dummies
emovi_individual <- emovi_individual %>% select(
  -c(ocup, ocup_99, ocup_0) # Drop non-relevant dummies
)

emovi_individual[emovi_individual == 2]   <- 0  # Replace 2 = 0
emovi_individual[emovi_individual == 8]   <- NA # Replace 9 = NA
emovi_individual[is.na(emovi_individual)] <- 0  # Replace NA = 0

emovi_individual$educ <- ifelse(emovi$p13 == 1 | emovi$p13 == 97, 0,
                                ifelse(emovi$p13 == 2, emovi$p14,
                                       ifelse(emovi$p13 == 3 | emovi$p13 == 4 | emovi$p13 == 7, emovi$p14 + 6,
                                              ifelse(emovi$p13 == 5 | emovi$p13 == 6 | emovi$p13 == 8, emovi$p14 + 9,
                                                     ifelse(emovi$p13 == 9 | emovi$p13 == 10 | emovi$p13 == 11, emovi$p14 + 12,
                                                            ifelse(emovi$p13 == 12, emovi$p14 + 16 , 0)
                                                     )
                                              )
                                       )
                                )
)

emovi_individual$educ[is.na(emovi_individual$educ)] <- 0

emovi_individual$ID <- paste0("emovi-", 1:nrow(emovi_individual))

### Generate SEI
#### Wealth
x_wealth <- emovi_individual[, 1:20]
poly     <- polychoric(x_wealth)

pca      <- prcomp(poly$rho, rank. = 5, center = T, scale = T)
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_individual_wealth_emovi.csv")

if (pca$rotation[1, 1] < 0) {
  emovi_individual$wealth <- predict(pca, newdata = x_wealth)[, 1]*-1
} else {
  emovi_individual$wealth <- predict(pca, newdata = x_wealth)[, 1]
}

emovi_individual$wealth_age <- predict(lm(wealth~poly(age, 2), data = emovi_individual)) # Get life-cycle Bias
emovi_individual$wealth_individual <- emovi_individual$wealth - emovi_individual$wealth_age   # Correct life-cycle Bias

#### Ocupation 
x_ocup   <- emovi_individual %>% select(starts_with("ocup"))
poly     <- polychoric(x_ocup)

pca      <- prcomp(poly$rho, rank. = 5, center = T, scale = T)
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_infdividual_ocup_emovi.csv")

if (pca$rotation[1, 1] < 0) {
  emovi_individual$ocup <- predict(pca, newdata = x_ocup)[, 1]*-1
} else {
  emovi_individual$ocup <- predict(pca, newdata = x_ocup)[, 1]
}

emovi_individual$ocup_age <- predict(lm(ocup~poly(age, 2), data = emovi_individual)) # Get life-cycle Bias
emovi_individual$ocup     <- emovi_individual$ocup - emovi_individual$ocup_age   # Correct life-cycle Bias

### SEI
x_pca <- emovi_individual %>%
  select(
    c(wealth, ocup, educ)  # Select variables for PCA
  )

pca <- prcomp(x_pca, center = T, scale. = T) # Estimate PCA
summary(pca)
loadings <- pca$rotation # Get loadings vector 
write.csv(loadings, "./Results/Loadings/loadings_individual_emovi.csv")

if (pca$rotation[1, 1] < 0) {
  emovi_individual$y_hat <- predict(pca)[, 1]*-1
} else {
  emovi_individual$y_hat <- predict(pca)[, 1]
}

emovi_individual$y_age <- predict(lm(y_hat~poly(age, 2), data = emovi_individual)) # Get life-cycle Bias
emovi_individual$y_individual <- emovi_individual$y_hat - emovi_individual$y_age        # Correct life-cycle Bias

### Ranks
emovi_individual <- emovi_individual %>% mutate(
  wealth_individual   = ntile(wealth_individual, 100),
  r_wealth_individual = ntile(wealth_individual, 50), 
  Q_wealth_individual = ntile(wealth_individual, 5),
  D_wealth_individual = ntile(wealth_individual, 10),
  
  SEI_individual = ntile(y_individual, 100),
  r_individual   = ntile(y_individual, 50),
  Q_individual   = ntile(y_individual, 5), 
  D_individual   = ntile(y_individual, 10)
)

# Educational Levels
mmsi <- read_csv("./Data/MMSI 2016/MMSI_2016.csv")
mmsi$ID <- paste0("mmsi-", 1:nrow(mmsi))
mmsi <- mmsi %>% transmute(
  ID,
  less_primary = ifelse(P8_3N == "00" | P8_3N == "01", 1, 0),
  primary      = ifelse(P8_3N == "02" | P8_3N == "06", 1, 0),
  secondary    = ifelse(P8_3N == "03" | P8_3N == "07", 1, 0),
  high_school  = ifelse(P8_3N == "04" | P8_3N == "05" | P8_3N == "08", 1, 0),
  grad         = ifelse(P8_3N == "09" | P8_3N == "10" | P8_3N == "11" | P8_3N == "12", 1, 0),
  
  less_primary_f = ifelse(NivEsc_Pad == 1 | NivEsc_Pad == 2, 1, 0),
  primary_f      = ifelse(NivEsc_Pad == 3 | NivEsc_Pad == 4, 1, 0),
  secondary_f    = ifelse(NivEsc_Pad == 5, 1, 0),
  high_school_f  = ifelse(NivEsc_Pad == 6, 1, 0),
  grad_f         = ifelse(NivEsc_Pad == 7, 1, 0),
  
  less_primary_m = ifelse(NivEsc_Mad == 1 | NivEsc_Mad == 2, 1, 0),
  primary_m      = ifelse(NivEsc_Mad == 3 | NivEsc_Mad == 4, 1, 0),
  secondary_m    = ifelse(NivEsc_Mad == 5, 1, 0),
  high_school_m  = ifelse(NivEsc_Mad == 6, 1, 0),
  grad_m         = ifelse(NivEsc_Mad == 7, 1, 0)
)

emovi <- read_dta("./Data/EMOVI 2017/ESRU-EMOVI 2017 Entrevistado.dta")
emovi$ID <- paste0("emovi-", 1:nrow(emovi))
emovi <- emovi %>% transmute(
  ID,
  less_primary = ifelse(p13 == 1 | p13 == 97, 1, 0),
  primary      = ifelse(p13 == 2, 1, 0),
  secondary    = ifelse(p13 == 3 | p13 == 4 | p13 == 7, 1, 0),
  high_school  = ifelse(p13 == 5 | p13 == 6 | p13 == 8 | p13 == 9, 1, 0),
  grad         = ifelse(p13 == 10 | p13 == 11 | p13 == 12, 1, 0),
  
  less_primary_f = ifelse(p42 == 2 | p42 == 8 | p43 == 1, 1, 0),
  primary_f      = ifelse(p43 == 2, 1, 0),
  secondary_f    = ifelse(p43 == 3 | p43 == 4 | p43 == 7, 1, 0),
  high_school_f  = ifelse(p43 == 5 | p43 == 6 | p43 == 8, 1, 0),
  grad_f         = ifelse(p43 == 9 | p43 == 10 | p43 == 11 | p43 == 12, 1, 0),
  
  less_primary_m = ifelse(p42m == 2 | p42m == 8 | p43m == 1, 1, 0),
  primary_m      = ifelse(p43m == 2, 1, 0),
  secondary_m    = ifelse(p43m == 3 | p43m == 4 | p43m == 7, 1, 0),
  high_school_m  = ifelse(p43m == 5 | p43m == 6 | p43m == 8, 1, 0),
  grad_m         = ifelse(p43m == 9 | p43m == 10 | p43m == 11 | p43m == 12, 1, 0),
)

educ <- rbind(mmsi, emovi)

# Joining them all
mmsi <- mmsi_parental %>% full_join(mmsi_individual, by = c("ID", "age"))
emovi <- emovi_parental %>% full_join(emovi_individual, by = c("ID", "age"))

df <- mmsi %>% rbind(emovi) %>% full_join(educ)
df$year <- ifelse(substr(df$ID, 1,1) == "m", 2016 - df$age, 2017 - df$age)
df$less_primary[is.na(df$less_primary)] <- 1
df[is.na(df)] <- 0

mmsi <- read_csv("./Data/MMSI 2016/MMSI_2016.csv")

controls_mmsi <- mmsi %>% 
  transmute(
    sex = ifelse(P1_1 == 2, 1, 0),
    rururb = tam_loc_ENH, 
    ent = ent_ENH,
    ind_lang = ifelse(replace(P3_3, is.na(P3_3), 2) == 1 | replace(P3_3, is.na(P4_3), 2) == 1, 1, 0)
  )

controls_mmsi$ID <- paste0("mmsi-", 1:nrow(mmsi))

emovi <- read_dta("./Data/EMOVI 2017/ESRU-EMOVI 2017 Entrevistado.dta")

controls_emovi <- emovi %>% 
  transmute(
    sex = ifelse(p06 == 2, 1, 0),
    rururb = p24, 
    ent = Estado,
    ind_lang = ifelse(p39 == 1 | p39m == 1, 1, 0)
  )

controls_emovi$ID <- paste0("emovi-", 1:nrow(emovi))

controls <- controls_mmsi %>% rbind(controls_emovi)

df <- df %>% full_join(controls)

write.csv(df, "./Data/final.csv")
