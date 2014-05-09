# Read the Analysis Data .csv file for further processing
myData <- read.csv(file.choose(), header = TRUE, sep = ",")

newData <- myData

# Backward stepwise Regression
log.model <- glm(isResponse ~ ., data=newData, family="binomial")
log.model.bck <- step (log.model, direction="backward")
summary(log.model.bck)
log.model.bck$anova

# Forward Stepwise Regression
log.model.null <- glm(isResponse ~ 1, data=newData, family="binomial")
log.model.fwd <- step (log.model.null, direction="forward", 
                       scope=list(lower= .~1, upper=~.zip * eb_demo_gender_match * eb_demo_married_match * eb_demo_age_match * eb_demo_income_match * eb_demo_children_match * eb_demo_affiliation_match * eb_demo_education_match * eb_demo_homeowner_match * eb_demo_homevalue_match * eb_demo_resident_match * eb_demo_occupation_first * eb_demo_occupation_match * eb_geo_dwelling * eb_ebureau_score_fraud_201207120303_adjusted * eb_ebureau_score_fraud_201208030303_adjusted * eb_ebureau_score_market_201207120301 * eb_demo_income_dollars * eb_demo_education_years * eb_demo_homevalue_dollars * eb_demo_resident_years * eb_age_maximum * newState * new_gender * new_married * new_children * new_homeowner * new_affiliation_conservative * new_match_first * new_match_last * new_match_address * new_match_email * new_match_social * new_match_phone * new_verify_first * new_verify_last * new_verify_address * new_verify_phone * new_verify_email * new_verify_social * new_quality_first * new_quality_last * new_quality_address * new_quality_phone * new_quality_email * new_quality_social * new_score_diff_fraud_0712 * new_score_diff_fraud_0803 * new_year_of_birth))
summary(log.model.fwd)
log.model.fwd$anova