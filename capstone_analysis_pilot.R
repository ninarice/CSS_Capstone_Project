# Test whether flavor bans are associated with e-commerce retailer presence
flav_ecomm <- table(analysis_data$flavor_ban, analysis_data$has_ecomm)
prop.table(flav_ecomm, 1)  # Proportions by flavor ban status

chi_result <- chisq.test(flav_ecomm)
print(chi_result)
# Result: No significant association between flavor ban presence and e-commerce.

# Logistic regression: flavor bans → e-commerce presence
logmodel <- glm(has_ecomm ~ flavor_ban, data = analysis_data, family = "binomial")
summary(logmodel)
exp(coef(logmodel))
exp(confint(logmodel))
# Result: Flavor bans are not a significant predictor (p = 0.855).

# --- EXPLORATORY MODELS ---

# Logistic regression: income → e-commerce presence
model_income <- glm(has_ecomm ~ MdnHHnc, data = analysis_data, family = "binomial")
summary(model_income)
exp(coef(model_income))
exp(confint(model_income))
# Result: Higher income is significantly associated with higher odds of e-commerce (p = 0.0343).

# Logistic regression: SDI → e-commerce presence
model_sdi <- glm(has_ecomm ~ sdi, data = analysis_data, family = "binomial")
summary(model_sdi)
exp(coef(model_sdi))
exp(confint(model_sdi))
# Result: Higher SDI is significantly associated with lower odds of e-commerce (p = 0.0441).

# Logistic regression: under-21 population → e-commerce presence
model_age <- glm(has_ecomm ~ Under21_per_cap, data = analysis_data, family = "binomial")
summary(model_age)
exp(coef(model_age))
exp(confint(model_age))
# Result: Marginal negative association (p = 0.0588); more youth may be linked to less e-commerce.

# Logistic regression: racial/ethnic proportions → e-commerce presence
model_race <- glm(has_ecomm ~ White + AfrcnAm + Hispanc + NativAm, data = analysis_data, family = "binomial")
summary(model_race)
exp(coef(model_race))
exp(confint(model_race))
# Result: No significant associations found for race/ethnicity variables.

# Multivariate model: income + SDI + age + race
model_multi <- glm(
  has_ecomm ~ MdnHHnc + sdi + Under21_per_cap + White + AfrcnAm + Hispanc + NativAm,
  data = analysis_data,
  family = "binomial"
)
summary(model_multi)
exp(coef(model_multi))
exp(confint(model_multi))
# Result: No variables remain significant, suggesting shared variance or reduced power.

# Interaction model: SDI × under-21 population
model_interact <- glm(
  has_ecomm ~ sdi * Under21_per_cap,
  data = analysis_data,
  family = "binomial"
)
summary(model_interact)
exp(coef(model_interact))
exp(confint(model_interact))
# Result: No significant interaction; youth proportion does not moderate SDI’s effect.

# Expanded interaction model with income
model_big_interact <- glm(
  has_ecomm ~ MdnHHnc + sdi + Under21_per_cap + sdi:Under21_per_cap,
  data = analysis_data,
  family = "binomial"
)
summary(model_big_interact)
# Result: Still no significant interaction; SES effects remain independent.

# Interaction: income × flavor ban
model_interaction <- glm(
  has_ecomm ~ MdnHHnc * flavor_ban,
  data = analysis_data,
  family = "binomial"
)
summary(model_interaction)
# Result: No significant interaction; income’s effect on e-commerce does not depend on flavor ban presence.

# Test whether SES predicts flavor bans
model_ses_to_ban <- glm(flavor_ban ~ MdnHHnc, data = analysis_data, family = "binomial")
summary(model_ses_to_ban)
exp(coef(model_ses_to_ban))
exp(confint(model_ses_to_ban))
# Result: Higher income significantly predicts flavor ban presence (p = 0.0146).

# Multivariate: SES variables predicting flavor bans
model_ses_to_ban <- glm(flavor_ban ~ MdnHHnc + sdi + Poverty, data = analysis_data, family = "binomial")
summary(model_ses_to_ban)
# Result: No individual SES variable remains significant; overlapping effects likely.

# Visualization: e-commerce prevalence by income tertile
library(ggplot2)
analysis_data %>%
  mutate(income_tertile = ntile(MdnHHnc, 3)) %>%
  group_by(income_tertile) %>%
  summarize(ecomm_rate = mean(has_ecomm)) %>%
  ggplot(aes(x = factor(income_tertile), y = ecomm_rate)) +
  geom_col() +
  labs(x = "Income Tertile", y = "E-commerce Prevalence")
# Visualization: Shows e-commerce presence increases across income levels.
