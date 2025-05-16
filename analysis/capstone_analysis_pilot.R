# Test whether flavor bans are associated with e-commerce retailer presence
flav_ecomm <- table(analysis_data$flavor_ban, analysis_data$has_ecomm)
prop.table(flav_ecomm, 1)  # Proportions by flavor ban status

chi_result <- chisq.test(flav_ecomm)
print(chi_result)
# Result: No significant association between flavor ban presence and e-commerce.

# Logistic regression: flavor bans → e-commerce presence
model_flavor <- glm(has_ecomm ~ flavor_ban, data = analysis_data, family = "binomial")
summary(model_flavor)
exp(coef(model_flavor))
exp(confint(model_flavor))
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
analysis_data %>%
  mutate(income_tertile = ntile(MdnHHnc, 3)) %>%
  group_by(income_tertile) %>%
  summarize(ecomm_rate = mean(has_ecomm)) %>%
  ggplot(aes(x = factor(income_tertile), y = ecomm_rate)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "E-Commerce Prevalence by Income Tertile",
    x = "Income Tertile (1 = Low, 3 = High)",
    y = "Proportion of Retailers with E-Commerce"
  ) +
  theme_minimal()


# Visualization: e-commerce prevalence by sdi tertile
analysis_data %>%
  mutate(sdi_tertile = ntile(sdi, 3)) %>%
  group_by(sdi_tertile) %>%
  summarize(ecomm_rate = mean(has_ecomm)) %>%
  ggplot(aes(x = factor(sdi_tertile), y = ecomm_rate)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "E-Commerce Prevalence by SDI Tertile",
    x = "SDI Tertile (1 = Low, 3 = High)",
    y = "Proportion of Retailers with E-Commerce"
  ) +
  theme_minimal()




#bar chart
analysis_data %>%
  mutate(ecomm_status = ifelse(has_ecomm == 1, "E-Commerce", "No E-Commerce")) %>%
  count(ecomm_status) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = ecomm_status, y = prop, fill = ecomm_status)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of Tobacco Retailers Offering E-Commerce",
    x = "",
    y = "Percentage of Retailers"
  ) +
  theme_minimal()

#pie chart
ecomm_summary <- analysis_data %>%
  mutate(ecomm_status = ifelse(has_ecomm == 1, "E-Commerce", "No E-Commerce")) %>%
  count(ecomm_status) %>%
  mutate(prop = n / sum(n),
         label = paste0(ecomm_status, "\n", round(prop * 100), "%"))

# Make the pie chart
ggplot(ecomm_summary, aes(x = "", y = prop, fill = ecomm_status)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = c("steelblue", "gray70")) +
  labs(title = "Share of Tobacco Retailers with E-Commerce") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

library(scales)

# 
ecomm_by_income <- analysis_data %>%
  mutate(income_tertile = ntile(MdnHHnc, 3)) %>%
  group_by(income_tertile) %>%
  summarize(ecomm_rate = mean(has_ecomm)) %>%
  mutate(income_label = factor(
    income_tertile,
    levels = 1:3,
    labels = c("Low Income", "Middle Income", "High Income")
  ))

# Assign plot to a variable
p <- ggplot(ecomm_by_income, aes(x = income_label, y = ecomm_rate, fill = income_label)) +
  geom_col(width = 0.7, color = "black", show.legend = FALSE) +
  geom_text(aes(label = percent(ecomm_rate, accuracy = 1)),
            vjust = -0.5, size = 5) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.2)) +
  labs(
    title = "E-Commerce Prevalence by Income Tertile",
    x = "Income Group",
    y = "Proportion of Retailers with E-Commerce"
  ) +
  scale_fill_manual(values = c("#92C5DE", "#4393C3", "#2166AC")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.text = element_text(color = "gray25", size = 13)
  )
p

# Save it
ggsave("ecomm_by_income.png", plot = p, width = 8.1, height = 8, dpi = 300)


library(ggplot2)

# Flavored product sellers (only)
df_flavored <- data.frame(
  Ban_Status = c("Ban Present (Violation)", "No Ban"),
  Count = c(11, 8)
)

library(dplyr)
library(ggplot2)
library(tidyr)

# Create tertiles
ecomm_summary_data <- analysis_data %>%
  mutate(
    income_tertile = ntile(MdnHHnc, 3),
    sdi_tertile = ntile(as.numeric(sdi), 3),
    under21_tertile = ntile(Under21_per_cap, 3),
    income = factor(income_tertile, labels = c("Low Income", "Mid Income", "High Income")),
    sdi = factor(sdi_tertile, labels = c("Low SDI", "Mid SDI", "High SDI")),
    youth = factor(under21_tertile, labels = c("Low Youth", "Mid Youth", "High Youth")),
    policy = factor(flavor_ban, labels = c("No Flavor Ban", "Flavor Ban"))
  ) %>%
  select(has_ecomm, income, sdi, youth, policy)

# Pivot and summarize
df_long <- analysis_data %>%
  pivot_longer(cols = c(income, sdi, youth, policy), names_to = "Category", values_to = "Group") %>%
  group_by(Category, Group) %>%
  summarise(
    n = n(),
    ecomm = sum(has_ecomm, na.rm = TRUE),
    ecomm_rate = round(100 * ecomm / n, 1),
    .groups = "drop"
  )

# Plot
ggplot(df_long, aes(x = Group, y = ecomm_rate, fill = Category)) +
  geom_col(color = "black", show.legend = FALSE) +
  geom_text(aes(label = paste0(ecomm_rate, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "E-Commerce Prevalence by Sociodemographic and Policy Factors",
    x = NULL,
    y = "Percent of Retailers with E-Commerce"
  ) +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, max(df_long$ecomm_rate, na.rm = TRUE) + 5) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


