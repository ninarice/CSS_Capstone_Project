#############################################
### STATISTICAL MODELS: BANS & PREDICTORS ###
#############################################

# Test association between flavor bans and e-commerce presence
flav_ecomm <- table(analysis_data$flavor_ban, analysis_data$has_ecomm)
prop.table(flav_ecomm, 1)  # Row-wise proportions by flavor ban status

# Chi-squared test of independence
chi_result <- chisq.test(flav_ecomm)
print(chi_result)
# Result: No significant association between flavor ban status and e-commerce availability


##################################
### LOGISTIC REGRESSION MODELS ###
##################################

# Model: flavor ban predicting e-commerce presence
model_flavor <- glm(has_ecomm ~ flavor_ban, data = analysis_data, family = "binomial")
summary(model_flavor)
exp(coef(model_flavor))        # Odds ratios
exp(confint(model_flavor))     # 95% CIs
# Result: Not significant (p = 0.855)

# Model: income predicting e-commerce presence
model_income <- glm(has_ecomm ~ MdnHHnc, data = analysis_data, family = "binomial")
summary(model_income)
exp(coef(model_income))
exp(confint(model_income))
# Result: Higher income significantly associated with e-commerce (p = 0.034)

# Rescale income to $10,000 increments for interpretability
analysis_data$MdnHHnc_k10 <- analysis_data$MdnHHnc / 10000
model_income_10000 <- glm(has_ecomm ~ MdnHHnc_k10 + sdi, data = analysis_data, family = "binomial")
exp(coef(model_income_10000))
exp(confint(model_income_10000))

# Model: SDI predicting e-commerce presence
model_sdi <- glm(has_ecomm ~ sdi, data = analysis_data, family = "binomial")
summary(model_sdi)
exp(coef(model_sdi))
exp(confint(model_sdi))
# Result: Higher SDI linked to lower odds of e-commerce (p = 0.044)

# Model: proportion under age 21 predicting e-commerce presence
model_age <- glm(has_ecomm ~ Under21_per_cap, data = analysis_data, family = "binomial")
summary(model_age)
exp(coef(model_age))
exp(confint(model_age))
# Result: Marginal negative association (p = 0.059)

# Model: racial/ethnic composition predicting e-commerce presence
model_race <- glm(has_ecomm ~ White + AfrcnAm + Hispanc + NativAm, data = analysis_data, family = "binomial")
summary(model_race)
exp(coef(model_race))
exp(confint(model_race))
# Result: No significant associations for any racial/ethnic group

# Full multivariate model with SES, age, and race
model_multi <- glm(
  has_ecomm ~ MdnHHnc + sdi + Under21_per_cap + White + AfrcnAm + Hispanc + NativAm,
  data = analysis_data,
  family = "binomial"
)
summary(model_multi)
exp(coef(model_multi))
exp(confint(model_multi))
# Result: No variables remain significant, suggesting shared variance

# Interaction model: SDI × under-21 population
model_interact <- glm(
  has_ecomm ~ sdi * Under21_per_cap,
  data = analysis_data,
  family = "binomial"
)
summary(model_interact)
exp(coef(model_interact))
exp(confint(model_interact))
# Result: No significant interaction; youth does not moderate SDI effect

# Extended interaction model: SES main effects + SDI × youth
model_big_interact <- glm(
  has_ecomm ~ MdnHHnc + sdi + Under21_per_cap + sdi:Under21_per_cap,
  data = analysis_data,
  family = "binomial"
)
summary(model_big_interact)
# Result: Interaction term remains non-significant

# Interaction model: income × flavor ban
model_interaction <- glm(
  has_ecomm ~ MdnHHnc * flavor_ban,
  data = analysis_data,
  family = "binomial"
)
summary(model_interaction)
# Result: No significant interaction; income effect not moderated by policy

######################################
### PREDICTING FLAVOR BAN PRESENCE ###
######################################

# Model: income predicting flavor ban presence
model_ses_to_ban <- glm(flavor_ban ~ MdnHHnc, data = analysis_data, family = "binomial")
summary(model_ses_to_ban)
exp(coef(model_ses_to_ban))
exp(confint(model_ses_to_ban))
# Result: Higher income significantly predicts flavor ban presence (p = 0.015)

# Model: income + SDI + poverty as SES predictors of flavor bans
model_ses_to_ban <- glm(flavor_ban ~ MdnHHnc + sdi + Poverty, data = analysis_data, family = "binomial")
summary(model_ses_to_ban)
# Result: No individual variable significant; likely overlapping effects

#############################################
### VISUALIZATION: E-COMMERCE PREVALENCE ###
#############################################

# Bar chart: E-commerce prevalence by income tertile
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


# Bar chart: E-commerce prevalence by SDI tertile
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


# Bar chart: Overall proportion of e-commerce retailers
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


# Pie chart: Share of e-commerce vs. non-e-commerce retailers
ecomm_summary <- analysis_data %>%
  mutate(ecomm_status = ifelse(has_ecomm == 1, "E-Commerce", "No E-Commerce")) %>%
  count(ecomm_status) %>%
  mutate(
    prop = n / sum(n),
    label = paste0(ecomm_status, "\n", round(prop * 100), "%")
  )

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


# Styled income-based bar chart with labels and custom colors
ecomm_by_income <- analysis_data %>%
  mutate(income_tertile = ntile(MdnHHnc, 3)) %>%
  group_by(income_tertile) %>%
  summarize(ecomm_rate = mean(has_ecomm)) %>%
  mutate(income_label = factor(
    income_tertile,
    levels = 1:3,
    labels = c("Low Income", "Middle Income", "High Income")
  ))

# Assign the plot to a variable
p <- ggplot(ecomm_by_income, aes(x = income_label, y = ecomm_rate, fill = income_label)) +
  geom_col(width = 0.7, color = "black", show.legend = FALSE) +
  geom_text(aes(label = percent(ecomm_rate, accuracy = 1)), vjust = -0.5, size = 5) +
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

# Display the plot
p

# Save the plot as a high-resolution image
ggsave("ecomm_by_income.png", plot = p, width = 8.1, height = 8, dpi = 300)

#############################################
### VIOLATION & E-COMMERCE SUMMARY PLOTS ###
#############################################

# Bar chart: Count of flavored product sellers in ban vs. non-ban areas
df_flavored <- data.frame(
  Ban_Status = c("Ban Present (Violation)", "No Ban"),
  Count = c(11, 8)
)

ggplot(df_flavored, aes(x = Ban_Status, y = Count, fill = Ban_Status)) +
  geom_col(color = "black", show.legend = FALSE) +
  labs(
    title = "Flavored Product Sales Among E-Commerce Retailers",
    x = "Jurisdiction",
    y = "Number of Retailers"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


##############################################
### E-COMMERCE BY DEMOGRAPHIC & POLICY TILES ###
##############################################

# Generate tertiles and categorical labels
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

# Reshape and summarize
df_long <- ecomm_summary_data %>%
  pivot_longer(cols = c(income, sdi, youth, policy), names_to = "Category", values_to = "Group") %>%
  group_by(Category, Group) %>%
  summarise(
    n = n(),
    ecomm = sum(has_ecomm, na.rm = TRUE),
    ecomm_rate = round(100 * ecomm / n, 1),
    .groups = "drop"
  )

# Bar chart: E-commerce prevalence across multiple factors
ggplot(df_long, aes(x = Group, y = ecomm_rate, fill = Category)) +
  geom_col(color = "black", show.legend = FALSE) +
  geom_text(aes(label = paste0(ecomm_rate, "%")), vjust = -0.5, size = 4) +
  labs(
    title = "E-Commerce Prevalence by Sociodemographic and Policy Factors",
    x = NULL,
    y = "Percent of Retailers with E-Commerce"
  ) +
  scale_fill_brewer(palette = "Set1") +
  ylim(0, max(df_long$ecomm_rate, na.rm = TRUE) + 5) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#####################
### GWR + MAPPING ###
#####################
##############################
### GWR STATIC MAP (CLEAN) ###
##############################

library(sf)
library(ggplot2)
library(dplyr)
library(GWmodel)


analysis_data_sf <- st_as_sf(
  analysis_data,
  wkt = "retailer_wkt",
  crs = 4326
) %>%
  st_transform(crs = st_crs(normalized_SE_sf)) %>%
  filter(!is.na(has_ecomm), !is.na(sdi)) %>%
  filter(!st_is_empty(st_geometry(.)))


# Convert to Spatial object for GWR
analysis_sp <- as(analysis_data_sf, "Spatial")

# Bandwidth selection
bw <- bw.gwr(
  has_ecomm ~ sdi,
  data = analysis_sp,
  approach = "AIC",
  kernel = "bisquare",
  adaptive = TRUE
)

# Run GWR
gwr_result <- gwr.basic(
  has_ecomm ~ sdi,
  data = analysis_sp,
  bw = bw,
  kernel = "bisquare",
  adaptive = TRUE
)

# Convert to sf object
gwr_sf <- st_as_sf(gwr_result$SDF)

# Extract only San Diego County tracts
normalized_SE_sd <- normalized_SE_sf %>% filter(County == "San Diego")

#  Reproject both to WGS84 for mapping
tracts_wgs <- st_transform(normalized_SE_sd, 4326)
gwr_sf_wgs <- st_transform(gwr_sf, 4326)

# Create the static map
ggplot() +
  geom_sf(data = tracts_wgs, fill = "gray90", color = "gray60", linewidth = 0.2) +
  geom_sf(data = gwr_sf_wgs, aes(color = sdi), size = 1.2) +
  scale_color_gradient2(
    low = "blue", mid = "white", high = "red",
    midpoint = 0,
    name = "Local SDI\nCoefficient"
  ) +
  labs(
    title = "Local Effect of SDI on E-Commerce Presence",
    subtitle = "Geographically Weighted Regression (GWR), San Diego County"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right"
  )




##########################################
### SUPPLEMENTARY DESCRIPTIVE STATS ###
##########################################

# Number of distinct jurisdiction names
length(unique(analysis_data$ApprxLc))  # Adjust column name if needed


# Summary stats (median) for e-commerce vs. non-e-commerce tracts
analysis_data %>%
  group_by(has_ecomm) %>%
  summarize(
    median_income = median(MdnHHnc_k10),
    median_sdi = median(sdi),
    median_under21 = median(Under21_per_cap),
    median_white = median(White, na.rm = TRUE),
    median_black = median(AfrcnAm, na.rm = TRUE),
    median_hispanic = median(Hispanc, na.rm = TRUE),
    median_nativeam = median(NativAm, na.rm = TRUE)
  )

# Summary stats (mean) for e-commerce vs. non-e-commerce tracts
analysis_data %>%
  group_by(has_ecomm) %>%
  summarize(
    mean_income = mean(MdnHHnc_k10),
    mean_sdi = mean(sdi),
    mean_under21 = mean(Under21_per_cap),
    mean_white = mean(White, na.rm = TRUE),
    mean_black = mean(AfrcnAm, na.rm = TRUE),
    mean_hispanic = mean(Hispanc, na.rm = TRUE),
    mean_nativeam = mean(NativAm, na.rm = TRUE)
  )


##############################################
### E-COMMERCE RETAILER LOCATION BREAKDOWN ###
##############################################

# Filter to only retailers with e-commerce
ecomm_shops <- analysis_data %>% filter(has_ecomm == 1)

# Count number located in San Diego (adjust string if needed)
n_sandiego <- sum(ecomm_shops$ApprxLc == "San Diego", na.rm = TRUE)

# Total number of e-commerce retailers
n_total_ecomm <- nrow(ecomm_shops)

# Compute percentage of e-commerce shops in San Diego
percent_sandiego <- round((n_sandiego / n_total_ecomm) * 100, 1)

# Output result
cat("Percentage of e-commerce shops in San Diego:", percent_sandiego, "%\n")

# List all unique reported locations
print(unique(ecomm_shops$ApprxLc))
