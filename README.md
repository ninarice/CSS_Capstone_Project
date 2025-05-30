# Impact of Local Flavored Tobacco Bans and Demographic Factors on Vape Shops’ E-Commerce Activity in San Diego County

**Capstone Project – Spring 2025**  
**Authors:** Jenna Brooks, Prosperity Land, & Nina Rice  
**Mentor:** Dr. Eric Leas  
**Affiliation:** The Tobacco E-Commerce Lab, UC San Diego

---

## 🔍 Overview
This project investigates how vape shops’ use of e-commerce varies across neighborhoods with and without flavored tobacco bans in San Diego County. Using API-sourced retailer data and demographic variables, we assessed policy impacts and created a public-facing dashboard to support enforcement of flavored tobacco restrictions.

---

## 📍 Dashboard Access
The interactive map dashboard is hosted on the Tobacco E-Commerce Lab’s website. Due to the sensitive nature of compliance data, access is restricted.  
**To request access**, please contact:  
📩 Dr. Eric Leas – [ecleas@ucsd.edu](mailto:ecleas@ucsd.edu)

---

## 📁 Repository Contents

- `/analysis/`:  
  R scripts and visualizations used for cleaning data, analyzing policy impacts, and generating plots.  
  Includes:  
  - `capstone_data_cleaning.R` – Raw data wrangling and processing  
  - `capstone_analysis_pilot.R` – Pilot analysis of predictors of e-commerce activity  
  - `flavor content analysis.qmd` – Quarto file analyzing flavor content among online retailers  
  - `compliance_plot.png` – Graph showing flavored product availability in banned areas  

- `/data/`:  
  Datasets used in the analysis, including policy coverage and demographic info.  
  Includes:  
  - `Flavor_ban_policy_CA_1` – Flavored tobacco policy data  
  - `SE_data` and `normalized_SE_data` – Neighborhood-level socioeconomic variables  
  - `.census_data.zip.icloud` and `.sdi_data.csv.icloud` – Census and SDI data  
  - `Pilot_data.RData` and `pilot_ready.csv` – Finalized pilot datasets for regression models  

- `/dashboard/`:  
  R Markdown file for generating the interactive dashboard  
  - `Dashboard_leaflet.Rmd` – Map-based dashboard built using Leaflet and census/policy overlays

---

## **Introduction**
We explore how flavored tobacco bans and neighborhood demographics are associated with vape shops' use of e-commerce. This work supports local enforcement by identifying gaps in compliance and leveraging public data.

## **Methods**
Using a sample of 311 vape shops in San Diego County, we merged business listing data with census tract demographics and policy coverage. Logistic regression tested predictors of e-commerce activity.

## **Results**
- **Flavor bans** were *not* significantly associated with e-commerce presence  
- **Higher income areas** had *increased* odds of e-commerce presence  
- **Higher deprivation and youth population** were linked to *reduced* odds  
- **57.9%** of delivery-enabled retailers in banned areas sold flavored products

## **Conclusion**
Flavor bans alone are insufficient to reduce online flavored tobacco sales. Data dashboards like ours can aid targeted enforcement efforts.

---

## 📊 Tools & Technologies

- Google Maps & Yelp APIs  
- Shapefiles & Census Data (SDI, ACS)  
- HTML/CSS/JavaScript (for dashboard)  
- R (for auxiliary data validation and visualization)

---

## 📜 Citation

If you use any code or data from this repository, please cite the manuscript:

> Brooks, J., Land, P., & Rice, N. (2025). *Impact of Local Flavored Tobacco Bans and Demographic Factors on Vape Shops’ E-Commerce Activity in San Diego County*. UC San Diego Capstone Project, The Tobacco E-Commerce Lab.

---

## ⚠️ Disclaimer

This project was conducted as an academic capstone and is not intended to be used for enforcement or legal action. Data were accurate to the best of our knowledge at the time of collection but may be subject to change or error.
