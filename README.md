# Impact of alleviating primary care shortages on mortality in the United States


Sanjay Basu, MD, PhD*, Russell S. Phillips, Seth A. Berkowitz, Bruce E. Landon,  Asaf Bitton, Robert L. Phillips

*sanjay_basu@hms.harvard.edu


We sought to identify how geographic inequalities in mortality may be closed by reducing inequality in PCP density. 

*Original study protocol:*


Data sources, outcomes, and covariates

The primary outcome was age-standardized life-expectancy at birth, using small area model estimates from the Institute for Health Metrics and Evaluation (IHME) that utilized data from the National Center for Health Statistics for the years 2010, 2015, and 2017. Secondary outcomes were age-standardized all-cause and cause-specific mortality in each of five major categories chosen because they may be affected by primary care (cardiovascular disease, cancer, infectious diseases, respiratory tract diseases, and substance use or injury), also using estimates from an IHME model that utilized death registration data from the National Vital Statistics System for the same years. The key inequality of interest was the difference in each outcome between counties that met the Healthcare Resources and Services Administration (HRSA) definition of a primary healthcare shortage area (having less than one PCP per 3,500 population), versus counties that did not meet this definition.

The key independent variable of interest was PCP density, defined as the number of non–federally employed physicians younger than 75 years old who were not hospital residents and whose major professional activity was outpatient care in general practice, family medicine, general internal medicine, or general pediatrics, per 100,000 population in each US county and in the District of Columbia (N = 3,103 counties), available from the public-use HRSA Area Health Resources File, which makes use of private data from the American Medical Association Physician Masterfile for 2010, 2015 and 2017.

We included several covariates that may confound associations between the outcomes and primary care density. These covariates were the calendar year, the density of specialist physicians reporting a principal activity of patient care per 100,000 population, the urban or rural status of the county, percentage of the population under the federal poverty threshold, percent of people over 25 years old without a high school diploma, percent female, percent Black, percent Hispanic, unemployment rate, number of days per month with pollution greater than national air quality standards, density of hospital beds per 100,000 population, percent enrolled in Medicare, and medical care cost index (in inflation-adjusted 2017 US Dollars, as an index of healthcare affordability).


Statistical analysis plan

We plan to apply  additive models (GAMs), which are a variant of generalized linear models that attempt to capture non-linearities--in our case, the theorized greater impact on life expectancy of each incremental increase in primary care density in low-density counties than in higher-density counties (see Appendix Text for formulation). We will  fit a GAM in which all above-mentioned covariates were standardized, scaled,31 and log-transformed to adjust for skew. Missing data will  be imputed with chained equations. We will use the fitted GAM to estimate the impact of increasing primary care density in counties with a primary care shortage (those with less than 1 PCP per 3,500 population) to the mean density among non-shortage counties. 

As a robustness check to ensure the direction and magnitude of effect were consistent between a GAM and a standard linear regression, we plan to fit a linear mixed-effects regression model, which focuses on how changes in primary care density over the study period were related to changes in each outcome over the study period, while allowing intercepts (baseline outcome levels) and slopes (associations between primary care density and each outcome) to vary among counties.

As a placebo test, we will include an additional outcome variable of deaths from interpersonal violence (e.g., murder), which would not be expected to be affected by primary care density. Since physicians may be attracted to lower crime communities, however, we will include this analysis to detect if our approach was appropriately accounting for factors that may lead to higher primary care density. 

In a cost analysis, we will estimate the marginal cost for loan forgiveness that would be necessary to achieve those shifts, if applying the previously-estimated amount of loan forgiveness that was found  to incentivize PCPs to locate to a shortage area after residency ($78,803 per physician per year in Public Service Loan Forgiveness cost in inflation-adjusted 2017 US Dollars).

In sensitivity analyses, we will assess how the results might change if targeting shortage areas to achieve at least 1 PCP per 1,500 population (66.7 PCPs per 100,000 population) as suggested previously by a National Rulemaking Committee, as well as achieving alternative thresholds from 1:1,500 to 1:3,500, and repeating the analysis with nurse practitioners included with PCPs. 


