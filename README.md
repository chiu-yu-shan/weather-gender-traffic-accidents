# weather-gender-traffic-accidents
Analysis of how weather conditions affect traffic accidents across genders in Taiwan.

## Dataset
The cleaned dataset can be accessed at the following link: https://drive.google.com/drive/folders/1eacm13gXMXoEhxK_IpQt18fY58wwOpAc?usp=drive_link 

## Research Background 
Previous studies have shown that weather conditions and driver gender both influence traffic accidents. However, these factors are often examined separately, and many studies rely on aggregated city-level weather data, implicitly assuming that accidents occurring within the same city experience similar weather conditions.

In reality, weather conditions can vary substantially across locations within the same city on the same day. To address these limitations, this study combines localized weather data matching with gender interaction analysis to examine whether rainfall affects traffic accident patterns differently across male and female drivers in Taiwan, while also comparing urban and rural areas to evaluate whether these patterns remain consistent across different regional contexts.

## Methods
To examine gender differences under varying rainfall conditions, this study applies statistical modeling with interaction terms between rainfall categories and driver gender. Localized weather data matching is used to reduce the limitations of aggregated city-level weather measurements and provide more location-specific weather conditions for accident observations. Two separate models are estimated:

1. **Accident frequency model**: a negative binomial regression of accident counts (aggregated by date, gender, rain category, and urbanization level) on rainfall category, gender, their interaction, and urbanization. A negative binomial specification was used instead of Poisson after diagnostic checks revealed substantial overdispersion in the Poisson model (residual deviance far exceeding residual degrees of freedom).
2. **Fatality model**: a logistic regression of accident-level fatality (whether an accident resulted in at least one death) on rainfall category, gender, and urbanization.

### Dependent Variables

* Number of traffic accidents (frequency model)
* Whether an accident was fatal (fatality model)

### Independent Variables

#### Rainfall Categories

* No Rain = <1 mm/day (reference group)
* Light Rain = 1–30 mm/day
* Moderate Rain = 30–60 mm/day
* Heavy Rain = >60 mm/day

Reference: Adapted from Taiwan Central Weather Administration rainfall warning criteria.

#### Gender Variable

* Female driver (reference group)
* Male driver

#### Urbanization Categories 

We utilized the official administrative classification of the Ministry of the Interior as a proxy for urbanization. Because Taiwanese law determines township suffixes based on structural development and population benchmarks, categorizing the data into Districts (High), Cities/Towns (Medium), and Townships (Low) provides a legally and structurally sound metric for urbanization. 

### Interaction Terms

* Light Rain × Male
* Moderate Rain × Male
* Heavy Rain × Male

(Included in the accident frequency model only.)

## Data Sources

### Traffic Accident Data

Source: [Taiwan Government Open Data Platform](https://data.gov.tw/)

The traffic accident dataset includes:

* Accident-level A1 and A2 records
* Date and time of accidents
* Latitude and longitude coordinates
* Party gender information
* Responsibility ranking

### Weather Data

Source: [Taiwan Climate Change Projection Information Platform (TCCIP)](https://tccip.ncdr.nat.gov.tw/)

The weather dataset includes:

* Rainfall (mm)
* Average temperature
* Date
* Latitude and longitude coordinates (0.01° grid)

### Dataset Coverage

* Coverage area: Taiwan nationwide
* Time period: 2022–2024
* Total observations: 600,000+ merged observations

## Conclusion

**Accident frequency.** Rainfall is associated with a sharp decline in accident counts relative to dry conditions: light rain corresponds to roughly 72% fewer accidents (IRR ≈ 0.28), and moderate and heavy rain both correspond to roughly 90% fewer accidents (IRR ≈ 0.10), all highly significant (p < 0.001). This pattern is large and consistent enough to warrant caution in interpretation — it may partly reflect reduced traffic volume or exposure during rainy conditions rather than a pure safety effect, since the model does not currently include an exposure offset. Male-involved accidents are associated with substantially higher counts than female-involved accidents (IRR ≈ 1.67, p < 0.001). The rain × gender interaction terms were not statistically significant in the negative binomial model (p = 0.46, 0.99, and 0.36 for light, moderate, and heavy rain respectively), indicating no robust evidence that the relationship between rainfall and accident frequency differs by gender.

**Urbanization and accident frequency.** More urbanized areas see substantially higher accident counts: medium-urbanization areas show about 73% more accidents than low-urbanization areas (IRR ≈ 1.73), and high-urbanization areas show roughly 8 times as many (IRR ≈ 7.77), both highly significant. This is consistent with higher traffic density and exposure in urban areas.

**Fatality risk.** A separate logistic regression of accident-level fatality shows a different and complementary pattern. Rainfall's effect on whether an accident is fatal is weaker and less consistent than its effect on accident frequency: moderate rain is associated with significantly lower odds of fatality (OR ≈ 0.65, p = 0.008), while light and heavy rain are not significant at conventional levels (p = 0.058 and p = 0.148, respectively). Male-involved accidents have roughly double the odds of being fatal compared to female-involved accidents (OR ≈ 1.99, p < 0.001). Urbanization shows a clear negative relationship with fatality risk: medium-urbanization areas have about 51% lower odds of fatality than low-urbanization (rural) areas (OR ≈ 0.49), and high-urbanization areas have about 68% lower odds (OR ≈ 0.32), both highly significant. This suggests that while urban areas experience more frequent accidents due to greater traffic density, accidents in rural areas are considerably more likely to be fatal when they occur — potentially reflecting higher travel speeds, longer emergency response times, or reduced access to trauma care outside urban centers.

**Summary.** Frequency and severity of accidents respond differently to the same factors. Rain is strongly associated with fewer accidents but only weakly and inconsistently associated with lower fatality risk. Urbanization shows the most consistent pattern across both models: more urbanized areas have more frequent accidents but those accidents are markedly less likely to be fatal, while rural areas show the reverse. Gender differences are robust across both models — male involvement is associated with both higher accident frequency and higher fatality risk — but there is no significant evidence that gender moderates the effect of rainfall on accident frequency.
