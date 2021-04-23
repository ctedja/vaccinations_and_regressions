# vaccinations_and_regressions
A work in-progress project analysing vaccination trends in the US. @MoctarAboubacar

Moctar wrote the below after exploring the data a little more and brainstorming the issue we are trying to address

## Proposed re-title (tentative): "Those who cannot and those who will not--understanding COVID vaccination rate disparities in the United States"

### Background
The US's vaccination rate is good, but it may be able to give some insights to other countries about ensuring equity in its rollout. Specifically, there is a need to understand the nature of areas with low vaccination rates. Two immediate explanations for counties with lagging vaccine rates come to mind:
* Socio-economic status: with racial and class disparities traditionally providing a barrier to healthcare in the country, COVID vaccination rollout has been anecdotally (or more than anecdotally? might be some NYT article on this with numbers/analysis also) slower in low-income primarily minority-populated areas.
* The growing anti-vaccination movement in the United States means that many are actively refusing to get vaccinated, posing a potential threat to eventual herd immunity. Despite the availability of vaccine stock, areas with high levels of 'anti-vaxxers' are also more likely to have lower vaccination rates.

### Question
Given the above it is interesting to understand the main factors associated with vaccination rates in the United States, and if relevant, to identify and further profile these two groups that are, for different reasons, barriers to the full vaccination of the adult population.

### Proposed Analysis Structure
1. EDA and defining/quantifying the issue: How do we define a low-vaccination county? How many people are in 'low vaccination' counties?
2. Regression modelling: Model 1 ('cannot') uses socio-economic status indicators, state-level fixed effects to look at vaccination rates. Model 2 ('will not') looks at 'antivaxxer' indicators related to vaccination rates. Model 3 ('all together') looks at both of these factors together. State fixed effects will be needed throughout, but also testing to see if there is spatial autocorrelation (and account for it). Analysis with the predictions from the best-fitting model can look at where the outliers are (on up side and down side) and why that may be.
3. Factor/Principal Components Analysis or spatial analysis/LISA: Given the idea of identifying these two groups, if the hypotheses hold about lower rates being more associated with certain types of counties, we can try to make a sound group of those counties--could use PCA to identify differences in factor loadings across different county profiles (this assumes for example that higher trump voting districts would have fewer minorities in them (well you got that right), but also that they might have different social economic statuses, not necessarily...so this is something to think more about), or the spatial approach: a simple Local Indicator of Spatial Autocorrelation (LISA), if applicable, can help to identify 'hot spots' of low vaccination rates, and we can group them spatially. We can then track these groups over time later on and see if they are advancing at the same rate, and zoom in on what could help to accelerate progress there.  



#### Additional variables it might be interesting to get (running list):
* Voteshare that went to Trump in the 2020 election, by county
* 'Speed of vaccine rollout', a measure we can try calculating by getting vaccination rates by county for maybe 2 previous dates and charting the growth rate--could be useful, to think about a little more.

#### Things to check
* 
