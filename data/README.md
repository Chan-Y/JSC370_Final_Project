# Data Acquisition

## 1. Criminal Records

The Toronto Police Service publishes criminal data on the [Public Safety Data Portal](https://data.torontopolice.on.ca/). The data used in project can be obtained in CSV format directly from the Data Portal.

- [Shootings and Firearm Discharges](https://data.torontopolice.on.ca/datasets/TorontoPS::shooting-and-firearm-discharges-open-data/about)

- [Break and Enter](https://data.torontopolice.on.ca/datasets/TorontoPS::break-and-enter-open-data/about)

- [Auto Theft](https://data.torontopolice.on.ca/datasets/TorontoPS::auto-theft-open-data/about)

- [Robbery](https://data.torontopolice.on.ca/datasets/TorontoPS::robbery-open-data/about)

## 2. Financial Indicators

The City of Toronto publishes and keeps updating the monthly [inflation rate](https://www.toronto.ca/city-government/data-research-maps/toronto-economy-labour-force-demographics/toronto-economic-dashboard/#detail/1.22) and [unemployment rates](https://www.toronto.ca/city-government/data-research-maps/toronto-economy-labour-force-demographics/toronto-economic-dashboard/#detail/1.02). The data is also provided in CSV format which can be downloaded directly from the website. 

## 3. Toronto Neighbourhoods Demographics and Criminal Rates

The City of Toronto also maintains Open Data Portal that provides datasets relative to Toronto neighbourhoods. Obtain data by import the `opendatatoronto` library then get resource by passing package name to `list_package_resources()`.

- [Toronto neighbourhoods geometry](https://open.toronto.ca/dataset/neighbourhoods/)

- [Toronto Demographics](https://open.toronto.ca/dataset/wellbeing-toronto-demographics/)



## 4. Mapbox Access Token

The map view used in this project requires a public access token from an individual's Mapbox account. The token is confidential hence not published in the project folder.

The token could be generated from [here](https://account.mapbox.com/access-tokens/). Save the token in text file named `.mb_token` and place in project folder under `/docs/resources/files/`.