# CondoBravo

### Final Project for NUS course *DBA3702 Descriptive Analytics with R* <br> (Academic Year 2023/2024 Semester 1, Fall 2023)
**Done by:** Chen Yuhan, Cheng Xin Yu Crystal, Cheong Eugene, Ng Teng Suan, Noh Daeho
<br>**Supervisor:** Professor Liu Qizhang

**CondoBravo** is a Shiny web application which aims to help foreign working professionals in Singapore better understand the nuances of the private property rental market in Singapore, and subsequently help them make more informed and educated decisions about choosing a place to rent. The interactive app contains location data of around 2000 unique private properties in Singapore, each with data on their nearest amenities (such as shopping malls, MRT/LRT stations etc.), and historic transaction data (such as price, floor area etc.) from January 2022 to September 2023. 

![image](https://github.com/tsu2000/condobravo/assets/106811131/f122a2f9-3640-4286-8dcd-d4d5555641ea)

Data on private property transactions was obtained mostly from the Urban Redevelopment Authority (URA) of Singapore [here](https://www.ura.gov.sg/property-market-information/pmiResidentialTransactionSearch) and other datsets used were obtained from various online sources such as Kaggle. All data was cleaned and organised into `.csv` files for better data management.

![image](https://github.com/tsu2000/condobravo/assets/106811131/d3be8828-c242-4fdf-95c7-15dcc2be2532)

This web app was a final project for an introductory-level programming course in R at the National University of Singapore.

**Notes:**
- This is a one-off project. There are no plans to update the timeframe for the historic property transaction data or the location data of amenities nearest to properties in the future.
- For those intending to clone this repository and use it for their own personal project, note that this app uses a Google Maps API key for certain features.
- This application is heavily inspired by a similar project done for this exact course with its code [here](https://github.com/kaiwei-tan/DBA3702_project).

**Link to web app on [shinyapps.io](https://www.shinyapps.io/):**

**Update 2024-05-16**: The API for the routes to the nearest amenities has stopped working, so only the district map can be accessed.

[![](https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue)](https://tsu2000.shinyapps.io/condobravo/)
