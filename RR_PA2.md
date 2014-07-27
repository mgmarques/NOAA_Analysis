---
title: "Tornado is the most health harmful Weather Event Type, but Excessive Heat cause
  the greatest mortalities and Hurricane has the greatest economic impacts."
author: "Marcelo Gomes Marques"
date: "Saturday, July 26, 2014"
output:
  html_document:
    highlight: pygments
    theme: united
    toc: yes
---
## Synopsis  
        
This report presents the worst weather events across the United States. It is considering their effects in harmful with respect to population health and what this have the greatest economic consequences.        

To achieve that, we obtain the storm data from U.S. National Oceanic and Atmospheric Administration (NOAA). We process these data by tidying, in special the event types that have too much data quality issues. We considering a subset of the data since 1996, when we have all the event types recorded in the database, sum the fatalities with injuries to obtain the total casualties and calculate the damage cost consider the magnitude and **property damage cost** and **crop damage cost**.  

