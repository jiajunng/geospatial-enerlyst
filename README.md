# Visual Enerlyst

A web application using R that leverages on energy datasets provided by Energy Market Authority (EMA) to perform geospatial analysis to identify energy usage clusters. Further analysis can then be performed to identify root causes for high or low energy consumption in these clusters and devise ways to achieve energy conversation as a nation. 

## Features

* Uploading and processing data on the fly
* Choropleth Map
* Local Moran's I
* Local Indicators of Spatial Association (LISA)

## To Use

1. Download EMA data from https://www.ema.gov.sg/Statistics.aspx
2. Cleaning up of raw data. The data to be uploaded should be in the following format:
   * Geocoded, consisting of X and Y coordinates with column name as "X" and "Y" respectively
   * Row 4(Overall) of the EMA data has to be removed
   * Should follows a naming convention of "YYYY_priv" for private housing data and "YYYY_pub" for public housing data
   * Should follow a file extension of xlsx
   * Merging of two 6 months data into a one year data (only applicable for public housing data) 
