# COVID19
COVID-19 vs Financial Markets History.

This is a script in R aim to compare COVID-19 Markets downturn vs other Crises and Outbreaks.
The input is Crashes_List.xlsx with 2 sheets: "Crises" and "Outbreaks", both of them with the Start and End date of each Crisis/Outbreak
The Script download historical data, then parse and prepare the datasets for plotting and finally export the plots in jpeg files.
Currently generates 9 plots, 6 for Crises(Changes, Cumulative Changes and Volume. Whole Period and First X Weeks) and 3 for Outbreaks (Changes, Cumulative Changes and Volume for the Whole Period).
