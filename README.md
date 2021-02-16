# TCD_FinalYearProject
 
 Code and plots (also displayed in an ``Rmd`` file) repository for my final year project at TCD.
 
 To-do List:
 
 - [x] Map cases by county (e.g. 14 day rate per 100,000) in Ireland. 
 - [ ] Adjust model for smoothing transition between phases (e.g. 
 <img src="https://render.githubusercontent.com/render/math?math=n=n_0"> and <img src="https://render.githubusercontent.com/render/math?math=n_0<n \leq n_0%2Bq"> cases).
 - [ ] Train (two months) and test (latest week or 2 weeks) subsets for model to avoid overfitting.
 - [ ] Multi-line legend labels that are left aligned. ``atop()`` doesn't align the way I want.
 - [ ] Factor in distance <img src="https://render.githubusercontent.com/render/math?math=\left|\left|y^*-y\right|\right|"> to ensure the model matches *cumulative cases* as well as daily cases
 - [ ] Improve optimisation algorithm (danger of local optima)
 - [ ] *Longer term predictions* based on entire dataset using statistical models (e.g. 3 months ahead based on past 12 months)
 
![County Plot](Plots/county-blank.png "Ireland cases by county, per 100k of the population").

![Ireland ARIMA](Plots/Ireland-arima.png "Ireland cases with Base, Periodic and ARIMA models").

![Ireland Periodic Parameters](Plots/Ireland-perparam.png "Periodic a and b parameters for Ireland").

![Ireland NNAR](Plots/Ireland-nn.png "Ireland cases with Base, Periodic and NNAR models").
