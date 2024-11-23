fig2a.rds

Describes the polygon response function shown in Figure 2a
x = smoke PM
est = coefficient for that smoke PM level 
lb = lower bound of bootstrapped 95% confidence interval
ub = upper bound of bootstrapped 95% confidence interval  



fig2b.rds

Describes the polygon response functions shown in Figure 2b
deg1 is coefficient on 1st degree of 4th degree polynomial
deg2 is coefficient on 2nd degree of 4th degree polynomial
And so on…
Each row corresponds to an outcome and is labeled as such.
To plot the polygons based on this you would define a smoke PM level (eg x = 0:75)
And then plot(x, x*deg1 + x^2*deg2 + x^3*deg3 + x^4*deg4)



fig3-main-panels.rds and fig3-subpanels.rds

Corresponds to estimates in main and sub panels of Figure 3, respectively

smokePM_bin = the smoke PM bin the estimates correspond to
est = point estimate (note we have already divided through by base rate to convert to % change)
se = standard error according to point estimate (note we have already divided through by base rate to convert to % change)   
brate = base rate
ub = upper bound 95% confidence interval corrected for multiple comparisons as described in paper
lb = lower bound 95% confidence interval corrected for multiple comparisons
ub_unadj =  upper bound 95% confidence interval without adjusting for multiple comparisons (not used in paper just included for reference)
lb_unadj =    lower bound 95% confidence interval without adjusting for multiple comparisons (not used in paper just included for reference)
