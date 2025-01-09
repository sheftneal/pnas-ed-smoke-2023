fig2a.rds/.csv

Describes the polygon response function shown in Figure 2a
x = smoke PM
est = coefficient for that smoke PM level 
lb = lower bound of bootstrapped 95% confidence interval
ub = upper bound of bootstrapped 95% confidence interval  



fig2b.rds/.csv

Describes the polygon response functions shown in Figure 2b
deg1 is coefficient on 1st degree of 4th degree polynomial
deg2 is coefficient on 2nd degree of 4th degree polynomial
And so on…
Each row corresponds to an outcome and is labeled as such.
To plot the polygons based on this you would define a smoke PM level (eg x = 0:75)
And then plot(x, x*deg1 + x^2*deg2 + x^3*deg3 + x^4*deg4)



fig3-main-panels.rds/.csv and fig3-subpanels.rds/.csv

Corresponds to estimates in main and sub panels of Figure 3, respectively
Note: we have already divided through by the base rate so the units are % change (for est, se, lb, ub, etc.)

smokePM_bin = the smoke PM bin the estimates correspond to
est = point estimate 
se = standard error according to point estimate 
brate = base rate
ub = upper bound 95% confidence interval corrected for multiple comparisons as described in paper
lb = lower bound 95% confidence interval corrected for multiple comparisons
ub_unadj =  upper bound 95% confidence interval without adjusting for multiple comparisons (not used in paper just included for reference)
lb_unadj =    lower bound 95% confidence interval without adjusting for multiple comparisons (not used in paper just included for reference)


fig4-sub-panels-insurance-heterogeneity.rds/.csv

Corresponds to estimates in sub panels of Figure 4. Main panels are the same as panels in Figure 3. Units correspond to Figur 4 in the paper and are % change relative to no smoke.

outcome = The category of ED visits
type = Least/Moderately/Most Insured
bin = smoke bin (1 = 0-5, 2 = 5-10, 3 = 10-25, 4 = 25-50, 5 = >50 ugm3-1)
est = estimate
se = standard error
lb = 95% CI lower bound
ub = 95% CI upper bound


fig5.rds/csv

bin = smoke bin (1 = 0-5, 2 = 5-10, 3 = 10-25, 4 = 25-50, 5 = >50 ugm3-1)
est = estimate
se = standard error
lb = 95% CI lower bound
category = {searches/traffic/park-visits} corresponding to panels a-c in fig 5
ub = 95% CI upper bound

