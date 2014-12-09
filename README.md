brazil2014_model
================

Predictive model for World Cup Brazil 2014
---------------------------------------------------

I developed a statistical model to predict the final standings of the World Cup Brazil 2014 using R with the library [glmnet] (http://cran.r-project.org/web/packages/glmnet/index.html).

The data finally considered in the model are those football games from 1974 to the present.

###Data##
Data were collected from [World Football Elo Ratings] (http://www.eloratings.net), which are in rawdata.csv. Includes football since 1920, reporting outcome of the match and ELO ranking of each country. I also took into account information taken from various gaming sites.


###Modeling##
I used Poisson regression to model the number of goals each team for each game considering the opponent.
The number of goals in each case was estimated by simulating 100k times a Poisson distribution with corresponding lambdas estimated by those regressions.

In each regression, smoothing constant was calculated by cross validation technique using 10 folds.

