## All equal, expected gini = 0
da <- as.data.frame(cbind(region = rep(1,20), income = rep(1, 20)))
huvud(da)

## All equal, expected gini = 0
da <- as.data.frame(cbind(region = rep(1,20), income = rep(10, 20)))
is(da)
huvud(da)

## Half takes all,  expected gini = 0.5
da <- as.data.frame(cbind(region = rep(1,20), income = c(rep(0, 10) ,rep(10, 10))))
huvud(da)

## Half takes all
## https://en.wikipedia.org/wiki/Gini_coefficient
da <- as.data.frame(cbind(region = rep(1,10), income = c(rep(0, 5) ,rep(10, 5))))
huvud(da)


da <- as.data.frame(cbind(region = rep(1,20), income = c(rep(0, 10) ,rep(1, 10))))
huvud(da)

da <- as.data.frame(cbind(region = rep(1,20), income = c( rep(1, 10), rep(0, 10))))
huvud(da)

da <- as.data.frame(cbind(region = rep(1,10000), income = c( rep(1, 5000), rep(0, 5000))))
huvud(da)

## One takes all,  expected gini = 1 - 1/N = 0.95
da <- as.data.frame(cbind(region = rep(1,20), income = c( rep(1, 1), rep(0, 19))))
huvud(da)

## One takes all  expected gini = 1 - 1/N = 0.9
da <- as.data.frame(cbind(region = rep(1,10), income = c( rep(1, 1), rep(0, 9))))
huvud(da)

## One takes all, expected gini = 1 - 1/N = 1-1/20000 0 0.99995
da <- as.data.frame(cbind(region = rep(1,20000), income = c( rep(1, 1), rep(0, 19999))))
huvud(da)
