\dontrun{

# function of parameters -----

r = seq(0, 3000, length = 25)
beta = c(qlogis(0.5), log(1000))
vcov = matrix(c(4.337749e-02,-1.869121e-03,-1.869121e-03,7.368978e-04), 2, 2)
names(beta) = rownames(vcov) = colnames(vcov) = paste0("beta", 1:length(beta))

f_pars = function(beta1, beta2, r){
    g0 = plogis(beta1)
    sigma = exp(beta2)
    g0 * exp(-r^2/2/sigma^2)
}
plot(r, f_pars(beta[1], beta[2], r), type = "l", xlim = c(0,3000), ylim = c(0,1))
delta = delta_method_f_pars(f_pars, beta, vcov, r = r)
lines(r, delta$lower, col = 2, lty = 2)
lines(r, delta$upper, col = 2, lty = 2)

# function of parameter vector -----

f_beta = function(beta, r){
    f_pars(beta[1], beta[2], r)
}
plot(r, f_beta(beta, r), type = "l", xlim = c(0,3000), ylim = c(0,1))
delta = delta_method_f_beta(f_beta, beta, vcov, r = r)
lines(r, delta$lower, col = 4, lty = 2)
lines(r, delta$upper, col = 4, lty = 2)

# linear model -----

y = mtcars$wt
x = mtcars$mpg
i = order(x)
y = y[i]
x = x[i]
plot(x, y)
X = cbind(1, splines::bs(x, degree = 3, df = 4, intercept = FALSE))
colnames(X) = paste0(0:(ncol(X) - 1))
head(X)
model = lm(y ~ X - 1)
lines(x, fitted(model))
delta = delta_method_X(X, coef(model), vcov(model))
lines(x, delta$lower, col = "blue", lty = 2)
lines(x, delta$upper, col = "blue", lty = 2)

}
