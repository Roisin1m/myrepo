## Question 3
### Part 1

```{r}
df <- with(mtcars, data.frame(y=mpg, x1=disp, x2=hp, x3=wt))
```

### Part 2
```{r}
nll_lm <- function(data, par) {
  y <- data$y
  X <- as.matrix(cbind(1, data[, c("x1", "x2", "x3")]))
  beta <- par[1:(ncol(X))]
  sigma <- par[length(par)]
  
  res <- y - X %*% beta
  
  nll <- - sum(dnorm(res, mean = 0, sd = sigma, log = TRUE))
  
  return(nll)
}
```


### Part 3
```{r}
initial_beta <- c(mean(df$y), rep(0, ncol(df) - 2)) 
initial_sigma <- sd(df$y) 
initial_par <- c(initial_beta, initial_sigma)

result <- optim(par = initial_par, fn = nll_lm, data = df, method = "L-BFGS-B", 
                lower = c(rep(-Inf, length(initial_beta)), 0.000001), 
                upper = c(rep(Inf, length(initial_beta)), Inf))

beta_hat <- result$par[1:(ncol(df))]
sigma_hat <- result$par[length(result$par)]

print(beta_hat)
print(sigma_hat)
```

### Part 4
It is necessary to implement the negative log-likelihood function because "optim()" minimizes function by default. Minimizing the negative log likelihood is the same as maximizing the log likelihood, which is the same as maximizing the likelihood. This transformation does not change the parameter estimates, they stay the same. 

### Part 5
```{r}
X <- as.matrix(cbind(1, df[, c("x1", "x2", "x3")]))
y <- df$y

B_matrix <- solve(t(X) %*% X) %*% t(X) %*% y
B_matrix

beta_hat_optim <- result$par[1:(ncol(X))]
beta_hat_optim
```

### Part 6
```{r}
res<- y- X %*% B_matrix

sigma_matrix <- sqrt(sum(res^2) / length(y)- ncol(X))


sigma_hat

S_matrix
```

### Part 7



### Part 8
```{r}

result_with_hessian <- optim(par = init_par, fn = nll_lm, data = df, method = "L-BFGS-B", 
                             lower = c(rep(-Inf, length(init_beta)), 0.00001), 
                             upper = c(rep(Inf, length(init_beta)), Inf), hessian = TRUE)

hessian <- result_with_hessian$hessian
standard_error <- sqrt(diag(solve(hessian)))

standard_error
```