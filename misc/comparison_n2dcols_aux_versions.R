## Computing 'n2dcols'
s <- c((1+sqrt(5))/2,
       1,
       11/8.5,
       14/8.5,
       sqrt(2))
n2dplots <- 1:100
## Old version (worked)
res.old <- sapply(s, function(s.) {
    r <- pmax(3, round(0.5 * (1 + sqrt( 1 + 4 * (n2dplots - 1) / s.))))
    sapply(r, function(r.) if(r. %% 2 == 0) r.+1 else r.)
    }) # (n2dplots, s)-matrix
## New version (theoretically justifiable)
res.new <- sapply(s, function(s.) {
    r <- pmax(3, round(0.5 * (1 + sqrt( 1 + 4 * n2dplots / s.))))
    sapply(r, function(r.) if(r. %% 2 == 0) r.+1 else r.)
    }) # (n2dplots, s)-matrix

## Plotting
col <- c("black", "blue", "green", "red", "orange")
plot(n2dplots, res.old[,1], type = "l", col = col[1], ylim = range(res.old, res.new),
     ylab = "Number of 2d columns (n2dcols)")
for(k in 1:length(s)) {
    lines(n2dplots, res.new[,k], col = adjustcolor(col[k], alpha.f = 0.3), lwd = 4)
    if(k >= 2) lines(n2dplots, res.old[,k], col = col[k])
}
