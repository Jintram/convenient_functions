
# Smooth function / rolling average
# input = list(X=..,Y=..)
shorthand_smooth = function(X, Y, n=7) {
    # idx
    m=round((n-1)/2)
    idx=(1+m):(length(Y)-m)
    
    # prep output
    if (length(X)!=length(Y)) {stop('len X != len Y')}
    Xout=rep(NA,length(X))
    Yout=rep(NA,length(Y))
    Xout[idx] = X[idx]
    
    # actual smoothing
    Yout[idx] = sapply(idx, function(i) {mean(Y[(i-m):(i+m)])})
    return(list(x=x,y=y))
}