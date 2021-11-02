
# Smooth function / rolling average
# input = list(X=..,Y=..)
shorthand_smooth = function(Y, n=7) { #X
    # idx
    m=round((n-1)/2)
    idx=(1+m):(length(Y)-m)
    
    # prep output
    #if (length(X)!=length(Y)) {stop('len X != len Y')}
    #Xout=rep(NA,length(X))
    Yout=rep(NA,length(Y))
    #Xout[idx] = X[idx]
    
    # actual smoothing
    Yout[idx] = sapply(idx, function(i) {mean(Y[(i-m):(i+m)])})
    return(Yout)
    #return(list(x=x,y=y))
}

shorthand_crosscorr = function(X, Y) { #X
    
    X_=X[(!is.na(X))&(!is.na(Y))]
    Y_=Y[(!is.na(X))&(!is.na(Y))]
    X=X_; Y=Y_
    
    N = length(Y)
    TAU = (-N+2):(N-2)
    
    crosscorr = sapply(TAU, function(tau) {
        print(tau)
        range = if (tau>0) { 1:(N-abs(tau))  } else { (1+abs(tau)):N }
        cor(X[range], Y[range+tau])
        })
    
    return(list(tau=TAU, corr=crosscorr))
    
}

give_better_textsize_plot_shorthand <- function(TEXTSIZE, myFamily='Arial'){
  
  if (is.null(myFamily)) {
    theme(#legend.position="none",
          text = element_text(size=TEXTSIZE),
          axis.text = element_text(size=TEXTSIZE),
          plot.title = element_text(size=TEXTSIZE))
  } else {
    theme(#legend.position="none",
          text = element_text(size=TEXTSIZE, family=myFamily),
          axis.text = element_text(size=TEXTSIZE, family=myFamily),
          plot.title = element_text(size=TEXTSIZE, family=myFamily))
  }
}
