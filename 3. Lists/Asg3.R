#Assignment 3: Lists

#Function definition:
requiredFunction=function(...) {
  lapply(list(...), function(x){
    list(data=x, mean=mean(x),sd = sd(x),minimum=min(x),maximum=max(x))
    })
}

#Test Case
requiredFunction(c(1:5),c(100:105))
