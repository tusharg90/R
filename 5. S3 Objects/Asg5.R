library(pryr)

#Class named MyStack with constructor creating list of atomic elements
MyStack= function(s){
  structure(list(data=s),class="MyStack")
}

#Defining the generic functions for peek, push & pop operations
peek=function(s){
  UseMethod("peek")
}
push=function(s,v){
  UseMethod("push")
}
pop=function(s){
  UseMethod("pop")
}

#Defining the class specific methods for peek, push & pop operations
peek.MyStack=function(s){
  s$data[1]
}
push.MyStack=function(s,v){
  MyStack(append(s$data,v,0))
}
pop.MyStack=function(s){
  MyStack(s$data[2:length(s$data)])
}


#Testing for test cases
s=MyStack(c(10,20,30))
s
str(s)
peek(s)
s=pop(s)

str(s)

s=push(s,400)
str(s)

