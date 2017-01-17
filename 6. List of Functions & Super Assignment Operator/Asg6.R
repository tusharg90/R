
#Loading the pryr package
library(pryr)

#Defining the closure with accountNum, balance, transactionLog & transactionCounter as necessary values

acc=function(accNum){
#Balance initialized to 0, accountNum to input accNum, transactionLog to NULL object & transactionCounter to 0
  accountNum=accNum
  balance=0
  transactionLog=NULL
  transactionCounter=0
  
  #Defining the list of required functions. The super assignment variable is used whereever appropriate
  #in order to store them in the closure
  #Whenever credit or debit operation is called, the balance value of closure acc is updated.
  functionList=list(
    credit=function(amt){
      openingBal=balance
      balance <<- balance + amt
      transactionCounter <<- transactionCounter +1      
      transactionLog <<- append(transactionLog,list(list(TNum=transactionCounter,Time=Sys.time(),Type="CR",OpeningBal=openingBal,Amount=amt,ClosingBal=balance)))
    },
    debit=function(amt){
      openingBal=balance
      balance <<- balance - amt
      transactionCounter <<- transactionCounter +1
      transactionLog <<- append(transactionLog,list(list(TNum=transactionCounter,Time=Sys.time(),Type="DR",OpeningBal=openingBal,Amount=amt,ClosingBal=balance)))
    },
    balance=function(){
      balance
    },
    number=function(){
      accountNum
    },
    get_trans=function(){
      transactionLog
    }
  )
}


#Test Cases for testing the closure, function list & super assignment operator


# (1) Calling	 the	 closure	 function	 (acc)	 with	 the	 account	 ID,	 and	 this	 returns	 a	 list	 of	 five	
# functions that	all	have	the	execution	environment of	acc	as	their	enclosing	environment.
a1=acc("1234")
str(a1)

# (2) Exploring Unenclose function to show balance is part of the function

unenclose(a1$balance)

# (3)	To	credit	an	account,	use	the	credit()	function.
a1$credit(100)
a1$balance()

# (4)	To	get	the	balance,	use	the	balance()	function
a1$balance()

# (5)	To	get	the	account	number,	use	the	number()	function.
a1$number()

# (6)	To	debit	an	account,	use	the	debit()	function.
a1$debit(20)
a1$balance()

# (7)	To	show	all	the	transactions,	use	the	get_trans()	function.
str(a1$get_trans())