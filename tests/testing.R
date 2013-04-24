# THIS SCRIPT IS MEANT TO TEST THE FUNCTIONS IN THE PACKAGE 'dsbase'

# LOAD REQUIRED LIBRARIES AND FUNCTIONS AND LOGIN TO SERVERS 
library('opal')
server1 <- opal.login('******', '*****', 'http://54.242.140.255')
server2 <- opal.login('******', '*****', 'http://54.242.46.59')
opals <- list(server1,server2)

# ASSIGN DATA FROM OPAL SOURCE TO R
datashield.assign(server1, 'D', 'HOPsim.hopsim1ob')
datashield.assign(server2, 'D', 'HOPsim.hopsim2ob')

# DATASHIELD LOADS DATA AS PARLISTS SO HERE WE CHANGE IT INTO DATAFRAMES AND ORDER BY INDIVIDUAL IDs
datashield.assign(opals, "T", quote(data.frame(as.list(D))))
datashield.assign(opals, "D", quote(order.frame(T, T$ind.id)))


# test the mean function 
datashield.aggregate(opals, quote(mean(D$bmi)))

# test the var function 
datashield.aggregate(opals, quote(var(D$bmi)))

# test the product function 
datashield.assign(opals, "P", quote(product(D$bmi, D$glu)))

# test the sum function 
datashield.assign(opals, "S", quote(sum(D$bmi)))

# test the sum function 
datashield.aggregate(opals, quote(summary(D$bmi)))

# test the order.frame function 
datashield.assign(opals, "D", quote(order.frame(T, T$ind.id)))

# For the functions 'contour.plot1', 'histogram.1' and 'table.2d'
# run the tests of the package 'dsbaseclient'.These functions are called
# the equivalent client functions functions.

