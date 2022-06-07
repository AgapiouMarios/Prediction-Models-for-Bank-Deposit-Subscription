# Prediction-Models-for-Bank-Deposit-Subscription

This data relates to telemarketing phone calls to sell long-term deposits. Within a campaign, the agents make phone calls to a list of clients to sell the product (outbound) or, if meanwhile, the client calls the contact-center for any other reason, he is asked to subscribe to the product (inbound). Thus, the result is a binary unsuccessful or successful contact. This study considers real data collected from one of the retail banks, from May 2008 to June 2010, in a total of 39883 phone contacts. Often, more than one contact to the same client was required, to access if the product (bank term deposit) would be ('yes') or not ('no') subscribed. It is important for the retail bank to predict the set of clients with highest probability to accept a term deposit based on their personal characteristics or behavior during the telemarketing process. The purpose of this project is to be able to predict a successful contact (the client subscribes to the product).

The following table shows all the variables that are available in our dataset and their descriptions.

Variable | Column Number | Type 
--- | --- | --- 
Age | 1 | numeric 
job	| 2	| categorical
marital | 3	| categorical
education |	4 |	categorical
default	| 5 |	categorical
housing	| 6	| categorical
loan	| 7	| categorical
contact	| 8	| categorical
month	| 9	| categorical  
day_of_week	| 10 |	categorical
duration |	11	| numeric
campaign	| 12	| numeric
pdays	| 13	| numeric
previous	| 14	| numeric
poutcome	| 15	| categorical
emp.var.rate	| 16	| numeric
cons.price.idx	| 17	| numeric
cons.conf.idx	| 18	| numeric
euribor3m	| 19	| numeric
nr.employed	| 20	| numeric
SUBSCRIBED	| 21	| categorical (binary)

