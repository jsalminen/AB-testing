# Load data
experiment <- read.csv("Final Project Results Experiment.csv", header = T)
control <- read.csv("Final Project Results Control.csv", header = T)

# SANITY CHECKS

# Calculate lower and upper bounds for number of pageviews (unique cookies)
qbinom(0.025, (sum(control$Pageviews) + sum(experiment$Pageviews)), 0.5) / 
  (sum(control$Pageviews) + sum(experiment$Pageviews))
qbinom(0.975, (sum(control$Pageviews) + sum(experiment$Pageviews)), 0.5) / 
  (sum(control$Pageviews) + sum(experiment$Pageviews))

# Calculate lower and upper bounds for number of clicks on "start free trial"
# button.
qbinom(0.025, (sum(control$Clicks) + sum(experiment$Clicks)), 0.5) /
  (sum(control$Clicks) + sum(experiment$Clicks))
qbinom(0.975, (sum(control$Clicks) + sum(experiment$Clicks)), 0.5) /
  (sum(control$Clicks) + sum(experiment$Clicks))

# Calculate click-through probabilities for control and experiment conditions
CTR_control <- sum(control$Clicks) / sum(control$Pageviews)
CTR_experiment <- sum(experiment$Clicks) / sum(experiment$Pageviews)

# Calculate lower and upper bounds for click-through probability (control condition)
qbinom(0.025, sum(control$Pageviews), CTR_control) /
  sum(control$Pageviews)
qbinom(0.975, sum(control$Pageviews), CTR_control) /
  sum(control$Pageviews)

# Click-through probability in the experiment
CTR_experiment

# Calculate the number of observed pageviews (unique cookies)
sum(control$Pageviews) / (sum(experiment$Pageviews) + sum(control$Pageviews))

# Calculate the number of observed clicks on "start free trial" button
sum(control$Clicks) / (sum(experiment$Clicks) + sum(control$Clicks))

# EFFECT SIZES

# Remove rows with missing values
control <- control[complete.cases(control), ]
experiment <- experiment[complete.cases(experiment), ]

# Calculate gross conversion rates for control and experiment conditions,
# and their difference
control_gross <- sum(control$Enrollments, na.rm = T) / sum(control$Clicks, na.rm = T)
experiment_gross <- sum(experiment$Enrollments, na.rm = T) / sum(experiment$Clicks, na.rm = T) 
gross <- experiment_gross - control_gross

# Calculate pooled standard error of gross conversion
gross_pooled <- (control_gross + experiment_gross) / 2
se_gross <- sqrt(gross_pooled * (1 - gross_pooled) * (1/sum(control$Clicks, na.rm = T) +
                                                      1/sum(experiment$Clicks)))

# Calculate 95 % confidence intervals for the difference in gross conversion
gross - qnorm(0.975) * se_gross
gross + qnorm(0.975) * se_gross

# Calculate net conversion rates for control and experiment conditions,
# and their difference
control_net <- sum(control$Payments, na.rm = T) / sum(control$Clicks, na.rm = T)
experiment_net <- sum(experiment$Payments, na.rm = T) / sum(experiment$Clicks, na.rm = T)
net <- experiment_net - control_net

# Calculate pooled standard error of gross conversion
net_pooled <- (control_net + experiment_net) / 2
se_net <- sqrt(net_pooled * (1 - net_pooled) * (1/sum(control$Clicks, na.rm = T) +                                                        1/sum(experiment$Clicks)))

# Calculate 95 % confidence intervals for the difference in gross conversion
net - qnorm(0.975) * se_net
net + qnorm(0.975) * se_net

# VALUES FOR THE SIGN TEST

# Calculate the number of days when enrollment rate in experiment 
# condition is higher than in control condition
control$enrollRate <- control$Enrollments / control$Clicks
experiment$enrollRate <- experiment$Enrollments / experiment$Clicks
enrollRates <- experiment$enrollRate > control$enrollRate
sum(enrollRates, na.rm = T)

# Calculate the total number of days
length(enrollRates)

# Calculate the number of days when payment rate in experiment 
# condition is higher than in control condition
control$paymentRate <- control$Payments / control$Clicks
experiment$paymentRate <- experiment$Payments / experiment$Clicks
paymentRates <- experiment$paymentRate > control$paymentRate
sum(paymentRates, na.rm = T)

# Calculate the total number of days
length(paymentRates)
