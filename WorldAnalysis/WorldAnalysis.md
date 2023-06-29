Online News Popularity Analysis
================
Rina Deka and Jessica Ayers
2023-06-29

# Introduction

**RINA**

*You should have an introduction section that briefly describes the data
and the variables you have to work with (just discuss the ones you want
to use). Your target variables is the shares variable.*

*You should also mention the purpose of your analysis and the methods
you’ll use to model the response. You’ll describe those in more detail
later.*

# Data

**RINA or JESS**

*Use a relative path to import the data. Subset the data to work on the
data channel of interest.*

# Summarizations

**BOTH**

*You should produce some basic (but meaningful) summary statistics and
plots about the training data you are working with (especially as it
relates to your response).*

*As you will automate this same analysis across other data, you can’t
describe the trends you see in the graph (unless you want to try to
automate that!). You should describe what to look for in the summary
statistics/plots to help the reader understand the summary or graph.*

# Modeling

**RINA: linear regression & boosted tree model & explanation of the
ensemble model you are using**

**JESS: linear regression & random forest model & explanation of the
idea of a linear regression model & explanation of the ensemble model
you are using**

*You’ll need to split the data into a training (70% of the data) and
test set (30% of the data). Use set.seed() to make things reproducible.*

*The goal is to create models for predicting the number of shares in
some way.*

*Each group member should contribute a linear regression model and an
ensemble tree-based model. As we are automating things, describing the
chosen model is tough, so no need to worry about that.*

*Both models should be chosen using cross-validation.*

## Comparison

**RINA or JESS**

*All four of the models should be compared on the test set and a winner
declared (this should be automated to be correct across all the created
documents).*

# Automation

**RINA or JESS**

*Once you’ve completed the above for a particular data channel, adapt
the code so that you can use a parameter in your build process. You
should be able to automatically generate an analysis report for each
data_channel_is\_ variable*

*You’ll end up with six total outputted documents.*
