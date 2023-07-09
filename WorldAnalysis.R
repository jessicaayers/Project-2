```{r packages, message = FALSE}
library(tidyverse)
library(rmarkdown)
library(caret)
library(corrplot)
library(gbm)
library(randomForest)
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.path="./images/")
knitr::opts_chunk$set(echo = TRUE)
```

```{r data, message = FALSE,warning=FALSE}
#import data set
#my data set is stored in the folder above where I have the project 2 folder
onpdata <- read_csv("../OnlineNewsPopularity/OnlineNewsPopularity.csv")
#create new variable to represent type of channel to use as parameters
for(i in 1:nrow(onpdata)){
  if(onpdata$data_channel_is_lifestyle[i] == 1){
    onpdata$channel[i] <- "Lifestyle"
  }
  else if(onpdata$data_channel_is_entertainment[i] == 1){
    onpdata$channel[i] <- "Entertainment"
  }
  else if(onpdata$data_channel_is_bus[i] == 1){
    onpdata$channel[i] <- "Bus"
  }
  else if(onpdata$data_channel_is_socmed[i] == 1){
    onpdata$channel[i] <- "Socmed"
  }
  else if(onpdata$data_channel_is_tech[i] == 1){
    onpdata$channel[i] <- "Tech"
  }
  else if(onpdata$data_channel_is_world[i] == 1){
    onpdata$channel[i] <- "World"
  }
}
```

```{r automation}
channelIDs <- unique(onpdata$channel)
output_file <- paste0(channelIDs, "Analysis.md")
params <- lapply(channelIDs, FUN = function(x){list(channel=x)})
analysis <- tibble(output_file, params)
```

```{r render, eval = FALSE}
for(i in 1:6){
onpdata_subset <- subset(onpdata, channel == params[[i]])
render(input = "WorldAnalysis.Rmd", output_file = output_file[i], params = params[i])
}
```
