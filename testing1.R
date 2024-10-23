library(DataExplorer)
library(ggplot)
fileUrl="C:\\Users\\USER\\Desktop\\5. credit_risk_classification (1).csv"

plot_missing(df)


View(df)
View(df[is.na(df$employment), ])
