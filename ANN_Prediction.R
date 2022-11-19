library(readxl)
Material_Database_Prediction <- read_excel("C:/Users/jmenuey/Downloads/Material_Database_Prediction.xlsx", 
                                           sheet = "Training and Validation data", 
                                           range = "A5:X52", col_names = FALSE)

colnames(Material_Database_Prediction) = c("Drug_Substance_Name",
                                           "Code","D90","MW_DS",
                                           "MP_DS","logP","BD_DS",
                                           "TD_DS","CL_DS","HR_DS",
                                           "TrueDensity_DS","Fw_DS",
                                           "FV_DS","BD_IB","TrD_IB",
                                           "FR_IB","RP","Ts_R","ED_R",
                                           "TS_R","SF_R","BD_FB","60ASTM",
                                           "Fw_FB")
## Running PCA
data.pca = prcomp(Material_Database_Prediction[, c(3:17)], center= TRUE, scale. = TRUE)
data.pca.values = as.data.frame(data.pca$x[, c(1:3)])
######
summary(data.pca)
str(data.pca)
data.pca$x
######


## Scaling Inputs ##
data_pca_scaled = scale(data.pca.values, center = TRUE, scale = TRUE)
##

## Combining PC1, PC2, PC3 to data
data_new_pca = cbind(Material_Database_Prediction, data_pca_scaled)
head(data_new_pca)
##



##finding # of hidden nodes##
beta = 1.0
inputs = 3
outputs = 2
datapoints = nrow(Material_Database_Prediction)
nodes = floor((((.7 * 48) / beta) - (outputs)) / (inputs + outputs + 1))
##

##Making training and test sets##
tr = round(0.7*nrow(data_new_pca), digits = 0)
te = round(0.3*nrow(data_new_pca), digits = 0)
split1 = sample(c(rep(0, tr), rep(1, te)))
trainset1 <- data_new_pca[split1 == 0, ]
testset1 <- data_new_pca[split1 == 1, ]
##



##Making and predicting ANN 1##
nn <- neuralnet(SF_R ~ PC1 + PC2 + PC3, 
                data = trainset1, hidden = 6, 
                linear.output = TRUE)
nn$result.matrix
plot(nn)
pr.nn = neuralnet::compute(nn, testset1)
pr.nn$net.result

## Testing Predictive Power of Model ##
Evaluation = cbind(Material_Database_Prediction[split1 == 1, 21],
                   pr.nn$net.result[ , 2])
##colnames(Evaluation) = c("TS_R - Actual", "SF_R - Actual","TS_R - Predicted","SF_R - Predicted")
colnames(Evaluation) = c( "Observed SF_R",
                         "Predicted SF_R")
Evaluation
##

##Plotting##
ggplot(Evaluation, aes(x = SF_Ro, y = SF_Rp)) +
  geom_point() +
  coord_cartesian(xlim = c(0.6,0.9), ylim = c(0.6, 0.9)) + 
  geom_abline(intercept=0, slope=1) +
  labs(y= "Predicted SF_R", x = "Observed SF_R")
  

autoplot(data.pca,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

autoplot(data.pca)
##

##RMS error calculation##
l = nrow(Evaluation)
sum = 0
for (i in l)
{
  pred = Evaluation[i, 2]
  obs = Evaluation[i, 1]
  temp1 = (obs - pred)^2
  sum = sum + temp
}
RMS1 = sqrt((sum / l))
##

##RRMS error calculation
numsum = 0
densum = 0
for (i in l)
{
  pred = Evaluation[i, 2]
  obs = Evaluation[i, 1]
  predsq = pred^2
  temp = (obs - pred)^2
  
  numsum = numsum + temp
  densum = densum + predsq
}
RRMS1 = sqrt((sum / l) / densum)
##


## Changing number of hidden nodes ##
node_evaluations = data.frame()
"4" = cbind(RMS1, RRMS1)
"5" = cbind(RMS1, RRMS1)
"6" = cbind(RMS1, RRMS1)
rm(fournode)
node_evaluations = rbind(node_evaluations, `4`, `5`, `6`)
colnames(node_evaluations) = c("RMS", "RRMS")
rownames(node_evaluations) = c("4", "5", "6")
node_evaluations
##