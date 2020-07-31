install.packages('readxl')
library(readxl)

# download the file at
# http://www.amstat.org/publications/jse/v19n3/decock/AmesHousing.xls
data = read_excel('~/Downloads/AmesHousing.xls')

Y = log(data[,which(names(data)=='SalePrice')])
colnames1 = c('Lot Frontage','Lot Area','Overall Qual','Overall Cond','Year Built','Year Remod/Add',
               'Mas Vnr Area','BsmtFin SF 1','BsmtFin SF 2','Bsmt Unf SF','1st Flr SF','2nd Flr SF','Low Qual Fin SF',
               'Bsmt Full Bath','Bsmt Half Bath','Full Bath','Half Bath','Bedroom AbvGr','Kitchen AbvGr','TotRms AbvGrd',
               'Fireplaces','Garage Cars','Garage Area','Wood Deck SF','Open Porch SF','Enclosed Porch',
               '3Ssn Porch','Screen Porch','Pool Area','Misc Val','Mo Sold','Yr Sold')
n = dim(data)[1]
X = matrix(0,n,0)
for(i in 1:length(colnames1)){
  X = cbind(X,data[,which(names(data)==colnames1[i])])
}

X$'Lot Frontage'[is.na(X$'Lot Frontage')] = 0
X$'Mas Vnr Area'[is.na(X$'Mas Vnr Area')] = 0

tmp = data[,which(names(data)=='Lot Shape')]
X$LotShape = 1*(tmp=='IR1')+2*(tmp=='IR2')+3*(tmp=='IR3')
tmp = data[,which(names(data)=='Land Slope')]
X$LandSlope = 1*(tmp=='Mod')+2*(tmp=='Sev')
tmp = data[,which(names(data)=='Condition 1')]
tmp1 = data[,which(names(data)=='Condition 2')]
X$Railroad = is.element(as.matrix(tmp),c('RRNn','RRAn','RRNe','RRAe')) | is.element(as.matrix(tmp1),c('RRNn','RRAn','RRNe','RRAe'))
tmp = data[,which(names(data)=='Exter Qual')]
X$ExterQual = 1*(tmp=='Po')+2*(tmp=='Fa')+3*(tmp=='TA')+4*(tmp=='Gd')+5*(tmp=='Ex')
tmp = data[,which(names(data)=='Exter Cond')]
X$ExterCond = 1*(tmp=='Po')+2*(tmp=='Fa')+3*(tmp=='TA')+4*(tmp=='Gd')+5*(tmp=='Ex')
tmp = data[,which(names(data)=='Bsmt Qual')]
X$BsmtQual = 1*(tmp=='Po')+2*(tmp=='Fa')+3*(tmp=='TA')+4*(tmp=='Gd')+5*(tmp=='Ex')
tmp = data[,which(names(data)=='Bsmt Cond')]
X$BsmtCond = 1*(tmp=='Po')+2*(tmp=='Fa')+3*(tmp=='TA')+4*(tmp=='Gd')+5*(tmp=='Ex')
tmp = data[,which(names(data)=='Bsmt Exposure')]
X$BsmtExposure = 1*(tmp=='No')+2*(tmp=='Mn')+3*(tmp=='Av')+4*(tmp=='Gd')
tmp = data[,which(names(data)=='Central Air')]
X$CentralAir = (tmp=='Y')
tmp = data[,which(names(data)=='Kitchen Qual')]
X$KitchenQual = 1*(tmp=='Po')+2*(tmp=='Fa')+3*(tmp=='TA')+4*(tmp=='Gd')+5*(tmp=='Ex')
tmp = data[,which(names(data)=='Functional')]
X$Functional = 1*(tmp=='Sal')+2*(tmp=='Sev')+3*(tmp=='Maj2')+4*(tmp=='Maj1')+5*(tmp=='Mod')+6*(tmp=='Min2')+7*(tmp=='Min1')+8**(tmp=='Typ')
tmp = data[,which(names(data)=='Fireplace Qu')]
X$FireplaceQu = 1*(tmp=='Po')+2*(tmp=='Fa')+3*(tmp=='TA')+4*(tmp=='Gd')+5*(tmp=='Ex')
tmp = data[,which(names(data)=='Garage Finish')]
X$GarageFinish = 1*(tmp=='Unf')+2*(tmp=='RFn')+3*(tmp=='Fin')
tmp = data[,which(names(data)=='Garage Qual')]
X$GarageQual = 1*(tmp=='Po')+2*(tmp=='Fa')+3*(tmp=='TA')+4*(tmp=='Gd')+5*(tmp=='Ex')
tmp = data[,which(names(data)=='Garage Cond')]
X$GarageCond = 1*(tmp=='Po')+2*(tmp=='Fa')+3*(tmp=='TA')+4*(tmp=='Gd')+5*(tmp=='Ex')
tmp = data[,which(names(data)=='Paved Drive')]
X$PavedDrive = 1*(tmp=='P')+2*(tmp=='Y')
tmp = data[,which(names(data)=='Fence')]
X$Fence = 1*(tmp=='MnWw')+2*(tmp=='GdWo')+3*(tmp=='MnPrv')+4*(tmp=='GdPrv')

Y = c(as.matrix(Y))
X = as.matrix(X)
Y = Y[-which(rowSums(is.na(X))>0)]
X = X[-which(rowSums(is.na(X))>0),]