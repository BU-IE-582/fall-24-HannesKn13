require(data.table, quietly = TRUE)
require(skimr)
require(ggcorrplot)
require(GGally)


data_path='/home/baydogan/Courses/IE582/Fall23/Data/Housing_Data/housingdata.csv'
feat_names_path='/home/baydogan/Courses/IE582/Fall23/Data/Housing_Data/names.txt'
data=fread(data_path)
feat_names=fread(feat_names_path,header=F)
names(data)=feat_names$V1
head(data,10)

skim(data)

ggpairs(data)

ggcorrplot(data)

pca_obj = princomp(data,cor=T)
summary(pca_obj,loadings=T)

plot(pca_obj)

data_transformed = copy(data)
data_transformed[,log_lstat:=log(LSTAT+0.001)]
ggpairs(data_transformed)

pca_obj_transformed = princomp(data_transformed,cor=T)
summary(pca_obj_transformed,loadings=T)


