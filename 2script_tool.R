library(tidyverse)
#install.packages('reshape2')
library(reshape2)
correlation_matrix <- function(df, 
                               type = "pearson",
                               digits = 3, 
                               decimal.mark = ".",
                               use = "all", 
                               show_significance = TRUE, 
                               replace_diagonal = FALSE, 
                               replacement = ""){
  
  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = type)
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
  
  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(!is.na(R) & R < 0) > 0) {
    Rformatted = ifelse(!is.na(R) & R > 0, paste0(" ", Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
    Rformatted = paste0(Rformatted, stars)
  }
  
  # make all character strings equally long
  max_length = max(nchar(Rformatted))
  Rformatted = vapply(Rformatted, function(x) {
    current_length = nchar(x)
    difference = max_length - current_length
    return(paste0(x, paste(rep(" ", difference), collapse = ''), sep = ''))
  }, FUN.VALUE = character(1))
  
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(Rnew) <- colnames(x)
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  
  return(Rnew)
}

#impport dataset
austin<-read.csv('austinHousingData.csv')
#get correlation to latest price data
cor_vals<-austin%>%
  correlation_matrix()
#create data frame
cor_vals<-as.data.frame(cor_vals)
#make readings numeric
num<-cor_vals$latestPrice%>%
  str_replace_all('\\*',' ')%>%
  as.numeric()
num<-as.data.frame(num)
#filter out the not significant variables
cor_vals<-austin%>%
  select(-zpid,-numOfCommunityFeatures)%>%
  correlation_matrix()%>%
  as.data.frame()
num<-cor_vals$latestPrice%>%
  str_replace_all('\\*',' ')%>%
  as.numeric()%>%
  as.data.frame()

#create a subset to work with of only significant correlation variables
sub<-row.names(cor_vals)
austin_sub<-austin[sub]
#save the cleaned data
write.csv(austin_sub,'2austin_subset_housing.csv')

#clean the workspace and run off of new data
austin_data<-read.csv('2austin_subset_housing.csv')%>%
  select(-X)


#
#
#
#
#Running a model from the entire Austin Area
#
#
#extract the correlation data from our cleaned dataset
liv<-lm(latestPrice~livingAreaSqFt,austin_data)
liv<-as.numeric(liv$coefficients)
liv_int<-liv[1]
liv_cor<-liv[2]

bth<-lm(latestPrice~numOfBathrooms,austin_data)
bth<-as.numeric(bth$coefficients)
bth_int<-bth[1]
bth_cor<-bth[2]

bed<-lm(latestPrice~numOfBedrooms,austin_data)
bed<-as.numeric(bed$coefficients)
bed_int<-bed[1]
bed_cor<-bed[2]

zip<-lm(latestPrice~zipcode,austin_data)
zip<-as.numeric(zip$coefficients)
zip_int<-zip[1]
zip_cor<-zip[2]

tax<-lm(latestPrice~propertyTaxRate,austin_data)
tax<-as.numeric(tax$coefficients)
tax_int<-tax[1]
tax_cor<-tax[2]

gar<-lm(latestPrice~garageSpaces,austin_data)
gar<-as.numeric(gar$coefficients)
gar_int<-gar[1]
gar_cor<-gar[2]

par<-lm(latestPrice~parkingSpaces,austin_data)
par<-as.numeric(par$coefficients)
par_int<-par[1]
par_cor<-par[2]

yer<-lm(latestPrice~yearBuilt,austin_data)
yer<-as.numeric(yer$coefficients)
yer_int<-yer[1]
yer_cor<-yer[2]

lsm<-lm(latestPrice~latest_salemonth,austin_data)
lsm<-as.numeric(lsm$coefficients)
lsm_int<-lsm[1]
lsm_cor<-lsm[2]

lsy<-lm(latestPrice~latest_saleyear,austin_data)
lsy<-as.numeric(lsy$coefficients)
lsy_int<-lsy[1]
lsy_cor<-lsy[2]

pho<-lm(latestPrice~numOfPhotos,austin_data)
pho<-as.numeric(pho$coefficients)
pho_int<-pho[1]
pho_cor<-pho[2]

acc<-lm(latestPrice~numOfAccessibilityFeatures,austin_data)
acc<-as.numeric(acc$coefficients)
acc_int<-acc[1]
acc_cor<-acc[2]

noa<-lm(latestPrice~numOfAppliances,austin_data)
noa<-as.numeric(noa$coefficients)
noa_int<-noa[1]
noa_cor<-noa[2]

npf<-lm(latestPrice~numOfParkingFeatures,austin_data)
npf<-as.numeric(npf$coefficients)
npf_int<-npf[1]
npf_cor<-npf[2]

pat<-lm(latestPrice~numOfPatioAndPorchFeatures,austin_data)
pat<-as.numeric(pat$coefficients)
pat_int<-pat[1]
pat_cor<-pat[2]

sec<-lm(latestPrice~numOfSecurityFeatures,austin_data)
sec<-as.numeric(sec$coefficients)
sec_int<-sec[1]
sec_cor<-sec[2]

now<-lm(latestPrice~numOfWaterfrontFeatures,austin_data)
now<-as.numeric(now$coefficients)
now_int<-now[1]
now_cor<-now[2]

nwf<-lm(latestPrice~numOfWindowFeatures,austin_data)
nwf<-as.numeric(nwf$coefficients)
nwf_int<-nwf[1]
nwf_cor<-nwf[2]

lot<-lm(latestPrice~lotSizeSqFt,austin_data)
lot<-as.numeric(lot$coefficients)
lot_int<-lot[1]
lot_cor<-lot[2]

pri<-lm(latestPrice~numOfPrimarySchools,austin_data)
pri<-as.numeric(pri$coefficients)
pri_int<-pri[1]
pri_cor<-pri[2]

ele<-lm(latestPrice~numOfElementarySchools,austin_data)
ele<-as.numeric(ele$coefficients)
ele_int<-ele[1]
ele_cor<-ele[2]

mid<-lm(latestPrice~numOfMiddleSchools,austin_data)
mid<-as.numeric(mid$coefficients)
mid_int<-mid[1]
mid_cor<-mid[2]

nhi<-lm(latestPrice~numOfHighSchools,austin_data)
nhi<-as.numeric(nhi$coefficients)
nhi_int<-nhi[1]
nhi_cor<-nhi[2]

dis<-lm(latestPrice~avgSchoolDistance,austin_data)
dis<-as.numeric(dis$coefficients)
dis_int<-dis[1]
dis_cor<-dis[2]

rat<-lm(latestPrice~avgSchoolRating,austin_data)
rat<-as.numeric(rat$coefficients)
rat_int<-rat[1]
rat_cor<-rat[2]

siz<-lm(latestPrice~avgSchoolSize,austin_data)
siz<-as.numeric(siz$coefficients)
siz_int<-siz[1]
siz_cor<-siz[2]

stu<-lm(latestPrice~MedianStudentsPerTeacher,austin_data)
stu<-as.numeric(stu$coefficients)
stu_int<-stu[1]
stu_cor<-stu[2]

sto<-lm(latestPrice~numOfStories,austin_data)
sto<-as.numeric(sto$coefficients)
sto_int<-sto[1]
sto_cor<-sto[2]


##
#
#run an average home cost for homes in selected sub area

selected<-read.csv('selected.csv')
selected<-selected%>%
  select(-zpid,-numOfCommunityFeatures)%>%
  mutate(Sale_p_bed1=(bed_int+(bed_cor*numOfBedrooms)))%>%
  mutate(Sale_p_bath1=(bth_int+(bth_cor*numOfBathrooms)))%>%
  mutate(Sale_p_sqft1=(liv_int+(liv_cor*livingAreaSqFt)))%>%
  mutate(Sale_p_zip=(zip_int+(zip_cor*zipcode)))%>%
  mutate(Sale_p_tax=(tax_int+(tax_cor*propertyTaxRate)))%>%
  mutate(Sale_p_gar=(gar_int+(gar_cor*garageSpaces)))%>%
  mutate(Sale_p_par=(par_int+(par_cor*parkingSpaces)))%>%
  mutate(Sale_p_yer=(yer_int+(yer_cor*yearBuilt)))%>%
  mutate(Sale_p_lsm=(lsm_int+(lsm_cor*latest_salemonth)))%>%
  mutate(Sale_p_lsy=(lsy_int+(lsy_cor*latest_saleyear)))%>%
  mutate(Sale_p_pho=(pho_int+(pho_cor*numOfPhotos)))%>%
  mutate(Sale_p_acc=(acc_int+(acc_cor*numOfAccessibilityFeatures)))%>%
  mutate(Sale_p_noa=(noa_int+(noa_cor*numOfAppliances)))%>%
  mutate(Sale_p_npf=(npf_int+(npf_cor*numOfParkingFeatures)))%>%
  mutate(Sale_p_pat=(pat_int+(pat_cor*numOfPatioAndPorchFeatures)))%>%
  mutate(Sale_p_sec=(sec_int+(sec_cor*numOfSecurityFeatures)))%>%
  mutate(Sale_p_now=(now_int+(now_cor*numOfWaterfrontFeatures)))%>%
  mutate(Sale_p_nwf=(nwf_int+(nwf_cor*numOfWindowFeatures)))%>%
  mutate(Sale_p_lot=(lot_int+(lot_cor*lotSizeSqFt)))%>%
  mutate(Sale_p_pri=(pri_int+(pri_cor*numOfPrimarySchools)))%>%
  mutate(Sale_p_ele=(ele_int+(ele_cor*numOfElementarySchools)))%>%
  mutate(Sale_p_mid=(mid_int+(mid_cor*numOfMiddleSchools)))%>%
  mutate(Sale_p_nhi=(nhi_int+(nhi_cor*numOfHighSchools)))%>%
  mutate(Sale_p_dis=(dis_int+(dis_cor*avgSchoolDistance)))%>%
  mutate(Sale_p_rat=(rat_int+(rat_cor*avgSchoolRating)))%>%
  mutate(Sale_p_siz=(siz_int+(siz_cor*avgSchoolSize)))%>%
  mutate(Sale_p_stu=(stu_int+(stu_cor*MedianStudentsPerTeacher)))%>%
  mutate(Sale_p_sto=(sto_int+(sto_cor*numOfStories)))%>%
  mutate(average_price=((Sale_p_sqft1+Sale_p_bed1+Sale_p_bath1+Sale_p_zip+
                           Sale_p_tax+Sale_p_gar+Sale_p_par+Sale_p_yer+
                           Sale_p_lsm+Sale_p_lsy+Sale_p_pho+Sale_p_acc+
                           Sale_p_noa+Sale_p_npf+Sale_p_pat+Sale_p_sec+
                           Sale_p_now+Sale_p_nwf+Sale_p_lot+Sale_p_pri+
                           Sale_p_ele+Sale_p_mid+Sale_p_nhi+Sale_p_dis+
                           Sale_p_rat+Sale_p_siz+Sale_p_stu+Sale_p_sto)/28))#%>%
  #select(numOfBedrooms,numOfBathrooms,livingAreaSqFt,latestPrice,latest_saleyear,average_price,lotSizeSqFt)
#print(selected)

model<-lm(selected$latestPrice~selected$average_price+I(livingAreaSqFt^3)+I(lotSizeSqFt^3),selected)
model<-as.numeric(model$coefficients)
mod_int<-model[1]
pri_cor<-model[2]
las_cor<-model[3]
lss_cor<-model[4]

selected_data<-selected%>%
  mutate(Sale_Estimate=mod_int+(pri_cor*selected$average_price)+(las_cor*(selected$livingAreaSqFt^3))+lss_cor*(selected$lotSizeSqFt^3))%>%
  mutate(Address=selected$streetAddress)%>%
  select(Address,numOfBedrooms,numOfBathrooms,livingAreaSqFt,lotSizeSqFt,latestPrice,latest_saleyear,average_price,Sale_Estimate)
print(selected_data%>%
        select(Address,Sale_Estimate,latestPrice,numOfBedrooms,numOfBathrooms,livingAreaSqFt,lotSizeSqFt))
#visualize your estimates against the last price
graph<-selected_data%>%
  select(Sale_Estimate,latestPrice,Address)%>%
  gather("Value", "Price", -Address)

ggplot(graph, aes(x =Address, y = Price, fill = Value)) +
  geom_col(position = "dodge")+scale_x_discrete(labels=NULL)+xlab('Home')
summary(lm(selected_data$latestPrice~selected_data$Sale_Estimate,selected_data))
#
#
#
#
#
#
#
#
#
#
#create an estimate tool off of a specific zipcode
#subset a zipcode
zips<-austin_data%>%
  group_by(zipcode)%>%
  count()
zips<-as.data.frame(zips[1])
selected<-read.csv('selected.csv')
area<-as.numeric(selected$zipcode[1])
place<-zips==area
zips1<-austin_data$zipcode==zips[place,]

#create a dataframe of features in the zipcode
d78617<-austin_data[zips1,]


#extract the correlation data from the features in the zipcode
liv<-lm(latestPrice~livingAreaSqFt,d78617)
liv<-as.numeric(liv$coefficients)
liv_int<-liv[1]
liv_cor<-liv[2]

bth<-lm(latestPrice~numOfBathrooms,d78617)
bth<-as.numeric(bth$coefficients)
bth_int<-bth[1]
bth_cor<-bth[2]

bed<-lm(latestPrice~numOfBedrooms,d78617)
bed<-as.numeric(bed$coefficients)
bed_int<-bed[1]
bed_cor<-bed[2]

gar<-lm(latestPrice~garageSpaces,d78617)
gar<-as.numeric(gar$coefficients)
gar_int<-gar[1]
gar_cor<-gar[2]

par<-lm(latestPrice~parkingSpaces,d78617)
par<-as.numeric(par$coefficients)
par_int<-par[1]
par_cor<-par[2]

yer<-lm(latestPrice~yearBuilt,d78617)
yer<-as.numeric(yer$coefficients)
yer_int<-yer[1]
yer_cor<-yer[2]

lsm<-lm(latestPrice~latest_salemonth,d78617)
lsm<-as.numeric(lsm$coefficients)
lsm_int<-lsm[1]
lsm_cor<-lsm[2]

lsy<-lm(latestPrice~latest_saleyear,d78617)
lsy<-as.numeric(lsy$coefficients)
lsy_int<-lsy[1]
lsy_cor<-lsy[2]

pho<-lm(latestPrice~numOfPhotos,d78617)
pho<-as.numeric(pho$coefficients)
pho_int<-pho[1]
pho_cor<-pho[2]

noa<-lm(latestPrice~numOfAppliances,d78617)
noa<-as.numeric(noa$coefficients)
noa_int<-noa[1]
noa_cor<-noa[2]

npf<-lm(latestPrice~numOfParkingFeatures,d78617)
npf<-as.numeric(npf$coefficients)
npf_int<-npf[1]
npf_cor<-npf[2]

pat<-lm(latestPrice~numOfPatioAndPorchFeatures,d78617)
pat<-as.numeric(pat$coefficients)
pat_int<-pat[1]
pat_cor<-pat[2]

sec<-lm(latestPrice~numOfSecurityFeatures,d78617)
sec<-as.numeric(sec$coefficients)
sec_int<-sec[1]
sec_cor<-sec[2]

nwf<-lm(latestPrice~numOfWindowFeatures,d78617)
nwf<-as.numeric(nwf$coefficients)
nwf_int<-nwf[1]
nwf_cor<-nwf[2]

lot<-lm(latestPrice~lotSizeSqFt,d78617)
lot<-as.numeric(lot$coefficients)
lot_int<-lot[1]
lot_cor<-lot[2]

dis<-lm(latestPrice~avgSchoolDistance,d78617)
dis<-as.numeric(dis$coefficients)
dis_int<-dis[1]
dis_cor<-dis[2]

rat<-lm(latestPrice~avgSchoolRating,d78617)
rat<-as.numeric(rat$coefficients)
rat_int<-rat[1]
rat_cor<-rat[2]

siz<-lm(latestPrice~avgSchoolSize,d78617)
siz<-as.numeric(siz$coefficients)
siz_int<-siz[1]
siz_cor<-siz[2]

stu<-lm(latestPrice~MedianStudentsPerTeacher,d78617)
stu<-as.numeric(stu$coefficients)
stu_int<-stu[1]
stu_cor<-stu[2]

sto<-lm(latestPrice~numOfStories,d78617)
sto<-as.numeric(sto$coefficients)
sto_int<-sto[1]
sto_cor<-sto[2]


#use the correlations to create an average price estimate
estimate<-selected%>%
  mutate(Sale_p_bed1=(bed_int+(bed_cor*numOfBedrooms)))%>%
  mutate(Sale_p_bath1=(bth_int+(bth_cor*numOfBathrooms)))%>%
  mutate(Sale_p_sqft1=(liv_int+(liv_cor*livingAreaSqFt)))%>%
  mutate(Sale_p_gar=(gar_int+(gar_cor*garageSpaces)))%>%
  mutate(Sale_p_par=(par_int+(par_cor*parkingSpaces)))%>%
  mutate(Sale_p_yer=(yer_int+(yer_cor*yearBuilt)))%>%
  mutate(Sale_p_lsm=(lsm_int+(lsm_cor*latest_salemonth)))%>%
  mutate(Sale_p_lsy=(lsy_int+(lsy_cor*latest_saleyear)))%>%
  mutate(Sale_p_pho=(pho_int+(pho_cor*numOfPhotos)))%>%
  mutate(Sale_p_noa=(noa_int+(noa_cor*numOfAppliances)))%>%
  mutate(Sale_p_npf=(npf_int+(npf_cor*numOfParkingFeatures)))%>%
  mutate(Sale_p_pat=(pat_int+(pat_cor*numOfPatioAndPorchFeatures)))%>%
  mutate(Sale_p_sec=(sec_int+(sec_cor*numOfSecurityFeatures)))%>%
  mutate(Sale_p_nwf=(nwf_int+(nwf_cor*numOfWindowFeatures)))%>%
  mutate(Sale_p_lot=(lot_int+(lot_cor*lotSizeSqFt)))%>%
  mutate(Sale_p_dis=(dis_int+(dis_cor*avgSchoolDistance)))%>%
  mutate(Sale_p_rat=(rat_int+(rat_cor*avgSchoolRating)))%>%
  mutate(Sale_p_siz=(siz_int+(siz_cor*avgSchoolSize)))%>%
  mutate(Sale_p_stu=(stu_int+(stu_cor*MedianStudentsPerTeacher)))%>%
  mutate(Sale_p_sto=(sto_int+(sto_cor*numOfStories)))%>%
  mutate(average_price=((Sale_p_sqft1+Sale_p_bed1+Sale_p_bath1+Sale_p_gar+
                           Sale_p_par+Sale_p_yer+Sale_p_lsm+Sale_p_lsy+
                           Sale_p_pho+Sale_p_noa+Sale_p_npf+Sale_p_pat+
                           Sale_p_sec+Sale_p_nwf+Sale_p_lot+Sale_p_dis+
                           Sale_p_rat+Sale_p_siz+Sale_p_stu+Sale_p_sto)/20))

#use the linear model with the average price estimate to extract correlation data
model<-lm(estimate$latestPrice~estimate$average_price+I(livingAreaSqFt^3)+I(lotSizeSqFt^3),estimate)
model<-as.numeric(model$coefficients)
mod_int<-model[1]
pri_cor<-model[2]
las_cor<-model[3]
lss_cor<-model[4]

#use the correlations from the linear model to estimate a sale price
selected_data<-estimate%>%
  mutate(Sale_Estimate=mod_int+(pri_cor*estimate$average_price)+(las_cor*(estimate$livingAreaSqFt^3))+lss_cor*(estimate$lotSizeSqFt^3))%>%
  mutate(Address=selected$streetAddress)%>%
  select(Address,numOfBedrooms,numOfBathrooms,livingAreaSqFt,lotSizeSqFt,latestPrice,latest_saleyear,average_price,Sale_Estimate)

#print your estimate table
print(selected_data%>%
        select(Address,Sale_Estimate,latestPrice,numOfBedrooms,numOfBathrooms,livingAreaSqFt,lotSizeSqFt,latest_saleyear))
#visualize your estimates against the last price
graph<-selected_data%>%
  select(Sale_Estimate,latestPrice,Address)%>%
  gather("Value", "Price", -Address)
  
ggplot(graph, aes(x =Address, y = Price, fill = Value)) +
  geom_col(position = "dodge")+scale_x_discrete(labels=NULL)+xlab('Home')
#summarize your analysis
summary(lm(selected_data$latestPrice~selected_data$Sale_Estimate,selected_data))
#
#
#
#
#Find the average costs for a home in the selected area with features you specify 
#based off of correlation and linear regression
#allow user input of features
Sqft<-readline(prompt="Enter Sqft: ")
Sqft<-as.numeric(Sqft)
Bathrooms<-readline(prompt="Enter # of Bathrooms: ")
Bathrooms<-as.numeric(Bathrooms)
Bedrooms<-readline(prompt="Enter # of Bedrooms: ")
Bedrooms<-as.numeric(Bedrooms)
#create average sale prices based on the input and your correlation
Sale_p_bed<-(bed_int+(bed_cor*Bedrooms))
Sale_p_bath<-(bth_int+(bth_cor*Bathrooms))
Sale_p_sqft<-(liv_int+(liv_cor*Sqft))
#provide estimate for a home in the austin area with the input features
#provide graphs showing the estimated averages and linear regression
estimate<-as.integer((Sale_p_sqft+Sale_p_bed+Sale_p_bath)/3)
print(selected_data%>%
        select(Address,Sale_Estimate,latestPrice,numOfBedrooms,numOfBathrooms,livingAreaSqFt,lotSizeSqFt,latest_saleyear))
paste("The average price for property with those features in zipcode ",area ," is $",estimate,sep="")
ggplot(d78617,aes(livingAreaSqFt,latestPrice))+geom_point(alpha=0.05)+xlim(1,4000)+ylim(1,1000000)+geom_smooth(method='lm',color='red')
ggplot(d78617,aes(numOfBathrooms,latestPrice))+geom_point(alpha=0.05)+xlim(0,5)+ylim(1,1000000)+geom_smooth(method='lm',color='red')
ggplot(d78617,aes(numOfBedrooms,latestPrice))+geom_point(alpha=0.05)+xlim(0,5)+ylim(1,1000000)+geom_smooth(method='lm',color='red')
