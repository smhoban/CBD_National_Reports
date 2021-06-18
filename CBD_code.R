
###################################

#This code will analyze and produce Figures for the manuscript:
#Genetic diversity is considered important but interpreted narrowly in country reports to the Convention on Biological Diversity: current actions and indicators are insufficient
#The first part, Part 1, compares the consistency between the reviewers who reviewed the same report 
#It breaks the percent agreement down by question
#It then chooses one of the pairs for inclusion in the statistical analysis to follow
#The second part, Part 2, goes through each of the questionnaire questions and does the following: defines factors/ gets data in proper format, checks for normality of data, performs a statistical test, and produces a graphic

setwd("C:/Users/shoban.DESKTOP-DLPV5IJ/Dropbox/Projects/IN_PROGRESS/CBD_National_Rep/")
setwd("C:/Users/shoban/Dropbox/Projects/IN_PROGRESS/CBD/CBD_National_Rep")

#Pull in the files that are ready to go
test_5th<-read.csv("REVISED_Qs_5th_Report_ready.csv")
test_6th<-read.csv("REVISED_Qs_6th_Report_ready.csv")

#########################################
#										#	
#		Part 1- COMPARE PAIRS			#
#										#						
#########################################

#We want to quantify the number of cells in the response table that "match" e.g. when the two reviewers
#put the same answer.  We don't need to compare every cell in the entire spreadshet though 
#(omit names, dates, the "other" responses, the page numbers etc.,.. focus only on the real responses.  
#The code below starts with "ans_to_check" which are the columns we will compare- these are the Answers 
#They are organized into sets e.g. the sets of actions so we can look at error rates per question
#The loop goes through each set of answers, and adds up all the matches
#Note that it considers it a match if one reviewer said Qualitative and the other Quantitative- 
#both reviewers identified the item in the report but may differ on its exact categorization  

#The dataset being analyzed is the result of a questionnaire on the CBD National Reports 
#Each row is a distinct country and the columns are the questions 
#Multiple types of question were included in the google form for data collection, so not all columns
#have the same format, but most were a simple checkbox
#Thus the question has one column in the spreadsheet and the actual form response is present in a cell
#If the person clicked multiple boxes, multiple responses will be in a cell, separated by a semicolon ;
#The other type of question included not only a checkbox but a category like Qualitative and Quantitative
#In these cases each question is spread across numerous columns
#Most statistical tests will be on the 'counts' of each questionnaire response

#Lastly note that some questions were included in the questionnaire but will be for a future 
#manuscript- for example questions on GMOs and SDGs


#####################
#	5th report		#
#####################

#This is the list of countries to search for that are paired
pair_list<-c("Bhut", "France", "Gambia", "Leban", "Peru", "Saint Vincent", "Togo", "United King")
#These are the columns from the full questionnaire spreadsheet to compare for the FIFTH
#					status and trends	actions			progress 	SDG		lesson	obst/needs	GMOy/na		indigenous
ans_to_check<-list(13:28,33:48,53:68,	84:95,99:110,	114,		118,	121,	125,		128,		130)
perfect_match<-c(unlist(lapply(ans_to_check,length)),9,12,5,5)
#Executive summary, Target, Target
matchsum<-matrix(ncol=length(pair_list),nrow=length(ans_to_check)+4)
for (ans in 1:length(ans_to_check)){
	for (p in 1:length(pair_list)){
		this_pair<-test_5th[grep(pair_list[p],test_5th[,5]),]
		#This code just requires them both to either equal "1" which is "" 
		#or to not equal "1" which means some other choice was made
		if (ans<6) matchsum[ans,p]<-sum((as.character(this_pair[2,ans_to_check[[ans]]])==1)&(as.character(this_pair[1,ans_to_check[[ans]]])==1))+sum((as.character(this_pair[2,ans_to_check[[ans]]])!=1)&(as.character(this_pair[1,ans_to_check[[ans]]])!=1))
		 else matchsum[ans,p]<-sum(as.vector(this_pair[2,ans_to_check[[ans]]]==this_pair[1,ans_to_check[[ans]]]),na.rm=T)
	}
}

 #the code above also does not compare the threats and importance questions- this will
 
 for (p in 1:length(pair_list)){
	this_pair<-test_5th[grep(pair_list[p],test_5th[,5]),]
	uses<-(strsplit(as.character(this_pair[,10]),split=";"))
	matchsum[ans+1,p]<-9-(length(union(uses[[1]],uses[[2]]))- length(intersect(uses[[1]],uses[[2]])))	#out of 9
	threats<-(strsplit(as.character(this_pair[,73]),split=";"))
	matchsum[ans+2,p]<-12-(length(union(threats[[1]],threats[[2]]))- length(intersect(threats[[1]],threats[[2]])))	#out of 12
	execsum<-(strsplit(as.character(this_pair[,7]),split=";"))
	matchsum[ans+3,p]<-5-(length(union(execsum[[1]],execsum[[2]]))- length(intersect(execsum[[1]],execsum[[2]])))	#out of 5
	target1<-(strsplit(as.character(this_pair[,77]),split=";"))
	matchsum[ans+4,p]<-5-(length(union(target1[[1]],target1[[2]]))- length(intersect(target1[[1]],target1[[2]])))	#out of 5
}


#############################################
#	What categories we're worst at			#
#############################################

rowMeans(matchsum/perfect_match)
	status (indicator)				Actions				Progress	SDG
 [1]  0.8515625 0.9375000 0.9296875 0.7812500 0.7916667 0.6250000 0.7500000
 	Lessons		Obstacl		GMO		indigenous	uses	threats		execsum
 [8]  0.7500000 0.2500000 1.0000000 0.7500000 0.6666667 0.7500000 0.8000000
		Target
[15] 0.8250000
#Note we are worst for Uses and Actions... 
#doesn't mean "wrong" but does mean someone often found something another person didn't

#################################
#	 Overall error rate 		#
#################################

#Because not quantitatively using lessons and obstacles, remove those (parts 8 and 9)
#Because agreement very poor for 5th, remove "progress"... 
#progress poor fit because "No progress or measure of success is given" and
#"Progress is described but reviewer is not able to determine" are considered not same but should be??
#Should maybe remove anyway because majority its not there 

#For this the Lessons and Obstacles (1 part each) are not considered (thus the -2 below)
sum(matchsum[-c(8:9),])/((length(unlist(ans_to_check))+9+12+5+5-2)*length(pair_list))
#83%
sum(matchsum[-c(6:9,12),])/((length(unlist(ans_to_check))+9+12+5+5-4-9)*length(pair_list))
#Without Progress, SDG, Lessons, Obstacles, and uses, it is 85%

#Code to remove one of each pair- loop over the list of pairs, find the rows with that pair, pick one to remove, remove it
 for (p in 1:length(pair_list)){
	pair_match<-which(grepl(pair_list[p],test_5th[,5]))
	test_5th<-test_5th[-pair_match[1],]		#if want to do random, use round(runif(1,1,2))
}


#####################
#	6th report		#
#####################

#This is the list of countries to search for that are paired for the SIXTH
pair_list<-c("Finl", "Gambi", "Niger", "Norw", "Spain", "Sweden", "Tanz")
#These are the columns to compare
 #					status and trends	actions			progress 	SDG		lesson	obst/needs	GMOy/na		indigenous
ans_to_check<-list(52:67,72:87,92:107,	14:25,29:40,	48,			112,			44,			132,		122)
perfect_match<-c(unlist(lapply(ans_to_check,length)),9,12,5,5)
matchsum<-matrix(ncol=length(pair_list),nrow=length(ans_to_check)+4)
for (ans in 1:length(ans_to_check)){
	for (p in 1:length(pair_list)){
		this_pair<-test_6th[grep(pair_list[p],test_6th[,5]),]
		 if (ans<6) matchsum[ans,p]<-sum((as.character(this_pair[2,ans_to_check[[ans]]])==1)&(as.character(this_pair[1,ans_to_check[[ans]]])==1))+sum((as.character(this_pair[2,ans_to_check[[ans]]])!=1)&(as.character(this_pair[1,ans_to_check[[ans]]])!=1))
		 else matchsum[ans,p]<-sum(as.vector(this_pair[2,ans_to_check[[ans]]]==this_pair[1,ans_to_check[[ans]]]),na.rm=T)
	}
}

#the code above also does not compare the threats and importance questions- this will
 
 for (p in 1:length(pair_list)){
	this_pair<-test_6th[grep(pair_list[p],test_6th[,5]),]
	uses<-(strsplit(as.character(this_pair[,134]),split=";"))
	matchsum[ans+1,p]<-9-(length(union(uses[[1]],uses[[2]]))- length(intersect(uses[[1]],uses[[2]])))	#out of 9
	threats<-(strsplit(as.character(this_pair[,129]),split=";"))
	matchsum[ans+2,p]<-12-(length(union(threats[[1]],threats[[2]]))- length(intersect(threats[[1]],threats[[2]])))	#out of 12
	execsum<-(strsplit(as.character(this_pair[,7]),split=";"))
	matchsum[ans+3,p]<-5-(length(union(execsum[[1]],execsum[[2]]))- length(intersect(execsum[[1]],execsum[[2]])))	#out of 5
	target1<-(strsplit(as.character(this_pair[,77]),split=";"))
	matchsum[ans+4,p]<-5-(length(union(target1[[1]],target1[[2]]))- length(intersect(target1[[1]],target1[[2]])))	#out of 5
}

#############################################
#	What categories we're worst at			#
#############################################

rowMeans(matchsum/perfect_match)

	status (indicator)				actions				progress	SDG
 [1] 0.8482143 0.8660714 0.8571429 0.7619048 0.7142857 0.7142857 0.5714286
	obstacles	GM		indigenous	uses		threats	execsum 	target 
 [8] 0.5714286 1.0000000 0.7142857 0.6031746 0.9047619 0.7142857 1.0000000
#Note we are worst for Uses and Actions... 
#doesn't mean "wrong" but does mean someone often found something another person didn't


#################################
#	 Overall error rate 		#
#################################

#For this the Obstacles (1 part) are not considered
sum(matchsum[-8,])/((length(unlist(ans_to_check))+9+12+5+5-1)*length(pair_list))
#81% overall... 86% for Status/ indicator/ trend... Actions 74%
sum(matchsum[-c(7,8,11)])/((length(unlist(ans_to_check))+9+12+5+5-1-1-9)*length(pair_list))
#without uses, obstacles, and SDG, its 89%


#Code to remove one of each pair- loop over the list of pairs, 
#find the rows with that pair, pick one to remove, remove it
 for (p in 1:length(pair_list)){
	pair_match<-which(grepl(pair_list[p],test_6th[,5]))
	test_6th<-test_6th[-pair_match[1],]
}


#########################################
#										#	
#		Part 2- STATISTICS TIME			#
#										#						
#########################################

#The following breaks down each country by its socio economic category according to the World Bank
#All statistical tests will be performed for all reports as a whole and as by these categories
socioec<-read.csv("socio_economic.csv")
country_sets<-list(1:57,which(socioec[,2]=="H"),which(socioec[,2]=="M"),which(socioec[,2]=="L"))
as.character(test_5th[,5])==as.character(socioec[,1])
#The following focuses on the continents... so what you need to do is pick one of these 
#socio eco or continent, and run all the code, then pick the other and run all the code
country_sets<-list(1:57,which(socioec[,3]=="Asia"),which(socioec[,3]=="Namerica"),which(socioec[,3]=="Samerica"),which(socioec[,3]=="Africa"),which(socioec[,3]=="Europe"))

#####################################
#									#
#	Gen Div in EXECUTIVE SUMMARY	#
#									#
#####################################

#Is Genetic Diversity mentioned in Executive Summary and if yes, how

test_5th[,7]
("biotech", "other comp", "not ment", "partial ref", "directly")
exec_sum_res<-array(dim=c(6,2,4))
for (i in 1:4){
#making each result into array with first array slot being all countries and each other array slot for countries in each each socioeconomic class (H, M, L)
#s is the subset of countries in that class
s<-country_sets[[i]]
exec_sum_res[,,i]<-matrix(c
 (sum(test_5th[s,7]==""), sum(test_6th[s,126]==""), #No executive summary or cannot answer
 sum(grepl("biotech",test_5th[s,7])), sum(grepl("biotech",test_6th[s,126])), #mention of biotechnology or access/ benefits
 sum(grepl("other comp",test_5th[s,7])), sum(grepl("other comp",test_6th[s,126])), #mentioned with other components (e.g. species)
 sum(grepl("not ment",test_5th[s,7])), sum(grepl("not ment",test_6th[s,126])), #no mention 
 sum(grepl("partial ref",test_5th[s,7])), sum(grepl("partial ref",test_6th[s,126])), #partial ref to gen div including breeds 
 sum(grepl("directly",test_5th[s,7])), sum(grepl("directly",test_6th[s,126])) #direct mention within spp genetic div/ pop gen
),byrow=T, ncol=2)
}
rownames(exec_sum_res)<-c("no executive summary or cannot answer", "mention of biotechnology or access/ benefits", "mentioned with other components (e.g. species)", "no mention", "partial reference (traits, gene banks, breeds", "direct mention genetic diversity at population level")
colnames(exec_sum_res)<-c("5th report","6th report")
exec_sum_res[,,1]
write.csv(exec_sum_res[,,1],file="exec_sum_res.csv")

#no executive summary or cannot answer    			          10         23
#mention of biotechnology or access/ benefits                 17          5
#mentioned with other components (e.g. species)                9          1
#no mention                                                    4          7
#partial reference (traits, gene banks, breeds                28          5
#direct mention genetic diversity at population level         18         21
 #NO TEST FOR 5TH VS 6TH 
 
 #For percentages reported in Appendix
 exec_sum_res[,,1]/57
 #For percentages reported in main text and in Appendix- of those who reported 
 exec_sum_res[,,1]/47
 #socioeco, not significant, p=0.58
 fisher.test(cbind(exec_sum_res[,1,2:4]))
 
 

#####################################
#									#
#	Gen Div TARGET + sp focus of	#
#									#
#####################################

 #Is there a Target 13 equivalent Target and what kind of species is it on... agric/ non agric/ non economic/ not specific
 
 (table(test_5th[,77]));  (table(test_6th[,7]))
 target_focus_res<-array(dim=c(5,2,4))
for (i in 1:4){
#making each result into array with first array slot being total and each other array slot for each socioeconomic class (H, M, L)
#s is the subset of countries in that class
s<-country_sets[[i]]
target_focus_res[,,i]<-matrix(c
 (sum(grepl("No national",test_5th[s,77])), sum(grepl("No national",test_6th[s,7])),	#no genetic diversity National Target
  sum(grepl("has no",test_5th[s,77])), sum(grepl("has no",test_6th[s,77])),		#no specification of spp use
  sum(grepl("only refers",test_5th[s,77])), sum(grepl("only refers",test_6th[s,7])),	#only agricultural/ domesticated spp
  sum(grepl("might also",test_5th[s,77])), sum(grepl("might also",test_6th[s,7])),	#includes non ag e.g., socio economic spp
  sum(grepl("clearly means",test_5th[s,77])), sum(grepl("clearly means",test_6th[s,7])) #includes non ag and non economic spp 
 ),byrow=T,ncol=2)
 }
 rownames(target_focus_res)<-c("no genetic diversity National Target", "no specification of spp use", "only agricultural/ domesticated spp", "includes non agricultural e.g., socio economic spp", "includes non agricultural and non economic spp ")
 colnames(target_focus_res)<-c("5th report","6th report")
 target_focus_res[,,1]
 write.csv(target_focus_res[,,1],file="target_focus_res.csv")
 
#                                                   5th report 6th report
#no genetic diversity National Target                       14          9
#no specification of spp use                                 9          0
#only agricultural/ domesticated spp                        13         12
#includes non agricultural e.g., socio economic spp         11         13
#includes non agricultural and non economic spp             10         18

#difference between countries with and without a gen div target, over time- NO SIGNIF DIFF
fisher.test(matrix(c(14,43,9,48),nrow=2,ncol=2,byrow=T),B=50000)		#p=0.351
#difference in type of species mentioned in the target
#all three categories
fisher.test(matrix(c(13,11,10,12,13,18),nrow=2,ncol=3,byrow=T),B=50000)	#p=0.505
#last  two categories 
fisher.test(matrix(c(11,10,13,18),nrow=2,ncol=2,byrow=T),B=50000)		#p=0.574
#pooling last two (socioeconomics) vs. agriculture 
fisher.test(matrix(c(13,21,12,31),nrow=2,ncol=2,byrow=T),B=50000)	#p=0.463

#socioeconomic- NO SIGNIF DIFF
 fisher.test(cbind(target_focus_res[,1,2:4]));   fisher.test(cbind(target_focus_res[,2,2:4]))
 fisher.test(cbind(target_focus_res[,1,2:4])+cbind(target_focus_res[,2,2:4]),simulate.p.val=50000)	#p=0.58 socioecon; 0.4098 continent
 

#########################################
#										#
#	PROGRESS on Aichi/ equiv target 	#
#										#
#########################################

 test_5th[,114]
 test_6th[,48]
 
 progress_res<-array(dim=c(8,2,4))
for (i in 1:4){
#making each result into array with first array slot being total and each other array slot for each socioeconomic class (H, M, L)
#s is the subset of countries in that class
s<-country_sets[[i]]
progress_res[,,i]<-matrix(c
(sum(grepl("exceed",test_5th[s,114])), sum(grepl("exceed",test_6th[s,48])), #On track to exceed
 sum(grepl("achieve",test_5th[s,114])), sum(grepl("achieve",test_6th[s,48])), #On track to achieve
 sum(grepl("insufficient",test_5th[s,114])), sum(grepl("insufficient",test_6th[s,48])), #Some progress but insufficient
 sum(grepl("no change",test_5th[s,114])), sum(grepl("no change",test_6th[s,48])), #No change
 sum(grepl("away",test_5th[s,114])), sum(grepl("away",test_6th[s,48])), #Moving away from the Target
 sum(grepl("Uknown",test_5th[s,114])), sum(grepl("Uknown",test_6th[s,48])), #Progress reported as Unknown,  
 sum(grepl("not able to",test_5th[s,114])) + sum(grepl("No progress",test_5th[s,114])), 		#Reviewer is not able to interpret progress to a category
		sum(grepl("not able to",test_6th[s,48])) + sum(grepl("No progress",test_6th[s,48])), #No progress/ measure of success is given on genetic Target (combined two choices)
 sum(grepl("Text refers",test_5th[s,114])), sum(grepl("Text refers",test_6th[s,48])) #national Targets not Aichi
),byrow=T, ncol=2)
}
########Does not include the 'cannot find' answer which is a different column, keep that separate

 rownames(progress_res)<-c("Progress = On track to exceed", "Progress = On track to achieve", "Progress = Some progress but insufficient", "Progress = No change", "Progress = Moving away from Target", "Progress = Unknown", "Cannot interpret or no measure given", "No Aichi Target included")
colnames(progress_res)<-c("5th report","6th report")
 write.csv(progress_res[,,1],file="progress_res.csv")

#NOTE THAT THIS PDF ONLY CONSIDERS CATEGORIES ASSIGNED AND NOT "CANNOT ANSWER"
 pdf("progress.pdf",width=15,height=9)
 par(oma=c(1,21,1,0),cex=1.6)
 barplot( t(progress_res[6:1,,1]),horiz=T,las=1,cex.axis=1.5,cex.names=1.4,beside=T,legend=c("5th Report", "6th Report"), col=c("purple","brown"),xlim=c(0,25))
 dev.off()

 #Compare progress for 5th and 6th reports- NO SIGNIF DIFF
  fisher.test(progress_res[1:6,,1])		#p=0.469  

#socioeconomic- NO SIGNIF DIFF
 fisher.test(cbind(progress_res[1:6,1,2:4]));   fisher.test(cbind(progress_res[1:6,2,2:4]))
  fisher.test(cbind(progress_res[1:6,1,2:4])+cbind(progress_res[1:6,2,2:4]),B=50000)	
  #p=0.21 socio econ; p=0.074 for continent for 6th report, asia more likely to report higher progress 

 
#####################################
#									#
#	Gen Div in OTHER TARGETS		#
#									#
#####################################

#number of countries that mention genetic diversity under another Target OTHER THAN Target 13
#Nagoya, another target, No mention 
other_target_res<-matrix(c
 (sum(grepl("Nagoya",test_5th[,80])), sum(grepl("Nagoya",test_6th[,10])),	#Nagoya/ access, benefits Target
  sum(grepl("another target",test_5th[,80])), sum(grepl("another target",test_6th[,10])),	#Another target
  sum(grepl("No mention",test_5th[,80])), sum(grepl("No mention",test_6th[,10]))	#No mention genetic div in other Targets
 ),byrow=T,ncol=2)
 rownames(other_target_res)<-c("Nagoya/ access and benefits Target", "Mention of genetic diversity in another Target description/ text", "No mention of genetic diversity under other Target descriptions/ text")
 colnames(other_target_res)<-c("5th report","6th report")
 other_target_res
sum(table(test_5th[,80])[c(3,5)]) 
sum(table(test_6th[,10])[c(3,5)]) 

  #                                                                    5th report 6th report
#Nagoya/ access and benefits Target                                            32         31
#Mention of genetic diversity in another Target description/ text              17         33
#No mention of genetic diversity under other Target descriptions/ text         14          8
#HOWEVER we will use Kevin's numbers- more detailed analysis actually assigning the Target number 

#This is number of countries with genetic div or tools mentioned under at least one non T13/ T16 Target 
fisher.test(matrix(c(11,46,28,29),nrow=2)) #p=0.001
#This is number of targets with at least one mention 
fisher.test(matrix(c(13,7,19,1),nrow=2)) 	#p=0.044
 
  
  
#####################################
#									#
#		Gen Div INDICATORS			#
#		STATUS AND TRENDS			#
#		NOTE THIS ONE HAS 			#
#		DIFF SETUP FROM OTHERS		#
#		Qual/ Quant answers			#
#									#
#####################################

#What indicators are mentioned and are they increasing/ decreasing

answers3<-c("Plant & animal gen. resources in facilities","Plant gen. resources inventoried","Plant gen. resources threatened","Std. Material Transfer Agreements","Local breeds at risk/ not-at-risk","Red List Index/ Red List status","Species Habitat Index","Species Protection Index","Develop/ Implement strategies for gen. div.","Gen. data metrics","Preserve indigenous/local gen. knowledge")
#,"A general or indirect statement","a species level indicator","non-genetic biodiversity indicator","Other")


#Note that below one column is omitted- the column "local/indigenous breeds at risk" was a duplicate- skip the second occurrence
 quant3a<-apply(test_5th[,c(13:17,19:28)],2,function(x) sum(grepl("Quant",x)))
 qual3a<-apply(test_5th[,c(13:17,19:28)],2,function(x) sum(grepl("Qual",x)))
 quant6a<-apply(test_6th[,c(52:56,58:67)],2,function(x) sum(grepl("Quant",x)))
 qual6a<-apply(test_6th[,c(52:56,58:67)],2,function(x) sum(grepl("Qual",x)))
 
 dec3b<-apply(test_5th[,c(33:37,39:48)],2,function(x) sum(grepl("Dec",x)))
 inc3b<-apply(test_5th[,c(33:37,39:48)],2,function(x) sum(grepl("Inc",x)))
 noc3b<-apply(test_5th[,c(33:37,39:48)],2,function(x) sum(grepl("No",x)))
 dec6b<-apply(test_6th[,c(72:76,78:87)],2,function(x) sum(grepl("Dec",x)))
 inc6b<-apply(test_6th[,c(72:76,78:87)],2,function(x) sum(grepl("Inc",x)))
 noc6b<-apply(test_6th[,c(72:76,78:87)],2,function(x) sum(grepl("No",x)))
 
 write.csv(cbind(quant3a,qual3a),file="quant_qual_indicators_5th_status.csv")
 write.csv(cbind(quant6a,qual6a),file="quant_qual_indicators_6th_status.csv")
 
 
 #Adding "other" responses- Just uncomment these lines and the name of the pdf, and run
quant3a<-quant3a+c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0)
qual3a<-qual3a+c(0,0,0,0,0,1,0,0,0,0,0,1,0,0,0)
 
pdf("plots_indicators_original.pdf",height=10, width=21)
par(mfrow=c(1,2),mar=c(2,1,2,1),oma=c(2,18,1,0),cex=1.6)
 barplot(t(cbind(quant3a,qual3a)),names=answers3,horiz=T,las=2,col=c("black","grey"),legend=c("Quantitative","Qualitative"),xlim=c(0,40),cex.names=0.9,cex.axis=0.9)
 mtext(side=3,line=1,paste("5th National Report",nrow(test_5th), "Countries"),cex=2)
 mtext(side=1,line=0.5, "number of reports",outer=T,cex=2)
 barplot(t(cbind(dec3b,inc3b,noc3b)),names=rep("",15),horiz=T,las=2,xlim=c(0,40),col=c("red","blue","yellow"),legend=c("Decreasing","Increasing","No Change"),cex.axis=1)
par(mfrow=c(1,2),oma=c(2,18,1,0),cex=1.6)
 barplot(t(cbind(quant6a,qual6a)),names=answers3,horiz=T,las=2,col=c("black","grey"),legend=c("Quantitative","Qualitative"),xlim=c(0,40),cex.names=0.9,cex.axis=0.9)
 mtext(side=3,line=1,paste("6th National Report",nrow(test_6th), "Countries"),cex=2)
 mtext(side=1,line=0.5, "number of reports",outer=T,cex=2)
 barplot(t(cbind(dec6b,inc6b,noc6b)),names=rep("",15),horiz=T,las=2,xlim=c(0,40),col=c("red","blue","yellow"),legend=c("Decreasing","Increasing","No Change"),cex.axis=1)
dev.off()
  
pdf("plots_indicators_qual_quant_w_other.pdf",height=10, width=21)
par(mfrow=c(1,2),mar=c(2,1,2,1),oma=c(2,18,1,0),cex=1.6)
 barplot(t(cbind(quant3a[1:11],qual3a[1:11])),names=answers3,horiz=T,las=1,col=c("black","grey"),xlim=c(0,40),cex.names=1.1,cex.axis=1.1) #legend=c("Quantitative","Qualitative")
 mtext(side=1,line=0.5, "number of reports",outer=T,cex=2)
 barplot(t(cbind(quant6a[1:11],qual6a[1:11])),names=rep("",11),horiz=T,las=1,col=c("black","grey"),xlim=c(0,40),cex.names=1.1,cex.axis=1.1)
 legend(20,12, c("Qualitative","Quantitative"),col=c('grey','black'),cex=1.3,lty=2:2,lwd=22)
dev.off()

 pdf("plots_indicators_inc_dec.pdf",height=10, width=21)
par(mfrow=c(1,2),mar=c(2,1,2,1),oma=c(2,18,1,0),cex=1.6)
 barplot(t(cbind(dec3b,inc3b,noc3b)),names=answers3,horiz=T,las=1,xlim=c(0,40),col=c("red","blue","yellow"),legend=c("Decreasing","Increasing","No Change"),cex.axis=1.1,cex.names=1.1)
 mtext(side=3,line=1,paste("5th National Report (left), 6th National Report (right), ",nrow(test_5th), "Countries"),cex=2)
 mtext(side=1,line=0.5, "number of reports",outer=T,cex=2)
 barplot(t(cbind(dec6b,inc6b,noc6b)),names=rep("",15),horiz=T,las=1,xlim=c(0,40),col=c("red","blue","yellow"),legend=c("Decreasing","Increasing","No Change"),cex.names=1.1,cex.axis=1.1)
dev.off()
  
 #COULD WORK ON: Count proportion increasing and decreasing across all indicators for 5th and 6th report.  
 
#Determine if categories used differ between time periods... NOT SIGNIF p=0.12... but not clear what
 chisq.test(cbind(apply(test_5th[,c(13:17,19:28)],2,function(x) sum(grepl("Q",x))), (apply(test_6th[,c(52:56,58:67)],2,function(x) sum(grepl("Q",x))))))
 
 #socioeconomic
 s<-country_sets
  #making each result into array with first array slot being total and each other array slot for each socioeconomic class (H, M, L)
	fisher.test(cbind(apply(test_5th[s[[2]],c(13:17,19:28)],2,function(x) sum(grepl("Q",x))), 
		apply(test_5th[s[[3]],c(13:17,19:28)],2,function(x) sum(grepl("Q",x))),
		apply(test_5th[s[[4]],c(13:17,19:28)],2,function(x) sum(grepl("Q",x)))),simulate.p.value=T)
	fisher.test(cbind(apply(test_6th[s[[2]],c(52:56,58:67)],2,function(x) sum(grepl("Q",x))), 
		apply(test_6th[s[[3]],c(52:56,58:67)],2,function(x) sum(grepl("Q",x))),
		apply(test_6th[s[[4]],c(52:56,58:67)],2,function(x) sum(grepl("Q",x)))),simulate.p.value=T)
	fisher.test(b,simulate.p.val=50000) 	#socioecon p=0.74... NOT SIGNIF; continent, 0.7016 and 0.8116
		
 #Get percentages for each report separately and a mean ... first function takes a mean across the 5th and 6th reports
 sort(round(rowMeans(cbind((apply(test_5th[,c(13:17,19:28)],2,function(x) sum(grepl("Q",x)))),
		(apply(test_6th[,c(52:56,58:67)],2,function(x) sum(grepl("Q",x))))))/57,2))
  write.csv(round(cbind(rowMeans(cbind((apply(test_5th[,c(13:17,19:28)],2,function(x) sum(grepl("Q",x)))),
			(apply(test_6th[,c(52:56,58:67)],2,function(x) sum(grepl("Q",x)))))),
		apply(test_6th[,c(52:56,58:67)],2,function(x) sum(grepl("Q",x))), 
		apply(test_5th[,c(13:17,19:28)],2,function(x) sum(grepl("Q",x))))
		/57,3),
		file="status_indicators.csv")
 
 #NO SIGNIF DIFF between 5th and 6th reports in number of mentions in terms of the STATUS; p=0.170
 wilcox.test((apply(test_5th[,c(13:17,19:28)],2,function(x) sum(grepl("Q",x)))),
		(apply(test_6th[,c(52:56,58:67)],2,function(x) sum(grepl("Q",x)))),paired=T)
	#LEVENE
	a<-as.data.frame(rbind(cbind(apply(test_5th[,c(13:17,19:28)],2,function(x) sum(grepl("Q",x))),
		rep("5th",15)),
		cbind(apply(test_6th[,c(52:56,58:67)],2,function(x) sum(grepl("Q",x))),
		rep("6th",15))))
	colnames(a)<-c("count","group")
	leveneTest(as.numeric(count)~group,a)	# p val 0.3243
	
#Determine if the number of increasing, decreasing changes per time period NO SIGNIF DIFF p=0.253
 chisq.test(matrix(c(sum(inc6b),sum(dec6b),sum(noc6b),sum(inc3b),sum(dec3b),sum(noc3b)),nrow=3,ncol=2))

 #Number of mentions in terms of the TREND- there were more trends reported in the 5th report
 wilcox.test(inc6b+dec6b+noc6b,inc3b+dec3b+noc3b,paired=T) 	#p=0.106  MARGIN SIGNIF
 sum(inc6b+dec6b+noc6b); sum(inc3b+dec3b+noc3b)
 
#how often reported status vs. trend... 224 VS 112.. NO TEST
sum(apply(test_6th[,c(52:56,58:67)],2,function(x) sum(grepl("Q",x))),apply(test_5th[,c(13:17,19:28)],2,function(x) sum(grepl("Q",x))))
sum(inc6b,dec6b,noc6b,inc3b,dec3b,noc3b) 


 

#####################################
#									#
#		Gen Div USES/IMPORTANCE		#
#									#
#####################################
 
 importance_res<-array(dim=c(10,2,4))
for (i in 1:4){
#making each result into array with first array slot being total and each other array slot for each socioeconomic class (H, M, L)
#s is the subset of countries in that class
s<-country_sets[[i]]
importance_res[,,i]<-matrix(c
 (sum(grepl("drought",test_5th[s,10])), sum(grepl("drought",test_6th[s,134])), #resilience such as to climate, drought, flooding, pollution, or new environmental pressures 
 sum(grepl("input",test_5th[s,10])), sum(grepl("input",test_6th[s,134])), # reducing “inputs” for production (such as for species in agriculture, forestry, fisheries, etc) i.e. pesticide, fertilizer 
 sum(grepl("output",test_5th[s,10])), sum(grepl("output",test_6th[s,134])), #increased productivity (e.g. output/ volume/ yield of forestry, fisheries, agriculture, etc.) 
 sum(grepl("creating",test_5th[s,10])), sum(grepl("creating",test_6th[s,134])), #other use of genetic diversity including creating or introducing new varieties 
 sum(grepl("inbreeding",test_5th[s,10])), sum(grepl("inbreeding",test_6th[s,134])), #avoid inbreeding in populations (wild or domestic) 
 sum(grepl("wild",test_5th[s,10])), sum(grepl("wild",test_6th[s,134])), #for adaptation e.g., via natural selection under changing environments 
 sum(grepl("stability",test_5th[s,10])), sum(grepl("stability",test_6th[s,134])), #maintain ecosystem stability or services, or human well being
 sum(grepl("general way",test_5th[s,10])), sum(grepl("general way",test_6th[s,134])), #resilience in a general way
 sum(grepl("no specifics",test_5th[s,10])), sum(grepl("no specifics",test_6th[s,134])), #stated as important but no specifics
 sum(grepl("Other:",test_5th[s,10])), sum(grepl("Other:",test_6th[s,134])) #Other:
),byrow=T, ncol=2)
}
rownames(importance_res)<-c("resilience to climate, drought, other env. change", "reduce “inputs” for production", "increase productivity (forest, fishery, agriculture) etc.", "other direct use e.g. new varieties ", "avoid inbreeding in populations", "adaptation via natural selection in changing environments", "ecosystem stability, services, or human well being", "resilience in a general way", "important but no specifics", "Other")
colnames(importance_res)<-c("5th report","6th report")
 
  write.csv(importance_res[,,1],file="importance_res.csv")
  
 pdf("importance.pdf",width=17,height=12)
 par(oma=c(2,29,1,0),cex=1.6)
 barplot( t(importance_res[,,1]),horiz=T,las=1,cex.axis=1.3,cex.names=1.4,beside=T,legend=c("5th Report", "6th Report"), col=c("purple","brown"),xlim=c(0,20))
 dev.off()
#                                                         5th report 6th report
#resilience to climate, drought, other env. change                 21         13
#reduce “inputs” for production                                     7          4
#increase productivity (forest, fishery, agriculture) etc.         21         13
#other direct use e.g. new varieties                               15         13
#avoid inbreeding in populations                                    4          4
#adaptation via natural selection in changing environments         15         11
#ecosystem stability, services, or human well being                 9          4
#resilience in a general way                                        5          7
#important but no specifics                                        10         11
#Other                                                              0          0

 #percentage per category for writing about in the text
 round(cbind(importance_res[,,1])/57,2)
  
 #Difference between the reports- the two time periods
 fisher.test(importance_res[,,1]) #no significant difference among categories among years, p=0.88
 #shapiro test indicates normality here
 t.test(importance_res[,1,1],importance_res[,2,1],paired=T)		#SIGNIG INCREASE in number, p=0.04, about 34% more in 5th report
 t.test(importance_res[-c(10),1,1],importance_res[-c(10),2,1],paired=T) 	#removing other- same
 	#LEVENE
	a<-as.data.frame(rbind(cbind(importance_res[,1,1],rep("5th",12)),
						cbind(importance_res[,2,1],rep("6th",12))))
	colnames(a)<-c("count","group")
	leveneTest(as.numeric(count)~group,a)		# p val 0.8154

#socioeconomic- NOT SIGNIF
 fisher.test(importance_res[-c(10),1,2:4],simulate.p.val=T);   fisher.test(importance_res[-c(10),2,2:4],simulate.p.val=T)
fisher.test(importance_res[-c(10),1,2:4]+importance_res[-c(10),2,2:4],simulate.p.val=T) #p=0.12 socioecon; 0.5747 continent

 
#####################################
#									#
#		Gen Div ACTIONS				#
#		GEN/SPEC + TIMING			#
#									#
#####################################
	
#Are there Actions planned regarding genetic diversity or Actions taken regarding genetic diversity, in or by this country?
#e.g. What are the actions/ measures taken regarding genetic diversity (conservation)?

#IMPORTANT: In this section each potential action is a column in the spreadsheet
#So we use apply() along columns and determine if the key word e.g. "General" or "Specific" appear in the cell, and sum that

action_txt_list<-c("Seed/ gene/ tissue bank", "Breeding or research agency", "Funding for gen. conservation", "In situ gen. conservation project", "Law or policy", "Building networks or data capacity", "Training/ education on gen. diversity", "Single time point gen. studies", "Gen. monitoring over time", "Other", "No specific action", "Not clear")

	#############################
	#		ACTION + GEN/SPEC	#
	#############################
	
######- Count which actions appear and if they are SPECIFIC VS GENERAL -############

 action_5th_res<-array(dim=c(12,2,4))
for (i in 1:4){
#making each result into array with first array slot being total and each other array slot for each socioeconomic class (H, M, L)
#s is the subset of countries in that class
s<-country_sets[[i]]
action_5th_res[,1,i]<-as.matrix(apply(test_5th[s,84:95],2,function(x) sum(grepl("General",x))))	#first get general, put in col 1
rownames(action_5th_res)<-action_txt_list
action_5th_res[,2,i]<-as.matrix(apply(test_5th[s,84:95],2,function(x) sum(grepl("Specific",x))))	#Spec = col 2
}

#socioeconomic- the matrices 2 through 4 are the H, M, L country categories
#NOT SIGNIF socioecon p=0.66, p=0.9065 continent 
fisher.test(sapply(2:4,function(i) rowSums(action_5th_res[,,i])),simulate.p.value=T)


action_6th_res<-array(dim=c(12,2,4))
for (i in 1:4){
#making each result into array with first array slot being total and each other array slot for each socioeconomic class (H, M, L)
#s is the subset of countries in that class
s<-country_sets[[i]]
action_6th_res[,1,i]<-apply(test_6th[s,14:25],2,function(x) sum(grepl("General",x))) 	#first get general, put in col 1
rownames(action_6th_res)<-action_txt_list
rownames(action_5th_res)<-action_txt_list
action_6th_res[,2,i]<-as.matrix(apply(test_6th[s,14:25],2,function(x) sum(grepl("Specific",x))))	#Spec = col 2
}

#socioeconomic- NOT SIGNIF
fisher.test(sapply(2:4,function(i) rowSums(action_6th_res[,,i])),simulate.p.value=T)	
#p=0.99 socioecon; 0.8821 continent

action_add_res<-action_5th_res+action_6th_res
fisher.test(sapply(2:4,function(i) rowSums(action_add_res[,,i])),simulate.p.value=T,B=50000) #p=0.76 socioecon , 0.7054 continent

 colnames(action_6th_res)=c("6Gen","6Spec")
 colnames(action_5th_res)=c("5Gen","5Spec")
write.csv(cbind(action_5th_res[,,1],action_6th_res[,,1]),file="action_res.csv")


#Adding "other" responses- Just uncomment these lines and the name of the pdf, and run
#action_5th_res[,1,1]<-action_5th_res[,1,1]+c(1,0,0,2,0,1,1,1,0,0,1,0)
#action_5th_res[,2,1]<-action_5th_res[,2,1]+c(0,0,0,0,0,0,0,1,0,0,0,0)
#Adding "other" responses
#action_6th_res[,1,1]<-action_6th_res[,1,1]+c(2,2,1,3,1,1,0,1,0,0,0,0)
#action_6th_res[,2,1]<-action_6th_res[,2,1]+c(0,0,0,1,0,3,0,2,0,0,0,0)
#pdf("plots_actions_w_other.pdf",height=10, width=20)

pdf("plots_actions.pdf",height=10, width=20)
par(mfrow=c(1,2),oma=c(3,25,0,1))
barplot(t(action_5th_res[1:9,,1]),horiz=T,beside=F,las=1,x.axt="n",cex.names=2,cex.axis=2,xlim=c(0,50))
axis(1,las=1,cex.axis=2)
#mtext(side=3,line=1,paste("5th National Report",nrow(test_5th), "Countries"),cex=2)
barplot(t(action_6th_res[1:9,,1]),horiz=T,beside=F,las=1,names=rep("",9),cex.axis=2,xlim=c(0,50))
axis(1,las=1,cex.axis=2)
 legend(26,11, c("General","Specific"),col=c('black','grey'),cex=1.8,lty=2:2,lwd=22)
#mtext(side=3,line=1,paste("6th National Report",nrow(test_6th), "Countries"),cex=2)
mtext(side=1,line=0.5, "number of reports",outer=T,cex=2)
dev.off()

 #Percentage of Specific, General or Both for writing about in the text
 round(rowMeans(cbind((apply(test_5th[,84:95],2,function(x) sum(grepl("e",x)))),
		(apply(test_6th[,14:25],2,function(x) sum(grepl("e",x))))))/57,2)
 
 #Difference between the reports in proportions in each category of action- the two time periods
 chisq.test(cbind(rowSums(action_5th_res[,,1]),rowSums(action_6th_res[,,1]))) #no signif diff among categories among years, p=0.347
 chisq.test(cbind(action_5th_res[,1,1],action_6th_res[,1,1]))	#No diff among categories/ yrs looking only @ general, p=0.858
 chisq.test(cbind(action_5th_res[,2,1],action_6th_res[,2,1]))	#No diff among categories/ yrs looking only @ specific, p=0.327
 chisq.test(cbind(action_5th_res[,1,1],action_5th_res[,2,1])) #no signif diff among specific and planned WITHIN 5th, p=0.142
 chisq.test(cbind(action_6th_res[,1,1],action_6th_res[,2,1])) #no signif diff among specific and planned WITHIN 6th, p=0.440
 
 #Difference between the two report periods in AMOUNT of actions overall
 #shapiro test indicates normality here
 t.test(rowSums(action_5th_res[,,1]),rowSums(action_6th_res[,,1]),paired=T)				#p=0.111
 colSums(cbind(rowSums(action_5th_res[,,1]),rowSums(action_6th_res[,,1])))/12			#19.9/17.08 or 17% more in the 6th
 #Remove other, not specific and not clear
 t.test(rowSums(action_5th_res[1:9,,1]),rowSums(action_6th_res[1:9,,1]),paired=T) 	#p=0.139
 colSums(cbind(rowSums(action_5th_res[1:9,,1]),rowSums(action_6th_res[1:9,,1])))/9		#17%
	#LEVENE
	a<-as.data.frame(rbind(cbind(rowSums(action_5th_res[1:9,,1]),rep("5th",9)),
						cbind(rowSums(action_6th_res[1:9,,1]),rep("6th",9))))
	colnames(a)<-c("count","group")
	leveneTest(as.numeric(count)~group,a)		# p val 0.7303
	
	#####################################################
	#		ACTION + TIMING IMP/PLAN- not for pub		#
	#####################################################
	
######- Count which actions appear and if they are TIMING ALREADY IMPLEMENTED OR PLANNED -############

action_5th_time<-apply(test_5th[,99:110],2,function(x) sum(grepl("Planned",x)))
action_5th_time<-as.matrix(action_5th_time)
rownames(action_5th_time)<-action_txt_list
action_5th_time<-cbind(action_5th_time,as.matrix(apply(test_5th[,99:110],2,function(x) sum(grepl("Already",x)))))
action_5th_time<-cbind(action_5th_time,as.matrix(apply(test_5th[,99:110],2,function(x) sum(grepl("Unclear",x)))))

action_6th_time<-apply(test_6th[,29:40],2,function(x) sum(grepl("Planned",x)))
action_6th_time<-as.matrix(action_6th_time)
rownames(action_6th_time)<-action_txt_list
action_6th_time<-cbind(action_6th_time,as.matrix(apply(test_6th[,29:40],2,function(x) sum(grepl("Already",x)))))
action_6th_time<-cbind(action_6th_time,as.matrix(apply(test_6th[,29:40],2,function(x) sum(grepl("Unclear",x)))))

pdf("plots_actions_time.pdf",height=10, width=25)
par(mfrow=c(1,2),oma=c(2,45,1,1))
barplot(t(action_5th_time),horiz=T,beside=F,las=2,cex.names=1.6,cex.axis=1.6,xlim=c(0,50),legend=c("Planned","Already Implemented", "Unclear"))
mtext(side=3,line=1,paste("5th National Report",nrow(test_5th), "Countries"),cex=2)
barplot(t(action_6th_time),horiz=T,beside=F,las=2,names=rep("",12),cex.axis=1.7,xlim=c(0,50),legend=c("Planned","Already Implemented", "Unclear"))
mtext(side=3,line=1,paste("6th National Report",nrow(test_6th), "Countries"),cex=2)
mtext(side=1,line=0.5, "number of reports",outer=T,cex=2)
dev.off()

#NONE SIGNIF
chisq.test(cbind(action_5th_time[,1:2],action_6th_time[,1:2])) #no signif diff among categories, plan vs. implemented p=0.53
chisq.test(cbind(action_5th_time[,1],action_6th_time[,1]))
chisq.test(cbind(action_5th_time[,2],action_6th_time[,2]))
chisq.test(cbind(action_5th_time[,1],action_6th_time[,2]))



	#########################################
	#		THREATS to Gen Diversity		#
	#########################################

#What genetic threats/ pressures/ drivers are reported- according to the report authors (potential or actual, measured or not)?
 threats_res<-array(dim=c(12,2,4))
for (i in 1:4){
#making each result into array with first array slot being total and each other array slot for each socioeconomic class (H, M, L)
#s is the subset of countries in that class
s<-country_sets[[i]]
threats_res[,,i]<-matrix(c
 (sum(grepl("connectiv",test_5th[s,73])), sum(grepl("connectiv",test_6th[s,129])), #Habitat fragmentation, loss of connectivity/ gene flow, etc
 sum(grepl("geographic",test_5th[s,73])), sum(grepl("geographic",test_6th[s,129])), # Decrease in geographic range size or decrease in habitat area, including habitat degradation
 sum(grepl("Overharvest",test_5th[s,73])), sum(grepl("Overharvest",test_6th[s,129])), #overharvest
 sum(grepl("Hybrid",test_5th[s,73])), sum(grepl("Hybrid",test_6th[s,129])), #Hybridization
 sum(grepl("Small pop",test_5th[s,73])), sum(grepl("Small pop",test_6th[s,129])), #Small population problems (inbreeding, low diversity, self incompatibility)
 sum(grepl("Pests",test_5th[s,73])), sum(grepl("Pests",test_6th[s,129])), #Pests, pathogens or other non-natives 
 sum(grepl("edge",test_5th[s,73])), sum(grepl("edge",test_6th[s,129])), #Range edge populations 
 sum(grepl("Climate",test_5th[s,73])), sum(grepl("Climate",test_6th[s,129])), #Climate change
 sum(grepl("Replacement",test_5th[s,73])), sum(grepl("Replacement",test_6th[s,129])), #Replacement of traditional/ native varieties or breeds
 sum(grepl("general phras",test_5th[s,73])), sum(grepl("general phras",test_6th[s,129])), #Some general phrasing such as “genetic threats are increasing” 
 sum(grepl("Genetically",test_5th[s,73])), sum(grepl("Genetically",test_6th[s,129])), #Genetically modified organisms
 sum(grepl("Other:",test_5th[s,73])), sum(grepl("Other:",test_6th[s,129])) #Other:
),byrow=T, ncol=2)
}
 rownames(threats_res)<-c("Habitat fragmentation, less gene flow, etc", "Decreased range size/ habitat area/ quality", "Overharvest", "Hybridization", "Small population problems (inbreeding, low diversity, etc.)", "Pests, pathogens or other non-natives", "Range edge populations ", "Climate change", "Replacement of traditional varieties or breeds", "General phrase ", "Genetically modified organisms", "Other")
colnames(threats_res)<-c("5th report","6th report")

 write.csv(threats_res,file="threats_res.csv")
 
 #adding "other" responses, just comment out these lines and re run the PDF with filename "with_other"
 #threats_res[,1,1]<-threats_res[,1,1]+c(0,2,1,0,0,0,0,1,4,1,0,2)
 #threats_res[,2,1]<-threats_res[,2,1]+c(0,1,1,0,1,0,0,1,0,2,1,1)
 
 pdf("threats.pdf",width=17,height=12)
 par(oma=c(2,29,1,0),cex=1.6)
 barplot( t(threats_res[1:11,,1]),horiz=T,las=1,cex.names=1.4,cex.axis=1.4,beside=T,legend=c("5th Report", "6th Report"), col=c("purple","brown"),xlim=c(0,20))
 dev.off()

 #percentage per category for explaining in the text
 sort(round(rowMeans(threats_res[,,1])/57,2)) 
 
 #Difference between the reports in categories- the two time periods
 fisher.test(threats_res[,,1],simulate.p.value=T) #NO SIGNIF DIFF among categories among years
 #Difference between the reports in number of mentions- the two time periods
 #shapiro test indicates normality here
 t.test(threats_res[,1,1],threats_res[,2,1],paired=T) #p=0.02
		#average number of fewer mentions in 6th report .. about 46% more in 5th report
		colSums(cbind(threats_res[,1,1],threats_res[,2,1]))/11
	#LEVENE
	a<-as.data.frame(rbind(cbind(threats_res[,1,1],rep("5th",12)),
						cbind(threats_res[,2,1],rep("6th",12))))
	colnames(a)<-c("count","group")
	leveneTest(as.numeric(count)~group,a)		# p val 0.4193
	
#socioeconomic
 fisher.test(threats_res[,1,2:4],simulate.p.val=T);   fisher.test(threats_res[,2,2:4],simulate.p.val=T)
 #p=0.3418, 1959 continent 
#socioec- pooled SIGNIF DIFF
 fisher.test(threats_res[,1,2:4]+threats_res[,2,2:4],simulate.p.val=T,B=50000)	#p=0.04
#middle and low income less small population size and less habitat fragm, but more replacement traditional var
#p=0.1267 continent 

		
 
#####################################
#									#
#		types of species			#
#		SPECIES TABLES				#
#									#
#####################################
 
setwd("C:/Users/shoban.DESKTOP-DLPV5IJ/Dropbox/Projects/IN_PROGRESS/CBD_National_Rep/Complete_Species_reviews_New_Here/")
library(readxl)

 # species_types<-c("Farmed/ domesticated animals (livestock, birds, fish)", "Wild relatives of domesticated animals", "Crops (cultivated/ domesticated, not necessarily improved)", "Crop wild relatives", "Forestry/ logging species (natural populations or plantations)", "Horticultural or ornamental species", "Species of conservation importance or concern", "Culturally important species", "Species providing ecosystem services", "Wild animals of direct harvest value (e.g. hunting)", "Wild plant species collected for use")
  species_types<-c("Farmed animals", "Wild relatives of domesticated animals", "Crops", "Crop wild relatives", "Forestry/ logging species", "Horticultural species", "Species of conservation concern", "Culturally important species", "Species providing ecosystem services", "Wild harvested animals", "Wild harvested plants")

 sp_tables_5th<-list.files(pattern="5th")
 table(substr(sp_tables_5th,1,4))	#Waiting on Andorra right now socioeconomic is one longer than file list
 
 #Also make into an array
 sp_5th_total<-array(0,dim=c(11,6,4))
for (i in 1:4){
	#making each result into array with first array slot being total and each other array slot for each socioeconomic class (H, M, L)
	#s is the subset of countries in that class
	s<-country_sets[[i]]
		 
	 for (f in s){
	  a<-read_excel(sp_tables_5th[f]);	a<-a[-c(5,8),1:6]
	  sp_5th_total[,,i]<-sp_5th_total[,,i]+apply(a, c(1,2), function(x) grepl("E", x))
	  } 
  }
  
#socioeconomic - NOT SIGNIF p=0.27
fisher.test(sapply(2:4,function(i) rowSums(sp_5th_total[,,i])),simulate.p.value=T)
#continent p=0.00005

  pdf("species_tables.pdf",width=18,height=9)
par(mfrow=c(1,2),mar=c(2,1,1,1),oma=c(3,19,1,1),cex=1.3)
barplot(  t(sp_5th_total[,,1])[2:5,],horiz=T,names=species_types,las=1,cex.names=1.3,cex.axis=1.4,col=c("brown","orange","tan","coral"),xlim=c(0,100))
  #,main=paste("Species types mentioned in",length(sp_tables_5th),"5th Reports")
  
  sp_tables_6th<-list.files(pattern="6th")
  table(substr(sp_tables_6th,1,4))
  
 sp_6th_total<-array(0,dim=c(11,6,4))
for (i in 1:4){
	#making each result into array with first array slot being total and each other array slot for each socioeconomic class (H, M, L)
	#s is the subset of countries in that class
	s<-country_sets[[i]]
		 
	 for (f in s){
	  a<-read_excel(sp_tables_6th[f]);	a<-a[-c(5,8),1:6]
	  if ( dim(a)[1]==11) sp_6th_total[,,i]<-sp_6th_total[,,i]+apply(a, c(1,2), function(x) grepl("E", x))
  }
  }
  
  write.csv(sp_5th_total[,,1],file="sp_5th_total.csv")
  write.csv(sp_6th_total[,,1],file="sp_6th_total.csv")

#socioeconomic 
fisher.test(sapply(2:4,function(i) rowSums(sp_6th_total[,,i])),simulate.p.value=T)
#continent p=0.9245
#pooled SIGNIF DIFF
sp_add_total<-sp_6th_total+sp_5th_total
fisher.test(sapply(2:4,function(i) rowSums(sp_add_total[,,i])),simulate.p.value=T,B=100000)
#marginal p=0.081, more hort, less cons concern and less ecos service
#continent 0.02589

barplot(  t(sp_6th_total[,,1])[2:5,],horiz=T,las=1,cex.names=1.3,cex.axis=1.4,col=c("brown","orange","tan","coral"),names=rep("",11),xlim=c(0,100))
#main=paste("Species types mentioned in",length(sp_tables_6th),"6th Report"),
   mtext(side=1,line=1.5, "number of reports",outer=T,cex=2)
 legend(60,12, c("threat","status","change","action"),col=c("brown","orange","tan","coral"),cex=1.3,lty=2:2,lwd=22)
  
  dev.off()
  
  #Test if categories (types of mentions) differ within a report and totals between reports 
  chisq.test(rbind(rowSums(sp_5th_total[,2:5,1]),rowSums(sp_6th_total[,2:5,1])))	#p=0.119
	chisq.test(sp_5th_total[,2:5,1])	#p=0.898
	chisq.test(sp_6th_total[,2:5,1])	#p=.963
	#number of reports overall 
  wilcox.test(rowSums(sp_5th_total[,2:5,1]),rowSums(sp_6th_total[,2:5,1]),paired=T) #p=0.256... about 17% more in the 5th report
  #make table of percentages
   t(round(rbind(rowSums(sp_5th_total[,2:5,1])/353,rowSums(sp_6th_total[,2:5,1])/302),3))
	#LEVENE
	a<-as.data.frame(rbind(cbind(rowSums(sp_5th_total[,2:5,1]),rep("5th",12)),
						cbind(rowSums(sp_6th_total[,2:5,1]),rep("6th",12))))
	colnames(a)<-c("count","group")
	leveneTest(as.numeric(count)~group,a)		# p val 0.3859
	
  
  
  #####################################
#									#
#		OBSTACLES/ NEEDS			#
#									#
#####################################


  #obstalces/ nceeds diff in number of countries per report time period
 (fisher.test(matrix(c(19,(57-19),31,(57-31)),nrow=2,ncol=2)))... p = 0.037
 #obstacles/ needs diff in proportions of categores per report time period
 fisher.test(matrix(c(13,9,3,13,5,0,47,15,8,11,11,8),nrow=2,byrow=T)
 #socio economic 6th
 fisher.test(matrix(c(9,23-9,8,15-8,14,19-14),nrow=2,byrow=F)) #p=0.08
 fisher.test(matrix(c(9+8,23-9+15-8,14,19-14),nrow=2,byrow=F)) #p=0.05
 #socio economic 5th
 fisher.test(matrix(c(3,23-3,7,14-7,9,20-9),nrow=2,byrow=F)) #p=0.02
 fisher.test(matrix(c(3+7,23-3+14-7,9,20-9),nrow=2,byrow=F)) #p=0.24
 
 
  
#####################################
#									#
#		Other- GMO, indigenous,		#
#		SDG/MDG, GSPC				#
#									#
#####################################
  
 
 #SDG/ MDG- mention
 table(test_5th[,118])
 table(test_6th[,112])
 fisher.test(rbind(table(test_5th[,118]),  table(test_6th[,112])))		p=0.0003
 
 #GMO- negative, neutral or positive
 table(test_5th[,128])
 table(test_6th[,132])
  fisher.test(table(test_5th[,128]),table(test_6th[,132]))
  
 #GSPC progress on two targets
 #target 5
 table(test_6th[,116])
 #target 9
 table(test_6th[,118])
   fisher.test(table(test_6th[,116]),  table(test_6th[,118]))
 
 #indigenous and local knowledge- mention
 table(test_5th[,130])
 table(test_6th[,122])
fisher.test(rbind(table(test_6th[,122]), table(test_5th[,130])))


#####################################
#									#
#		WORD Count					#
#		General checks				#
#									#
#####################################

#word count
word_count<-read.csv("word_count_reports.csv")
#is one longer? Yes
t.test(word_count[,4],word_count[,8],paired=T)	#p=0.010
#How much longer 
mean(word_count[,8])/mean(word_count[,4])	# 1.240428
#Is there a correlation between length of 5th and 6th (corr better than regression probably)
plot(cbind(word_count[,4],word_count[,8]))
summary(lm(word_count[,4]~word_count[,8]))
abline(lm(word_count[,4]~word_count[,8]))
cor(word_count[,4],word_count[,8])	#	0.5619043

#Correlation with socio economic?
summary(aov(word_count[,8]~word_count[,10])); summary(aov(word_count[,4]~word_count[,10]))
boxplot((word_count[,4]~word_count[,10]))
boxplot((word_count[,8]~word_count[,10]))