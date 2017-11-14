#'Read in Survey Data Exported In Tabular Format from Survey Solutions HQ
#'
#'Takes a zip file produced by the Data Export Menu from Survey Solutions HQ and
#'imports all data frames in the R environment, assigns factor levels and adds
#'full text for variable labels using \code{\link[Hmisc]{label}}
#'
#'@param zipfile The path of a zip file produced in the Data Export Menu for the
#'  Main Survey Data in "Tabular Format".
#'@param encoding default is "UTF-8". Other possible options are "unknown" and 
#'  "Latin-1". Note: it is not used to re-encode the input, rather enables 
#'  handling of encoded strings in their native encoding.
#'@param saveData default is FALSE. If TRUE then filename will be derived from 
#'  input file location. Can also be an explcit filename.
#'@param dupLabels default is TRUE. If TRUE, and the same label is used for 
#'  multiple codes within a factor, then the output file will not distinguish 
#'  between the same code. If FALSE then new unique labels will be created by 
#'  concatenating the label and the code.
#'@param ... Additional import options for \code{\link[data.table]{fread}}
#'@keywords SurveySolutions Import Tabular Survey
#'@return A series of data frames
#'@export
#'@examples
#'setwd("C:/Users/User")
#'#Using data downloaded from the Survey Solutions Demo Server
#'#https://demo.mysurvey.solutions
#'#Login: Headquarters1 Password: Headquarters1
#'SurveySolutions2R("Example-FP_31_Tabular_All.zip",saveData=FALSE)
#'@importFrom Hmisc label "label<-"
#'@importFrom stringr str_split_fixed str_split
#'@importFrom data.table fread
#'@importFrom utils read.table unzip

SurveySolutions2R<-function(zipfile,encoding = "UTF-8",dupLabels=TRUE,saveData=FALSE,...){
  
   filelist<-unzip(zipfile,list = TRUE)
      

dofiles<-filelist$Name[grep(".do",filelist$Name)]
datafiles<-gsub(".do",".tab",dofiles)
  
for(j in 1:length(dofiles)){

Data1<-data.frame(read.table(unz(zipfile,datafiles[j]),header=TRUE,sep="\t",fill=TRUE,
                                    encoding = encoding,stringsAsFactors = FALSE,...))


SS1<-scan(unz(zipfile,dofiles[j]),encoding = encoding,what="character",sep="\n",quiet=TRUE)
closeAllConnections()

Variables<-SS1[substr(SS1,1,14)=="label variable"]
Variables<-gsub("label variable ","",Variables)

VarNames<-trimws(gsub('\`\\"(.*?)\\\"\'',"",Variables))
VarLabels<-gsub('\`\\"|\\\"\'',"",regmatches(Variables, regexpr('\`\\"(.*?)\\\"\'', Variables)))

ValueSets<-SS1[substr(SS1,1,12)=="label values"]
ValueSets<-data.frame(str_split_fixed(ValueSets,pattern = " ",4)[,3:4])
colnames(ValueSets)<-c("VarNames","VarValues")

MetaData<-data.frame(VarNames,VarLabels,stringsAsFactors = FALSE)
MetaData<-merge(MetaData,ValueSets,all.x=T,stringsAsFactors = FALSE)

MetaData<-merge(MetaData,data.frame(VarNames=colnames(Data1),order=1:ncol(Data1),stringsAsFactors = FALSE),all=TRUE,sort = FALSE)

MetaData<-MetaData[order(MetaData$order),]
MetaData$VarValues<-as.character(MetaData$VarValues)
varlabels<-ifelse(MetaData$VarLabels==""|is.na(MetaData$VarLabels),MetaData$VarNames,MetaData$VarLabels)





FactorValues<-SS1[substr(SS1,1,12)=="label define"]
FactorValues<-as.character(gsub("label define ","",FactorValues))
FactorList<-(str_split_fixed(FactorValues," ",2))[,1]


for(i in seq_along(FactorList)){
decoded<-trimws(gsub(FactorList[i],"",str_split(FactorValues[i],'\\\"\'|\`\\\"')[[1]]))
levels1=decoded[seq(1,length(decoded)-2,by=2)]
labels1=decoded[seq(2,length(decoded)-1,by=2)]

if(dupLabels==FALSE){
labels1[labels1%in%labels1[duplicated(labels1)]]<-
  paste(labels1[labels1%in%labels1[duplicated(labels1)]]," (",
        levels1[labels1%in%labels1[duplicated(labels1)]],")",sep="")

Data1[,subset(MetaData,MetaData$VarValues==FactorList[i])$VarNames]<-factor(
  Data1[,subset(MetaData,MetaData$VarValues==FactorList[i])$VarNames],
  levels=levels1,
  labels=labels1)
}

if(dupLabels==TRUE){
  Data1[,subset(MetaData,MetaData$VarValues==FactorList[i])$VarNames]<-as.character(Data1[,subset(MetaData,MetaData$VarValues==FactorList[i])$VarNames])
  
  for(k in 1:length(levels1)){
    Data1[,subset(MetaData,MetaData$VarValues==FactorList[i])$VarNames][Data1[,subset(MetaData,MetaData$VarValues==FactorList[i])$VarNames]==levels1[k]]<-
      labels1[k]
  }
  
  Data1[,subset(MetaData,MetaData$VarValues==FactorList[i])$VarNames]<-factor(Data1[,subset(MetaData,MetaData$VarValues==FactorList[i])$VarNames],
  levels=unique(labels1))                                                                    
}

}


for(i in seq_along(Data1)){
label(Data1[, i]) <- varlabels[i]
}
pos <- 1
envir = as.environment(pos)
assign(gsub(".do","",dofiles[j]),Data1,envir=envir)
}


interview_actions<-data.frame(read.table(unz(zipfile,"interview_actions.tab"),header=TRUE,sep="\t",fill=TRUE,
                                         encoding = encoding,stringsAsFactors = FALSE,...))
interview_comments<-data.frame(read.table(unz(zipfile,"interview_comments.tab"),header=TRUE,sep="\t",fill=TRUE,
                                          encoding = encoding,stringsAsFactors = FALSE,...))

assign("interview_comments",interview_comments,envir=envir)
assign("interview_actions",interview_actions,envir=envir)

dataframelist<-c(gsub(".do","",dofiles),"interview_actions","interview_comments")

if(saveData!=FALSE){
if(saveData==TRUE){
  save(list=dataframelist,
     file = gsub(".zip",".RData",zipfile), envir = .GlobalEnv)
  cat(paste("File saved in",gsub(".zip",".RData",zipfile)),sep="\n")
}
else{
  save(list=dataframelist,
       file = saveData, envir = .GlobalEnv)
  cat(paste("File saved in",saveData,sep="\n"))
}
}
}
