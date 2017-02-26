#Duration Gap  || >2 Weeks   ||  = 2 weeks   || < 2 weeks
#Same Client   || Roll Off   ||  No Roll Off || No Roll Off
#Different Client|| Roll Off ||  Roll Off    || No Roll Off

#Other Condition: One employee can work on multiple projects on the same week

#Sample.csv
#Employee_Number,Week_Number,Parent_CA
#100011,1,Chevron Corporation

#Expected Output:- Table with row = Organisation Name, column = week number 1 to 53, entry in cell = Employee_Number of the employee who rolled of from the corresponding organisation in the corresponding week
#When multiple roll offs from the same organisation in the same week -> create new row for each employee_number corresponding to the organisaton name & week number





setwd("/home/Roll Off Query")
data<-read.csv("Sample.csv")
data<-data[order(data$Employee_Number,data$Week_Number,data$Parent_CA),] 

prev_week=as.character(data$Week_Number[1])
prev_org=as.character(data$Parent_CA[1])
prev_emp_id=as.character(data$Employee_Number[1])
prev_org_li=c(prev_org)
org_li=c(prev_org)
df = data.frame(matrix(character(), 0, 54,dimnames=list(c(), c("Parent_CA", paste0("week",1:53)))),stringsAsFactors=F)
for (i in 2:nrow(data)){
  Roll_Off=0 #Considering No Roll Off
  curr_emp_id=as.character(data$Employee_Number[i])
  curr_week=as.character(data$Week_Number[i])
  curr_org=as.character(data$Parent_CA[i])
  if(prev_emp_id==curr_emp_id){
    if(prev_week!=curr_week)
    {
      prev_org_li=org_li
      org_li=c(curr_org)
    } 
  if(as.numeric(curr_week)-as.numeric(prev_week)>3){#Difference 3 means 2 weeks in between
    Roll_Off=1
    org_li=c(curr_org)
  }
  if(as.numeric(curr_week)-as.numeric(prev_week)==3){#Difference 3 means 2 weeks in between
    if (!(curr_org %in% prev_org_li)){
      Roll_Off=1
    }
    
  }
  if(Roll_Off==1)
  {
    new_r=c(as.character(prev_org),as.character(rep("",as.numeric(prev_week))),as.character(prev_emp_id),as.character(rep("",(ncol(df)-as.numeric(prev_week)-2))))
    df=rbind(df,new_r,stringsAsFactors=F)
  }
    if(prev_week==curr_week)
    {
      org_li=c(org_li,curr_org)
    }
  }
  if(prev_emp_id!=curr_emp_id){
      org_li=c(curr_org)
      new_r=c(as.character(prev_org),as.character(rep("",as.numeric(prev_week))),as.character(prev_emp_id),as.character(rep("",(ncol(df)-as.numeric(prev_week)-2))))
    df=rbind(df,new_r,stringsAsFactors=F)
  }
  
  prev_week=curr_week
  prev_org=curr_org
  prev_emp_id=curr_emp_id
}
#For the last employee
for (i in 1:length(org_li)){
  new_r=c(as.character(org_li[i]),as.character(rep("",as.numeric(prev_week))),as.character(prev_emp_id),as.character(rep("",(ncol(df)-as.numeric(prev_week)-2))))
  df=rbind(df,new_r,stringsAsFactors=F)
  
}
colnames(df)<-c("Parent_CA", paste0("week",1:53))

write.csv(file="output.csv",x=df,row.names = F)
