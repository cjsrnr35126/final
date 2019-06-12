data.h=read.csv("C:/Users/little2/Desktop/h.csv",header = T)
head(data.h)
data.kp=data.h[,24:35]
data.kp=data.kp[-1,]

data.kp=`colnames<-`(data.kp,c("Q1","Q2","Q3","Q3.1","Q3.1.1","Q3.2","Q4.1","Q4.2","Q4.3","Q5.1","Q5.2","Q5.3"))
attach(data.kp)

change5.f=function(x,pat1,pat2,pat3,pat4,pat5){
  x=gsub(pat1,5,x)
  x=gsub(pat2,4,x)
  x=gsub(pat3,3,x)
  x=gsub(pat4,2,x)
  x=gsub(pat5,1,x)
  return(x)
}
Q1=change5.f(Q1,"2005�� ����","2008~2009�� ����","2010~2013�� ����","2014~2017�� ����","2018�� ����")
Q2=change5.f(Q2,"�Ϸ翡 ���� ��","����","�����Ͽ� ���� ��","�Ѵ޿� ���� ��","���� ���� �ʴ´�.")
Q3=gsub("��",1,Q3);Q3=gsub("�ƴϿ�",0,Q3);Q3
Q3.1=change5.f(Q3.1,"20���� �̻�","15���� �̻� ~ 20���� �̸�","10���� �̻� ~ 15���� �̸�","5���� �̻� ~ 10���� �̸�","5���� �̸�")
Q3.1.1=gsub("4","�Ҹ���",Q3.1.1);Q3.1.1=gsub("2","����",Q3.1.1)
Q3.1.1=change5.f(Q3.1.1,"�ſ� ����","����","����","��4","�ſ� �Ҹ���")
Q3.2 #freq

Q4.1=gsub("���� �ƴϴ�.","�ſ� �׷��� �ʴ�.",Q4.1)
Q4.1=change5.f(Q4.1,"�ſ� �׷���.","�׷���.","�����̴�.","�ƴϴ�.","�ſ� �׷��� �ʴ�.")
Q4.2=gsub("���� �ƴϴ�.","�ſ� �׷��� �ʴ�.",Q4.2)
Q4.2=change5.f(Q4.2,"�ſ� �׷���.","�׷���.","�����̴�.","�ƴϴ�.","�ſ� �׷��� �ʴ�.")
Q4.3=gsub("���� �ƴϴ�.","�ſ� �׷��� �ʴ�.",Q4.3)
Q4.3=change5.f(Q4.3,"�ſ� �׷���.","�׷���.","�����̴�.","�ƴϴ�.","�ſ� �׷��� �ʴ�.")

Q5.1=gsub("���� �ƴϴ�.","�ſ� �׷��� �ʴ�.",Q5.1)
Q5.1=change5.f(Q5.1,"�ſ� �׷���.","�׷���.","�����̴�.","�ƴϴ�.","�ſ� �׷��� �ʴ�.")
Q5.2=gsub("���� �ƴϴ�.","�ſ� �׷��� �ʴ�.",Q5.2)
Q5.2=change5.f(Q5.2,"�ſ� �׷���.","�׷���.","�����̴�.","�ƴϴ�.","�ſ� �׷��� �ʴ�.")
Q5.3=gsub("���� �ƴϴ�.","�ſ� �׷��� �ʴ�.",Q5.3)
Q5.3=change5.f(Q5.3,"�ſ� �׷���.","�׷���.","�����̴�.","�ƴϴ�.","�ſ� �׷��� �ʴ�.")


score.df=data.frame(as.integer(Q1),as.integer(Q2),as.integer(Q3),as.integer(Q3.1),as.integer(Q3.1.1),as.integer(Q4.1),as.integer(Q4.2),as.integer(Q4.3),as.integer(Q5.1),as.integer(Q5.2),as.integer(Q5.3))
score.df[is.na(score.df)] <- 0

score.df[1,]
score.df$Attention=rowSums(score.df[,1:5])
score.df$Result=rowSums(score.df[,6:11])
A=score.df$Attention;R=score.df$Result
plot(A,R)
cor.test(A,R)
lm(R~A)
abline(lm(R~A))
user=c()
for (i in 1:22) {user[i]=paste0("Player",i,sep="")}

#`row.names<-.data.frame`(score.df,user)
group=c()
for (j in 1:22) {
  if(score.df[j,]$as.integer.Q1.>=3){
    if(score.df[j,]$as.integer.Q3.==1){
      group[j]="A"
    }else{
      group[j]="C"
    }
  }else{
    if(score.df[j,]$as.integer.Q3.==1){
      group[j]="B"
    }else{
      group[j]="D"
    }
  }}

group
score.df$Group=group
score.df




pv=data.h[2:4]
pv=`colnames<-`(pv,c("sex",'since','loc'))
pv$sex=gsub("����","M",pv$sex);pv$sex=gsub("����","W",pv$sex)
pv$since=gsub("2��","low",pv$since);pv$since=gsub("1��","low",pv$since)
pv$since=gsub("3��","high",pv$since);pv$since=gsub("4��","high",pv$since);pv$since=gsub("5�� �̻�","high",pv$since)


pv$sex=gsub(1,"1",pv$sex);pv$sex=gsub(0,"0",pv$sex)

total.d=cbind(pv[-1,],score.df)
total.d

install.packages("ggplot2")
library(ggplot2)
ggplot(data=total.d,aes(x=Attention,y=Result)) + geom_point(shape="*",size=10)

ggplot(data=total.d,aes(x=Attention,y=Result,colour=sex)) + geom_point(shape="*",size=10)
ggplot(data=total.d,aes(x=Attention,y=Result,colour=loc)) + geom_point(shape="*",size=10)
ggplot(data=total.d,aes(x=Attention,y=Result,colour=since)) + geom_point(shape="*",size=10)
ggplot(data=total.d,aes(x=Attention,y=Result,colour=Group)) + geom_point(shape="*",size=10)


ggplot(data=total.d,aes(x=Attention,y=Result)) + geom_point(shape="*",size=10,color="blue")+stat_smooth(method = 'lm',se=F,color="red")
summary(lm(R~A))
cor.test(A,R)
shapiro.test(A)
shapiro.test(R)

total.d$Group=as.factor(total.d$Group)
summary(aov(total.d$Result~total.d$Group))
bartlett.test(total.d$Result~total.d$Group)
