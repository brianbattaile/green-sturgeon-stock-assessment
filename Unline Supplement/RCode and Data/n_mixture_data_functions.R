load_data = function(selected_data){
  Count2020<-as.matrix(read.table("Count2020.csv",
                                  row.names=1,
                                  sep=",",
                                  header=TRUE))
  Count2021<-as.matrix(read.table("Count2021.csv",
                                  row.names=1,
                                  sep=",",
                                  header=TRUE))
  Count2022<-as.matrix(read.table("Count2022.csv",
                                  row.names=1,
                                  sep=",",
                                  header=TRUE))
  #2022 Data that is more "Selective"
  Count2022S<-as.matrix(read.table("Count2022S.csv",
                                   row.names=1,
                                   sep=",",
                                   header=TRUE))
  #JAGS Doesn't like row or column names...seems kinda stupid but whatever
  rownames(Count2020)<-NULL
  rownames(Count2020)<-NULL
  colnames(Count2021)<-NULL
  colnames(Count2021)<-NULL
  rownames(Count2022S)<-NULL
  rownames(Count2022)<-NULL
  rownames(Count2022S)<-NULL
  rownames(Count2022)<-NULL
 
  data = list(Count2022 = Count2022,
              Count2022S = Count2022S,
              Count2021 = Count2021,
              Count2020 = Count2020)

  data_out =data[[deparse(substitute(selected_data))]]

  return(data_out)
}

