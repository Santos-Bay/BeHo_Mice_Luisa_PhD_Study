tube_test_winner= read.csv("~/Documents/PhD 2020/ZiBA:BeHo 2020/Data analysis/Tube test data analysis/Tube test results of winners.csv", header = TRUE)
tube_test_winner <- tube_test_winner[,c("Name","Test.code","Test.number","Winner", "Looser", "Duration..seconds.","Cage..from.Cage.","Sex..from.Cage...from.Cage.", "Experimental.Group..from.Cage...from.Cage.")] #I select only the columns mentioned here. The rest are discarded.

## dividing the data by treatment ##
Optimal = data.frame()
Heat = data.frame()
Cold = data.frame()
Diet = data.frame()
Antibiotics = data.frame()
FMT = data.frame()

for (i in 1:nrow(tube_test_winner)){
  if (substr(tube_test_winner$Test.code[i],5,7)== "OPT"){
    Optimal= rbind(Optimal,c(tube_test_winner[i,]))
  }
  if (substr(tube_test_winner$Test.code[i],5,8)== "HEAT"){
    Heat= rbind(Heat,c(tube_test_winner[i,]))
  }
  if (substr(tube_test_winner$Test.code[i],5,8)== "COLD"){
    Cold= rbind(Cold,c(tube_test_winner[i,]))
  }
  if (substr(tube_test_winner$Test.code[i],5,8)== "DIET"){
    Diet= rbind(Diet,c(tube_test_winner[i,]))
  }
  if (substr(tube_test_winner$Test.code[i],5,8)== "ANTI"){
    Antibiotics= rbind(Antibiotics,c(tube_test_winner[i,]))
  }
  if (substr(tube_test_winner$Test.code[i],5,7)== "FMT"){
    FMT= rbind(FMT,c(tube_test_winner[i,]))
  }
}






## dividing the data by cage ##
cage5M = data.frame()
cage4M = data.frame()
cage6M = data.frame()
cage1M = data.frame()
cage12F = data.frame()
cage15F = data.frame()
cage16F = data.frame()
cage14F = data.frame()
#  c("Name","Test.code","Test.number","Winner", "Looser", "Winner.position","Duration (seconds)", "Notes", "Video file","Date"))

for (i in 1:nrow(tube_test_winner)){
  if (substr(tube_test_winner$Test.code[i],0,3)== "05M"){
    cage5M= rbind(cage5M,c(tube_test_winner[i,]))
  }
  if (substr(tube_test_winner$Test.code[i],0,3)== "06M"){
    cage6M= rbind(cage6M,c(tube_test_winner[i,]))
  }
  if (substr(tube_test_winner$Test.code[i],0,3)== "04M"){
    cage4M= rbind(cage4M,c(tube_test_winner[i,]))
  }
  if (substr(tube_test_winner$Test.code[i],0,3)== "01M"){
    cage1M= rbind(cage1M,c(tube_test_winner[i,]))
  }
}


