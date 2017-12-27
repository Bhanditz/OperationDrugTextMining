stringreal = "I feel anaemia and pain"
str2 =c("anaemia","pain")
str3 = NULL
id=grepl(str2,stringreal)
id
if (id)
{
  str3<- str2
}
str3[2]
str2[1]
stringreal[1]

for (i in 1:length(stringreal))
{
  
  for (j in 1:length(str2))
  {
    id=grepl(str2[j],stringreal[i])
    if (id)
    {
      str3[j]<- str2[j]
    }
  }
}
str3



x <- list()
x[[1]] <- c(-0.438185, -0.766791, 0.695282)
x[[2]] <- c(-0.759100, 0.034400, 0.524807)
x[1,1]
alply(as.matrix(df),1,"[")

