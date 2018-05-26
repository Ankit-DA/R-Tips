datas<-read.csv("F:\\R\\Country_Parcel.csv",header=T,stringsAsFactors=FALSE)
sPDF <- joinCountryData2Map(datas,nameJoinColumn='Country',joinCode='ISO2')
options("scipen"=100, "digits"=4) # Avoid scientific notation
dev.off()
mapDevice('windows',width=10,height=4.5) # windows specific to open a window for graph output
colorgroup<-c(0,100,1000,10000,20000,50000,100000,200000,1000000,max(datas$Count))
colourPalette <- brewer.pal(9,'YlOrRd')
test<-mapCountryData(sPDF, nameColumnToPlot="Count", catMethod = colorgroup, missingCountryCol = "gray",addLegend=FALSE,colourPalette=colourPalette,oceanCol="skyblue1",borderCol="grey33",lwd=0.5)
do.call(addMapLegend,c(test,legendLabels="all"))
######################
######################
datas<-read.csv("F:\\R\\Country_Parcel.csv",header=T,stringsAsFactors=FALSE)
sPDF <- joinCountryData2Map(datas,nameJoinColumn='Country',joinCode='ISO2')
options("scipen"=100, "digits"=4) # Avoid scientific notation
dev.off()
mapDevice('windows',width=10,height=4.5) # windows specific to open a window for graph output
colorgroup.gmvk<-c(0,1,100,1000,5000,10000,50000,100000,200000,500000)
colourPalette <- brewer.pal(9,'RdPu')
test<-mapCountryData(sPDF, nameColumnToPlot="GMVk", catMethod = colorgroup.gmvk, missingCountryCol = "gray",addLegend=FALSE,colourPalette=colourPalette,oceanCol="skyblue1",borderCol="grey33",lwd=0.5,mapTitle="GMV (Total Sale) for each Destination Country by GSP Program in '000 $")
do.call(addMapLegend,c(test,legendLabels="all"))