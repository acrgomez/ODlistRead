setwd("~/Desktop/OD/")
require(gdata)

readList <- function(file,ID=NA){
    
    if(is.character(file)) {
        file = read.xls(file,
                 na.strings=c("NA","#DIV/0!",""),
                 method="csv", blank.lines.skip=T)
    }
    wells <- c("A","B","C","D","E","F","G","H")
    dilutions <- c(100*2^(0:10),0)


    cols <- c('prep','ID', 2^(0:10)*100,0)
    rows <- wells
    temp <- matrix(NA,nrow = 8,ncol=14)
    colnames(temp) <- cols
    rownames(temp) <- rows
    temp <- as.data.frame(temp)
    
    temp$prep <- file$Plate[1]
    temp$ID <- ID
    
    for(ii in 3:14) temp[,ii] <- file$Abs[(1+8*(ii-3)):(8+8*(ii-3))]
    
    cutoff <- mean(temp$'0') + 4*sd(temp$'0')
    titer <- rep(NA,8)
    
    row=1
    while(row <=8){
        col=3
        while (col <= 12){
            if(temp[row,col+1]<cutoff){
                titer[row] = 2^(col-3)*100
                row=row+1
                col = 13
            }
            col= col+1
        }
    }
    
    titer[is.na(titer)]=102400
                          
    return(list(plate=temp,cutoff=cutoff,titers=titer))
}
    


wb <- loadWorkbook("Oct27_IgG_2_list.xls", create = FALSE)


File = read.xls("Nov04_IgGU_10_list.xls",skip=1,skipNul=T, nrows=99,header=T
                ,colClasses=c(rep("character",5),"numeric","numeric","numeric"))
    
aaa=read.xls("oct26IgGlist.xls")
head(File)
tail(File)    
