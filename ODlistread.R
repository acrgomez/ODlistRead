require(gdata)

readListOD <- function(file,ID=NA,ext){
    
    if(ext=='xls'){
        dat = read.xls(file,skip=1,nrows=96,
                        colClasses=c(rep("character",8),rep(NULL,33)))
                        #doesn't read the empty columns that excel adds#
        dat = dat[,1:8]
    }
    
    if(ext=='txt'){
        dat=read.table(file,skip=1)
        dat$V6 <- NULL
    }
    
    wells <- c("A","B","C","D","E","F","G","H")
    dilutions <- c(100*2^(0:10),0)


    cols <- c('prep','ID', 2^(0:10)*100,0)
    rows <- wells
    temp <- matrix(NA,nrow = 8,ncol=14)
    colnames(temp) <- cols
    rownames(temp) <- rows
    temp <- as.data.frame(temp)
    
    temp$prep <- dat[1,1]
    temp$ID <- ID
    
    for(ii in 3:14) temp[,ii] <- as.numeric(dat[,7][(1+8*(ii-3)):(8+8*(ii-3))])
    
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
    
readListOD("~/Desktop/OD/Nov02_IgGU_7_list.xls",ext='xls')
readListOD("Desktop/OD/Nov02_IgG_7_list.txt",ext='txt')
