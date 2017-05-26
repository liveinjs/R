if (!require('readr')) install.packages('readr')
library(readr)

#通达信自选股数据格式
#开头：c(0x0d, 0x0a)
#前缀：0x31，沪市，0x30，深市
#代码：6位
#分割：c(0x0d, 0x0a)

#大智慧自选股数据格式
#开头:c(0xa6, 0x00, 0x51, 0xff, 0x01)
#分割:c(0x00, 0x00, 0x00, 0x00, 0xd6, 0x2e, 0x25, 0x59)
#前缀c(0x53, 0x48),SH,SZ,C(0x53, 0x5a)

tdx2dzh<-function(tdx,dzh){
  cat(c("开始同步",tdx,"\r\n"))
  #初始化
  dzhraw<-c(0xa6, 0x00, 0x51, 0xff, 0x01)
  dzhsh<-c(0x53, 0x48)
  dzhsz<-c(0x53, 0x5a)
  dzhfg<-c(0x00, 0x00, 0x00, 0x00, 0xd6, 0x2e, 0x25, 0x59)
  
  #提取通达信数据
  n<-unlist(strsplit(read_file(tdx),"\r\n"))
  lapply(n,function(i,r=dzhraw){
    iraw<-charToRaw(i)
    if(iraw[1] %in% c(31,30)){
    if (iraw[1] == 0x31) qz<-dzhsh 
    if (iraw[1] == 0x30) qz<-dzhsz
    dzhraw<<-c(r,c(qz,tail(iraw,-1),dzhfg))
    }
  })
  write_file(as.raw(dzhraw),dzh)
  cat(paste(c("同步成功",dzh,date(),"\r\n\r\n")))
}

tb<-function(tdx,dzh,m=0){
  mt<-file.info(tdx)$mtime
  if (mt != m) {
    tdx2dzh(tdx,dzh) 
  }
  return(mt)
}



###################################
#需要同步的文件
tdxf1<-'D:/new_tdx/T0002/blocknew/CX.blk'
tdxf2<-'D:/new_tdx/T0002/blocknew/ZX.blk'
tdxf3<-'D:/new_tdx/T0002/blocknew/ZDX.blk'
tdxf4<-'D:/new_tdx/T0002/blocknew/DX.blk'

#大智慧文件
dzhf1<-"D:/dzh365/USERDATA/block/自选股.BLK"
dzhf2<-"D:/dzh365/USERDATA/block/自选股1.BLK"
dzhf3<-"D:/dzh365/USERDATA/block/自选股2.BLK"
dzhf4<-"D:/dzh365/USERDATA/block/自选股3.BLK"

m1<-0
m2<-0
m3<-0
m4<-0

repeat{
  
  m1<<-tb(tdxf1,dzhf1,m1)
  m2<<-tb(tdxf2,dzhf2,m2)
  m3<<-tb(tdxf3,dzhf3,m3)
  m4<<-tb(tdxf4,dzhf4,m4)
  Sys.sleep(10)
}
