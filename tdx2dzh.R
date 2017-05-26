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
  print("开始同步")
  #初始化
  dzhraw<-c(0xa6, 0x00, 0x51, 0xff, 0x01)
  dzhsh<-c(0x53, 0x48)
  dzhsz<-c(0x53, 0x5a)
  dzhfg<-c(0x00, 0x00, 0x00, 0x00, 0xd6, 0x2e, 0x25, 0x59)
  
  #提取通达信数据
  n<-unlist(strsplit(read_file(tdx),"\r\n"))
  lapply(n,function(i,r=dzhraw){
    iraw<-charToRaw(i)
    print(iraw[1])
    if(iraw[1] %in% c(31,30)){
    if (iraw[1] == 0x31) qz<-dzhsh 
    if (iraw[1] == 0x30) qz<-dzhsz
    dzhraw<<-c(r,c(qz,tail(iraw,-1),dzhfg))
    }
  })
  write_file(as.raw(dzhraw),dzh)
  print(dzhraw)
  print("同步成功")
}

tb<-function(tdx,dzh,m=0){
  if (file.info(tdx)$mtime != m) {
    tdx2dzh(tdx,dzh) 
  }
}



###################################
#需要同步的文件
tdxf1<-'D:/new_tdx/T0002/blocknew/ZX.blk'
tdxf2<-'D:/new_tdx/T0002/blocknew/ZDX.blk'
tdxf3<-'D:/new_tdx/T0002/blocknew/DX.blk'

#大智慧文件
dzhf1<-"D:/dzh365/USERDATA/block/自选股.BLK"
dzhf2<-"D:/dzh365/USERDATA/block/自选股1.BLK"
dzhf3<-"D:/dzh365/USERDATA/block/自选股2.BLK"

tb(tdxf1,dzhf1)
tb(tdxf2,dzhf2)
tb(tdxf3,dzhf3)

###############实时自动同步################
# m1<-file.info(txdf1)$mtime
# m2<-file.info(txdf2)$mtime
# m3<-file.info(txdf3)$mtime
# repeat{
#   Sys.sleep(60)
#   tb(txdf1,dzhf2,m1)
#   tb(txdf2,dzhf2,m2)
#   tb(txdf3,dzhf3,m3)
# }
