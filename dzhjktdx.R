if (!require('readr')) install.packages('readr')
library(readr)
#预警文件头
fh<-c(0x08, 0xf9, 0x90, 0x3d, 0x04, 0x00)
#预警文件尾
ff<-c(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00)

#数字的内存表示
toraw<-function(x){
  con <- rawConnection(raw(0), "r+")
  writeBin(x, con,size=4)
  xr<-rawConnectionValue(con)
  close(con)
  return(xr)
}

cs16<-function(cs){
  n<-length(cs)
  return(c(cs,rep(0,16-n)))
}

#预警条件设置函数
yj<-function(jk,nm,zx,ou,zq,gs,cs,tsy,lj){
  x<-c(0x00, 0x00, 0x01, 0x00, 0x02)
  x<-c(x, jk)
  x<-c(x,charToRaw(nm))
  x<-c(x,c(0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
           0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 
           0x00, 0x00, 0x00, 0x00, 0x00, 0x00, zx, 
           0x00, 0x00, 0x00))
  x<-c(x,charToRaw(ou))
  x<-c(x,c(0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, zq, 0x00, 0x00, 0x00))
  x<-c(x,c(charToRaw(gs),rep(0x00,(10-length(charToRaw(gs))))))
  x<-c(x,toraw(cs))# 16位参数
  x<-c(x,c(tsy, 0x00, 0x00, 0x00))# 0x0F 勾选
  x<-c(x,charToRaw(lj))
  return(x)
}

#监控板块设置函数
jk<-function(yj,tdxblk){
  dzhsh<-c(0x53, 0x48)
  dzhsz<-c(0x53, 0x5a)
  
  hd<-rep(0x00,2)
  ed<-c(0x00, 0x8f, 0xe4, 0x15)
  w<-c()
  
  f<-unlist(strsplit(read_file(tdxblk),"\r\n"))
  
  lapply(f,function(i,r=w){
    iraw<-charToRaw(i)
    if(iraw[1] %in% c(31,30)){
      if (iraw[1] == 0x31) qz<-dzhsh 
      if (iraw[1] == 0x30) qz<-dzhsz
      w<<-c(r,c(qz,tail(iraw,-1),ed))
    }
  })
  return(c(yj,rev(toraw(c(as.hexmode(length(f)))[1:2])),w))
}
###############
tdxf1<-'D:/new_tdx/T0002/blocknew/CX.blk'
tdxf2<-'D:/new_tdx/T0002/blocknew/ZX.blk'
tdxf3<-'D:/new_tdx/T0002/blocknew/ZDX.blk'
tdxf4<-'D:/new_tdx/T0002/blocknew/DX.blk'

yj1<-yj(0x08,"DK11",0x07,"自选股5",0x11,"HY2",cs16(c(0)),0x0f,"D:/提示/000.wav")
jk1<-jk(yj1,tdxf1)

yj2<-yj(0x08,"DK",0x07,"自选股6",0x08,"HY2",cs16(c(0)),0x0f,"D:/提示/000.wav")
jk2<-jk(yj2,tdxf2)

yj3<-yj(0x08,"FK30",0x07,"自选股7",0x06,"HY2",cs16(c(0)),0x0f,"D:/提示/000.wav")
jk3<-jk(yj3,tdxf2)

yj4<-yj(0x08,"DK11",0x07,"自选股8",0x06,"HY2",cs16(c(0)),0x0f,"D:/提示/000.wav")
jk4<-jk(yj4,tdxf2)



dzhdat<-"test.dat"
write_file(as.raw(c(fh,jk1,jk2,jk3,jk4,ff)),dzhdat)
