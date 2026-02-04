package device

import WuKong.HasCoreParameter
import WuKong.HasCoreConst
import chisel3._
import chisel3.util._
import bus.simplebus._
import utils._
import chisel3.util.experimental.BoringUtils
import device.RAMHelper
import WuKong.Backend.fu.LSU
/**
  * DTCM module
  * - Storage: 1024 sets, 8-way, 8-bit per way (total 8KB)
  * - Addr mapping: setIdx = addr[12:3]
  * - Byte-write mask preserved: SimpleBus `wmask` (8 bits) maps to SRAM waymask
  * - Interface: two `SimpleBusUC` ports (io.in(0), io.in(1)) and two `outMissmem` ports
  *
  * Arbitration / ordering policy (as requested):
  * - Priority: port0 > port1 when both issue DTCM accesses simultaneously.
  * - If both port0 and port1 are accessing DTCM (read or write) at the same time,
  *   DTCM will service port0 first and then port1. During this two-operation
  *   sequence DTCM will _not_ accept any new requests from either port (stall
  *   by deasserting `req.ready`). This enforces the strict ordering requirement.
  * - If only one port accesses DTCM, it is served normally.
  * - Requests targeting addresses outside the DTCM range are forwarded to
  *   `outMissmem(i)` for that port.
  */
class DTCM(base: Long = 0x80000000L) extends Module with HasCoreParameter {
  val io = IO(new Bundle {
    val in = Vec(2, Flipped(new SimpleBusUC(addrBits = VAddrBits)))
    val outMissmem = Vec(2, new SimpleBusUC(addrBits = VAddrBits))
  })
  val memByte = 16 * 1024 // 16KB DTCM
  // Banked SRAM instantiation
  //SRAM的初始化
  val sram = Module(new RAMHelper(memByte))
  sram.io.clk := clock
  sram.io.en := false.B
  sram.io.wen := false.B // default no write
  sram.io.wmask := 0.U
  sram.io.wdata := 0.U
  sram.io.wIdx := 0.U
  sram.io.rIdx := 0.U
  // Helpers per-port
  def inRange(addr: UInt) = addr >= base.U && addr < (base + 16 * 1024).U

  val reqValid = VecInit((0 until 2).map(i => io.in(i).req.valid))
  val reqReady = VecInit((0 until 2).map(i => io.in(i).req.ready))
  val reqInDTCM = VecInit((0 until 2).map(i => inRange(io.in(i).req.bits.addr)))
  val reqIsRead = VecInit(
                          io.in(0).req.bits.isRead(),  // 端口0可以是read
                          true.B)   // 端口1只能是read
  
  val reqIsWrite = VecInit(
                          !reqIsRead(0), // 端口0可以是write,cmd==1是write
                           false.B )                      // 端口1不能write，硬编码false
                          

  //初始化

  // default: no forward to missmem
  for (i <- 0 until 2) {
    io.outMissmem(i).req.valid := false.B
    io.outMissmem(i).req.bits.addr := 0.U
    io.outMissmem(i).req.bits.size := 0.U
    io.outMissmem(i).req.bits.cmd := 0.U
    io.outMissmem(i).req.bits.wmask := 0.U
    io.outMissmem(i).req.bits.wdata := 0.U
    io.outMissmem(i).resp.ready := true.B
    io.in(i).resp.valid := false.B
    io.in(i).resp.bits := 0.U.asTypeOf(new SimpleBusRespBundle())
  }
 //先处理miss的情况
 val InDTCM = VecInit((0 until 2).map(i => reqValid(i) && reqInDTCM(i)))
 val missDTCM = VecInit((0 until 2).map(i => reqValid(i) && !reqInDTCM(i)))
 val inDTCMstate = RegInit(VecInit(Seq.fill(2)(false.B)))
// val writeFinish = RegInit(VecInit(Seq.fill(2)(false.B)))
//     writeFinish(0) := io.in(0).req.fire() && reqIsWrite(0)
//     writeFinish(1) := io.in(1).req.fire() && reqIsWrite(1)
//InDTCM——>write  ,InDTCMstate——>read

 for (i <- 0 until 2) {
    when (io.in(i).resp.valid)
     {inDTCMstate(i) := false.B}
    when (InDTCM(i) && reqIsRead(i))
     {inDTCMstate(i) := true.B}
  } 


  for (i <- 0 until 2) {
    when (InDTCM(i)||inDTCMstate(i) && !missDTCM(i)) {
      io.outMissmem(i).req.valid := false.B
      io.outMissmem(i).req.bits.addr := 0.U
      io.outMissmem(i).req.bits.size := 0.U
      io.outMissmem(i).req.bits.cmd := 0.U
      io.outMissmem(i).req.bits.wmask := 0.U
      io.outMissmem(i).req.bits.wdata := 0.U
      io.outMissmem(i).resp.ready := true.B
    }.otherwise {
      io.outMissmem(i).req.valid := io.in(i).req.valid
      io.outMissmem(i).req.bits := io.in(i).req.bits
      io.in(i).req.ready := io.outMissmem(i).req.ready
      io.outMissmem(i).resp.ready := true.B
      io.in(i).resp.valid := io.outMissmem(i).resp.valid
      io.in(i).resp.bits := io.outMissmem(i).resp.bits
    }
  }
  //然后是DTCM本身的访问
  val bothLSU = (reqValid(0) && reqInDTCM(0)   && (reqIsRead(0) || reqIsWrite(0))) &&
    (reqValid(1) && reqInDTCM(1)  && (reqIsRead(1) || reqIsWrite(1)))
  val only0DTCM = (reqValid(0) && reqInDTCM(0) && (reqIsRead(0) || reqIsWrite(0))) &&
    !(reqValid(1) && reqInDTCM(1) && (reqIsRead(1) || reqIsWrite(1)))
  val only1DTCM = (reqValid(1) && reqInDTCM(1) && (reqIsRead(1) || reqIsWrite(1))) &&
    !(reqValid(0) && reqInDTCM(0) && (reqIsRead(0) || reqIsWrite(0)))
  val isbothLSU = RegInit(false.B)
  val isonly0DTCM = RegInit(false.B)
  val isonly1DTCM = RegInit(false.B)
  val WriteFinishReg0 = RegNext(io.in(0).req.fire() && reqIsWrite(0),false.B)
  val WriteFinishReg1 = RegNext(io.in(1).req.fire() && reqIsWrite(1),false.B)
  when (io.in(0).resp.valid || WriteFinishReg0)
    {isbothLSU := false.B}
  when (bothLSU)
    {isbothLSU :=true.B}
  //only0DTCM
  when (io.in(0).resp.valid || WriteFinishReg0)
    {isonly0DTCM := false.B}
  when (only0DTCM)
    {isonly0DTCM :=true.B}
  //only1DTCM
  when (io.in(1).resp.valid || WriteFinishReg1)
    {isonly1DTCM := false.B}
  when (only1DTCM)
    {isonly1DTCM :=true.B}
  //标志位信号
  
  
  val flag = RegInit(false.B) //标志位，表示接下来需要处理line1
  val setIdx1 = (io.in(1).req.bits.addr(VAddrBits - 1, 3)) & 2047.U
  val setIdx0 = (io.in(0).req.bits.addr(VAddrBits - 1, 3)) & 2047.U
  //通用逻辑
      def handlePort(portIdx: Int, setIdx: UInt): Unit = {
      val req = io.in(portIdx).req.bits
      //mask为bit颗粒度的写掩码
      val fullMask = MaskExpand(req.wmask)
      
      when (req.isRead()) {
        sram.io.en := true.B
        sram.io.rIdx := setIdx
      } .elsewhen (req.isWrite()) {
        sram.io.en := true.B
        sram.io.wIdx := setIdx
        sram.io.wen := true.B
        sram.io.wdata := req.wdata
        sram.io.wmask := fullMask
      } .otherwise {
        sram.io.en := false.B
        sram.io.wen := false.B // default no write
        sram.io.wmask := 0.U
        sram.io.wdata := 0.U
        sram.io.wIdx := 0.U
        sram.io.rIdx := 0.U
      }
    }

 //when (DTCMMiss(0)||DTCMMiss(1)) {
 //  //sram in
 //  sram.io.w.req.valid := false.B
 //  sram.io.r.req.valid := false.B
 //  sram.io.w.req.bits.apply(
 //          valid = false.B,
 //          data = VecInit(Seq.fill(8)(0.U(8.W))),
 //          setIdx = 0.U,
 //          waymask = 0.U
 //  )
 //  sram.io.r.req.bits.apply(0.U)
    //flag 处理line1，要求必须要在line0结束后才能处理
    //line0完成
    val respValidReg0 = RegNext(io.in(0).req.fire() && reqIsRead(0),false.B)
    val respValidReg1 = RegNext(io.in(1).req.fire() && reqIsRead(1),false.B)
    val respRdataReg0 = RegNext(sram.io.rdata)
    val respRdataReg1 = RegNext(sram.io.rdata)
    val respInDTCMReg0 = RegNext(reqInDTCM(0))
    val respInDTCMReg1 = RegNext(reqInDTCM(1))
    val line0Finish = io.in(0).resp.valid || RegNext(WriteFinishReg0  ,false.B) 
    val line1Finish = io.in(1).resp.valid || RegNext(WriteFinishReg1  ,false.B)
    //line1和LINE0在同一个周期发起请求，line0优先且line1保留，但是他们都是ready的
    //line1为delay了一个周期才处理
    //因此line1的resp是delay了两个周期才valid
    //在both状态下，一瞬间ready都是true.B，随后拉低，并保留1和0的所有信号，ready拉低，直到line1完成
    //保留both状态下的line1的信号
    val saveLine1Bits = RegInit(0.U.asTypeOf(new SimpleBusReqBundle(addrBits = VAddrBits)))
    val saveLine1Idx = RegInit(0.U)

     when(bothLSU && reqReady(0) && reqReady(1)) {
       saveLine1Bits := io.in(1).req.bits
       saveLine1Idx := setIdx1
     }
    //both条件下line1的respvalid
    val line1start = RegNext(flag && line0Finish,false.B)
    val saveLine1RespValid = RegNext( line1start ,false.B)
    //line0状态,从both开始到line0完成
    val line0state = RegInit(false.B)
    val line0FinishDelay = RegNext(line0Finish ,false.B)
    val line1FinishDelay = RegNext(line1Finish ,false.B)
    when(line0Finish)
      {line0state := false.B}
    when(bothLSU && reqReady(0) && reqReady(1))
      {line0state := true.B}
      //line1状态,从line1start开始到line1完成
      //这里采用组合逻辑实现，line1FinishDelay也就是line1完成的下一个周期拉高时候，line1state瞬间拉低
    val line1state = RegInit(false.B)
    when(line1Finish)
      {line1state := false.B}
    when(line1start)
      {line1state := true.B}
//处理逻辑
  //在line1state下，暂停所有请求
     when(line1state ){
      io.in(0).req.ready := false.B
      io.in(1).req.ready := false.B
    }.elsewhen(line1start){ 
    io.in(0).req.ready := false.B
    io.in(1).req.ready := false.B
    flag := false.B
    //处理line1
     //   when(inRange(saveLine1Bits.addr)){
          handlePort(1, saveLine1Idx)
     //   }.otherwise {
     //     io.outMissmem(1).req.valid := true.B
     //     io.outMissmem(1).req.bits := saveLine1Bits
     //     io.outMissmem(1).resp.ready := true.B
     //     //io.in(1).resp.valid := io.outMissmem(1).resp.valid
     //     //io.in(1).resp.bits := io.outMissmem(1).resp.bits
     //   }        
  //瞬间将line0信号给到，当前状态只维持一个周期
  }.elsewhen(line0state){
     io.in(0).req.ready := false.B
     io.in(1).req.ready := false.B

  }.elsewhen (bothLSU){
    flag := true.B
    io.in(0).req.ready := true.B
    io.in(1).req.ready := true.B
    //判断是否在DTCM内
      //   when(reqInDTCM(0)){
           handlePort(0, setIdx0)
      //  }.otherwise {
      //    io.outMissmem(0).req.valid := io.in(0).req.valid
      //    io.outMissmem(0).req.bits := io.in(0).req.bits
      //    io.in(0).req.ready := io.outMissmem(0).req.ready
      //    io.outMissmem(0).resp.ready := true.B
      //    io.in(0).resp.valid := io.outMissmem(0).resp.valid
      //    io.in(0).resp.bits := io.outMissmem(0).resp.bits
      //  }  
  }.elsewhen (only0DTCM ){
    flag := false.B
    io.in(0).req.ready := true.B
    io.in(1).req.ready := true.B
    handlePort(0, setIdx0)
   // io.in(0).resp.valid := respValidReg0
   // io.in(0).resp.bits.rdata := respRdataReg0
   // io.in(0).resp.bits.cmd := Mux(respValidReg0, 
   //                       SimpleBusCmd.readLast, 
   //                       SimpleBusCmd.writeResp)
  }.elsewhen (only1DTCM ){
    flag := false.B
    io.in(0).req.ready := true.B
    io.in(1).req.ready := true.B
    handlePort(1, setIdx1)
    
  }.otherwise {
    // default: no sram access
    //flag := false.B
    sram.io.en := false.B
    sram.io.wen := false.B // default no write
    sram.io.wmask := 0.U
    sram.io.wdata := 0.U
    sram.io.wIdx := 0.U
    sram.io.rIdx := 0.U
  // io.in(0).resp.valid := false.B
  // io.in(1).resp.valid := false.B
    io.in(0).req.ready := Mux(InDTCM(0)||inDTCMstate(0),true.B ,io.outMissmem(0).req.ready) 
    io.in(1).req.ready := Mux(InDTCM(1)||inDTCMstate(1),true.B ,io.outMissmem(1).req.ready)
  }
//处理resp
  when(isonly0DTCM){
    io.in(0).resp.valid :=  respValidReg0
    io.in(0).resp.bits.rdata := respRdataReg0
    io.in(0).resp.bits.cmd := Mux(respValidReg0,
                      SimpleBusCmd.readLast, 
                      SimpleBusCmd.writeResp)
  }

  when(isonly1DTCM){
    io.in(1).resp.valid := respValidReg1
    io.in(1).resp.bits.rdata := respRdataReg1
    io.in(1).resp.bits.cmd := Mux(respValidReg1,
                      SimpleBusCmd.readLast, 
                      SimpleBusCmd.writeResp)
  }
  when(line0state){
    //处理line0
   // when(respInDTCMReg0){
      io.in(0).resp.valid := respValidReg0
      io.in(0).resp.bits.rdata := respRdataReg0
      io.in(0).resp.bits.cmd := Mux(respValidReg0, 
                            SimpleBusCmd.readLast, 
                            SimpleBusCmd.writeResp)
   // }.otherwise {
   //   io.outMissmem(0).req.valid := io.in(0).req.valid
   //   io.outMissmem(0).req.bits := io.in(0).req.bits
   //   io.in(0).req.ready := io.outMissmem(0).req.ready
   //   io.outMissmem(0).resp.ready := true.B
   //   io.in(0).resp.valid := io.outMissmem(0).resp.valid
   //   io.in(0).resp.bits := io.outMissmem(0).resp.bits
   // }     
  }
  
  when(line1state){
    //处理line1
  //  when(inRange(saveLine1Bits.addr)){
      io.in(1).resp.valid := saveLine1RespValid
      io.in(1).resp.bits.rdata := respRdataReg1
      io.in(1).resp.bits.cmd := Mux(saveLine1RespValid, 
                            SimpleBusCmd.readLast, 
                            SimpleBusCmd.writeResp)
  // }.otherwise {
  //   io.outMissmem(1).req.valid := io.in(1).req.valid
  //   io.outMissmem(1).req.bits := io.in(1).req.bits
  //   io.in(1).req.ready := io.outMissmem(1).req.ready
  //   io.outMissmem(1).resp.ready := true.B
  //   io.in(1).resp.valid := io.outMissmem(1).resp.valid
  //   io.in(1).resp.bits := io.outMissmem(1).resp.bits
  // }     
  }
  for (i <- 0 until 2) {
  when(io.outMissmem(i).resp.valid){
    io.in(i).resp.valid := io.outMissmem(i).resp.valid
    io.in(i).resp.bits := io.outMissmem(i).resp.bits
  }
}
}
    
    
    



    
    
