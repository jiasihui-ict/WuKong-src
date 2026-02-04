/**************************************************************************************
* ITCM - instruction tightly-coupled memory
* size: default 256KB, data width: 64-bit, non-burst, backed by RAMHelper (BlackBox)
**************************************************************************************/

package device

import WuKong.HasCoreParameter
import WuKong.HasCoreConst
import chisel3._
import chisel3.util._
import bus.simplebus._
import chisel3.util.experimental.loadMemoryFromFile

/**
  * Simple read-only ITCM module with SimpleBusUC interface.
  * - memByte: size in bytes (default 256*1024)
  * - base: physical base address (for documentation; address crossing must be handled by crossbar)
  *
  * This module uses the existing `RAMHelper` BlackBox as the storage backend.
  * It accepts SimpleBusUC read requests and returns 64-bit read data.
  */

class ITCM(memByte: Int = 256 * 1024, base: Long = 0x80000000L,userBits: Int = 91 ) extends Module with HasCoreParameter {
  val io = IO(new Bundle {
    // carry user bits so IFU can extract outPredictPkt from resp.user
    val in = Flipped(new SimpleBusUC(userBits = userBits, addrBits = VAddrBits))
    val flush = Input(Bool())
    
    // forward miss with user as well
    val outMissmem = new SimpleBusUC(userBits = userBits, addrBits = VAddrBits)
  })
//看是否在ITCM范围内,不在范围内则发出miss信号向mem发起请求
  val inITCM = io.in.req.bits.addr >= base.U && io.in.req.bits.addr < (base + memByte).U
  val Miss = io.in.req.valid && !inITCM
  //io.outMissmem.req.valid := Miss 
  //io.outMissmem.req.bits.addr := io.in.req.bits.addr
  //io.outMissmem.resp.ready := io.in.resp.ready
  //当前情况ITCM不可能MISS
  when(Miss){
    io.outMissmem.req.bits := io.in.req.bits
    io.outMissmem.req.valid := io.in.req.valid
    io.in.req.ready := io.outMissmem.req.ready

    io.outMissmem.resp.ready := io.in.resp.ready
    io.in.resp.bits := io.outMissmem.resp.bits
    io.in.resp.valid := io.outMissmem.resp.valid

  }.otherwise{
    io.outMissmem.req.valid := false.B
    io.outMissmem.req.bits.addr := 0.U
    io.outMissmem.req.bits.wmask := 0.U
    io.outMissmem.req.bits.cmd := 0.U
    io.outMissmem.req.bits.wdata := 0.U
    io.outMissmem.req.bits.size := 0.U
    io.outMissmem.req.bits.user.foreach(_ := 0.U)
    io.outMissmem.resp.ready := false.B
  }

  // number of 64-bit words
 //选取中间地址位
  val offsetBits = log2Up(memByte)
  val offsetMask = (1 << offsetBits) - 1
 //这里就是提取ram地址的中间位
  def index(addr: UInt) = (addr & offsetMask.U) >> log2Ceil(DataBytes)

  val mem = Module(new RAMHelper(memByte))
  mem.io.clk := clock
  mem.io.en := true.B

  // read state machine
  val regrIdx = RegInit(0.U(offsetBits.W))
  mem.io.rIdx := regrIdx
  //mem.io.rIdx := Mux (inITCM, index(io.in.req.bits.addr), index(base.U))
  mem.io.wIdx := 0.U
  mem.io.wdata := 0.U
  mem.io.wmask := 0.U
  mem.io.wen := 0.B
  // wire RAMHelper indexes; RAMHelper expects DataBits-width index in original design
  // extend readIdx width to DataBits and assign

  // register returned data
  //在RAMHelper中读取数据，需要时钟驱动延迟了
  //val rdataReg = mem.io.rdata
  //io.in.resp.bits.rdata := rdataReg
  // ready/valid signals
  //raddr延迟了，data就不用再延迟了
  val valid = RegInit(false.B)
  val ITCMrdata = WireInit(0.U(DataBits.W))
  val ITCMusr = RegInit(0.U(userBits.W))
  val readoutFire = io.in.resp.valid && io.in.resp.ready
  when (readoutFire) { 
    valid := false.B
    regrIdx := 0.U
    ITCMrdata := 0.U
    ITCMusr := 0.U
   }
  when (io.in.req.valid && io.in.resp.ready && !io.flush) { 
    valid := true.B 
    regrIdx := Mux (inITCM, index(io.in.req.bits.addr), index(base.U))
    ITCMrdata := mem.io.rdata
    ITCMusr := io.in.req.bits.user.get
  }
  when (io.flush) {
    valid := false.B 
    regrIdx := 0.U
    ITCMrdata := 0.U
    ITCMusr := 0.U
  }

  io.in.req.ready := Mux(inITCM, io.in.resp.ready, io.outMissmem.req.ready)
  io.in.resp.valid := valid || (io.outMissmem.resp.valid && !io.flush)
  // capture user on accepted request (hit path)
  //
  
  // ensure resp.cmd is set
  io.in.resp.bits.cmd := 6.U  // SimpleBusCmd.read
  // INST output and user pass-through
  //io.in.resp.bits.rdata := Mux(valid, mem.io.rdata, Mux(io.outMissmem.resp.valid, io.outMissmem.resp.bits.rdata, 0.U))
  io.in.resp.bits.rdata := Mux(valid, 
                            ITCMrdata,
                            Mux(io.outMissmem.resp.valid,
                                io.outMissmem.resp.bits.rdata,
                                0.U))
  //io.in.resp.bits.user := Mux(valid, io.in.req.bits.user, Mux(io.outMissmem.resp.valid, io.outMissmem.resp.bits.user, 0.U))
   if (userBits > 0) {
    // 只有当userBits>0时才处理
    when(valid) {
      io.in.resp.bits.user.foreach(_ := ITCMusr)
    }.elsewhen(io.outMissmem.resp.valid) {
      io.in.resp.bits.user.foreach(_ := io.outMissmem.resp.bits.user.get)
    }.otherwise {
      io.in.resp.bits.user.foreach(_ := 0.U)
    }
  }


}

