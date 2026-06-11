package WuKong.Backend.fu

import WuKong._
import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import WuKong.isa.FuOpType
import WuKong.{CoreModule, CoreBundle}
class MOUIO extends CoreBundle {
  val pipelinevalid = Input(Bool())
  val in = Flipped(Decoupled(new Bundle {
    val func = Output(FuOpType())
  }))
  val out = Decoupled(Output(UInt(XLEN.W)))
  val flush = Input(Bool())
}

class MOU extends CoreModule{
  val io = IO(new MOUIO)
  val sbIsempty = WireInit(false.B)
  BoringUtils.addSink(sbIsempty, "sbIsempty")
  //JSH修改，DCache_done这个信号被其他模块注释掉了，根本没有，我这里直接ture
  val DCache_done = WireInit(true.B)
  //val DCache_done = WireInit(false.B)
  BoringUtils.addSink(DCache_done, "DCache_done")
  val s1_idle :: pipeline_check :: sbcheck :: flush_dcache :: flush_icache :: flush_icache_finish :: flush_done :: Nil = Enum(7)
  val state1 = RegInit(s1_idle)
  //jsh增加icachefinish状态
  val icacheFinish = WireInit(false.B)
  BoringUtils.addSink(icacheFinish, "icacheFinish")
  dontTouch(icacheFinish)

  switch(state1) {
    is(s1_idle) {
      when(io.in.valid) {
        state1 := pipeline_check
      }
    }
    is(pipeline_check) {
      when(io.flush) {
        state1 := s1_idle
      }.elsewhen(!io.pipelinevalid) {
        state1 := sbcheck
      }
    }
    is(sbcheck) {
      when(io.flush) {
        state1 := s1_idle
      }.elsewhen(sbIsempty) {
        state1 := flush_dcache
      }
    }
    is(flush_dcache) {
      when(DCache_done) {
        state1 := flush_icache
      }.elsewhen(io.flush) {
        state1 := s1_idle
      }
    }
    //jsh修改
    is(flush_icache) {
      when(io.flush) {
        state1 := s1_idle
      }.otherwise{
        state1 := flush_icache_finish
      }
    }
    is(flush_icache_finish){
     when(io.flush){ 
        state1 := s1_idle
      }.elsewhen(icacheFinish)
      {
        state1 := flush_done
     }
    }
    is(flush_done) {
      state1 := s1_idle
    }
  }
//  when (state1 === s1_idle && io.in.valid) { state1 := pipeline_check }
//  when (state1 === pipeline_check && (!io.pipelinevalid)) {state1 := sbcheck}
//  when (state1 === sbcheck && sbIsempty) {state1 := flush_dcache}
//  when (state1 === flush_dcache && DCache_done) {state1 := flush_icache}
//  when (state1 === flush_icache) {state1 := flush_done}
//  when (state1 === flush_done) {state1 := s1_idle}
//  when (io.flush) {state1 := s1_idle}

  val flushICache = (state1 === flush_icache)
  BoringUtils.addSource(flushICache, "MOUFlushICache")
  val flushDCache = (state1 === flush_dcache)
  // BoringUtils.addSource(flushDCache, "MOUFlushDCache")

  io.out.bits := 0.U
  io.in.ready := state1 === s1_idle
  io.out.valid := state1 === flush_done

}
