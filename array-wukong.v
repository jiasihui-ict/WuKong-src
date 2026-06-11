module array_ext(
  input RW0_clk,
  input [6:0] RW0_addr,
  input RW0_en,
  input RW0_wmode,
  input [71:0] RW0_wdata,
  output [71:0] RW0_rdata
);


  reg reg_RW0_ren;
  reg [6:0] reg_RW0_addr;
  reg [71:0] ram [127:0];


`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
  initial begin
    #`RANDOMIZE_DELAY begin end
    for (initvar = 0; initvar < 128; initvar = initvar + 1)
      ram[initvar] = {3 {$random}};
    reg_RW0_addr = {1 {$random}};
  end
`endif


  integer i;


  always @(posedge RW0_clk)
    reg_RW0_ren <= RW0_en && !RW0_wmode;


  always @(posedge RW0_clk)
    if (RW0_en && !RW0_wmode)
      reg_RW0_addr <= RW0_addr;


  always @(posedge RW0_clk)
    if (RW0_en && RW0_wmode) begin
      for (i = 0; i < 1; i = i + 1) begin
        ram[RW0_addr][i*72 +: 72] <= RW0_wdata[i*72 +: 72];
      end
    end


`ifdef RANDOMIZE_GARBAGE_ASSIGN
  reg [95:0] RW0_random;


`ifdef RANDOMIZE_MEM_INIT
  initial begin
    #`RANDOMIZE_DELAY begin end
    RW0_random = {$random, $random, $random};
    reg_RW0_ren = RW0_random[0];
  end
`endif


  always @(posedge RW0_clk)
    RW0_random <= {$random, $random, $random};


  assign RW0_rdata = reg_RW0_ren ? ram[reg_RW0_addr] : RW0_random[71:0];


`else
  assign RW0_rdata = ram[reg_RW0_addr];
`endif


endmodule










module array_0_ext(
  input RW0_clk,
  input [5:0] RW0_addr,
  input RW0_en,
  input RW0_wmode,
  input [63:0] RW0_wdata,
  output reg [63:0] RW0_rdata
);




  reg [63:0] ram [63:0];
  
  always @(posedge RW0_clk) begin
    if (RW0_en) begin
      if (RW0_wmode) begin
        ram[RW0_addr] <= RW0_wdata;  // 写
      end else begin
        RW0_rdata <= ram[RW0_addr];  // 读（同周期输出）
      end
    end
  end




endmodule




module S011HD1P_X32Y2D128_BW_tmp(
    Q, Q1, CLK, CEN, WEN, BWEN, A, A1, D
);
parameter Bits = 128;
parameter Word_Depth = 64;
parameter Add_Width = 6;
parameter Wen_Width = 128;


output reg [Bits-1:0] Q;
output reg [Bits-1:0] Q1;
input                 CLK;
input                 CEN;
input                 WEN;
input [Wen_Width-1:0] BWEN;
input [Add_Width-1:0] A;
input [Add_Width-1:0] A1;
input [Bits-1:0]      D;


wire cen  = ~CEN;
wire wen  = ~WEN;
wire [Wen_Width-1:0] bwen = ~BWEN;


reg [Bits-1:0] ram [0:Word_Depth-1];
always @(posedge CLK) begin
    if(cen && wen) begin
        ram[A] <= (D & bwen) | (ram[A] & ~bwen);
    end
    Q <= cen && !wen ? ram[A] : {4{$random}};
    Q1 <= cen && !wen ? ram[A1] : {4{$random}};
end


endmodule




module S011HD1P_X32Y2D128_BW(
    Q, CLK, CEN, WEN, BWEN, A, D
);
parameter Bits = 128;
parameter Word_Depth = 64;
parameter Add_Width = 6;
parameter Wen_Width = 128;




output reg [Bits-1:0] Q;
input                 CLK;
input                 CEN;
input                 WEN;
input [Wen_Width-1:0] BWEN;
input [Add_Width-1:0] A;
input [Bits-1:0]      D;




wire cen  = ~CEN;
wire wen  = ~WEN;
wire [Wen_Width-1:0] bwen = ~BWEN;




reg [Bits-1:0] ram [0:Word_Depth-1];
always @(posedge CLK) begin
    if(cen && wen) begin
        ram[A] <= (D & bwen) | (ram[A] & ~bwen);
    end
    Q <= cen && !wen ? ram[A] : {4{$random}};
end




endmodule

