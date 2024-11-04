// Generated by CIRCT firtool-1.62.0
module cell_0(
  input         clock,
                reset,
  input  [15:0] io_lane_in_1,
                io_lane_in_2,
                io_stage_in_1,
  input  [3:0]  io_cell_mode,
  input         io_lane_out_1_select,
  output [15:0] io_lane_out_1,
                io_lane_out_2,
                io_stage_out_1,
                io_accum_out
);

  reg [15:0] lane_out_1_reg;
  reg [15:0] lane_out_2_reg;
  reg [15:0] stage_out_1_reg;
  reg [15:0] accum_reg;
  reg [15:0] accum_out_reg;
  always @(posedge clock) begin
    if (reset) begin
      lane_out_1_reg <= 16'h0;
      lane_out_2_reg <= 16'h0;
      stage_out_1_reg <= 16'h0;
      accum_reg <= 16'h0;
      accum_out_reg <= 16'h0;
    end
    else if (io_cell_mode == 4'h0) begin
      lane_out_1_reg <= io_lane_in_1;
      stage_out_1_reg <= io_stage_in_1;
      accum_reg <= io_lane_in_1 * io_stage_in_1 + accum_reg;
      accum_out_reg <= accum_reg;
    end
    else if (io_cell_mode == 4'h1) begin
      lane_out_1_reg <= io_lane_out_1_select ? io_lane_in_1 : io_lane_in_2;
      lane_out_2_reg <= 16'h0;
    end
    else if (io_cell_mode == 4'h2) begin
      lane_out_1_reg <= io_lane_out_1_select ? io_lane_in_1 : io_lane_in_2;
      lane_out_2_reg <= io_lane_in_1;
    end
    else if (io_cell_mode == 4'h3) begin
      lane_out_1_reg <= io_lane_out_1_select ? io_lane_in_1 : io_lane_in_2;
      lane_out_2_reg <= 16'h0;
    end
    else if (io_cell_mode == 4'h4) begin
      lane_out_1_reg <= io_lane_out_1_select ? io_lane_in_1 : io_lane_in_2;
      lane_out_2_reg <= io_lane_in_2;
    end
    else if (io_cell_mode == 4'h5) begin
      lane_out_1_reg <= io_lane_out_1_select ? io_lane_in_1 : io_lane_in_2;
      lane_out_2_reg <= io_lane_in_1 * io_lane_in_2;
    end
    else if (io_cell_mode == 4'h6) begin
      lane_out_1_reg <= io_lane_out_1_select ? io_lane_in_1 : io_lane_in_2;
      lane_out_2_reg <= io_lane_in_1;
    end
    else if (io_cell_mode == 4'h7) begin
      lane_out_1_reg <= io_lane_out_1_select ? io_lane_in_1 : io_lane_in_2;
      lane_out_2_reg <= io_lane_in_2;
    end
    else if (io_cell_mode == 4'h8) begin
      lane_out_1_reg <= io_lane_out_1_select ? io_lane_in_1 : io_lane_in_2;
      lane_out_2_reg <= io_lane_in_1;
    end
  end // always @(posedge)
  assign io_lane_out_1 = lane_out_1_reg;
  assign io_lane_out_2 = lane_out_2_reg;
  assign io_stage_out_1 = stage_out_1_reg;
  assign io_accum_out = accum_out_reg;
endmodule


