//> using scala "2.13.12"
//> using dep "org.chipsalliance::chisel:6.3.0"
//> using plugin "org.chipsalliance:::chisel-plugin:6.3.0"
//> using options "-unchecked", "-deprecation", "-language:reflectiveCalls", "-feature", "-Xcheckinit", "-Xfatal-warnings", "-Ywarn-dead-code", "-Ywarn-unused", "-Ymacro-annotations"
import chisel3._
import chisel3.util.{switch, is}
import _root_.circt.stage.ChiselStage


class cell extends Module {
	val io = new Bundle {
		val lane_in_1, lane_in_2, stage_in_1 = IO(Input(SInt(16.W)))
		val cell_mode = IO(Input(UInt(16.W)))
		val lane_out_1_select = IO(Input(Bool()))
		val lane_out_1, lane_out_2, stage_out_1, accum_out = IO(Output(SInt(16.W)))
	}
	
    val lane_out_1_reg = RegInit(0.S(16.W))
    val lane_out_2_reg = RegInit(0.S(16.W))
    val stage_out_1_reg = RegInit(0.S(16.W))
    val accum_reg = RegInit(0.S(16.W))
    val accum_out_reg = RegInit(0.S(16.W))
	
    val constant = 0.S(16.W)
	
	switch (io.cell_mode)
	{
		is ( 0.U(4.W) ) { // MAC
			accum_reg := io.lane_in_1 * io.stage_in_1 + accum_reg
			accum_out_reg := accum_reg
			lane_out_1_reg := io.lane_in_1
			stage_out_1_reg := io.stage_in_1
		}
		is ( 1.U(4.W) ) { // in1 x constant
			lane_out_2_reg := io.lane_in_1 * constant
			lane_out_1_reg := Mux(io.lane_out_1_select, io.lane_in_1, io.lane_in_2)
		}
		is ( 2.U(4.W) ) { // in1 + constant
			lane_out_2_reg := io.lane_in_1 + constant
			lane_out_1_reg := Mux(io.lane_out_1_select, io.lane_in_1, io.lane_in_2)
		}
		is ( 3.U(4.W) ) { // in2 x constant
			lane_out_2_reg := io.lane_in_2 * constant
			lane_out_1_reg := Mux(io.lane_out_1_select, io.lane_in_1, io.lane_in_2)
		}
		is ( 4.U(4.W) ) { // in2 + constant
			lane_out_2_reg := io.lane_in_2 + constant
			lane_out_1_reg := Mux(io.lane_out_1_select, io.lane_in_1, io.lane_in_2)
		}
		is ( 5.U(4.W) ) { // in1 x in2
			lane_out_2_reg := io.lane_in_1 * io.lane_in_2
			lane_out_1_reg := Mux(io.lane_out_1_select, io.lane_in_1, io.lane_in_2)
		}
		is ( 6.U(4.W) ) { // in1 + in2
			lane_out_2_reg := io.lane_in_1 + io.lane_in_2
			lane_out_1_reg := Mux(io.lane_out_1_select, io.lane_in_1, io.lane_in_2)
		}
		is ( 7.U(4.W) ) { // in1 x constant + in2
			lane_out_2_reg := io.lane_in_1 * constant + io.lane_in_2
			lane_out_1_reg := Mux(io.lane_out_1_select, io.lane_in_1, io.lane_in_2)
		}
		is ( 8.U(4.W) ) { // in2 x constant + in1
			lane_out_2_reg := io.lane_in_2 * constant + io.lane_in_1
			lane_out_1_reg := Mux(io.lane_out_1_select, io.lane_in_1, io.lane_in_2)
		}
	}
	
	
	// assign output signal
	io.lane_out_1 := lane_out_1_reg
    io.lane_out_2 := lane_out_2_reg
    io.stage_out_1 := stage_out_1_reg
	io.accum_out := accum_out_reg
}








class baseline_pcu_8x6 extends Module {
	val r = 8
	val c = 6
	val io = new Bundle {
		val lane_in_1 = IO(Input(Vec(r, SInt(16.W))))
		val lane_in_2 = IO(Input(Vec(r, SInt(16.W))))
		val stage_in_1 = IO(Input(Vec(c, SInt(16.W))))
		val cell_mode = IO(Input(Vec(r*c, UInt(16.W))))
		val lane_out_1_select = IO(Input(Vec(r*c, Bool())))
		val select = IO(Input(Vec(7, Bool())))

		val lane_out_1 = IO(Output(Vec(r, SInt(16.W))))
		val lane_out_2 = IO(Output(Vec(r, SInt(16.W))))
		val stage_out_1 = IO(Output(Vec(c, SInt(16.W))))
		val accum_out = IO(Output(Vec(r*c, SInt(16.W))))
	}
	
	
	val cells = Seq.tabulate(r*c) {
		_ => Module(new cell())
	}

	// input to cells
	for (i <- 0 until r)
	{
		cells(i*c).io.lane_in_1 := io.lane_in_1(i)
		cells(i*c).io.lane_in_2 := io.lane_in_2(i)
	}
	
	for (j <- 0 until c)
	{
		cells(j).io.stage_in_1 := io.stage_in_1(j)
	}
	
	for (i <- 0 until r)
	{
		for (j <- 0 until c)
		{
			cells(i*c+j).io.cell_mode := io.cell_mode(i*c+j)
			cells(i*c+j).io.lane_out_1_select := io.lane_out_1_select(i*c+j)
		}
	}
	
	



	
	// cells to cells

	// connections for reduce
	var dict = Map.empty[(Int, Int, String), List[(Int, Int, String)]] // dst, src

	dict = dict + ((0, 1, "t") -> List((1, 0, "t")))
    dict = dict + ((2, 1, "t") -> List((3, 0, "t")))
    dict = dict + ((4, 1, "t") -> List((5, 0, "t")))
	dict = dict + ((6, 1, "t") -> List((7, 0, "t")))
	dict = dict + ((0, 2, "t") -> List((2, 1, "t")))
	dict = dict + ((4, 2, "t") -> List((6, 1, "t")))
	dict = dict + ((0, 3, "t") -> List((4, 2, "t")))

	var cnt = 0
	for (i <- 0 until r)
	{	
		for (j <- 1 until c)
		{


			if (dict.contains((i, j, "t"))) // dst top
			{
				val src = dict((i, j, "t"))
				if (src.size == 1)
				{
					val src_x = src(0)._1
					val src_y = src(0)._2
					val src_tb = src(0)._3
					val src_idx = src_x*c+src_y

					if (src_tb == "t")
					{
						cells(i*c+j).io.lane_in_1 := Mux(io.select(cnt), cells(src_idx).io.lane_out_1, cells(i*c+j-1).io.lane_out_1)
					} else
					{
						cells(i*c+j).io.lane_in_1 := Mux(io.select(cnt), cells(src_idx).io.lane_out_2, cells(i*c+j-1).io.lane_out_1)
					}
					cnt += 1
				}
			} else
			{
				cells(i*c+j).io.lane_in_1 := cells(i*c+j-1).io.lane_out_1
			}



			






			if (dict.contains((i, j, "b"))) // dst bottom
			{
				val src = dict((i, j, "b"))
				if (src.size == 1)
				{
					val src_x = src(0)._1
					val src_y = src(0)._2
					val src_tb = src(0)._3
					val src_idx = src_x*c+src_y

					if (src_tb == "t")
					{
						cells(i*c+j).io.lane_in_2 := Mux(io.select(cnt), cells(src_idx).io.lane_out_1, cells(i*c+j-1).io.lane_out_2)
					} else
					{
						cells(i*c+j).io.lane_in_2 := Mux(io.select(cnt), cells(src_idx).io.lane_out_2, cells(i*c+j-1).io.lane_out_2)
					}
					cnt += 1
				}
			} else
			{
				cells(i*c+j).io.lane_in_2 := cells(i*c+j-1).io.lane_out_2
			}

			

		}
	}





















	
	for (i <- 1 until r)
	{	
		for (j <- 0 until c)
		{
			cells(i*c+j).io.stage_in_1 := cells((i-1)*c+j).io.stage_out_1
		}
	}
	
	
	
	
	
	// cells to output
	for (i <- 0 until r)
	{
		io.lane_out_1(i) := cells(i*c+c-1).io.lane_out_1
		io.lane_out_2(i) := cells(i*c+c-1).io.lane_out_2
	}
	
	for (j <- 0 until c)
	{
		io.stage_out_1(j) := cells((r-1)*c+j).io.stage_out_1
	}
	
	for (i <- 0 until r)
	{
		for (j <- 0 until c)
		{
			io.accum_out(i*c+j) := cells(i*c+j).io.accum_out
		}
	}


}












class fft_pcu_8x6 extends Module {
	val r = 8
	val c = 6
	val io = new Bundle {
		val lane_in_1 = IO(Input(Vec(r, SInt(16.W))))
		val lane_in_2 = IO(Input(Vec(r, SInt(16.W))))
		val stage_in_1 = IO(Input(Vec(c, SInt(16.W))))
		val cell_mode = IO(Input(Vec(r*c, UInt(16.W))))
		val lane_out_1_select = IO(Input(Vec(r*c, Bool())))
		val select1 = IO(Input(Vec(31, Bool())))
		val select2 = IO(Input(Vec(2, UInt(2.W))))


		val lane_out_1 = IO(Output(Vec(r, SInt(16.W))))
		val lane_out_2 = IO(Output(Vec(r, SInt(16.W))))
		val stage_out_1 = IO(Output(Vec(c, SInt(16.W))))
		val accum_out = IO(Output(Vec(r*c, SInt(16.W))))
	}
	
	
	val cells = Seq.tabulate(r*c) {
		_ => Module(new cell())
	}

	// input to cells
	for (i <- 0 until r)
	{
		cells(i*c).io.lane_in_1 := io.lane_in_1(i)
		cells(i*c).io.lane_in_2 := io.lane_in_2(i)
	}
	
	for (j <- 0 until c)
	{
		cells(j).io.stage_in_1 := io.stage_in_1(j)
	}
	
	for (i <- 0 until r)
	{
		for (j <- 0 until c)
		{
			cells(i*c+j).io.cell_mode := io.cell_mode(i*c+j)
			cells(i*c+j).io.lane_out_1_select := io.lane_out_1_select(i*c+j)
		}
	}
	
	
	
	// cells to cells
	
	
	
	// connections for reduce and FFT
	var dict = Map.empty[(Int, Int, String), List[(Int, Int, String)]] // dst, src


	// 31 len 1, 2 len 2
	// reduce
	dict = dict + ((0, 1, "t") -> List((1, 0, "t")))
    dict = dict + ((2, 1, "t") -> List((3, 0, "t")))
    dict = dict + ((4, 1, "t") -> List((5, 0, "t")))
	dict = dict + ((6, 1, "t") -> List((7, 0, "t")))
	dict = dict + ((4, 2, "t") -> List((6, 1, "t")))
	dict = dict + ((0, 2, "t") -> List((2, 1, "t"), (2, 1, "b")))
	dict = dict + ((0, 3, "t") -> List((4, 2, "t"), (1, 2, "b")))


	// FFT
	// dict = dict + ((0, 2, "t") -> List((2, 1, "b")))
    dict = dict + ((1, 2, "t") -> List((3, 1, "b")))
    dict = dict + ((2, 2, "t") -> List((0, 1, "b")))
    dict = dict + ((3, 2, "t") -> List((1, 1, "b")))
    dict = dict + ((4, 2, "t") -> List((6, 1, "b")))
    dict = dict + ((5, 2, "t") -> List((7, 1, "b")))
    dict = dict + ((6, 2, "t") -> List((4, 1, "b")))
    dict = dict + ((7, 2, "t") -> List((5, 1, "b")))
    // dict = dict + ((0, 3, "t") -> List((1, 2, "b")))
    dict = dict + ((1, 3, "t") -> List((0, 2, "b")))
    dict = dict + ((2, 3, "t") -> List((5, 2, "b")))
    dict = dict + ((3, 3, "t") -> List((4, 2, "b")))
    dict = dict + ((4, 3, "t") -> List((3, 2, "b")))
    dict = dict + ((5, 3, "t") -> List((2, 2, "b")))
    dict = dict + ((6, 3, "t") -> List((7, 2, "b")))
    dict = dict + ((7, 3, "t") -> List((6, 2, "b")))
    dict = dict + ((2, 5, "t") -> List((0, 4, "b")))
    dict = dict + ((3, 5, "t") -> List((1, 4, "b")))
    dict = dict + ((0, 5, "t") -> List((2, 4, "b")))
    dict = dict + ((1, 5, "t") -> List((3, 4, "b")))
    dict = dict + ((6, 5, "t") -> List((4, 4, "b")))
    dict = dict + ((7, 5, "t") -> List((5, 4, "b")))
    dict = dict + ((4, 5, "t") -> List((6, 4, "b")))
    dict = dict + ((5, 5, "t") -> List((7, 4, "b")))

	dict = dict + ((2, 3, "b") -> List((4, 2, "b")))
    dict = dict + ((3, 3, "b") -> List((5, 2, "b")))
	dict = dict + ((4, 3, "b") -> List((2, 2, "b")))
	dict = dict + ((5, 3, "b") -> List((3, 2, "b")))
	
	

	
	var cnt1 = 0
	var cnt2 = 0
	for (i <- 0 until r)
	{	
		for (j <- 1 until c)
		{
			if (dict.contains((i, j, "t"))) // dst is top
			{
				val src = dict((i, j, "t"))

				if (src.size == 1)
				{
					val src_x = src(0)._1
					val src_y = src(0)._2
					val src_tb = src(0)._3
					val src_idx = src_x*c+src_y

					if (src_tb == "t")
					{
						cells(i*c+j).io.lane_in_1 := Mux(io.select1(cnt1), cells(src_idx).io.lane_out_1, cells(i*c+j-1).io.lane_out_1)
					} else
					{
						cells(i*c+j).io.lane_in_1 := Mux(io.select1(cnt1), cells(src_idx).io.lane_out_2, cells(i*c+j-1).io.lane_out_1)
					}
					cnt1 += 1
				} else if (src.size == 2)
				{
					val src_x_1 = src(0)._1
					val src_y_1 = src(0)._2
					val src_tb_1 = src(0)._3
					val src_idx_1 = src_x_1*c+src_y_1

					val src_x_2 = src(1)._1
					val src_y_2 = src(1)._2
					val src_tb_2 = src(1)._3
					val src_idx_2 = src_x_2*c+src_y_2


					when (io.select2(cnt2) === 0.U(2.W))
					{
						cells(i*c+j).io.lane_in_1 := cells(i*c+j-1).io.lane_out_1
					}.elsewhen(io.select2(cnt2) === 1.U(2.W))
					{
						cells(i*c+j).io.lane_in_1 := cells(i*c+j-1).io.lane_out_1
					}.elsewhen(io.select2(cnt2) === 2.U(2.W))
					{
						if (src_tb_1 == "t")
						{
							cells(i*c+j).io.lane_in_1 := cells(src_idx_1).io.lane_out_1
						} else
						{
							cells(i*c+j).io.lane_in_1 := cells(src_idx_1).io.lane_out_2
						}
					}.otherwise
					{
						if (src_tb_2 == "t")
						{
							cells(i*c+j).io.lane_in_1 := cells(src_idx_2).io.lane_out_1
						} else
						{
							cells(i*c+j).io.lane_in_1 := cells(src_idx_2).io.lane_out_2
						}
					}



					cnt2 += 1
				}
			} else
			{
				cells(i*c+j).io.lane_in_1 := cells(i*c+j-1).io.lane_out_1
			}



			






			if (dict.contains((i, j, "b"))) // dst is bottom
			{
				val src = dict((i, j, "b"))
				if (src.size == 1)
				{
					val src_x = src(0)._1
					val src_y = src(0)._2
					val src_tb = src(0)._3
					val src_idx = src_x*c+src_y

					if (src_tb == "t")
					{
						cells(i*c+j).io.lane_in_2 := Mux(io.select1(cnt1), cells(src_idx).io.lane_out_1, cells(i*c+j-1).io.lane_out_2)
					} else
					{
						cells(i*c+j).io.lane_in_2 := Mux(io.select1(cnt1), cells(src_idx).io.lane_out_2, cells(i*c+j-1).io.lane_out_2)
					}
					cnt1 += 1
				} else if (src.size == 2)
				{

					val src_x_1 = src(0)._1
					val src_y_1 = src(0)._2
					val src_tb_1 = src(0)._3
					val src_idx_1 = src_x_1*c+src_y_1

					val src_x_2 = src(1)._1
					val src_y_2 = src(1)._2
					val src_tb_2 = src(1)._3
					val src_idx_2 = src_x_2*c+src_y_2


					when (io.select2(cnt2) === 0.U(2.W))
					{
						cells(i*c+j).io.lane_in_2 := cells(i*c+j-1).io.lane_out_2
					}.elsewhen(io.select2(cnt2) === 1.U(2.W))
					{
						cells(i*c+j).io.lane_in_2 := cells(i*c+j-1).io.lane_out_2
					}.elsewhen(io.select2(cnt2) === 2.U(2.W))
					{
						if (src_tb_1 == "t")
						{
							cells(i*c+j).io.lane_in_2 := cells(src_idx_1).io.lane_out_1
						} else
						{
							cells(i*c+j).io.lane_in_2 := cells(src_idx_1).io.lane_out_2
						}
					}.otherwise
					{
						if (src_tb_2 == "t")
						{
							cells(i*c+j).io.lane_in_2 := cells(src_idx_2).io.lane_out_1
						} else
						{
							cells(i*c+j).io.lane_in_2 := cells(src_idx_2).io.lane_out_2
						}
					}



					cnt2 += 1
				}
			} else
			{
				cells(i*c+j).io.lane_in_2 := cells(i*c+j-1).io.lane_out_2
			}



		}
	}
	
	




	
	
	for (i <- 1 until r)
	{	
		for (j <- 0 until c)
		{
			cells(i*c+j).io.stage_in_1 := cells((i-1)*c+j).io.stage_out_1
		}
	}
	
	
	
	
	
	// cells to output
	for (i <- 0 until r)
	{
		io.lane_out_1(i) := cells(i*c+c-1).io.lane_out_1
		io.lane_out_2(i) := cells(i*c+c-1).io.lane_out_2
	}
	
	for (j <- 0 until c)
	{
		io.stage_out_1(j) := cells((r-1)*c+j).io.stage_out_1
	}
	
	for (i <- 0 until r)
	{
		for (j <- 0 until c)
		{
			io.accum_out(i*c+j) := cells(i*c+j).io.accum_out
		}
	}

	
}


















class hs_scan_pcu_8x6 extends Module {
	val r = 8
	val c = 6
	val io = new Bundle {
		val lane_in_1 = IO(Input(Vec(r, SInt(16.W))))
		val lane_in_2 = IO(Input(Vec(r, SInt(16.W))))
		val stage_in_1 = IO(Input(Vec(c, SInt(16.W))))
		val cell_mode = IO(Input(Vec(r*c, UInt(16.W))))
		val lane_out_1_select = IO(Input(Vec(r*c, Bool())))
		val select = IO(Input(Vec(24, Bool())))

		val lane_out_1 = IO(Output(Vec(r, SInt(16.W))))
		val lane_out_2 = IO(Output(Vec(r, SInt(16.W))))
		val stage_out_1 = IO(Output(Vec(c, SInt(16.W))))
		val accum_out = IO(Output(Vec(r*c, SInt(16.W))))
	}
	
	
	val cells = Seq.tabulate(r*c) {
		_ => Module(new cell())
	}

	// input to cells
	for (i <- 0 until r)
	{
		cells(i*c).io.lane_in_1 := io.lane_in_1(i)
		cells(i*c).io.lane_in_2 := io.lane_in_2(i)
	}
	
	for (j <- 0 until c)
	{
		cells(j).io.stage_in_1 := io.stage_in_1(j)
	}
	
	for (i <- 0 until r)
	{
		for (j <- 0 until c)
		{
			cells(i*c+j).io.cell_mode := io.cell_mode(i*c+j)
			cells(i*c+j).io.lane_out_1_select := io.lane_out_1_select(i*c+j)
		}
	}
	
	
	
	// cells to cells
	
	
	
	// connections for reduce and FFT
	var dict = Map.empty[(Int, Int, String), List[(Int, Int, String)]] // dst, src


	// reduce + hs scan
	dict = dict + ((0, 1, "t") -> List((1, 0, "t")))
	dict = dict + ((1, 1, "t") -> List((2, 0, "t")))
	dict = dict + ((2, 1, "t") -> List((3, 0, "t")))
	dict = dict + ((3, 1, "t") -> List((4, 0, "t")))
	dict = dict + ((4, 1, "t") -> List((5, 0, "t")))
	dict = dict + ((5, 1, "t") -> List((6, 0, "t")))
	dict = dict + ((6, 1, "t") -> List((7, 0, "t")))

	dict = dict + ((0, 2, "t") -> List((2, 1, "t")))
	dict = dict + ((1, 2, "t") -> List((3, 1, "t")))
	dict = dict + ((2, 2, "t") -> List((4, 1, "t")))
	dict = dict + ((3, 2, "t") -> List((5, 1, "t")))
	dict = dict + ((4, 2, "t") -> List((6, 1, "t")))
	dict = dict + ((5, 2, "t") -> List((7, 1, "t")))

	dict = dict + ((0, 3, "t") -> List((4, 2, "t")))
	dict = dict + ((1, 3, "t") -> List((5, 2, "t")))
	dict = dict + ((2, 3, "t") -> List((6, 2, "t")))
	dict = dict + ((3, 3, "t") -> List((7, 2, "t")))


	dict = dict + ((0, 4, "t") -> List((1, 3, "t")))
	dict = dict + ((1, 4, "t") -> List((2, 3, "t")))
	dict = dict + ((2, 4, "t") -> List((3, 3, "t")))
	dict = dict + ((3, 4, "t") -> List((4, 3, "t")))
	dict = dict + ((4, 4, "t") -> List((5, 3, "t")))
	dict = dict + ((5, 4, "t") -> List((6, 3, "t")))
	dict = dict + ((6, 4, "t") -> List((7, 3, "t")))

	var cnt1 = 0
	var cnt2 = 0
	for (i <- 0 until r)
	{	
		for (j <- 1 until c)
		{
			if (dict.contains((i, j, "t"))) // dst is top
			{
				val src = dict((i, j, "t"))

				if (src.size == 1)
				{
					val src_x = src(0)._1
					val src_y = src(0)._2
					val src_tb = src(0)._3
					val src_idx = src_x*c+src_y

					if (src_tb == "t")
					{
						cells(i*c+j).io.lane_in_1 := Mux(io.select(cnt1), cells(src_idx).io.lane_out_1, cells(i*c+j-1).io.lane_out_1)
					} else
					{
						cells(i*c+j).io.lane_in_1 := Mux(io.select(cnt1), cells(src_idx).io.lane_out_2, cells(i*c+j-1).io.lane_out_1)
					}
					cnt1 += 1
				}
			} else
			{
				cells(i*c+j).io.lane_in_1 := cells(i*c+j-1).io.lane_out_1
			}



			






			if (dict.contains((i, j, "b"))) // dst is bottom
			{
				val src = dict((i, j, "b"))
				if (src.size == 1)
				{
					val src_x = src(0)._1
					val src_y = src(0)._2
					val src_tb = src(0)._3
					val src_idx = src_x*c+src_y

					if (src_tb == "t")
					{
						cells(i*c+j).io.lane_in_2 := Mux(io.select(cnt1), cells(src_idx).io.lane_out_1, cells(i*c+j-1).io.lane_out_2)
					} else
					{
						cells(i*c+j).io.lane_in_2 := Mux(io.select(cnt1), cells(src_idx).io.lane_out_2, cells(i*c+j-1).io.lane_out_2)
					}
					cnt1 += 1
				}
			} else
			{
				cells(i*c+j).io.lane_in_2 := cells(i*c+j-1).io.lane_out_2
			}



		}
	}
	
	




	
	
	for (i <- 1 until r)
	{	
		for (j <- 0 until c)
		{
			cells(i*c+j).io.stage_in_1 := cells((i-1)*c+j).io.stage_out_1
		}
	}
	
	
	
	
	
	// cells to output
	for (i <- 0 until r)
	{
		io.lane_out_1(i) := cells(i*c+c-1).io.lane_out_1
		io.lane_out_2(i) := cells(i*c+c-1).io.lane_out_2
	}
	
	for (j <- 0 until c)
	{
		io.stage_out_1(j) := cells((r-1)*c+j).io.stage_out_1
	}
	
	for (i <- 0 until r)
	{
		for (j <- 0 until c)
		{
			io.accum_out(i*c+j) := cells(i*c+j).io.accum_out
		}
	}

	
}















class b_scan_pcu_8x6 extends Module {
	val r = 8
	val c = 6
	val io = new Bundle {
		val lane_in_1 = IO(Input(Vec(r, SInt(16.W))))
		val lane_in_2 = IO(Input(Vec(r, SInt(16.W))))
		val stage_in_1 = IO(Input(Vec(c, SInt(16.W))))
		val cell_mode = IO(Input(Vec(r*c, UInt(16.W))))
		val lane_out_1_select = IO(Input(Vec(r*c, Bool())))
		val select = IO(Input(Vec(20, Bool())))

		val lane_out_1 = IO(Output(Vec(r, SInt(16.W))))
		val lane_out_2 = IO(Output(Vec(r, SInt(16.W))))
		val stage_out_1 = IO(Output(Vec(c, SInt(16.W))))
		val accum_out = IO(Output(Vec(r*c, SInt(16.W))))
	}
	
	
	val cells = Seq.tabulate(r*c) {
		_ => Module(new cell())
	}

	// input to cells
	for (i <- 0 until r)
	{
		cells(i*c).io.lane_in_1 := io.lane_in_1(i)
		cells(i*c).io.lane_in_2 := io.lane_in_2(i)
	}
	
	for (j <- 0 until c)
	{
		cells(j).io.stage_in_1 := io.stage_in_1(j)
	}
	
	for (i <- 0 until r)
	{
		for (j <- 0 until c)
		{
			cells(i*c+j).io.cell_mode := io.cell_mode(i*c+j)
			cells(i*c+j).io.lane_out_1_select := io.lane_out_1_select(i*c+j)
		}
	}
	
	
	
	// cells to cells
	
	
	
	// connections for reduce and FFT
	var dict = Map.empty[(Int, Int, String), List[(Int, Int, String)]] // dst, src


	// reduce + b scan
	dict = dict + ((0, 1, "t") -> List((1, 0, "t")))
	dict = dict + ((2, 1, "t") -> List((3, 0, "t")))
	dict = dict + ((4, 1, "t") -> List((5, 0, "t")))
	dict = dict + ((6, 1, "t") -> List((7, 0, "t")))
	dict = dict + ((0, 2, "t") -> List((2, 1, "t")))
	dict = dict + ((4, 2, "t") -> List((6, 1, "t")))
	dict = dict + ((0, 3, "t") -> List((4, 2, "t")))

	dict = dict + ((4, 3, "t") -> List((0, 2, "t")))
	dict = dict + ((2, 4, "t") -> List((0, 3, "t")))
	dict = dict + ((0, 4, "t") -> List((2, 3, "t")))
	dict = dict + ((6, 4, "t") -> List((4, 3, "t")))
	dict = dict + ((4, 4, "t") -> List((6, 3, "t")))
	dict = dict + ((0, 5, "t") -> List((1, 4, "t")))
	dict = dict + ((1, 5, "t") -> List((0, 4, "t")))
	dict = dict + ((3, 5, "t") -> List((2, 4, "t")))
	dict = dict + ((2, 5, "t") -> List((3, 4, "t")))
	dict = dict + ((5, 5, "t") -> List((4, 4, "t")))
	dict = dict + ((4, 5, "t") -> List((5, 4, "t")))
	dict = dict + ((6, 5, "t") -> List((7, 4, "t")))
	dict = dict + ((7, 5, "t") -> List((6, 4, "t")))


	var cnt1 = 0
	var cnt2 = 0
	for (i <- 0 until r)
	{	
		for (j <- 1 until c)
		{
			if (dict.contains((i, j, "t"))) // dst is top
			{
				val src = dict((i, j, "t"))

				if (src.size == 1)
				{
					val src_x = src(0)._1
					val src_y = src(0)._2
					val src_tb = src(0)._3
					val src_idx = src_x*c+src_y

					if (src_tb == "t")
					{
						cells(i*c+j).io.lane_in_1 := Mux(io.select(cnt1), cells(src_idx).io.lane_out_1, cells(i*c+j-1).io.lane_out_1)
					} else
					{
						cells(i*c+j).io.lane_in_1 := Mux(io.select(cnt1), cells(src_idx).io.lane_out_2, cells(i*c+j-1).io.lane_out_1)
					}
					cnt1 += 1
				}
			} else
			{
				cells(i*c+j).io.lane_in_1 := cells(i*c+j-1).io.lane_out_1
			}



			






			if (dict.contains((i, j, "b"))) // dst is bottom
			{
				val src = dict((i, j, "b"))
				if (src.size == 1)
				{
					val src_x = src(0)._1
					val src_y = src(0)._2
					val src_tb = src(0)._3
					val src_idx = src_x*c+src_y

					if (src_tb == "t")
					{
						cells(i*c+j).io.lane_in_2 := Mux(io.select(cnt1), cells(src_idx).io.lane_out_1, cells(i*c+j-1).io.lane_out_2)
					} else
					{
						cells(i*c+j).io.lane_in_2 := Mux(io.select(cnt1), cells(src_idx).io.lane_out_2, cells(i*c+j-1).io.lane_out_2)
					}
					cnt1 += 1
				}
			} else
			{
				cells(i*c+j).io.lane_in_2 := cells(i*c+j-1).io.lane_out_2
			}



		}
	}
	
	




	
	
	for (i <- 1 until r)
	{	
		for (j <- 0 until c)
		{
			cells(i*c+j).io.stage_in_1 := cells((i-1)*c+j).io.stage_out_1
		}
	}
	
	
	
	
	
	// cells to output
	for (i <- 0 until r)
	{
		io.lane_out_1(i) := cells(i*c+c-1).io.lane_out_1
		io.lane_out_2(i) := cells(i*c+c-1).io.lane_out_2
	}
	
	for (j <- 0 until c)
	{
		io.stage_out_1(j) := cells((r-1)*c+j).io.stage_out_1
	}
	
	for (i <- 0 until r)
	{
		for (j <- 0 until c)
		{
			io.accum_out(i*c+j) := cells(i*c+j).io.accum_out
		}
	}

	
}







object PCU extends App {
    println(
        ChiselStage.emitSystemVerilog(
            gen = new b_scan_pcu_8x6,
            firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info")
        )
    )

	println(chisel3.BuildInfo.version)
}