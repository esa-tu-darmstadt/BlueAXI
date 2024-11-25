package AXI4_Lite;

import AXI4_Lite_Master :: *;
import AXI4_Lite_Slave :: *;
import AXI4_Lite_Types :: *;
import ClientServer :: *;
import GetPut :: *;
import Connectable :: *;

export AXI4_Lite :: *;
export AXI4_Lite_Master :: *;
export AXI4_Lite_Slave :: *;
export AXI4_Lite_Types :: *;

/*
========================
	AXI 4 Lite Master Combined
========================
*/

interface AXI4_Lite_Master_Fab#(numeric type addrwidth, numeric type datawidth);
	(* prefix="" *)
	interface AXI4_Lite_Master_Rd_Fab#(addrwidth, datawidth) rd;
	(* prefix="" *)
	interface AXI4_Lite_Master_Wr_Fab#(addrwidth, datawidth) wr;
endinterface

typedef Server#(AXI4_Lite_Read_Rq_Pkg#(addrwidth), AXI4_Lite_Read_Rs_Pkg#(datawidth)) AXI4_Lite_Read_Server#(numeric type addrwidth, numeric type datawidth);
typedef Server#(AXI4_Lite_Write_Rq_Pkg#(addrwidth, datawidth), AXI4_Lite_Write_Rs_Pkg) AXI4_Lite_Write_Server#(numeric type addrwidth, numeric type datawidth);

interface AXI4_Lite_Master#(numeric type addrwidth, numeric type datawidth);
	interface AXI4_Lite_Master_Fab#(addrwidth, datawidth) fab;
	interface AXI4_Lite_Read_Server#(addrwidth, datawidth) read;
	interface AXI4_Lite_Write_Server#(addrwidth, datawidth) write;
endinterface


module mkAXI4_Lite_Master#(Integer bufferSizeRead, Integer bufferSizeWrite)(AXI4_Lite_Master#(addrwidth, datawidth));
	AXI4_Lite_Master_Rd#(addrwidth, datawidth) rd_master <- mkAXI4_Lite_Master_Rd(bufferSizeRead);
	AXI4_Lite_Master_Wr#(addrwidth, datawidth) wr_master <- mkAXI4_Lite_Master_Wr(bufferSizeWrite);

	interface AXI4_Lite_Master_Fab fab;
		interface rd = rd_master.fab;
		interface wr = wr_master.fab;
	endinterface
	interface read = toGPServer(rd_master.request, rd_master.response);
	interface write = toGPServer(wr_master.request, wr_master.response);
endmodule

/*
========================
	AXI 4 Lite Slave Combined
========================
*/

interface AXI4_Lite_Slave_Fab#(numeric type addrwidth, numeric type datawidth);
	(* prefix="" *)
	interface AXI4_Lite_Slave_Rd_Fab#(addrwidth, datawidth) rd;
	(* prefix="" *)
	interface AXI4_Lite_Slave_Wr_Fab#(addrwidth, datawidth) wr;
endinterface


typedef Client#(AXI4_Lite_Read_Rq_Pkg#(addrwidth), AXI4_Lite_Read_Rs_Pkg#(datawidth)) AXI4_Lite_Read_Client#(numeric type addrwidth, numeric type datawidth);
typedef Client#(AXI4_Lite_Write_Rq_Pkg#(addrwidth, datawidth), AXI4_Lite_Write_Rs_Pkg) AXI4_Lite_Write_Client#(numeric type addrwidth, numeric type datawidth);

interface AXI4_Lite_Slave#(numeric type addrwidth, numeric type datawidth);
	interface AXI4_Lite_Slave_Fab#(addrwidth, datawidth) fab;
	interface AXI4_Lite_Read_Client#(addrwidth, datawidth) read;
	interface AXI4_Lite_Write_Client#(addrwidth, datawidth) write;
endinterface

module mkAXI4_Lite_Slave#(Integer bufferSizeRead, Integer bufferSizeWrite)(AXI4_Lite_Slave#(addrwidth, datawidth));
	AXI4_Lite_Slave_Rd#(addrwidth, datawidth) rd_slave <- mkAXI4_Lite_Slave_Rd(bufferSizeRead);
	AXI4_Lite_Slave_Wr#(addrwidth, datawidth) wr_slave <- mkAXI4_Lite_Slave_Wr(bufferSizeWrite);

	interface AXI4_Lite_Slave_Fab fab;
		interface rd = rd_slave.fab;
		interface wr = wr_slave.fab;
	endinterface
	interface read = toGPClient(rd_slave.request, rd_slave.response);
	interface write = toGPClient(wr_slave.request, wr_slave.response);
endmodule

/*
========================
	Connectable
========================
*/

instance Connectable#(AXI4_Lite_Master_Fab#(addrwidth, datawidth), AXI4_Lite_Slave_Fab#(addrwidth, datawidth));
    module mkConnection#(AXI4_Lite_Master_Fab#(addrwidth, datawidth) master, AXI4_Lite_Slave_Fab#(addrwidth, datawidth) slave)(Empty);
        mkConnection(master.rd, slave.rd);
        mkConnection(master.wr, slave.wr);
    endmodule
endinstance

instance Connectable#(AXI4_Lite_Master_Wr_Fab#(addrwidth, datawidth), AXI4_Lite_Slave_Wr_Fab#(addrwidth, datawidth));
	module mkConnection#(AXI4_Lite_Master_Wr_Fab#(addrwidth, datawidth) master, AXI4_Lite_Slave_Wr_Fab#(addrwidth, datawidth) slave)(Empty);
		rule forward1; master.pawready(slave.awready); endrule
		rule forward2; slave.pawvalid(master.awvalid); endrule
		rule forward3; slave.pawaddr(master.awaddr); endrule
		rule forward4; slave.pawprot(master.awprot); endrule
		rule forward5; master.pwready(slave.wready); endrule
		rule forward6; slave.pwvalid(master.wvalid); endrule
		rule forward7; slave.pwdata(master.wdata); endrule
		rule forward8; slave.pwstrb(master.wstrb); endrule
		rule forward9; master.pbvalid(slave.bvalid); endrule
		rule forward10; slave.pbready(master.bready); endrule
		rule forward11; master.pbresp(slave.bresp); endrule
	endmodule
endinstance

instance Connectable#(AXI4_Lite_Slave_Wr_Fab#(addrwidth, datawidth), AXI4_Lite_Master_Wr_Fab#(addrwidth, datawidth));
	module mkConnection#(AXI4_Lite_Slave_Wr_Fab#(addrwidth, datawidth) slave, AXI4_Lite_Master_Wr_Fab#(addrwidth, datawidth) master)(Empty);
		mkConnection(master, slave);
	endmodule
endinstance

instance Connectable#(AXI4_Lite_Master_Rd_Fab#(addrwidth, datawidth), AXI4_Lite_Slave_Rd_Fab#(addrwidth, datawidth));
	module mkConnection#(AXI4_Lite_Master_Rd_Fab#(addrwidth, datawidth) master, AXI4_Lite_Slave_Rd_Fab#(addrwidth, datawidth) slave)(Empty);
		rule forward1; slave.parvalid(master.arvalid); endrule
		rule forward2; master.parready(slave.arready); endrule
		rule forward3; slave.paraddr(master.araddr); endrule
		rule forward4; slave.parprot(master.arprot); endrule

		rule forward5; slave.prready(master.rready); endrule
		rule forward6; master.prvalid(slave.rvalid); endrule
		rule forward7; master.prdata(slave.rdata); endrule
		rule forward8; master.prresp(slave.rresp); endrule
	endmodule
endinstance

instance Connectable#(AXI4_Lite_Slave_Rd_Fab#(addrwidth, datawidth), AXI4_Lite_Master_Rd_Fab#(addrwidth, datawidth));
	module mkConnection#(AXI4_Lite_Slave_Rd_Fab#(addrwidth, datawidth) slave, AXI4_Lite_Master_Rd_Fab#(addrwidth, datawidth) master)(Empty);
		mkConnection(master, slave);
	endmodule
endinstance

endpackage