package AXI3;

import GetPut :: *;
import Connectable :: *;
import ClientServer :: *;

import AXI3_Types :: *;
import AXI3_Slave :: *;
import AXI3_Master :: *;

export AXI3_Types :: *;
export AXI3_Slave :: *;
export AXI3_Master :: *;
export AXI3 :: *;

/*
========================
    AXI 3 Master Combined
========================
*/

interface AXI3_Master_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
    (* prefix="" *)
    interface AXI3_Master_Rd_Fab#(addrwidth, datawidth, id_width) rd;
	(* prefix="" *)
    interface AXI3_Master_Wr_Fab#(addrwidth, datawidth, id_width) wr;
endinterface

typedef Server#(AXI3_Read_Rq#(addrwidth, id_width), AXI3_Read_Rs#(datawidth, id_width)) AXI3_Read_Server#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
interface AXI3_Write_Server#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
    interface Put#(AXI3_Write_Rq_Addr#(addrwidth, id_width)) request_addr;
    interface Put#(AXI3_Write_Rq_Data#(datawidth, id_width)) request_data;
    interface Get#(AXI3_Write_Rs#(id_width)) response;
endinterface

interface AXI3_Master#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
	interface AXI3_Master_Fab#(addrwidth, datawidth, id_width) fab;
	interface AXI3_Read_Server#(addrwidth, datawidth, id_width) read;
	interface AXI3_Write_Server#(addrwidth, datawidth, id_width) write;
endinterface


module mkAXI3_Master#(Integer bufferInRead, Integer bufferOutRead, Bool bramRead, Integer bufferInAddrWrite, Integer bufferInDataWrite, Integer bufferOutWrite, Bool bramWrite)(AXI3_Master#(addrwidth, datawidth, id_width));
	AXI3_Master_Rd#(addrwidth, datawidth, id_width) rd_master <- mkAXI3_Master_Rd(bufferInRead, bufferOutRead, bramRead);
	AXI3_Master_Wr#(addrwidth, datawidth, id_width) wr_master <- mkAXI3_Master_Wr(bufferInAddrWrite, bufferInDataWrite, bufferOutWrite, bramWrite);

	interface AXI3_Master_Fab fab;
		interface rd = rd_master.fab;
		interface wr = wr_master.fab;
	endinterface
	interface read = toGPServer(rd_master.request, rd_master.response);
	interface AXI3_Write_Server write;
        interface request_addr = wr_master.request_addr;
        interface request_data = wr_master.request_data;
        interface response = wr_master.response;
    endinterface
endmodule

/*
========================
	AXI 3 Slave Combined
========================
*/

interface AXI3_Slave_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
	(* prefix="" *)
	interface AXI3_Slave_Rd_Fab#(addrwidth, datawidth, id_width) rd;
	(* prefix="" *)
	interface AXI3_Slave_Wr_Fab#(addrwidth, datawidth, id_width) wr;
endinterface


typedef Client#(AXI3_Read_Rq#(addrwidth, id_width), AXI3_Read_Rs#(datawidth, id_width)) AXI3_Read_Client#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
interface AXI3_Write_Client#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
    interface Get#(AXI3_Write_Rq_Addr#(addrwidth, id_width)) request_addr;
    interface Get#(AXI3_Write_Rq_Data#(datawidth, id_width)) request_data;
    interface Put#(AXI3_Write_Rs#(id_width)) response;
endinterface

interface AXI3_Slave#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
	interface AXI3_Slave_Fab#(addrwidth, datawidth, id_width) fab;
	interface AXI3_Read_Client#(addrwidth, datawidth, id_width) read;
	interface AXI3_Write_Client#(addrwidth, datawidth, id_width) write;
endinterface


module mkAXI3_Slave#(Integer bufferInRead, Integer bufferOutRead, Integer bufferSizeAddrWrite, Integer bufferSizeDataWrite, Integer bufferSizeOutWrite)(AXI3_Slave#(addrwidth, datawidth, id_width));
	AXI3_Slave_Rd#(addrwidth, datawidth, id_width) rd_slave <- mkAXI3_Slave_Rd(bufferInRead, bufferOutRead);
	AXI3_Slave_Wr#(addrwidth, datawidth, id_width) wr_slave <- mkAXI3_Slave_Wr(bufferSizeAddrWrite, bufferSizeDataWrite, bufferSizeOutWrite);

	interface AXI3_Slave_Fab fab;
		interface rd = rd_slave.fab;
		interface wr = wr_slave.fab;
	endinterface
	interface read = toGPClient(rd_slave.request, rd_slave.response);
	interface AXI3_Write_Client write;
        interface request_addr = wr_slave.request_addr;
        interface request_data = wr_slave.request_data;
        interface response = wr_slave.response;
    endinterface
endmodule

/*
========================
    Connectable
========================
*/
instance Connectable#(AXI3_Master_Fab#(addrwidth, datawidth, id_width), AXI3_Slave_Fab#(addrwidth, datawidth, id_width));
    module mkConnection#(AXI3_Master_Fab#(addrwidth, datawidth, id_width) master, AXI3_Slave_Fab#(addrwidth, datawidth, id_width) slave)(Empty);
        mkConnection(master.rd, slave.rd);
        mkConnection(master.wr, slave.wr);
    endmodule
endinstance

instance Connectable#(AXI3_Master_Rd_Fab#(addrwidth, datawidth, id_width), AXI3_Slave_Rd_Fab#(addrwidth, datawidth, id_width));
    module mkConnection#(AXI3_Master_Rd_Fab#(addrwidth, datawidth, id_width) master, AXI3_Slave_Rd_Fab#(addrwidth, datawidth, id_width) slave)(Empty);
        rule forward1; master.parready(slave.arready); endrule
        rule forward2; slave.parvalid(master.arvalid); endrule
        rule forward3;
            slave.parchannel(master.arid, master.araddr, master.arlen, master.arsize, master.arburst,
                master.arlock, master.arcache, master.arprot);
        endrule

        rule forward4; master.prvalid(slave.rvalid); endrule
        rule forward5; slave.prready(master.rready); endrule

        rule forward6;
            master.prchannel(slave.rid, slave.rdata, slave.rresp, slave.rlast);
        endrule
    endmodule
endinstance

instance Connectable#(AXI3_Slave_Rd_Fab#(addrwidth, datawidth, id_width), AXI3_Master_Rd_Fab#(addrwidth, datawidth, id_width));
    module mkConnection#(AXI3_Slave_Rd_Fab#(addrwidth, datawidth, id_width) slave, AXI3_Master_Rd_Fab#(addrwidth, datawidth, id_width) master)(Empty);
        mkConnection(master, slave);
    endmodule
endinstance

instance Connectable#(AXI3_Master_Wr_Fab#(addrwidth, datawidth, id_width), AXI3_Slave_Wr_Fab#(addrwidth, datawidth, id_width));
    module mkConnection#(AXI3_Master_Wr_Fab#(addrwidth, datawidth, id_width) master, AXI3_Slave_Wr_Fab#(addrwidth, datawidth, id_width) slave)(Empty);
        rule forward1; master.pawready(slave.awready); endrule
        rule forward2; slave.pawvalid(master.awvalid); endrule
        rule forward3; slave.pawchannel(master.awid, master.awaddr, master.awlen, master.awsize, master.awburst, master.awlock, master.awcache, master.awprot); endrule

        rule forward4; master.pwready(slave.wready); endrule
        rule forward5; slave.pwvalid(master.wvalid); endrule
        rule forward6; slave.pwchannel(master.wid, master.wdata, master.wstrb, master.wlast); endrule

        rule forward7; master.pbvalid(slave.bvalid); endrule
        rule forward8; slave.pbready(master.bready); endrule
        rule forward9; master.bin(slave.bresp, slave.bid); endrule
    endmodule
endinstance

instance Connectable#(AXI3_Slave_Wr_Fab#(addrwidth, datawidth, id_width), AXI3_Master_Wr_Fab#(addrwidth, datawidth, id_width));
    module mkConnection#(AXI3_Slave_Wr_Fab#(addrwidth, datawidth, id_width) slave, AXI3_Master_Wr_Fab#(addrwidth, datawidth, id_width) master)(Empty);
        mkConnection(master, slave);
    endmodule
endinstance

endpackage