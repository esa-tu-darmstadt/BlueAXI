package AXI4;

import AXI4_Master :: *;
import AXI4_Slave :: *;
import AXI4_Types :: *;

export AXI4_Master :: *;
export AXI4_Slave :: *;
export AXI4_Types :: *;
export AXI4 :: *;

import GetPut :: *;
import ClientServer :: *;
import Connectable :: *;

/*
========================
    AXI 4 Master Combined
========================
*/

interface AXI4_Master_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
    (* prefix="" *)
    interface AXI4_Master_Rd_Fab#(addrwidth, datawidth, id_width, user_width) rd;
	(* prefix="" *)
    interface AXI4_Master_Wr_Fab#(addrwidth, datawidth, id_width, user_width) wr;
endinterface

typedef Server#(AXI4_Read_Rq#(addrwidth, id_width, user_width), AXI4_Read_Rs#(datawidth, id_width ,user_width)) AXI4_Read_Server#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
interface AXI4_Write_Server#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
    interface Put#(AXI4_Write_Rq_Addr#(addrwidth, id_width, user_width)) request_addr;
    interface Put#(AXI4_Write_Rq_Data#(datawidth, user_width)) request_data;
    interface Get#(AXI4_Write_Rs#(id_width, user_width)) response;
endinterface

interface AXI4_Master#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
	interface AXI4_Master_Fab#(addrwidth, datawidth, id_width, user_width) fab;
	interface AXI4_Read_Server#(addrwidth, datawidth, id_width, user_width) read;
	interface AXI4_Write_Server#(addrwidth, datawidth, id_width, user_width) write;
endinterface


module mkAXI4_Master#(Integer bufferInRead, Integer bufferOutRead, Bool bramRead, Integer bufferInAddrWrite, Integer bufferInDataWrite, Integer bufferOutWrite, Bool bramWrite)(AXI4_Master#(addrwidth, datawidth, id_width, user_width));
	AXI4_Master_Rd#(addrwidth, datawidth, id_width, user_width) rd_master <- mkAXI4_Master_Rd(bufferInRead, bufferOutRead, bramRead);
	AXI4_Master_Wr#(addrwidth, datawidth, id_width, user_width) wr_master <- mkAXI4_Master_Wr(bufferInAddrWrite, bufferInDataWrite, bufferOutWrite, bramWrite);

	interface AXI4_Master_Fab fab;
		interface rd = rd_master.fab;
		interface wr = wr_master.fab;
	endinterface
	interface read = toGPServer(rd_master.request, rd_master.response);
	interface AXI4_Write_Server write;
        interface request_addr = wr_master.request_addr;
        interface request_data = wr_master.request_data;
        interface response = wr_master.response;
    endinterface
endmodule

/*
========================
	AXI 4 Slave Combined
========================
*/

interface AXI4_Slave_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
	(* prefix="" *)
	interface AXI4_Slave_Rd_Fab#(addrwidth, datawidth, id_width, user_width) rd;
	(* prefix="" *)
	interface AXI4_Slave_Wr_Fab#(addrwidth, datawidth, id_width, user_width) wr;
endinterface


typedef Client#(AXI4_Read_Rq#(addrwidth, id_width, user_width), AXI4_Read_Rs#(datawidth, id_width, user_width)) AXI4_Read_Client#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
interface AXI4_Write_Client#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
    interface Get#(AXI4_Write_Rq_Addr#(addrwidth, id_width, user_width)) request_addr;
    interface Get#(AXI4_Write_Rq_Data#(datawidth, user_width)) request_data;
    interface Put#(AXI4_Write_Rs#(id_width, user_width)) response;
endinterface

interface AXI4_Slave#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
	interface AXI4_Slave_Fab#(addrwidth, datawidth, id_width, user_width) fab;
	interface AXI4_Read_Client#(addrwidth, datawidth, id_width, user_width) read;
	interface AXI4_Write_Client#(addrwidth, datawidth, id_width, user_width) write;
endinterface


module mkAXI4_Slave#(Integer bufferInRead, Integer bufferOutRead, Integer bufferSizeAddrWrite, Integer bufferSizeDataWrite, Integer bufferSizeOutWrite)(AXI4_Slave#(addrwidth, datawidth, id_width, user_width));
	AXI4_Slave_Rd#(addrwidth, datawidth, id_width, user_width) rd_slave <- mkAXI4_Slave_Rd(bufferInRead, bufferOutRead);
	AXI4_Slave_Wr#(addrwidth, datawidth, id_width, user_width) wr_slave <- mkAXI4_Slave_Wr(bufferSizeAddrWrite, bufferSizeDataWrite, bufferSizeOutWrite);

	interface AXI4_Slave_Fab fab;
		interface rd = rd_slave.fab;
		interface wr = wr_slave.fab;
	endinterface
	interface read = toGPClient(rd_slave.request, rd_slave.response);
	interface AXI4_Write_Client write;
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

instance Connectable#(AXI4_Master_Fab#(addrwidth, datawidth, id_width, user_width), AXI4_Slave_Fab#(addrwidth, datawidth, id_width, user_width));
    module mkConnection#(AXI4_Master_Fab#(addrwidth, datawidth, id_width, user_width) master, AXI4_Slave_Fab#(addrwidth, datawidth, id_width, user_width) slave)(Empty);
        mkConnection(master.rd, slave.rd);
        mkConnection(master.wr, slave.wr);
    endmodule
endinstance

instance Connectable#(AXI4_Master_Rd_Fab#(addrwidth, datawidth, id_width, user_width), AXI4_Slave_Rd_Fab#(addrwidth, datawidth, id_width, user_width));
    module mkConnection#(AXI4_Master_Rd_Fab#(addrwidth, datawidth, id_width, user_width) master, AXI4_Slave_Rd_Fab#(addrwidth, datawidth, id_width, user_width) slave)(Empty);
        rule forward1; master.parready(slave.arready); endrule
        rule forward2; slave.parvalid(master.arvalid); endrule
        rule forward3;
            slave.parchannel(master.arid, master.araddr, master.arlen, master.arsize, master.arburst,
                master.arlock, master.arcache, master.arprot, master.arqos, master.arregion, master.aruser);
        endrule

        rule forward4; master.prvalid(slave.rvalid); endrule
        rule forward5; slave.prready(master.rready); endrule

        rule forward6;
            master.prchannel(slave.rid, slave.rdata, slave.rresp, slave.rlast, slave.ruser);
        endrule
    endmodule
endinstance

instance Connectable#(AXI4_Slave_Rd_Fab#(addrwidth, datawidth, id_width, user_width), AXI4_Master_Rd_Fab#(addrwidth, datawidth, id_width, user_width));
    module mkConnection#(AXI4_Slave_Rd_Fab#(addrwidth, datawidth, id_width, user_width) slave, AXI4_Master_Rd_Fab#(addrwidth, datawidth, id_width, user_width) master)(Empty);
        mkConnection(master, slave);
    endmodule
endinstance

instance Connectable#(AXI4_Master_Wr_Fab#(addrwidth, datawidth, id_width, user_width), AXI4_Slave_Wr_Fab#(addrwidth, datawidth, id_width, user_width));
    module mkConnection#(AXI4_Master_Wr_Fab#(addrwidth, datawidth, id_width, user_width) master, AXI4_Slave_Wr_Fab#(addrwidth, datawidth, id_width, user_width) slave)(Empty);
        rule forward1; master.pawready(slave.awready); endrule
        rule forward2; slave.pawvalid(master.awvalid); endrule
        rule forward3; slave.pawchannel(master.awid, master.awaddr, master.awlen, master.awsize, master.awburst, master.awlock, master.awcache, master.awprot, master.awqos, master.awregion, master.awuser); endrule

        rule forward4; master.pwready(slave.wready); endrule
        rule forward5; slave.pwvalid(master.wvalid); endrule
        rule forward6; slave.pwchannel(master.wdata, master.wstrb, master.wlast, master.wuser); endrule

        rule forward7; master.pbvalid(slave.bvalid); endrule
        rule forward8; slave.pbready(master.bready); endrule
        rule forward9; master.bin(slave.bresp, slave.bid, slave.buser); endrule
    endmodule
endinstance

instance Connectable#(AXI4_Slave_Wr_Fab#(addrwidth, datawidth, id_width, user_width), AXI4_Master_Wr_Fab#(addrwidth, datawidth, id_width, user_width));
    module mkConnection#(AXI4_Slave_Wr_Fab#(addrwidth, datawidth, id_width, user_width) slave, AXI4_Master_Wr_Fab#(addrwidth, datawidth, id_width, user_width) master)(Empty);
        mkConnection(master, slave);
    endmodule
endinstance


module mkTestIfc(TestIfc);
    AXI4_Master_Wr#(32, 32, 1, 1) master_wr <- mkAXI4_Master_Wr(2, 2, 2, True);
    AXI4_Master_Rd#(32, 32, 1, 1) master_rd <- mkAXI4_Master_Rd(2, 2, True);

    AXI4_Slave_Wr#(32, 32, 1, 1) slave_wr <- mkAXI4_Slave_Wr(2, 2, 2);
    AXI4_Slave_Rd#(32, 32, 1, 1) slave_rd <- mkAXI4_Slave_Rd(2, 2);

    mkConnection(master_rd.fab, slave_rd.fab);
    mkConnection(master_wr.fab, slave_wr.fab);

    //interface m_wr = master_wr.fab;
    //interface m_rd = master_rd.fab;

    //interface s_wr = slave_wr.fab;
    //interface s_rd = slave_rd.fab;
endmodule

endpackage