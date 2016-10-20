package AXI4_Lite_Master;

import GetPut :: *;
import FIFO :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import Connectable :: *;
import Clocks :: *;

// Project specific

import AXI4_Lite_Types :: *;
import AXI4_Lite_Slave :: *;

/*
========================
	AXI 4 Lite Master Read
========================
*/
(* always_ready, always_enabled *)
interface AXI4_Lite_Master_Rd_Fab#(numeric type addrwidth, numeric type datawidth);
  method Bool arvalid;
  (*prefix = ""*)method Action parready((*port="arready"*)Bool a);
  method Bit#(addrwidth) araddr;
  method AXI4_Lite_Prot arprot;

  method Bool rready;
  (*prefix = ""*)method Action prvalid((*port="rvalid"*)Bool v);
  (*prefix = ""*)method Action prdata((*port="rdata"*) Bit#(datawidth) d);
  (*prefix = ""*)method Action prresp((*port="rresp"*)AXI4_Lite_Response resp);
endinterface

interface AXI4_Lite_Master_Rd#(numeric type addrwidth, numeric type datawidth);
  (* prefix="" *)
  interface AXI4_Lite_Master_Rd_Fab#(addrwidth, datawidth) fab;
  interface Put#(AXI4_Lite_Read_Rq_Pkg#(addrwidth)) request;
  interface Get#(AXI4_Lite_Read_Rs_Pkg#(datawidth)) response;
endinterface

module mkAXI4_Lite_Master_Rd#(Integer bufferSize)(AXI4_Lite_Master_Rd#(addrwidth, datawidth));

    let isRst <- isResetAsserted();

	FIFOF#(AXI4_Lite_Read_Rq_Pkg#(addrwidth)) in = ?;
	FIFOF#(AXI4_Lite_Read_Rs_Pkg#(datawidth)) out = ?;

    if(bufferSize == 0) begin
        in <- mkBypassFIFOF();
        out <- mkBypassFIFOF();
    end else if(bufferSize == 1) begin
        in <- mkPipelineFIFOF();
        out <- mkPipelineFIFOF();
    end else if(bufferSize == 2) begin
        in <- mkFIFOF();
        out <- mkFIFOF();
    end else begin
        in <- mkSizedFIFOF(bufferSize);
        out <- mkSizedFIFOF(bufferSize);
    end

	Wire#(Bool) arreadyIn <- mkBypassWire();
	Wire#(Bit#(addrwidth)) araddrOut <- mkDWire(unpack(0));
	Wire#(AXI4_Lite_Prot) arprotOut <- mkDWire(UNPRIV_SECURE_DATA);

	rule deqIn if(!isRst && arreadyIn && in.notEmpty());
		in.deq();
	endrule

	rule forwardIn;
		araddrOut <= in.first().addr();
		arprotOut <= in.first().prot();
	endrule

	Wire#(Bool) rvalidIn <- mkBypassWire();
	Wire#(Bit#(datawidth)) rdataIn <- mkBypassWire();
	Wire#(AXI4_Lite_Response) rrespIn <- mkBypassWire();

	rule enqOut if(!isRst && rvalidIn && out.notFull());
		AXI4_Lite_Read_Rs_Pkg#(datawidth) t;
		t.data = rdataIn;
		t.resp = rrespIn;
		out.enq(t);
	endrule

	interface Put request = toPut(in);
	interface Get response = toGet(out);

	interface AXI4_Lite_Master_Rd_Fab fab;
	  	interface parready = arreadyIn._write;
		interface arvalid = !isRst && in.notEmpty();
  		interface araddr = araddrOut;
  		interface arprot = arprotOut;

  		interface rready = !isRst && out.notFull();
  		interface prvalid = rvalidIn._write;
  		interface prdata = rdataIn._write;
  		interface prresp = rrespIn._write;
	endinterface
endmodule

/*
========================
	AXI 4 Lite Master Write
========================
*/
(* always_ready, always_enabled *)
interface AXI4_Lite_Master_Wr_Fab#(numeric type addrwidth, numeric type datawidth);
	(*prefix=""*)method Action pawready((*port="awready"*) Bool r);
	method Bool awvalid;
	method Bit#(addrwidth) awaddr;
	method AXI4_Lite_Prot awprot;

	(*prefix=""*)method Action pwready((*port="wready"*)Bool r);
	method Bool wvalid;
	method Bit#(datawidth) wdata;
	method Bit#(TDiv#(datawidth, 8)) wstrb;

	(*prefix=""*)method Action pbvalid((*port="bvalid"*) Bool b);
	method Bool bready;
	(*prefix=""*)method Action pbresp((*port="bresp"*) AXI4_Lite_Response r);
endinterface

interface AXI4_Lite_Master_Wr#(numeric type addrwidth, numeric type datawidth);
  (* prefix="" *)
  interface AXI4_Lite_Master_Wr_Fab#(addrwidth, datawidth) fab;
  interface Put#(AXI4_Lite_Write_Rq_Pkg#(addrwidth, datawidth)) request;
  interface Get#(AXI4_Lite_Write_Rs_Pkg) response;
endinterface

module mkAXI4_Lite_Master_Wr#(Integer bufferSize)(AXI4_Lite_Master_Wr#(addrwidth, datawidth));

    let isRst <- isResetAsserted();

	FIFO#(AXI4_Lite_Write_Rq_Pkg#(addrwidth, datawidth)) in = ?;
	FIFOF#(AXI4_Lite_Write_Rs_Pkg) out = ?;

    if(bufferSize == 0) begin
        in <- mkBypassFIFO();
        out <- mkBypassFIFOF();
    end else if(bufferSize == 1) begin
        in <- mkPipelineFIFO();
        out <- mkPipelineFIFOF();
    end else if(bufferSize == 2) begin
        in <- mkFIFO();
        out <- mkFIFOF();
    end else begin
        in <- mkSizedFIFO(bufferSize);
        out <- mkSizedFIFOF(bufferSize);
    end

	FIFOF#(Tuple2#(Bit#(addrwidth), AXI4_Lite_Prot)) addrOut <- mkBypassFIFOF();
	FIFOF#(Tuple2#(Bit#(datawidth), Bit#(TDiv#(datawidth, 8)))) dataOut <- mkBypassFIFOF();

	rule splitAddrData;
		let p = in.first(); in.deq();
		addrOut.enq(tuple2(p.addr, p.prot));
		dataOut.enq(tuple2(p.data, p.strb));
	endrule

	Wire#(Bool) awreadyIn <- mkBypassWire();
	Wire#(Bit#(addrwidth)) awaddrOut <- mkDWire(0);
	Wire#(AXI4_Lite_Prot) awprotOut <- mkDWire(UNPRIV_SECURE_DATA);

	rule deqAddr if(!isRst && awreadyIn && addrOut.notEmpty());
		addrOut.deq();
	endrule

	rule forwardAddr;
		awaddrOut <= tpl_1(addrOut.first());
		awprotOut <= tpl_2(addrOut.first());
	endrule

	Wire#(Bool) wreadyIn <- mkBypassWire();
	Wire#(Bit#(datawidth)) wdataOut <- mkDWire(0);
	Wire#(Bit#(TDiv#(datawidth, 8))) wstrbOut <- mkDWire(0);

	rule deqData if(!isRst && wreadyIn && dataOut.notEmpty());
		dataOut.deq();
	endrule

	rule forwardData;
		wdataOut <= tpl_1(dataOut.first());
		wstrbOut <= tpl_2(dataOut.first());
	endrule

	Wire#(Bool) bvalidIn <- mkBypassWire();
	Wire#(AXI4_Lite_Response) brespIn <- mkBypassWire;

	rule forwardResp if(!isRst && bvalidIn && out.notFull);
		AXI4_Lite_Write_Rs_Pkg p;
		p.resp = brespIn;
		out.enq(p);
	endrule

	interface Put request = toPut(in);
	interface Get response = toGet(out);

	interface AXI4_Lite_Master_Wr_Fab fab;

		interface pawready = awreadyIn._write();
		interface awvalid = !isRst && addrOut.notEmpty();
		interface awaddr = awaddrOut;
		interface awprot = awprotOut;

		interface pwready = wreadyIn._write;
		interface wvalid = !isRst && dataOut.notEmpty();
		interface wdata = wdataOut;
		interface wstrb = wstrbOut;

		interface pbvalid = bvalidIn._write;
		interface bready = !isRst && out.notFull;
		interface pbresp = brespIn._write;
	endinterface
endmodule

/*
========================
	Connectable
========================
*/

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

module mkAXI4_Lite_Master_Rd_Dummy(AXI4_Lite_Master_Rd#(addrwidth, datawidth));
    interface AXI4_Lite_Master_Rd_Fab fab;
    	interface arvalid = False;
    	interface araddr = unpack(0);
    	interface arprot = unpack(0);

    	interface rready = False;
    endinterface
endmodule

module mkAXI4_Lite_Master_Wr_Dummy(AXI4_Lite_Master_Wr#(addrwidth, datawidth));
    interface AXI4_Lite_Master_Wr_Fab fab;
    	interface awvalid = False;
    	interface awaddr = unpack(0);
    	interface awprot = unpack(0);

    	interface wvalid = False;
    	interface wdata = unpack(0);
    	interface wstrb = unpack(0);

    	interface bready = False;
    endinterface
endmodule

function Action axi4_lite_write(AXI4_Lite_Master_Wr#(a, d) m,
								Bit#(a) addr, Bit#(d) data);
    action
        m.request.put(AXI4_Lite_Write_Rq_Pkg {
        	addr: addr,
        	prot: UNPRIV_SECURE_DATA,
        	data: data,
        	strb: unpack(-1)
        	});
    endaction
endfunction

function Action axi4_lite_write_strb(AXI4_Lite_Master_Wr#(a, d) m,
                                     Bit#(a) addr, Bit#(d) data, Bit#(TDiv#(d, 8)) strb);
    action
        m.request.put(AXI4_Lite_Write_Rq_Pkg {
            addr: addr,
            prot: UNPRIV_SECURE_DATA,
            data: data,
            strb: strb
            });
    endaction
endfunction

function ActionValue#(AXI4_Lite_Response) axi4_lite_write_response(AXI4_Lite_Master_Wr#(a, d) m);
    actionvalue
        let r <- m.response.get();
        return r.resp;
    endactionvalue
endfunction

function Action axi4_lite_read(AXI4_Lite_Master_Rd#(a, d) m, Bit#(a) addr);
    action
        m.request.put(AXI4_Lite_Read_Rq_Pkg {
        	    addr: addr,
        		prot: UNPRIV_SECURE_DATA
        	});
    endaction
endfunction

function ActionValue#(Bit#(d)) axi4_lite_read_response(AXI4_Lite_Master_Rd#(a, d) m);
    actionvalue
        let r <- m.response.get();
        return r.data;
    endactionvalue
endfunction

endpackage
