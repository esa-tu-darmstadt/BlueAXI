package AXI4_Lite_Slave;

import GetPut :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import Connectable :: *;
import Clocks :: *;

// Project specific
import AXI4_Lite_Types :: *;

/*
========================
	AXI 4 Lite Slave Read
========================
*/

(* always_ready, always_enabled *)
interface AXI4_Lite_Slave_Rd_Fab#(numeric type addrwidth, numeric type datawidth);
  method Bool arready;
  (* prefix=""*)method Action parvalid((*port="arvalid"*)Bool arvalid);
  (* prefix=""*)method Action paraddr((*port="araddr"*)Bit#(addrwidth) a);
  (* prefix=""*)method Action parprot((* port="arprot" *)AXI4_Lite_Prot p);

  method Bool rvalid;
  (*prefix=""*)method Action prready((*port="rready"*)Bool rready);
  method Bit#(datawidth) rdata();
  method AXI4_Lite_Response rresp();
endinterface

interface AXI4_Lite_Slave_Rd#(numeric type addrwidth, numeric type datawidth);
  (* prefix="" *)
  interface AXI4_Lite_Slave_Rd_Fab#(addrwidth, datawidth) fab;
  interface Get#(AXI4_Lite_Read_Rq_Pkg#(addrwidth)) request;
  method AXI4_Lite_Read_Rq_Pkg#(addrwidth) first();
  interface Put#(AXI4_Lite_Read_Rs_Pkg#(datawidth)) response;
endinterface

module mkAXI4_Lite_Slave_Rd#(Integer bufferSize)(AXI4_Lite_Slave_Rd#(addrwidth, datawidth));

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

	Wire#(Bool)           arvalidIn <- mkBypassWire();
	Wire#(Bit#(addrwidth)) araddrIn <- mkBypassWire();
	Wire#(AXI4_Lite_Prot)  arprotIn <- mkBypassWire();

	rule addrInWrite if(!isRst && arvalidIn && in.notFull());
		AXI4_Lite_Read_Rq_Pkg#(addrwidth) req;
		req.addr = araddrIn;
		req.prot = arprotIn;
		in.enq(req);
	endrule

	Wire#(Bool)           rreadyIn <- mkBypassWire();
	Wire#(Bit#(datawidth)) rdataOut <- mkDWire(unpack(0));
	Wire#(AXI4_Lite_Response) rrespOut <- mkDWire(OKAY);

	rule deqOut if(!isRst && rreadyIn && out.notEmpty());
		out.deq();
	endrule

	rule putOutData;
		rdataOut <= out.first().data;
		rrespOut <= out.first().resp;
	endrule

	method AXI4_Lite_Read_Rq_Pkg#(addrwidth) first() = in.first();

	interface Get request = toGet(in);
	interface Put response = toPut(out);

	interface AXI4_Lite_Slave_Rd_Fab fab;

		interface arready = !isRst && in.notFull();
		interface parvalid = arvalidIn._write();
  		interface paraddr = araddrIn._write();
  		interface parprot = arprotIn._write();

  		interface prready = rreadyIn._write;
  		interface rvalid = !isRst && out.notEmpty();
  		interface rdata = rdataOut;
  		interface rresp = rrespOut;
	endinterface
endmodule

/*
========================
	AXI 4 Lite Slave Write
========================
*/
(* always_ready, always_enabled *)
interface AXI4_Lite_Slave_Wr_Fab#(numeric type addrwidth, numeric type datawidth);
  method Bool awready;
  (* prefix=""*)method Action pawvalid((*port="awvalid"*)Bool awvalid);
  (* prefix=""*)method Action pawaddr((*port="awaddr"*)Bit#(addrwidth) a);
  (* prefix=""*)method Action pawprot((* port="awprot" *)AXI4_Lite_Prot p);

  method Bool wready;
  (* prefix=""*)method Action pwvalid((*port="wvalid"*)Bool wvalid);
  (* prefix=""*)method Action pwdata((*port="wdata"*)Bit#(datawidth) a);
  (* prefix=""*)method Action pwstrb((* port="wstrb" *)Bit#(TDiv#(datawidth, 8)) p);

  method Bool bvalid;
  (*prefix=""*)method Action pbready((*port="bready"*)Bool b);
  method AXI4_Lite_Response bresp();
endinterface

interface AXI4_Lite_Slave_Wr#(numeric type addrwidth, numeric type datawidth);
  (* prefix="" *)
  interface AXI4_Lite_Slave_Wr_Fab#(addrwidth, datawidth) fab;
  interface Get#(AXI4_Lite_Write_Rq_Pkg#(addrwidth, datawidth)) request;
  method AXI4_Lite_Write_Rq_Pkg#(addrwidth, datawidth) first();
  interface Put#(AXI4_Lite_Write_Rs_Pkg) response;
endinterface

module mkAXI4_Lite_Slave_Wr#(Integer bufferSize)(AXI4_Lite_Slave_Wr#(addrwidth, datawidth));

    let isRst <- isResetAsserted();

	FIFOF#(AXI4_Lite_Write_Rq_Pkg#(addrwidth, datawidth)) in = ?;
	FIFOF#(AXI4_Lite_Write_Rs_Pkg) out = ?;

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

	FIFOF#(AXI4_Lite_Read_Rq_Pkg#(addrwidth)) addrIn <- mkBypassFIFOF();
	FIFOF#(Tuple2#(Bit#(datawidth), Bit#(TDiv#(datawidth, 8)))) dataIn <- mkBypassFIFOF();

	rule mergeAddrData;
		AXI4_Lite_Write_Rq_Pkg#(addrwidth, datawidth) p;
		p.addr = addrIn.first().addr; addrIn.deq();
		p.data = tpl_1(dataIn.first()); dataIn.deq();
		p.strb = tpl_2(dataIn.first());
		p.prot = addrIn.first().prot;
		in.enq(p);
	endrule

	Wire#(Bool)           awvalidIn <- mkBypassWire();
	Wire#(Bit#(addrwidth)) awaddrIn <- mkBypassWire();
	Wire#(AXI4_Lite_Prot)  awprotIn <- mkBypassWire();

	rule addrInWrite if(!isRst && awvalidIn && addrIn.notFull());
		AXI4_Lite_Read_Rq_Pkg#(addrwidth) req;
		req.addr = awaddrIn;
		req.prot = awprotIn;
		addrIn.enq(req);
	endrule

	Wire#(Bool)           wvalidIn <- mkBypassWire();
	Wire#(Bit#(datawidth)) wdataIn <- mkBypassWire();
	Wire#(Bit#(TDiv#(datawidth, 8)))  wstrbIn <- mkBypassWire();

	rule dataInWrite if(!isRst && wvalidIn && dataIn.notFull());
		dataIn.enq(tuple2(wdataIn, wstrbIn));
	endrule

	Wire#(Bool) breadyIn <- mkBypassWire();
	Wire#(AXI4_Lite_Response)  brespOut <- mkDWire(OKAY);

	rule outWrite if(!isRst && breadyIn && out.notEmpty());
		out.deq();
	endrule

	rule outForward;
		brespOut <= out.first().resp();
	endrule

	method AXI4_Lite_Write_Rq_Pkg#(addrwidth, datawidth) first() = in.first();
	interface Get request = toGet(in);
	interface Put response = toPut(out);

	interface AXI4_Lite_Slave_Wr_Fab fab;
		interface awready = !isRst && addrIn.notFull();
		interface pawvalid = awvalidIn._write();
  		interface pawaddr = awaddrIn._write();
  		interface pawprot = awprotIn._write();

		interface wready = !isRst && dataIn.notFull();
		interface pwvalid = wvalidIn._write();
  		interface pwdata = wdataIn._write();
  		interface pwstrb = wstrbIn._write();

  		interface bvalid = !isRst && out.notEmpty();
  		interface bresp = brespOut;
  		interface pbready = breadyIn._write();

	endinterface
endmodule

module mkAXI4_Lite_Slave_Rd_Dummy(AXI4_Lite_Slave_Rd#(addrwidth, datawidth));
    interface AXI4_Lite_Slave_Rd_Fab fab;

        interface arready = False;

        interface rvalid = False;
        interface rdata = unpack(0);
        interface rresp = unpack(0);
    endinterface
endmodule

module mkAXI4_Lite_Slave_Wr_Dummy(AXI4_Lite_Slave_Wr#(addrwidth, datawidth));
    interface AXI4_Lite_Slave_Wr_Fab fab;
        interface awready = False;

        interface wready = False;

        interface bvalid = False;
        interface bresp = unpack(0);
    endinterface
endmodule

endpackage
