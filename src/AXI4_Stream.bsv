package AXI4_Stream;

import GetPut :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import Connectable :: *;
import Clocks :: *;

// Project specific

/*
=============
	Types
=============
*/

typedef struct {
		Bit#(datawidth) data;
		Bit#(userwidth) user;
		Bit#(TDiv#(datawidth, 8)) keep;
		Bit#(destwidth) dest;
		Bit#(idwidth) id;
		Bool last;
	} AXI4_Stream_Pkg#(numeric type datawidth, numeric type userwidth, numeric type idwidth, numeric type destwidth) deriving(Bits, Eq, FShow);

typedef AXI4_Stream_Pkg#(32, 1, 8, 4) AXI4_Stream_Pkg_32;
typedef AXI4_Stream_Pkg#(64, 1, 8, 4) AXI4_Stream_Pkg_64;
typedef AXI4_Stream_Pkg#(128, 1, 8, 4) AXI4_Stream_Pkg_128;
typedef AXI4_Stream_Pkg#(256, 1, 8, 4) AXI4_Stream_Pkg_256;
typedef AXI4_Stream_Pkg#(512, 1, 8, 4) AXI4_Stream_Pkg_512;

/*
========================
	AXI 4 Stream Read
========================
*/

(* always_ready, always_enabled *)
interface AXI4_Stream_Rd_Fab#(numeric type datawidth, numeric type userwidth, numeric type idwidth, numeric type destwidth);
  method Bool tready;
  (*prefix=""*)method Action ptvalid((*port="tvalid"*) Bool tvalid);
  (*prefix=""*)method Action ptdata((*port="tdata"*)Bit#(datawidth) data);
  (*prefix=""*)method Action ptuser((*port="tuser"*)Bit#(userwidth) user);
  (*prefix=""*)method Action ptkeep((*port="tkeep"*)Bit#(TDiv#(datawidth, 8)) keep);
  (*prefix=""*)method Action ptDest((*port="tDest"*)Bit#(destwidth) dest);
  (*prefix=""*)method Action ptid((*port="tid"*)Bit#(idwidth) id);
  (*prefix=""*)method Action ptlast((*port="tlast"*)Bool last);
endinterface

interface AXI4_Stream_Rd#(numeric type datawidth, numeric type userwidth, numeric type idwidth, numeric type destwidth);
  (* prefix="" *)
  interface AXI4_Stream_Rd_Fab#(datawidth, userwidth, idwidth, destwidth) fab;
  interface Get#(AXI4_Stream_Pkg#(datawidth, userwidth, idwidth, destwidth)) pkg;
endinterface

module mkAXI4_Stream_Rd#(Integer bufferSize)(AXI4_Stream_Rd#(datawidth, userwidth, idwidth, destwidth))
	provisos(Div#(datawidth, 8, keepwidth));

    let isRst <- isResetAsserted();

	Wire#(Bool) 					  tvalidIn <- mkBypassWire();
	Wire#(Bit#(datawidth)) 			  dataIn <- mkBypassWire();
	Wire#(Bit#(userwidth)) 			  userIn <- mkBypassWire();
	Wire#(Bit#(keepwidth))	 		  keepIn <- mkBypassWire();
	Wire#(Bit#(destwidth))		  destIn <- mkBypassWire();
	Wire#(Bit#(idwidth))		  idIn   <- mkBypassWire();
	Wire#(Bool) 					  lastIn <- mkBypassWire();

	FIFOF#(AXI4_Stream_Pkg#(datawidth, userwidth, idwidth, destwidth)) in;
	if(bufferSize == 0)
		in <- mkBypassFIFOF();
	else if(bufferSize == 1)
		in <- mkPipelineFIFOF();
	else
		in <- mkSizedFIFOF(bufferSize);

	rule writeFIFO if(!isRst && tvalidIn && in.notFull());
		AXI4_Stream_Pkg#(datawidth, userwidth, idwidth, destwidth) s;
		s.data = dataIn;
		s.user = userIn;
		s.keep = keepIn;
		s.dest = destIn;
		s.id   = idIn;
		s.last = lastIn;
		in.enq(s);
	endrule

	interface Get pkg = toGet(in);

	interface AXI4_Stream_Rd_Fab fab;
		interface tready = !isRst && in.notFull();
		interface ptvalid = tvalidIn._write;
		interface ptdata = dataIn._write;
		interface ptuser = userIn._write;
		interface ptkeep = keepIn._write;
		interface ptDest = destIn._write;
		interface ptid   = idIn._write;
		interface ptlast = lastIn._write;
	endinterface
endmodule

/*
========================
	AXI 4 Stream Write
========================
*/

(* always_ready, always_enabled *)
interface AXI4_Stream_Wr_Fab#(numeric type datawidth, numeric type userwidth, numeric type idwidth, numeric type destwidth);
  method Bool tvalid;
  (*prefix=""*)method Action ptready((*port="tready"*) Bool tr);
  method Bit#(datawidth) tdata;
  method Bool tlast;
  method Bit#(userwidth) tuser;
  method Bit#(TDiv#(datawidth, 8)) tkeep;
  method Bit#(destwidth) tDest;
  method Bit#(idwidth) tid;
endinterface

interface AXI4_Stream_Wr#(numeric type datawidth, numeric type userwidth, numeric type idwidth, numeric type destwidth);
  (* prefix="" *)
  interface AXI4_Stream_Wr_Fab#(datawidth, userwidth, idwidth, destwidth) fab;
  interface Put#(AXI4_Stream_Pkg#(datawidth, userwidth, idwidth, destwidth)) pkg;
endinterface

module mkAXI4_Stream_Wr#(Integer bufferSize)(AXI4_Stream_Wr#(datawidth, userwidth, idwidth, destwidth))
	provisos(Div#(datawidth, 8, keepwidth));

    let isRst <- isResetAsserted();

	FIFOF#(AXI4_Stream_Pkg#(datawidth, userwidth, idwidth, destwidth)) out;
	if(bufferSize == 0)
		out <- mkBypassFIFOF();
	else if(bufferSize == 1)
		out <- mkPipelineFIFOF();
	else
		out <- mkSizedFIFOF(bufferSize);

	Wire#(Bool) treadyIn <- mkBypassWire;
	Wire#(Bool) tvalidOut <- mkDWire(False);
	Wire#(Bit#(datawidth)) tdataOut <- mkDWire(unpack(0));
	Wire#(Bit#(userwidth)) tuserOut <- mkDWire(unpack(0));
	Wire#(Bit#(keepwidth)) tkeepOut <- mkDWire(unpack(0));
	Wire#(Bit#(destwidth)) tdestOut <- mkDWire(unpack(0));
	Wire#(Bit#(idwidth)) tidOut <- mkDWire(unpack(0));
	Wire#(Bool) tlastOut <- mkDWire(False);

	rule deqFIFO if(!isRst && treadyIn && out.notEmpty());
		out.deq();
	endrule

	rule writeOutputs;
		tdataOut <= out.first().data();
		tlastOut <= out.first().last();
		tuserOut <= out.first().user();
		tkeepOut <= out.first().keep();
		tdestOut <= out.first().dest();
		tidOut   <= out.first().id();
	endrule

	interface AXI4_Stream_Wr_Fab fab;
		interface tvalid = !isRst && out.notEmpty();
		interface ptready = treadyIn._write();
		interface tdata = tdataOut;
		interface tlast = tlastOut;
		interface tuser = tuserOut;
		interface tkeep = tkeepOut;
		interface tDest = tdestOut;
		interface tid = tidOut;
	endinterface

	interface Put pkg = toPut(out);
endmodule
/*
========================
DUMMY
========================
*/
module mkAXI4_Stream_Wr_Dummy(AXI4_Stream_Wr#(datawidth, userwidth, idwidth, destwidth));
	interface AXI4_Stream_Wr_Fab fab;
		interface tvalid = False;
		interface tdata = 0;
		interface tlast = False;
		interface tuser = 0;
		interface tkeep = 0;
		interface tDest = 0;
		interface tid = 0;
		method Action ptready(Bool tr);
		endmethod
	endinterface
	interface Put pkg;
		method Action put(AXI4_Stream_Pkg#(datawidth, userwidth, idwidth, destwidth) p);
		endmethod
	endinterface
endmodule

module mkAXI4_Stream_Rd_Dummy(AXI4_Stream_Rd#(datawidth, userwidth, idwidth, destwidth));
	interface AXI4_Stream_Rd_Fab fab;
		interface tready = False;
  		method Action ptvalid(Bool tvalid);
  		endmethod
  		method Action ptdata(Bit#(datawidth) data);
  		endmethod
  		method Action ptuser(Bit#(userwidth) user);
  		endmethod
  		method Action ptkeep(Bit#(TDiv#(datawidth, 8)) keep);
  		endmethod
  		method Action ptDest(Bit#(destwidth) dest);
  		endmethod
		method Action ptid(Bit#(idwidth) id);
		endmethod
  		method Action ptlast(Bool last);
  		endmethod
	endinterface

	interface Get pkg;
		method ActionValue#(AXI4_Stream_Pkg#(datawidth, userwidth, idwidth, destwidth)) get();
			return unpack(0);
		endmethod
	endinterface
endmodule

/*
========================
	Connectable
========================
*/

instance Connectable#(AXI4_Stream_Wr_Fab#(datawidth, userwidth, idwidth, destwidth), AXI4_Stream_Rd_Fab#(datawidth, userwidth, idwidth, destwidth));
	module mkConnection#(AXI4_Stream_Wr_Fab#(datawidth, userwidth, idwidth, destwidth) wr, AXI4_Stream_Rd_Fab#(datawidth, userwidth, idwidth, destwidth) rd)(Empty);

		rule forward1;
			wr.ptready(rd.tready());
		endrule
		rule forward2;
			rd.ptvalid(wr.tvalid());
		endrule
		rule forward3;
			rd.ptdata(wr.tdata());
		endrule
		rule forward4;
			rd.ptlast(wr.tlast());
		endrule
		rule forward5;
			rd.ptuser(wr.tuser());
		endrule
		rule forward6;
			rd.ptkeep(wr.tkeep());
		endrule
		rule forward7;
			rd.ptDest(wr.tDest());
		endrule
		rule forward8;
			rd.ptid(wr.tid());
		endrule

	endmodule
endinstance

endpackage
