package AXI3_Master;

import GetPut :: *;
import FIFO :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import BRAMFIFO :: *;
import Connectable :: *;
import Clocks :: *;

// Project specific

import AXI3_Types :: *;
import AXI3_Slave :: *;

/*
========================
    AXI 4 Master Read
========================
*/

(* always_ready, always_enabled *)
interface AXI3_Master_Rd_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
  method Bool arvalid;
  (*prefix = ""*)method Action parready((*port="arready"*)Bool a);
  method Bit#(id_width) arid;
  method Bit#(addrwidth) araddr;
  method UInt#(AXI3_BURST_SIZE) arlen;
  method AXI3_BurstSize arsize;
  method AXI3_BurstType arburst;
  method AXI3_Lock arlock;
  method AXI3_Read_Cache arcache;
  method AXI3_Prot arprot;

  method Bool rready;
  (*prefix = ""*)method Action prvalid((*port="rvalid"*)Bool v);
  (*prefix = ""*)method Action prchannel((*port="rid"*)Bit#(id_width) id,
                                         (*port="rdata"*) Bit#(datawidth) d,
                                         (*port="rresp"*)AXI3_Response resp,
                                         (*port="rlast"*)Bool last);
endinterface

interface AXI3_Master_Rd#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
  (* prefix="" *)
  interface AXI3_Master_Rd_Fab#(addrwidth, datawidth, id_width) fab;
  interface Put#(AXI3_Read_Rq#(addrwidth, id_width)) request;
  interface Get#(AXI3_Read_Rs#(datawidth, id_width)) response;
  interface AXI3_Read_Rs#(datawidth, id_width) snoop;
endinterface

module mkAXI3_Master_Rd#(Integer bufferIn, Integer bufferOut, Bool bram)(AXI3_Master_Rd#(addrwidth, datawidth, id_width));

    let isRst <- isResetAsserted();

    FIFOF#(AXI3_Read_Rq#(addrwidth, id_width)) in = ?;
    if(bufferIn == 0)
        in <- mkBypassFIFOF();
    else if(bufferIn == 1)
        in <- mkPipelineFIFOF();
    else if(bram)
        in <- mkSizedBRAMFIFOF(bufferIn);
    else
        in <- mkSizedFIFOF(bufferIn);

    FIFOF#(AXI3_Read_Rs#(datawidth, id_width)) out <- mkSizedFIFOF(bufferOut);
    if(bufferOut == 0)
        out <- mkBypassFIFOF();
    else if(bufferOut == 1)
        out <- mkPipelineFIFOF();
    else if(bram)
        out <- mkSizedBRAMFIFOF(bufferOut);
    else
        out <- mkSizedFIFOF(bufferOut);


    Wire#(Bool) arreadyIn <- mkBypassWire();
    Wire#(Bit#(id_width)) warid <- mkDWire(unpack(0));
    Wire#(Bit#(addrwidth)) waraddr <- mkDWire(unpack(0));
    Wire#(UInt#(AXI3_BURST_SIZE)) warlen <- mkDWire(unpack(0));
    Wire#(AXI3_BurstSize) warsize <- mkDWire(unpack(0));
    Wire#(AXI3_BurstType) warburst <- mkDWire(unpack(0));
    Wire#(AXI3_Lock) warlock <- mkDWire(unpack(0));
    Wire#(AXI3_Read_Cache) warcache <- mkDWire(unpack(0));
    Wire#(AXI3_Prot) warprot <- mkDWire(unpack(0));

    rule deqIn if(!isRst && arreadyIn && in.notEmpty());
        in.deq();
    endrule

    rule forwardIn;
        warid <= in.first().id();
        waraddr <= in.first().addr();
        warlen <= in.first().burst_length();
        warsize <= in.first().burst_size();
        warburst <= in.first().burst_type();
        warlock <= in.first().lock();
        warcache <= in.first().cache();
        warprot <= in.first().prot();
    endrule

    Wire#(Bool) rvalidIn <- mkBypassWire();
    Wire#(AXI3_Read_Rs#(datawidth, id_width)) rinpkg <- mkBypassWire();

    function Action readChannel(Bit#(id_width) id, Bit#(datawidth) d, AXI3_Response resp, Bool last);
        action
            rinpkg <= AXI3_Read_Rs {id: id, data: d, last: last, resp: resp};
        endaction
    endfunction

    rule enqOut if(!isRst && rvalidIn && out.notFull());
        out.enq(rinpkg);
    endrule

    interface Put request = toPut(in);
    interface Get response = toGet(out);

    interface AXI3_Read_Rs snoop = out.first();

    interface AXI3_Master_Rd_Fab fab;
        interface parready = arreadyIn._write;
        interface arvalid = !isRst && in.notEmpty();
        interface arid = warid;
        interface araddr = waraddr;
        interface arlen = warlen;
        interface arsize = warsize;
        interface arburst = warburst ;
        interface arlock = warlock;
        interface arcache = warcache ;
        interface arprot = warprot;

        interface rready = !isRst && out.notFull();
        interface prvalid = rvalidIn._write;
        interface prchannel = readChannel;
    endinterface
endmodule

/*
========================
    AXI 4 Lite Master Write
========================
*/
(* always_ready, always_enabled *)
interface AXI3_Master_Wr_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
    (*prefix=""*)method Action pawready((*port="awready"*) Bool r);
    method Bool awvalid;
    method Bit#(id_width) awid;
    method Bit#(addrwidth) awaddr;
    method UInt#(AXI3_BURST_SIZE) awlen;
    method AXI3_BurstSize awsize;
    method AXI3_BurstType awburst;
    method AXI3_Lock awlock;
    method AXI3_Write_Cache awcache;
    method AXI3_Prot awprot;

    (*prefix=""*)method Action pwready((*port="wready"*)Bool r);
    method Bool wvalid;
    method Bit#(id_width) wid;
    method Bit#(datawidth) wdata;
    method Bit#(TDiv#(datawidth, 8)) wstrb;
    method Bool wlast;

    (*prefix=""*)method Action pbvalid((*port="bvalid"*) Bool b);
    method Bool bready;
    (*prefix=""*)method Action bin((*port="bresp"*) AXI3_Response r, (*port="bid"*) Bit#(id_width) bid);
endinterface

interface AXI3_Master_Wr#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
  (* prefix="" *)
  interface AXI3_Master_Wr_Fab#(addrwidth, datawidth, id_width) fab;
  interface Put#(AXI3_Write_Rq_Addr#(addrwidth, id_width)) request_addr;
  interface Put#(AXI3_Write_Rq_Data#(datawidth, id_width)) request_data;
  interface Get#(AXI3_Write_Rs#(id_width)) response;
  method AXI3_Write_Rs#(id_width) snoop;
endinterface

module mkAXI3_Master_Wr#(Integer bufferInAddr, Integer bufferInData, Integer bufferOut, Bool bram)(AXI3_Master_Wr#(addrwidth, datawidth, id_width));
    let isRst <- isResetAsserted();

    FIFOF#(AXI3_Write_Rq_Addr#(addrwidth, id_width)) in_addr = ?;
    if(bufferInAddr == 0)
        in_addr <- mkBypassFIFOF();
    else if(bufferInAddr == 1)
        in_addr <- mkPipelineFIFOF();
    else if(bram)
        in_addr <- mkSizedBRAMFIFOF(bufferInAddr);
    else
        in_addr <- mkSizedFIFOF(bufferInAddr);

    FIFOF#(AXI3_Write_Rq_Data#(datawidth, id_width)) in_data = ?;
    if(bufferInData == 0)
        in_data <- mkBypassFIFOF();
    else if(bufferInData == 1)
        in_data <- mkPipelineFIFOF();
    else if(bram)
        in_data <- mkSizedBRAMFIFOF(bufferInData);
    else
        in_data <- mkSizedFIFOF(bufferInData);

    FIFOF#(AXI3_Write_Rs#(id_width)) out = ?;
    if(bufferOut == 0)
        out <- mkBypassFIFOF();
    else if(bufferOut == 1)
        out <- mkPipelineFIFOF();
    else if(bram)
        out <- mkSizedBRAMFIFOF(bufferOut);
    else
        out <- mkSizedFIFOF(bufferOut);

    Wire#(Bool)             wawready  <- mkBypassWire();
    Wire#(Bit#(id_width))   wawid     <- mkDWire(unpack(0));
    Wire#(Bit#(addrwidth))  wawaddr   <- mkDWire(unpack(0));
    Wire#(UInt#(AXI3_BURST_SIZE))         wawlen    <- mkDWire(unpack(0));
    Wire#(AXI3_BurstSize)   wawsize   <- mkDWire(unpack(0));
    Wire#(AXI3_BurstType)   wawburst  <- mkDWire(unpack(0));
    Wire#(AXI3_Lock)        wawlock   <- mkDWire(unpack(0));
    Wire#(AXI3_Write_Cache) wawcache  <- mkDWire(unpack(0));
    Wire#(AXI3_Prot)        wawprot   <- mkDWire(unpack(0));

    rule deqInAddr if(!isRst && wawready && in_addr.notEmpty());
        in_addr.deq();
    endrule

    rule forwardInAddr;
        wawid <= in_addr.first().id();
        wawaddr <= in_addr.first().addr();
        wawlen <= in_addr.first().burst_length();
        wawsize <= in_addr.first().burst_size();
        wawburst <= in_addr.first().burst_type();
        wawlock <= in_addr.first().lock();
        wawcache <= in_addr.first().cache();
        wawprot <= in_addr.first().prot();
    endrule

    Wire#(Bool)                         wwready  <- mkBypassWire();
    Wire#(Bit#(id_width))               wwid      <- mkDWire(unpack(0));
    Wire#(Bit#(datawidth))              wwdata   <- mkDWire(unpack(0));
    Wire#(Bit#(TDiv#(datawidth, 8)))    wwstrb   <- mkDWire(unpack(0));
    Wire#(Bool)                         wwlast   <- mkDWire(unpack(0));

    rule deqInData if(!isRst && wwready && in_data.notEmpty());
        in_data.deq();
    endrule

    rule forwardInData;
        wwid    <= in_data.first().id();
        wwdata <= in_data.first().data();
        wwstrb <= in_data.first().strb();
        wwlast <= in_data.first().last();
    endrule

    Wire#(Bool) wpbvalid <- mkBypassWire();
    Wire#(AXI3_Write_Rs#(id_width)) rinpkg <- mkBypassWire();

    function Action respChannel(AXI3_Response r, Bit#(id_width) bid);
        action
            rinpkg <= AXI3_Write_Rs {id: bid, resp: r};
        endaction
    endfunction

    rule enqOut if(!isRst && wpbvalid && out.notFull());
        out.enq(rinpkg);
    endrule


    interface Put request_addr = toPut(in_addr);
    interface Put request_data = toPut(in_data);
    interface Get response = toGet(out);
    interface AXI3_Write_Rs snoop = out.first();

    interface AXI3_Master_Wr_Fab fab;
        interface pawready  = wawready._write;
        interface awvalid   = !isRst && in_addr.notEmpty();
        interface awid      = wawid;
        interface awaddr    = wawaddr;
        interface awlen     = wawlen;
        interface awsize    = wawsize;
        interface awburst   = wawburst;
        interface awlock    = wawlock;
        interface awcache   = wawcache;
        interface awprot    = wawprot;

        interface pwready   = wwready._write;
        interface wvalid    = !isRst && in_data.notEmpty();
        interface wid       = wwid;
        interface wdata     = wwdata;
        interface wstrb     = wwstrb;
        interface wlast     = wwlast;

        interface pbvalid = wpbvalid._write;
        interface bready = !isRst && out.notFull();
        interface bin = respChannel;
    endinterface
endmodule

interface TestIfc;
//    (*prefix="M_AXI"*)interface AXI3_Master_Wr_Fab#(32, 32, 1, 1) m_wr;
//    (*prefix="M_AXI"*)interface AXI3_Master_Rd_Fab#(32, 32, 1, 1) m_rd;

//    (*prefix="S_AXI"*)interface AXI3_Slave_Wr_Fab#(32, 32, 1, 1) s_wr;
//    (*prefix="S_AXI"*)interface AXI3_Slave_Rd_Fab#(32, 32, 1, 1) s_rd;
endinterface

//module mkTestIfc(TestIfc);
//    AXI3_Master_Wr#(32, 32, 1, 1) master_wr <- mkAXI3_Master_Wr(2, 2, 2, True);
//    AXI3_Master_Rd#(32, 32, 1, 1) master_rd <- mkAXI3_Master_Rd(2, 2, True);
//
//    AXI3_Slave_Wr#(32, 32, 1, 1) slave_wr <- mkAXI3_Slave_Wr(2, 2, 2);
//    AXI3_Slave_Rd#(32, 32, 1, 1) slave_rd <- mkAXI3_Slave_Rd(2, 2);
//
//    mkConnection(master_rd.fab, slave_rd.fab);
//    mkConnection(master_wr.fab, slave_wr.fab);
//
//    //interface m_wr = master_wr.fab;
//    //interface m_rd = master_rd.fab;
//
//    //interface s_wr = slave_wr.fab;
//    //interface s_rd = slave_rd.fab;
//endmodule
//
/*
========================
    Connectable
========================
*/
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

module mkAXI3_Master_Rd_Dummy(AXI3_Master_Rd#(addrwidth, datawidth, id_width));
    interface AXI3_Master_Rd_Fab fab;
        interface arvalid = False;
        interface arid = unpack(0);
        interface araddr = unpack(0);
        interface arlen = unpack(0);
        interface arsize = unpack(0);
        interface arburst = unpack(0) ;
        interface arlock = unpack(0);
        interface arcache = unpack(0) ;
        interface arprot = unpack(0);

        interface rready = False;

        method Action prvalid(Bool v);
        endmethod
        method Action prchannel(Bit#(id_width) id,
                                            Bit#(datawidth) d,
                                            AXI3_Response resp,
                                            Bool last);
        endmethod

        method Action parready(Bool a);
        endmethod
  endinterface
endmodule

module mkAXI3_Master_Wr_Dummy(AXI3_Master_Wr#(addrwidth, datawidth, id_width));
    interface AXI3_Master_Wr_Fab fab;
        interface awvalid   = False;
        interface awid      = unpack(0);
        interface awaddr    = unpack(0);
        interface awlen     = unpack(0);
        interface awsize    = unpack(0);
        interface awburst   = unpack(0);
        interface awlock    = unpack(0);
        interface awcache   = unpack(0);
        interface awprot    = unpack(0);

        interface wvalid    = False;
        interface wdata     = unpack(0);
        interface wstrb     = unpack(0);
        interface wlast     = unpack(0);

        interface bready = False;

        method Action pawready(Bool r);
        endmethod
        method Action pwready(Bool r);
        endmethod
        method Action pbvalid(Bool b);
        endmethod
        method Action bin(AXI3_Response r, Bit#(id_width) bid);
        endmethod
    endinterface
endmodule

// Helper functions to simplify usage of the above modules
function Action axi3_write_data_single(AXI3_Master_Wr#(a, b, c) m, Bit#(a) addr, Bit#(b) data, Bit#(TDiv#(b, 8)) byte_enable);
  action
      let p = AXI3_Write_Rq_Addr {
        id: 0,
        addr: addr,
        burst_length: 0,
        burst_size: bitsToBurstSizeAXI3(valueOf(b)),
        burst_type: INCR,
        lock: NORMAL,
        cache: NORMAL_NON_CACHEABLE_NON_BUFFERABLE,
        prot: UNPRIV_SECURE_DATA
      };
      let d = AXI3_Write_Rq_Data {
        id: 0,
        data: data,
        strb: byte_enable,
        last: True
      };

      m.request_addr.put(p);
      m.request_data.put(d);
  endaction
endfunction

function Action axi3_write_addr(AXI3_Master_Wr#(a, b, c) m, Bit#(a) addr, UInt#(AXI3_BURST_SIZE) beats);
  action
      let p = AXI3_Write_Rq_Addr {
        id: 0,
        addr: addr,
        burst_length: beats,
        burst_size: bitsToBurstSizeAXI3(valueOf(b)),
        burst_type: INCR,
        lock: NORMAL,
        cache: NORMAL_NON_CACHEABLE_NON_BUFFERABLE,
        prot: UNPRIV_SECURE_DATA
      };

      m.request_addr.put(p);
  endaction
endfunction

function Action axi3_write_data(AXI3_Master_Wr#(a, b, c) m, Bit#(b) data, Bit#(TDiv#(b, 8)) byte_enable, Bool last);
  action
      let d = AXI3_Write_Rq_Data {
        id: 0,
        data: data,
        strb: byte_enable,
        last: last
      };

      m.request_data.put(d);
  endaction
endfunction

function ActionValue#(AXI3_Write_Rs#(c)) axi3_write_response(AXI3_Master_Wr#(a, b, c) m);
  actionvalue
        let r <- m.response.get();
        return r;
  endactionvalue
endfunction

function Action axi3_read_data(AXI3_Master_Rd#(a, b, c) m, Bit#(a) addr, UInt#(AXI3_BURST_SIZE) beats);
  action
      let p = AXI3_Read_Rq {
        id: 0,
        addr: addr,
        burst_length: beats,
        burst_size: bitsToBurstSizeAXI3(valueOf(b)),
        burst_type: INCR,
        lock: NORMAL,
        cache: NORMAL_NON_CACHEABLE_NON_BUFFERABLE,
        prot: UNPRIV_SECURE_DATA
      };
      m.request.put(p);
  endaction
endfunction

function ActionValue#(Bit#(b)) axi3_read_response(AXI3_Master_Rd#(a, b, c) m);
  actionvalue
      let r <- m.response.get();
      return r.data;
  endactionvalue
endfunction

endpackage
