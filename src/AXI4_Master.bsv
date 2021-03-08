package AXI4_Master;

import GetPut :: *;
import FIFO :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import BRAMFIFO :: *;
import Connectable :: *;
import Clocks :: *;

// Project specific

import AXI4_Types :: *;
import AXI4_Slave :: *;

/*
========================
    AXI 4 Master Read
========================
*/

(* always_ready, always_enabled *)
interface AXI4_Master_Rd_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
  method Bool arvalid;
  (*prefix = ""*)method Action parready((*port="arready"*)Bool a);
  method Bit#(id_width) arid;
  method Bit#(addrwidth) araddr;
  method UInt#(8) arlen;
  method AXI4_BurstSize arsize;
  method AXI4_BurstType arburst;
  method AXI4_Lock arlock;
  method AXI4_Read_Cache arcache;
  method AXI4_Prot arprot;
  method Bit#(4) arqos;
  method Bit#(4) arregion;
  method Bit#(user_width) aruser;

  method Bool rready;
  (*prefix = ""*)method Action prvalid((*port="rvalid"*)Bool v);
  (*prefix = ""*)method Action prchannel((*port="rid"*)Bit#(id_width) id,
                                         (*port="rdata"*) Bit#(datawidth) d,
                                         (*port="rresp"*)AXI4_Response resp,
                                         (*port="rlast"*)Bool last,
                                         (*port="ruser"*)Bit#(user_width) user);
endinterface

interface AXI4_Master_Rd#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
  (* prefix="" *)
  interface AXI4_Master_Rd_Fab#(addrwidth, datawidth, id_width, user_width) fab;
  interface Put#(AXI4_Read_Rq#(addrwidth, id_width, user_width)) request;
  interface Get#(AXI4_Read_Rs#(datawidth, id_width, user_width)) response;
  interface AXI4_Read_Rs#(datawidth, id_width, user_width) snoop;
endinterface

module mkAXI4_Master_Rd#(Integer bufferIn, Integer bufferOut, Bool bram)(AXI4_Master_Rd#(addrwidth, datawidth, id_width, user_width));

    let isRst <- isResetAsserted();

    FIFOF#(AXI4_Read_Rq#(addrwidth, id_width, user_width)) in = ?;
    if(bufferIn == 0)
        in <- mkBypassFIFOF();
    else if(bufferIn == 1)
        in <- mkPipelineFIFOF();
    else if(bram)
        in <- mkSizedBRAMFIFOF(bufferIn);
    else
        in <- mkSizedFIFOF(bufferIn);

    FIFOF#(AXI4_Read_Rs#(datawidth, id_width, user_width)) out = ?;
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
    Wire#(UInt#(8)) warlen <- mkDWire(unpack(0));
    Wire#(AXI4_BurstSize) warsize <- mkDWire(unpack(0));
    Wire#(AXI4_BurstType) warburst <- mkDWire(unpack(0));
    Wire#(AXI4_Lock) warlock <- mkDWire(unpack(0));
    Wire#(AXI4_Read_Cache) warcache <- mkDWire(unpack(0));
    Wire#(AXI4_Prot) warprot <- mkDWire(unpack(0));
    Wire#(Bit#(4)) warqos <- mkDWire(unpack(0));
    Wire#(Bit#(4)) warregion <- mkDWire(unpack(0));
    Wire#(Bit#(user_width)) waruser <- mkDWire(unpack(0));

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
        warqos <= in.first().qos();
        warregion <= in.first().region();
        waruser <= in.first().user();
    endrule

    Wire#(Bool) rvalidIn <- mkBypassWire();
    Wire#(AXI4_Read_Rs#(datawidth, id_width, user_width)) rinpkg <- mkBypassWire();

    function Action readChannel(Bit#(id_width) id, Bit#(datawidth) d, AXI4_Response resp, Bool last, Bit#(user_width) user);
        action
            rinpkg <= AXI4_Read_Rs {id: id, data: d, last: last, user: user, resp: resp};
        endaction
    endfunction

    rule enqOut if(!isRst && rvalidIn && out.notFull());
        out.enq(rinpkg);
    endrule

    interface Put request = toPut(in);
    interface Get response = toGet(out);

    interface AXI4_Read_Rs snoop = out.first();

    interface AXI4_Master_Rd_Fab fab;
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
        interface arqos = warqos;
        interface arregion = warregion;
        interface aruser = waruser;

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
interface AXI4_Master_Wr_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
    (*prefix=""*)method Action pawready((*port="awready"*) Bool r);
    method Bool awvalid;
    method Bit#(id_width) awid;
    method Bit#(addrwidth) awaddr;
    method UInt#(8) awlen;
    method AXI4_BurstSize awsize;
    method AXI4_BurstType awburst;
    method AXI4_Lock awlock;
    method AXI4_Write_Cache awcache;
    method AXI4_Prot awprot;
    method Bit#(4) awqos;
    method Bit#(4) awregion;
    method Bit#(user_width) awuser;

    (*prefix=""*)method Action pwready((*port="wready"*)Bool r);
    method Bool wvalid;
    method Bit#(datawidth) wdata;
    method Bit#(TDiv#(datawidth, 8)) wstrb;
    method Bool wlast;
    method Bit#(user_width) wuser;

    (*prefix=""*)method Action pbvalid((*port="bvalid"*) Bool b);
    method Bool bready;
    (*prefix=""*)method Action bin((*port="bresp"*) AXI4_Response r, (*port="bid"*) Bit#(id_width) bid, (*port="buser"*) Bit#(user_width) buser);
endinterface

interface AXI4_Master_Wr#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
  (* prefix="" *)
  interface AXI4_Master_Wr_Fab#(addrwidth, datawidth, id_width, user_width) fab;
  interface Put#(AXI4_Write_Rq_Addr#(addrwidth, id_width, user_width)) request_addr;
  interface Put#(AXI4_Write_Rq_Data#(datawidth, user_width)) request_data;
  interface Get#(AXI4_Write_Rs#(id_width, user_width)) response;
  method AXI4_Write_Rs#(id_width, user_width) snoop;
endinterface

module mkAXI4_Master_Wr#(Integer bufferInAddr, Integer bufferInData, Integer bufferOut, Bool bram)(AXI4_Master_Wr#(addrwidth, datawidth, id_width, user_width));
    let isRst <- isResetAsserted();

    FIFOF#(AXI4_Write_Rq_Addr#(addrwidth, id_width, user_width)) in_addr = ?;
    if(bufferInAddr == 0)
        in_addr <- mkBypassFIFOF();
    else if(bufferInAddr == 1)
        in_addr <- mkPipelineFIFOF();
    else if(bram)
        in_addr <- mkSizedBRAMFIFOF(bufferInAddr);
    else
        in_addr <- mkSizedFIFOF(bufferInAddr);

    FIFOF#(AXI4_Write_Rq_Data#(datawidth, user_width)) in_data = ?;
    if(bufferInData == 0)
        in_data <- mkBypassFIFOF();
    else if(bufferInData == 1)
        in_data <- mkPipelineFIFOF();
    else if(bram)
        in_data <- mkSizedBRAMFIFOF(bufferInData);
    else
        in_data <- mkSizedFIFOF(bufferInData);

    FIFOF#(AXI4_Write_Rs#(id_width, user_width)) out = ?;
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
    Wire#(UInt#(8))         wawlen    <- mkDWire(unpack(0));
    Wire#(AXI4_BurstSize)   wawsize   <- mkDWire(unpack(0));
    Wire#(AXI4_BurstType)   wawburst  <- mkDWire(unpack(0));
    Wire#(AXI4_Lock)        wawlock   <- mkDWire(unpack(0));
    Wire#(AXI4_Write_Cache) wawcache  <- mkDWire(unpack(0));
    Wire#(AXI4_Prot)        wawprot   <- mkDWire(unpack(0));
    Wire#(Bit#(4))          wawqos    <- mkDWire(unpack(0));
    Wire#(Bit#(4))          wawregion <- mkDWire(unpack(0));
    Wire#(Bit#(user_width)) wawuser   <- mkDWire(unpack(0));

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
        wawqos <= in_addr.first().qos();
        wawregion <= in_addr.first().region();
        wawuser <= in_addr.first().user();
    endrule

    Wire#(Bool)                         wwready  <- mkBypassWire();
    Wire#(Bit#(datawidth))              wwdata   <- mkDWire(unpack(0));
    Wire#(Bit#(TDiv#(datawidth, 8)))    wwstrb   <- mkDWire(unpack(0));
    Wire#(Bool)                         wwlast   <- mkDWire(unpack(0));
    Wire#(Bit#(user_width))             wwuser   <- mkDWire(unpack(0));

    rule deqInData if(!isRst && wwready && in_data.notEmpty());
        in_data.deq();
    endrule

    rule forwardInData;
        wwdata <= in_data.first().data();
        wwstrb <= in_data.first().strb();
        wwlast <= in_data.first().last();
        wwuser <= in_data.first().user();
    endrule

    Wire#(Bool) wpbvalid <- mkBypassWire();
    Wire#(AXI4_Write_Rs#(id_width, user_width)) rinpkg <- mkBypassWire();

    function Action respChannel(AXI4_Response r, Bit#(id_width) bid, Bit#(user_width) buser);
        action
            rinpkg <= AXI4_Write_Rs {id: bid, user: buser, resp: r};
        endaction
    endfunction

    rule enqOut if(!isRst && wpbvalid && out.notFull());
        out.enq(rinpkg);
    endrule


    interface Put request_addr = toPut(in_addr);
    interface Put request_data = toPut(in_data);
    interface Get response = toGet(out);
    interface AXI4_Write_Rs snoop = out.first();

    interface AXI4_Master_Wr_Fab fab;
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
        interface awqos     = wawqos;
        interface awregion  = wawregion;
        interface awuser    = wawuser;


        interface pwready   = wwready._write;
        interface wvalid    = !isRst && in_data.notEmpty();
        interface wdata     = wwdata;
        interface wstrb     = wwstrb;
        interface wlast     = wwlast;
        interface wuser     = wwuser;

        interface pbvalid = wpbvalid._write;
        interface bready = !isRst && out.notFull();
        interface bin = respChannel;
    endinterface
endmodule

interface TestIfc;
//    (*prefix="M_AXI"*)interface AXI4_Master_Wr_Fab#(32, 32, 1, 1) m_wr;
//    (*prefix="M_AXI"*)interface AXI4_Master_Rd_Fab#(32, 32, 1, 1) m_rd;

//    (*prefix="S_AXI"*)interface AXI4_Slave_Wr_Fab#(32, 32, 1, 1) s_wr;
//    (*prefix="S_AXI"*)interface AXI4_Slave_Rd_Fab#(32, 32, 1, 1) s_rd;
endinterface

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

/*
========================
    Connectable
========================
*/
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

module mkAXI4_Master_Rd_Dummy(AXI4_Master_Rd#(addrwidth, datawidth, id_width, user_width));
    interface AXI4_Master_Rd_Fab fab;
        interface arvalid = False;
        interface arid = unpack(0);
        interface araddr = unpack(0);
        interface arlen = unpack(0);
        interface arsize = unpack(0);
        interface arburst = unpack(0) ;
        interface arlock = unpack(0);
        interface arcache = unpack(0) ;
        interface arprot = unpack(0);
        interface arqos = unpack(0);
        interface arregion = unpack(0);
        interface aruser = unpack(0);

        interface rready = False;

        method Action prvalid(Bool v);
        endmethod
        method Action prchannel(Bit#(id_width) id,
                                            Bit#(datawidth) d,
                                            AXI4_Response resp,
                                            Bool last,
                                            Bit#(user_width) user);
        endmethod

        method Action parready(Bool a);
        endmethod
  endinterface
endmodule

module mkAXI4_Master_Wr_Dummy(AXI4_Master_Wr#(addrwidth, datawidth, id_width, user_width));
    interface AXI4_Master_Wr_Fab fab;
        interface awvalid   = False;
        interface awid      = unpack(0);
        interface awaddr    = unpack(0);
        interface awlen     = unpack(0);
        interface awsize    = unpack(0);
        interface awburst   = unpack(0);
        interface awlock    = unpack(0);
        interface awcache   = unpack(0);
        interface awprot    = unpack(0);
        interface awqos     = unpack(0);
        interface awregion  = unpack(0);
        interface awuser    = unpack(0);

        interface wvalid    = False;
        interface wdata     = unpack(0);
        interface wstrb     = unpack(0);
        interface wlast     = unpack(0);
        interface wuser     = unpack(0);

        interface bready = False;

        method Action pawready(Bool r);
        endmethod
        method Action pwready(Bool r);
        endmethod
        method Action pbvalid(Bool b);
        endmethod
        method Action bin(AXI4_Response r, Bit#(id_width) bid, Bit#(user_width) buser);
        endmethod
    endinterface
endmodule

// Helper functions to simplify usage of the above modules
function Action axi4_write_data_single(AXI4_Master_Wr#(a, b, c, d) m, Bit#(a) addr, Bit#(b) data, Bit#(TDiv#(b, 8)) byte_enable);
  action
      let p = AXI4_Write_Rq_Addr {
        id: 0,
        addr: addr,
        burst_length: 0,
        burst_size: bitsToBurstSize(valueOf(b)),
        burst_type: INCR,
        lock: NORMAL,
        cache: NORMAL_NON_CACHEABLE_NON_BUFFERABLE,
        prot: UNPRIV_SECURE_DATA,
        qos: 0,
        region: 0,
        user: 0
      };
      let d = AXI4_Write_Rq_Data {
        data: data,
        strb: byte_enable,
        last: True,
        user: 0
      };

      m.request_addr.put(p);
      m.request_data.put(d);
  endaction
endfunction

function Action axi4_write_addr(AXI4_Master_Wr#(a, b, c, d) m, Bit#(a) addr, UInt#(8) beats);
  action
      let p = AXI4_Write_Rq_Addr {
        id: 0,
        addr: addr,
        burst_length: beats,
        burst_size: bitsToBurstSize(valueOf(b)),
        burst_type: INCR,
        lock: NORMAL,
        cache: NORMAL_NON_CACHEABLE_NON_BUFFERABLE,
        prot: UNPRIV_SECURE_DATA,
        qos: 0,
        region: 0,
        user: 0
      };

      m.request_addr.put(p);
  endaction
endfunction

function Action axi4_write_data(AXI4_Master_Wr#(a, b, c, d) m, Bit#(b) data, Bit#(TDiv#(b, 8)) byte_enable, Bool last);
  action
      let d = AXI4_Write_Rq_Data {
        data: data,
        strb: byte_enable,
        last: last,
        user: 0
      };

      m.request_data.put(d);
  endaction
endfunction

function ActionValue#(AXI4_Write_Rs#(c,d)) axi4_write_response(AXI4_Master_Wr#(a, b, c, d) m);
  actionvalue
        let r <- m.response.get();
        return r;
  endactionvalue
endfunction

function Action axi4_read_data(AXI4_Master_Rd#(a, b, c, d) m, Bit#(a) addr, UInt#(8) beats);
  action
      let p = AXI4_Read_Rq {
        id: 0,
        addr: addr,
        burst_length: beats,
        burst_size: bitsToBurstSize(valueOf(b)),
        burst_type: INCR,
        lock: NORMAL,
        cache: NORMAL_NON_CACHEABLE_NON_BUFFERABLE,
        prot: UNPRIV_SECURE_DATA,
        qos: 0,
        region: 0,
        user: 0
      };
      m.request.put(p);
  endaction
endfunction

function ActionValue#(Bit#(b)) axi4_read_response(AXI4_Master_Rd#(a, b, c, d) m);
  actionvalue
      let r <- m.response.get();
      return r.data;
  endactionvalue
endfunction

endpackage
