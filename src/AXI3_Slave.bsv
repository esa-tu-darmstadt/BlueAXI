package AXI3_Slave;

import GetPut :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import Connectable :: *;
import Clocks :: *;

// Project specific
import AXI3_Types :: *;

/*
========================
    AXI 4 Slave Read
========================
*/

(* always_ready, always_enabled *)
interface AXI3_Slave_Rd_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
  (*prefix = ""*)method Action parvalid((*port="arvalid"*)Bool v);
  method Bool arready();
  (*prefix = ""*)method Action parchannel((*port="arid"*)Bit#(id_width) id,
                                          (*port="araddr"*)Bit#(addrwidth) addr,
                                          (*port="arlen"*)UInt#(AXI3_BURST_SIZE) burst_length,
                                          (*port="arsize"*)AXI3_BurstSize burst_size,
                                          (*port="arburst"*)AXI3_BurstType burst_type,
                                          (*port="arlock"*)AXI3_Lock lock,
                                          (*port="arcache"*)AXI3_Read_Cache cache,
                                          (*port="arprot"*)AXI3_Prot prot
                                        );

  (*prefix = ""*)method Action prready((*port="rready"*)Bool r);
  method Bool rvalid();
  method Bit#(id_width) rid;
  method Bit#(datawidth) rdata;
  method AXI3_Response rresp;
  method Bool rlast;
endinterface

interface AXI3_Slave_Rd#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
  (* prefix="" *)
  interface AXI3_Slave_Rd_Fab#(addrwidth, datawidth, id_width) fab;

  interface Get#(AXI3_Read_Rq#(addrwidth, id_width)) request;
  interface Put#(AXI3_Read_Rs#(datawidth, id_width)) response;
endinterface

module mkAXI3_Slave_Rd#(Integer bufferIn, Integer bufferOut)(AXI3_Slave_Rd#(addrwidth, datawidth, id_width));

    let isRst <- isResetAsserted();

    FIFOF#(AXI3_Read_Rq#(addrwidth, id_width)) in = ?;

    if(bufferIn == 0)
      in <- mkBypassFIFOF();
    else if(bufferIn == 1)
      in <- mkPipelineFIFOF();
    else
      in <- mkSizedFIFOF(bufferIn);

    FIFOF#(AXI3_Read_Rs#(datawidth, id_width)) out = ?;

    if(bufferOut == 0)
      out <- mkBypassFIFOF();
    else if(bufferOut == 1)
      out <- mkPipelineFIFOF();
    else
      out <- mkSizedFIFOF(bufferIn);

    Wire#(Bool) arvalidIn <- mkBypassWire();
    Wire#(AXI3_Read_Rq#(addrwidth, id_width)) arinpkg <- mkBypassWire();

    function Action readChannel(Bit#(id_width) id,
                                Bit#(addrwidth) addr,
                                UInt#(AXI3_BURST_SIZE) burst_length,
                                AXI3_BurstSize burst_size,
                                AXI3_BurstType burst_type,
                                AXI3_Lock lock,
                                AXI3_Read_Cache cache,
                                AXI3_Prot prot);
        action
            arinpkg <= AXI3_Read_Rq {id: id, addr: addr, burst_length: burst_length, burst_size: burst_size, burst_type: burst_type
                                    , lock: lock, cache: cache, prot: prot};
        endaction
    endfunction

    rule enqIn if(!isRst && arvalidIn && in.notFull());
        in.enq(arinpkg);
    endrule

    Wire#(Bool)             prreadyIn <- mkBypassWire();
    Wire#(Bit#(id_width))   wrid   <- mkDWire(unpack(0));
    Wire#(Bit#(datawidth))  wrdata <- mkDWire(unpack(0));
    Wire#(AXI3_Response)    wrresp <- mkDWire(unpack(0));
    Wire#(Bool)             wrlast <- mkDWire(unpack(0));

    rule deqOut if(!isRst && prreadyIn && out.notEmpty());
        out.deq();
    endrule

    rule forwardOut;
        wrid <= out.first().id();
        wrdata <= out.first().data();
        wrresp <= out.first().resp();
        wrlast <= out.first().last();
    endrule

    interface Get request = toGet(in);
    interface Put response = toPut(out);

    interface AXI3_Slave_Rd_Fab fab;
        interface parvalid = arvalidIn._write;
        interface arready = !isRst && in.notFull();
        interface parchannel = readChannel;

        interface prready = prreadyIn._write();
        interface rvalid = !isRst && out.notEmpty();
        interface rid = wrid;
        interface rdata = wrdata;
        interface rresp = wrresp;
        interface rlast = wrlast;
    endinterface
endmodule

module mkAXI3_Slave_Rd_Dummy(AXI3_Slave_Rd#(addrwidth, datawidth, id_width));
    interface AXI3_Slave_Rd_Fab fab;
        interface arready = False;
        interface rvalid = False;
        interface rid = unpack(0);
        interface rdata = unpack(0);
        interface rresp = unpack(0);
        interface rlast = unpack(0);
    endinterface
endmodule

/*
========================
    AXI 4 Slave Write
========================
*/
(* always_ready, always_enabled *)
interface AXI3_Slave_Wr_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
    method Bool awready();
    (*prefix=""*)method Action pawvalid((*port="awvalid"*) Bool v);
    (*prefix=""*)method Action pawchannel((*port="awid"*)Bit#(id_width) id,
                                          (*port="awaddr"*)Bit#(addrwidth) addr,
                                          (*port="awlen"*)UInt#(AXI3_BURST_SIZE) burst_length,
                                          (*port="awsize"*)AXI3_BurstSize burst_size,
                                          (*port="awburst"*)AXI3_BurstType burst_type,
                                          (*port="awlock"*)AXI3_Lock lock,
                                          (*port="awcache"*)AXI3_Write_Cache cache,
                                          (*port="awprot"*)AXI3_Prot prot
        );

    method Bool wready;
    (*prefix=""*)method Action pwvalid((*port="wvalid"*)Bool r);
    (*prefix=""*)method Action pwchannel((*port="wid"*)Bit#(id_width) id,
                                         (*port="wdata"*)Bit#(datawidth) data,
                                         (*port="wstrb"*)Bit#(TDiv#(datawidth, 8)) strb,
                                         (*port="wlast"*)Bool last
                                        );

    (*prefix=""*)method Action pbready((*port="bready"*) Bool r);
    method Bool bvalid;
    method AXI3_Response bresp;
    method Bit#(id_width) bid;
endinterface

interface AXI3_Slave_Wr#(numeric type addrwidth, numeric type datawidth, numeric type id_width);
  (* prefix="" *)
  interface AXI3_Slave_Wr_Fab#(addrwidth, datawidth, id_width) fab;

  interface Get#(AXI3_Write_Rq_Addr#(addrwidth, id_width)) request_addr;
  interface Get#(AXI3_Write_Rq_Data#(datawidth, id_width)) request_data;

  interface Put#(AXI3_Write_Rs#(id_width)) response;
endinterface

module mkAXI3_Slave_Wr#(Integer bufferSizeAddr, Integer bufferSizeData, Integer bufferSizeOut)
                                (AXI3_Slave_Wr#(addrwidth, datawidth, id_width));

    let isRst <- isResetAsserted();

    FIFOF#(AXI3_Write_Rq_Addr#(addrwidth, id_width)) in_addr = ?;

    if(bufferSizeAddr == 0)
      in_addr <- mkBypassFIFOF();
    else if(bufferSizeAddr == 1)
      in_addr <- mkPipelineFIFOF();
    else
      in_addr <- mkSizedFIFOF(bufferSizeAddr);

    FIFOF#(AXI3_Write_Rq_Data#(datawidth, id_width)) in_data = ?;

    if(bufferSizeData == 0)
      in_data <- mkBypassFIFOF();
    else if(bufferSizeData == 1)
      in_data <- mkPipelineFIFOF();
    else
      in_data <- mkSizedFIFOF(bufferSizeData);

    FIFOF#(AXI3_Write_Rs#(id_width)) out = ?;

    if(bufferSizeOut == 0)
      out <- mkBypassFIFOF();
    else if(bufferSizeOut == 1)
      out <- mkPipelineFIFOF();
    else
      out <- mkSizedFIFOF(bufferSizeOut);

    Wire#(Bool) wawvalid <- mkBypassWire();
    Wire#(AXI3_Write_Rq_Addr#(addrwidth, id_width)) arinpkg_addr <- mkBypassWire();

    function Action readChannel_addr(Bit#(id_width) id,
                                     Bit#(addrwidth) addr,
                                     UInt#(AXI3_BURST_SIZE) burst_length,
                                     AXI3_BurstSize burst_size,
                                     AXI3_BurstType burst_type,
                                     AXI3_Lock lock,
                                     AXI3_Write_Cache cache,
                                     AXI3_Prot prot);
        action
            arinpkg_addr <= AXI3_Write_Rq_Addr {id: id, addr: addr, burst_length: burst_length, burst_size: burst_size, burst_type: burst_type
                                    , lock: lock, cache: cache, prot: prot};
        endaction
    endfunction

    rule enqAddr if(!isRst && wawvalid && in_addr.notFull());
        in_addr.enq(arinpkg_addr);
    endrule

    Wire#(Bool) wwvalid <- mkBypassWire();
    Wire#(AXI3_Write_Rq_Data#(datawidth, id_width)) arinpkg_data <- mkBypassWire();

    function Action readChannel_data(Bit#(id_width) id,
                                     Bit#(datawidth) data,
                                     Bit#(TDiv#(datawidth, 8)) strb,
                                     Bool last
                                     );
        action
            arinpkg_data <= AXI3_Write_Rq_Data {id: id, data: data, strb: strb, last: last};
        endaction
    endfunction

    rule enqData if(!isRst && wwvalid && in_data.notFull());
        in_data.enq(arinpkg_data);
    endrule

    Wire#(Bool)             pbreadyIn <- mkBypassWire();
    Wire#(Bit#(id_width))   wbid   <- mkDWire(unpack(0));
    Wire#(AXI3_Response)    wbresp <- mkDWire(unpack(0));

    rule deqOut if(!isRst && pbreadyIn && out.notEmpty());
        out.deq();
    endrule

    rule forwardOut;
        wbid <= out.first().id();
        wbresp <= out.first().resp();
    endrule

    interface Get request_addr = toGet(in_addr);
    interface Get request_data = toGet(in_data);
    interface Put response = toPut(out);

    interface AXI3_Slave_Wr_Fab fab;
        interface awready = !isRst && in_addr.notFull();
        interface pawvalid = wawvalid._write;
        interface pawchannel = readChannel_addr;

        interface wready = !isRst && in_data.notFull();
        interface pwvalid = wwvalid._write;
        interface pwchannel = readChannel_data;

        interface pbready = pbreadyIn._write;
        interface bvalid = !isRst && out.notEmpty();
        interface bresp = wbresp;
        interface bid = wbid;
    endinterface
endmodule

module mkAXI3_Slave_Wr_Dummy(AXI3_Slave_Wr#(addrwidth, datawidth, id_width));
      interface AXI3_Slave_Wr_Fab fab;
        interface awready = False;

        interface wready = False;

        interface bvalid = False;
        interface bresp = unpack(0);
        interface bid = unpack(0);
    endinterface
endmodule

endpackage
