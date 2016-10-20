package AXI4_Slave;

import GetPut :: *;
import FIFOF :: *;
import SpecialFIFOs :: *;
import Connectable :: *;
import Clocks :: *;

// Project specific
import AXI4_Types :: *;

/*
========================
    AXI 4 Slave Read
========================
*/

(* always_ready, always_enabled *)
interface AXI4_Slave_Rd_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
  (*prefix = ""*)method Action parvalid((*port="arvalid"*)Bool v);
  method Bool arready();
  (*prefix = ""*)method Action parchannel((*port="arid"*)Bit#(id_width) id,
                                          (*port="araddr"*)Bit#(addrwidth) addr,
                                          (*port="arlen"*)UInt#(8) burst_length,
                                          (*port="arsize"*)AXI4_BurstSize burst_size,
                                          (*port="arburst"*)AXI4_BurstType burst_type,
                                          (*port="arlock"*)AXI4_Lock lock,
                                          (*port="arcache"*)AXI4_Read_Cache cache,
                                          (*port="arprot"*)AXI4_Prot prot,
                                          (*port="arqos"*)Bit#(4) qos,
                                          (*port="arregion"*)Bit#(4) region,
                                          (*port="aruser"*)Bit#(user_width) user
                                        );

  (*prefix = ""*)method Action prready((*port="rready"*)Bool r);
  method Bool rvalid();
  method Bit#(id_width) rid;
  method Bit#(datawidth) rdata;
  method AXI4_Response rresp;
  method Bool rlast;
  method Bit#(user_width) ruser;
endinterface

interface AXI4_Slave_Rd#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
  (* prefix="" *)
  interface AXI4_Slave_Rd_Fab#(addrwidth, datawidth, id_width, user_width) fab;

  interface Get#(AXI4_Read_Rq#(addrwidth, id_width, user_width)) request;
  interface Put#(AXI4_Read_Rs#(datawidth, id_width, user_width)) response;
endinterface

module mkAXI4_Slave_Rd#(Integer bufferIn, Integer bufferOut)(AXI4_Slave_Rd#(addrwidth, datawidth, id_width, user_width));

    let isRst <- isResetAsserted();

    FIFOF#(AXI4_Read_Rq#(addrwidth, id_width, user_width)) in = ?;

    if(bufferIn == 0)
      in <- mkBypassFIFOF();
    else if(bufferIn == 1)
      in <- mkPipelineFIFOF();
    else
      in <- mkSizedFIFOF(bufferIn);

    FIFOF#(AXI4_Read_Rs#(datawidth, id_width, user_width)) out = ?;

    if(bufferOut == 0)
      out <- mkBypassFIFOF();
    else if(bufferOut == 1)
      out <- mkPipelineFIFOF();
    else
      out <- mkSizedFIFOF(bufferIn);

    Wire#(Bool) arvalidIn <- mkBypassWire();
    Wire#(AXI4_Read_Rq#(addrwidth, id_width, user_width)) arinpkg <- mkBypassWire();

    function Action readChannel(Bit#(id_width) id,
                                Bit#(addrwidth) addr,
                                UInt#(8) burst_length,
                                AXI4_BurstSize burst_size,
                                AXI4_BurstType burst_type,
                                AXI4_Lock lock,
                                AXI4_Read_Cache cache,
                                AXI4_Prot prot,
                                Bit#(4) qos,
                                Bit#(4) region,
                                Bit#(user_width) user);
        action
            arinpkg <= AXI4_Read_Rq {id: id, addr: addr, burst_length: burst_length, burst_size: burst_size, burst_type: burst_type
                                    , lock: lock, cache: cache, prot: prot, qos: qos, region: region, user: user};
        endaction
    endfunction

    rule enqIn if(!isRst && arvalidIn && in.notFull());
        in.enq(arinpkg);
    endrule

    Wire#(Bool)             prreadyIn <- mkBypassWire();
    Wire#(Bit#(id_width))   wrid   <- mkDWire(unpack(0));
    Wire#(Bit#(datawidth))  wrdata <- mkDWire(unpack(0));
    Wire#(AXI4_Response)    wrresp <- mkDWire(unpack(0));
    Wire#(Bool)             wrlast <- mkDWire(unpack(0));
    Wire#(Bit#(user_width)) wruser <- mkDWire(unpack(0));

    rule deqOut if(!isRst && prreadyIn && out.notEmpty());
        out.deq();
    endrule

    rule forwardOut;
        wrid <= out.first().id();
        wrdata <= out.first().data();
        wrresp <= out.first().resp();
        wrlast <= out.first().last();
        wruser <= out.first().user();
    endrule

    interface Get request = toGet(in);
    interface Put response = toPut(out);

    interface AXI4_Slave_Rd_Fab fab;
        interface parvalid = arvalidIn._write;
        interface arready = !isRst && in.notFull();
        interface parchannel = readChannel;

        interface prready = prreadyIn._write();
        interface rvalid = !isRst && out.notEmpty();
        interface rid = wrid;
        interface rdata = wrdata;
        interface rresp = wrresp;
        interface rlast = wrlast;
        interface ruser = wruser;
    endinterface
endmodule

module mkAXI4_Slave_Rd_Dummy(AXI4_Slave_Rd#(addrwidth, datawidth, id_width, user_width));
    interface AXI4_Slave_Rd_Fab fab;
        interface arready = False;
        interface rvalid = False;
        interface rid = unpack(0);
        interface rdata = unpack(0);
        interface rresp = unpack(0);
        interface rlast = unpack(0);
        interface ruser = unpack(0);
    endinterface
endmodule

/*
========================
    AXI 4 Slave Write
========================
*/
(* always_ready, always_enabled *)
interface AXI4_Slave_Wr_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
    method Bool awready();
    (*prefix=""*)method Action pawvalid((*port="awvalid"*) Bool v);
    (*prefix=""*)method Action pawchannel((*port="awid"*)Bit#(id_width) id,
                                          (*port="awaddr"*)Bit#(addrwidth) addr,
                                          (*port="awlen"*)UInt#(8) burst_length,
                                          (*port="awsize"*)AXI4_BurstSize burst_size,
                                          (*port="awburst"*)AXI4_BurstType burst_type,
                                          (*port="awlock"*)AXI4_Lock lock,
                                          (*port="awcache"*)AXI4_Write_Cache cache,
                                          (*port="awprot"*)AXI4_Prot prot,
                                          (*port="awqos"*)Bit#(4) qos,
                                          (*port="awregion"*)Bit#(4) region,
                                          (*port="awuser"*)Bit#(user_width) user
        );

    method Bool wready;
    (*prefix=""*)method Action pwvalid((*port="wvalid"*)Bool r);
    (*prefix=""*)method Action pwchannel((*port="wdata"*)Bit#(datawidth) data,
                                         (*port="wstrb"*)Bit#(TDiv#(datawidth, 8)) strb,
                                         (*port="wlast"*)Bool last,
                                         (*port="wuser"*)Bit#(user_width) user
                                        );

    (*prefix=""*)method Action pbready((*port="bready"*) Bool r);
    method Bool bvalid;
    method AXI4_Response bresp;
    method Bit#(id_width) bid;
    method Bit#(user_width) buser;
endinterface

interface AXI4_Slave_Wr#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
  (* prefix="" *)
  interface AXI4_Slave_Wr_Fab#(addrwidth, datawidth, id_width, user_width) fab;

  interface Get#(AXI4_Write_Rq_Addr#(addrwidth, id_width, user_width)) request_addr;
  interface Get#(AXI4_Write_Rq_Data#(datawidth, user_width)) request_data;

  interface Put#(AXI4_Write_Rs#(id_width, user_width)) response;
endinterface

module mkAXI4_Slave_Wr#(Integer bufferSizeAddr, Integer bufferSizeData, Integer bufferSizeOut)
                                (AXI4_Slave_Wr#(addrwidth, datawidth, id_width, user_width));

    let isRst <- isResetAsserted();

    FIFOF#(AXI4_Write_Rq_Addr#(addrwidth, id_width, user_width)) in_addr = ?;

    if(bufferSizeAddr == 0)
      in_addr <- mkBypassFIFOF();
    else if(bufferSizeAddr == 1)
      in_addr <- mkPipelineFIFOF();
    else
      in_addr <- mkSizedFIFOF(bufferSizeAddr);

    FIFOF#(AXI4_Write_Rq_Data#(datawidth, user_width)) in_data = ?;

    if(bufferSizeData == 0)
      in_data <- mkBypassFIFOF();
    else if(bufferSizeData == 1)
      in_data <- mkPipelineFIFOF();
    else
      in_data <- mkSizedFIFOF(bufferSizeData);

    FIFOF#(AXI4_Write_Rs#(id_width, user_width)) out = ?;

    if(bufferSizeOut == 0)
      out <- mkBypassFIFOF();
    else if(bufferSizeOut == 1)
      out <- mkPipelineFIFOF();
    else
      out <- mkSizedFIFOF(bufferSizeOut);

    Wire#(Bool) wawvalid <- mkBypassWire();
    Wire#(AXI4_Write_Rq_Addr#(addrwidth, id_width, user_width)) arinpkg_addr <- mkBypassWire();

    function Action readChannel_addr(Bit#(id_width) id,
                                     Bit#(addrwidth) addr,
                                     UInt#(8) burst_length,
                                     AXI4_BurstSize burst_size,
                                     AXI4_BurstType burst_type,
                                     AXI4_Lock lock,
                                     AXI4_Write_Cache cache,
                                     AXI4_Prot prot,
                                     Bit#(4) qos,
                                     Bit#(4) region,
                                     Bit#(user_width) user);
        action
            arinpkg_addr <= AXI4_Write_Rq_Addr {id: id, addr: addr, burst_length: burst_length, burst_size: burst_size, burst_type: burst_type
                                    , lock: lock, cache: cache, prot: prot, qos: qos, region: region, user: user};
        endaction
    endfunction

    rule enqAddr if(!isRst && wawvalid && in_addr.notFull());
        in_addr.enq(arinpkg_addr);
    endrule

    Wire#(Bool) wwvalid <- mkBypassWire();
    Wire#(AXI4_Write_Rq_Data#(datawidth, user_width)) arinpkg_data <- mkBypassWire();

    function Action readChannel_data(Bit#(datawidth) data,
                                     Bit#(TDiv#(datawidth, 8)) strb,
                                     Bool last,
                                     Bit#(user_width) user
                                     );
        action
            arinpkg_data <= AXI4_Write_Rq_Data {data: data, strb: strb, last: last, user: user};
        endaction
    endfunction

    rule enqData if(!isRst && wwvalid && in_data.notFull());
        in_data.enq(arinpkg_data);
    endrule

    Wire#(Bool)             pbreadyIn <- mkBypassWire();
    Wire#(Bit#(id_width))   wbid   <- mkDWire(unpack(0));
    Wire#(AXI4_Response)    wbresp <- mkDWire(unpack(0));
    Wire#(Bit#(user_width)) wbuser <- mkDWire(unpack(0));

    rule deqOut if(!isRst && pbreadyIn && out.notEmpty());
        out.deq();
    endrule

    rule forwardOut;
        wbid <= out.first().id();
        wbresp <= out.first().resp();
        wbuser <= out.first().user();
    endrule

    interface Get request_addr = toGet(in_addr);
    interface Get request_data = toGet(in_data);
    interface Put response = toPut(out);

    interface AXI4_Slave_Wr_Fab fab;
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
        interface buser = wbuser;
    endinterface
endmodule

module mkAXI4_Slave_Wr_Dummy(AXI4_Slave_Wr#(addrwidth, datawidth, id_width, user_width));
      interface AXI4_Slave_Wr_Fab fab;
        interface awready = False;

        interface wready = False;

        interface bvalid = False;
        interface bresp = unpack(0);
        interface bid = unpack(0);
        interface buser = unpack(0);
    endinterface
endmodule

endpackage
