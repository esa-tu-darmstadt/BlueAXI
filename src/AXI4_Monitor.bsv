package AXI4_Monitor;

import FIFO :: *;
import GetPut :: *;

import AXI4_Types :: *;
import AXI4_Lite_Types :: *;

(* always_ready, always_enabled *)
interface AXI4_Monitor_Rd_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
  (*prefix = ""*)method Action ar_channel((*port="arvalid"*) Bool arvalid,
                           (*port="arready"*) Bool arready,
                           (*port="arid"*)Bit#(id_width) arid,
                           (*port="araddr"*)Bit#(addrwidth) araddr,
                           (*port="arlen"*)UInt#(8) arlen,
                           (*port="arsize"*)AXI4_BurstSize arsize,
                           (*port="arburst"*)AXI4_BurstType arburst,
                           (*port="arlock"*)AXI4_Lock arlock,
                           (*port="arcache"*)AXI4_Read_Cache arcache,
                           (*port="arprot"*)AXI4_Prot arprot,
                           (*port="arqos"*)Bit#(4) arqos,
                           (*port="arregion"*)Bit#(4) arregion,
                           (*port="aruser"*)Bit#(user_width) aruser
                           );

  (*prefix = ""*)method Action rr_channel((*port="rvalid"*) Bool rvalid,
                           (*port="rready"*) Bool rready,
                           (*port="rid"*)Bit#(id_width) id,
                           (*port="rdata"*) Bit#(datawidth) d,
                           (*port="rresp"*)AXI4_Response resp,
                           (*port="rlast"*)Bool last,
                           (*port="ruser"*)Bit#(user_width) user
    );
endinterface

interface AXI4_Monitor_Rd#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
  (* prefix="" *)
  interface AXI4_Monitor_Rd_Fab#(addrwidth, datawidth, id_width, user_width) fab;
  interface Get#(AXI4_Read_Rq#(addrwidth, id_width, user_width)) request;
  interface Get#(AXI4_Read_Rs#(datawidth, id_width, user_width)) response;
endinterface

module mkAXI4_Monitor_Rd(AXI4_Monitor_Rd#(addrwidth, datawidth, id_width, user_width));
    Wire#(AXI4_Read_Rq#(addrwidth, id_width, user_width)) request_w <- mkWire();
    FIFO#(AXI4_Read_Rq#(addrwidth, id_width, user_width)) request_out <- mkFIFO();

    rule setRequestOut;
        request_out.enq(request_w);
    endrule

    (*preempts="setRequestOut, setRequestOutDropped"*)
    rule setRequestOutDropped;
        $display("Dropped AXI4_Read_Rq ", fshow(request_w));
    endrule

    Wire#(AXI4_Read_Rs#(datawidth, id_width, user_width)) response_w <- mkWire();
    FIFO#(AXI4_Read_Rs#(datawidth, id_width, user_width)) response_out <- mkFIFO();

    rule setResponseOut;
        response_out.enq(response_w);
    endrule

    (*preempts="setResponseOut, setResponseOutDropped"*)
    rule setResponseOutDropped;
        $display("Dropped AXI4_Read_Rs ", fshow(response_w));
    endrule

    interface AXI4_Monitor_Rd_Fab fab;
        method Action ar_channel(Bool arvalid, Bool arready, Bit#(id_width) arid, Bit#(addrwidth) araddr,
                                 UInt#(8) arlen, AXI4_BurstSize arsize, AXI4_BurstType arburst, AXI4_Lock arlock,
                                 AXI4_Read_Cache arcache, AXI4_Prot arprot, Bit#(4) arqos, Bit#(4) arregion,
                                 Bit#(user_width) aruser);
            if(arvalid && arready) begin
                request_w <= AXI4_Read_Rq {
                    id: arid,
                    addr: araddr,
                    burst_length: arlen,
                    burst_size: arsize,
                    burst_type: arburst,
                    lock: arlock,
                    cache: arcache,
                    prot: arprot,
                    qos: arqos,
                    region: arregion,
                    user: aruser
                };
            end
        endmethod

        method Action rr_channel(Bool rvalid, Bool rready, Bit#(id_width) id, Bit#(datawidth) d, AXI4_Response resp, Bool last,
                               Bit#(user_width) user);
            if(rvalid && rready) begin
                response_w <= AXI4_Read_Rs {
                    id: id,
                    data: d,
                    resp: resp,
                    last: last,
                    user: user
                };
            end
        endmethod
    endinterface

    interface request = toGet(request_out);
    interface response = toGet(response_out);
endmodule

(* always_ready, always_enabled *)
interface AXI4_Monitor_Wr_Fab#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);

    (*prefix = ""*)method Action aw_channel((*port="awready"*) Bool awready,
                             (*port="awvalid"*) Bool awvalid,
                             (*port="awid"*) Bit#(id_width) awid,
                             (*port="awaddr"*) Bit#(addrwidth) awaddr,
                             (*port="awlen"*) UInt#(8) awlen,
                             (*port="awsize"*) AXI4_BurstSize awsize,
                             (*port="awburst"*) AXI4_BurstType awburst,
                             (*port="awlock"*) AXI4_Lock awlock,
                             (*port="awcache"*) AXI4_Write_Cache awcache,
                             (*port="awprot"*) AXI4_Prot awprot,
                             (*port="awqos"*) Bit#(4) awqos,
                             (*port="awregion"*) Bit#(4) awregion,
                             (*port="awuser"*) Bit#(user_width) awuser
                            );

    (*prefix = ""*)method Action w_channel((*port="wready"*)Bool wready,
                            (*port="wvalid"*) Bool wvalid,
                            (*port="wdata"*) Bit#(datawidth) wdata,
                            (*port="wstrb"*) Bit#(TDiv#(datawidth, 8)) wstrb,
                            (*port="wlast"*) Bool wlast,
                            (*port="wuser"*) Bit#(user_width) wuser
        );

    (*prefix = ""*)method Action b_channel((*port="bvalid"*) Bool bvalid,
                            (*port="bready"*) Bool bready,
                            (*port="bresp"*) AXI4_Response r,
                            (*port="bid"*) Bit#(id_width) bid,
                            (*port="buser"*) Bit#(user_width) buser
                    );
endinterface

interface AXI4_Monitor_Wr#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width);
  (*prefix = ""*)interface AXI4_Monitor_Wr_Fab#(addrwidth, datawidth, id_width, user_width) fab;
  interface Get#(AXI4_Write_Rq_Addr#(addrwidth, id_width, user_width)) request_addr;
  interface Get#(AXI4_Write_Rq_Data#(datawidth, user_width)) request_data;
  interface Get#(AXI4_Write_Rs#(id_width, user_width)) response;
endinterface

module mkAXI4_Monitor_Wr(AXI4_Monitor_Wr#(addrwidth, datawidth, id_width, user_width));
    Wire#(AXI4_Write_Rq_Addr#(addrwidth, id_width, user_width)) request_addr_w <- mkWire();
    FIFO#(AXI4_Write_Rq_Addr#(addrwidth, id_width, user_width)) request_addr_out <- mkFIFO();

    rule setRequestAddrOut;
        request_addr_out.enq(request_addr_w);
    endrule

    (*preempts="setRequestAddrOut, setRequestAddrOutDropped"*)
    rule setRequestAddrOutDropped;
        $display("Dropped AXI4_Write_Rq_Addr ", fshow(request_addr_w));
    endrule

    Wire#(AXI4_Write_Rq_Data#(datawidth, user_width)) request_data_w <- mkWire();
    FIFO#(AXI4_Write_Rq_Data#(datawidth, user_width)) request_data_out <- mkFIFO();

    rule setRequestDataOut;
        request_data_out.enq(request_data_w);
    endrule

    (*preempts="setRequestDataOut, setRequestDataOutDropped"*)
    rule setRequestDataOutDropped;
        $display("Dropped AXI4_Write_Rq_Data ", fshow(request_data_w));
    endrule

    Wire#(AXI4_Write_Rs#(id_width, user_width)) response_w <- mkWire();
    FIFO#(AXI4_Write_Rs#(id_width, user_width)) response_out <- mkFIFO();

    rule setResponseOut;
        response_out.enq(response_w);
    endrule

    (*preempts="setResponseOut, setResponseOutDropped"*)
    rule setResponseOutDropped;
        $display("Dropped AXI4_Write_Rs ", fshow(response_w));
    endrule

    interface AXI4_Monitor_Wr_Fab fab;
        method Action aw_channel(Bool awready, Bool awvalid, Bit#(id_width) awid,
                                 Bit#(addrwidth) awaddr, UInt#(8) awlen, AXI4_BurstSize awsize,
                                 AXI4_BurstType awburst, AXI4_Lock awlock, AXI4_Write_Cache awcache,
                                 AXI4_Prot awprot, Bit#(4) awqos, Bit#(4) awregion, Bit#(user_width) awuser);
            if(awready && awvalid) begin
                request_addr_w <= AXI4_Write_Rq_Addr {
                        id: awid,
                        addr: awaddr,
                        burst_length: awlen,
                        burst_size: awsize,
                        burst_type: awburst,
                        lock: awlock,
                        cache: awcache,
                        prot: awprot,
                        qos: awqos,
                        region: awregion,
                        user: awuser
                    };
            end
        endmethod

        method Action w_channel(Bool wready, Bool wvalid, Bit#(datawidth) wdata, Bit#(TDiv#(datawidth, 8)) wstrb,
                                Bool wlast, Bit#(user_width) wuser);
            if(wready && wvalid) begin
                request_data_w <= AXI4_Write_Rq_Data {
                        data: wdata,
                        strb: wstrb,
                        last: wlast,
                        user: wuser
                    };
            end
        endmethod

        method Action b_channel(Bool bvalid, Bool bready, AXI4_Response r, Bit#(id_width) bid, Bit#(user_width) buser);
            if(bvalid && bready) begin
                response_w <= AXI4_Write_Rs {
                        id: bid,
                        resp: r,
                        user: buser
                    };
            end
        endmethod
    endinterface

    interface request_addr = toGet(request_addr_out);
    interface request_data = toGet(request_data_out);
    interface response = toGet(response_out);
endmodule

endpackage