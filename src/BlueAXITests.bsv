package BlueAXITests;

import Vector :: *;
import GetPut :: *;
import DefaultValue :: *;

import AXI4_Types :: *;
import AXI4_Slave :: *;
import AXI4_Master :: *;
import AXI4_Lite_Master :: *;
import AXI4_Lite_Types :: *;
import BlueLib :: *;

typedef struct {
    Bit#(a) data;
    Bit#(TDiv#(a, 8)) strb;
} AxiBeat#(numeric type a) deriving(Bits, Eq, FShow);

interface AXIMemoryVerifier#(numeric type addr_width, numeric type data_width, numeric type id_width, numeric type numChecksMax);
    interface AXI4_Slave_Rd_Fab#(addr_width, data_width, id_width, 0) rd;
    interface AXI4_Slave_Wr_Fab#(addr_width, data_width, id_width, 0) wr;
    interface Put#(Tuple2#(Vector#(numChecksMax, AxiBeat#(data_width)), UInt#(TLog#(numChecksMax)))) expectedRead;
    interface Put#(Tuple2#(Vector#(numChecksMax, AxiBeat#(data_width)), UInt#(TLog#(numChecksMax)))) expectedWrite;
    method Bool requestCompleted();
endinterface

module mkAXIMemoryVerifier#(String mem_name)(AXIMemoryVerifier#(addr_width, data_width, id_width, numChecksMax))
    provisos(Mul#(data_bytes, 8, data_width),
             Div#(data_width, 8, data_bytes),
             Add#(a__, 8, addr_width));
    AXI4_Slave_Rd#(addr_width, data_width, id_width, 0) slave_rd <- mkAXI4_Slave_Rd(4, 32);
    AXI4_Slave_Wr#(addr_width, data_width, id_width, 0) slave_wr <- mkAXI4_Slave_Wr(2, 2, 2);

    Reg#(Bit#(addr_width)) addr_counter_write <- mkReg(0);
    Reg#(UInt#(9)) transfers_left_write <- mkReg(0);
    Reg#(Bit#(id_width)) cur_id_write <- mkRegU();

    Reg#(Vector#(numChecksMax, AxiBeat#(data_width))) expectedWriteData <- mkRegU;
    Reg#(UInt#(TLog#(numChecksMax))) writeChecksTotal <- mkReg(0);
    Reg#(UInt#(TLog#(numChecksMax))) writeChecksCurrent <- mkReg(0);

    rule handleWriteRequest if(transfers_left_write == 0);
        let r <- slave_wr.request_addr.get();
        transfers_left_write <= extend(r.burst_length) + 1;
        addr_counter_write <= r.addr;
        cur_id_write <= r.id;
        let addrLast = r.addr + (extend(pack(r.burst_length)) + 1) * fromInteger(valueOf(data_bytes));
        if(((addrLast - r.addr)/4096) != 0) begin
          $display("ERROR 4k boundary crossing!");
          $fatal();
        end
    endrule

    rule handleWriteData if(transfers_left_write != 0);
        let r <- slave_wr.request_data.get();
        transfers_left_write <= transfers_left_write - 1;
        addr_counter_write <= addr_counter_write + fromInteger(valueOf(TDiv#(data_width, 8)));
        let addr = addr_counter_write >> valueOf(TLog#(TDiv#(data_width, 8)));

        writeChecksCurrent <= writeChecksCurrent + 1;
        if(writeChecksCurrent >= writeChecksTotal) begin
            printColorTimed(RED, $format(mem_name + ": Got extra write request"));
        end

        if(writeChecksCurrent == writeChecksTotal - 1) begin
            printColorTimed(GREEN, $format(mem_name + ": Got last request"));
        end

        let expected = expectedWriteData[writeChecksCurrent];
        let expectedData = expected.data;
        let expectedStrb = expected.strb;

        if(expectedStrb != r.strb) begin
            printColorTimed(RED, $format("ERROR " + mem_name + ": Got false strb. Expected %x != received %x", expectedStrb, r.strb));
            $fatal();
        end

        Vector#(data_bytes, Bit#(8)) dataVec = unpack(r.data);
        Vector#(data_bytes, Bool) strbVec = unpack(r.strb);
        for(Integer i = 0; i < valueOf(data_bytes); i = i + 1) begin
            if(strbVec[i])
                dataVec[i] = dataVec[i];
            else
                dataVec[i] = 0;
        end

        if(expectedData != pack(dataVec)) begin
            printColorTimed(RED, $format("ERROR " + mem_name + ": Got false data. Expected %x != received %x", expectedData, r.data));
            $fatal();
        end

        if(transfers_left_write == 1) begin
            slave_wr.response.put(AXI4_Write_Rs {id: cur_id_write, resp: OKAY, user: 0});
        end
    endrule

    Reg#(Bit#(addr_width)) addr_counter <- mkReg(0);
    Reg#(UInt#(9)) transfers_left <- mkReg(0);
    Reg#(Bit#(id_width)) cur_id <- mkRegU();

    Reg#(Vector#(numChecksMax, AxiBeat#(data_width))) readDataExpected <- mkRegU;
    Reg#(UInt#(TLog#(numChecksMax))) readChecksTotal <- mkReg(0);
    Reg#(UInt#(TLog#(numChecksMax))) readChecksCurrent <- mkReg(0);

    rule handleReadRequest if(transfers_left == 0);
        let r <- slave_rd.request.get();
        transfers_left <= extend(r.burst_length) + 1;
        addr_counter <= r.addr;
        cur_id <= r.id;
        let addrLast = r.addr + (extend(pack(r.burst_length)) + 1) * fromInteger(valueOf(data_bytes));
        if(((addrLast - r.addr)/4096) != 0) begin
          $display("ERROR 4k boundary crossing!");
          $fatal();
        end
    endrule

    rule returnReadValue if(transfers_left != 0);
        transfers_left <= transfers_left - 1;
        addr_counter <= addr_counter + fromInteger(valueOf(TDiv#(data_width, 8)));

        let addr = addr_counter >> valueOf(TLog#(TDiv#(data_width, 8)));

        readChecksCurrent <= readChecksCurrent + 1;
        if(readChecksCurrent >= readChecksTotal) begin
            printColorTimed(RED, $format("ERROR " + mem_name + ": Got extra read request"));
            $fatal();
        end
        if(readChecksCurrent == readChecksTotal - 1) begin
            printColorTimed(GREEN, $format(mem_name + ": Got last request"));
        end

        let data = readDataExpected[readChecksCurrent].data;

        slave_rd.response.put(AXI4_Read_Rs {data: data, id: cur_id, resp: OKAY, last: transfers_left == 1, user: 0});
    endrule

    interface Put expectedWrite;
        method Action put(Tuple2#(Vector#(numChecksMax, AxiBeat#(data_width)), UInt#(TLog#(numChecksMax))) data);
            expectedWriteData <= tpl_1(data);
            writeChecksTotal <= tpl_2(data);
            writeChecksCurrent <= 0;
        endmethod
    endinterface

    interface Put expectedRead;
        method Action put(Tuple2#(Vector#(numChecksMax, AxiBeat#(data_width)), UInt#(TLog#(numChecksMax))) data);
            readDataExpected <= tpl_1(data);
            readChecksCurrent <= 0;
            readChecksTotal <= tpl_2(data);
        endmethod
    endinterface

    method Bool requestCompleted();
        return (readChecksTotal == readChecksCurrent) && (writeChecksTotal == writeChecksCurrent);
    endmethod

    interface rd = slave_rd.fab;
    interface wr = slave_wr.fab;
endmodule

function Action sendData(AXI4_Lite_Master_Wr#(a, b) m, Bit#(a) addr, Bit#(b) data, Bit#(TDiv#(b, 8)) strb);
    action
        m.request.put(AXI4_Lite_Write_Rq_Pkg {
            addr: addr,
            prot: UNPRIV_SECURE_DATA,
            data: data,
            strb: strb
            });
    endaction
endfunction

function Action readData(AXI4_Lite_Master_Rd#(a, b) m, Bit#(a) addr);
    action
        m.request.put(AXI4_Lite_Read_Rq_Pkg {addr: addr, prot: UNPRIV_SECURE_DATA});
    endaction
endfunction

function ActionValue#(Bit#(b)) readResponse(AXI4_Lite_Master_Rd#(a, b) m);
    actionvalue
        let r <- m.response.get();
        return r.data;
    endactionvalue
endfunction

function Action dropResponse(AXI4_Lite_Master_Wr#(a, b) m);
    action
        let r <- m.response.get();
    endaction
endfunction

function Action sendDataFull(AXI4_Master_Wr#(a, b, c, d) m, Bit#(a) addr, Bit#(b) data, Bit#(TDiv#(b, 8)) strb);
    action
        m.request_addr.put(AXI4_Write_Rq_Addr {
            id: 0,
            addr: addr,
            burst_length: 0,
            burst_size: bitsToBurstSize(valueOf(a)),
            burst_type: defaultValue,
            lock: defaultValue,
            cache: defaultValue,
            prot: defaultValue,
            qos: 0,
            region: 0,
            user: 0
            });

        m.request_data.put(AXI4_Write_Rq_Data {
        data: data,
        strb: strb,
        last: True,
        user: 0
        });
    endaction
endfunction

function Action readDataFull(AXI4_Master_Rd#(a, b, c, d) m, Bit#(a) addr);
    action
        m.request.put(AXI4_Read_Rq {
            id: 0,
            addr: addr,
            burst_length: 0,
            burst_size: bitsToBurstSize(valueOf(a)),
            burst_type: defaultValue,
            lock: defaultValue,
            cache: defaultValue,
            prot: defaultValue,
            qos: 0,
            region: 0,
            user: 0
        });
    endaction
endfunction

function ActionValue#(Bit#(b)) readResponseFull(AXI4_Master_Rd#(a, b, c, d) m);
    actionvalue
        let r <- m.response.get();
        return r.data;
    endactionvalue
endfunction

function Action dropResponseFull(AXI4_Master_Wr#(a, b, c, d) m);
    action
        let r <- m.response.get();
    endaction
endfunction

endpackage
