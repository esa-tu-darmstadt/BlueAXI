package GenericAxi4LiteSlave;

import GetPut :: *;
import FIFO :: *;
import SpecialFIFOs :: *;
import Vector :: *;
import List :: *;
import BUtils :: *;
import BRAM :: *;

import AXI4_Lite_Types :: *;
import AXI4_Lite_Slave :: *;

function List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) bramHandler(Integer start, Integer elements, BRAMServerBE#(a, t, n) r, List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) op)
    provisos(Bits#(a, a_sz),
             Bits#(t, axiDataWidth),
             Mul#(8, n, axiDataWidth),
             Div#(axiDataWidth, 8, n)
             );
    let length = elements * valueOf(n);
    op = List::cons(tagged ReadRangeDelayed ReadOperationRangeDelayed { index_min: fromInteger(start),
                                                                        index_max: fromInteger((start + length) - 1),
                                                                        fun: bramReader(start, r),
                                                                        ret: bramReaderRet(r) }, op);

    op = List::cons(tagged WriteRange WriteOperationRange { index_min: fromInteger(start),
                                                            index_max: fromInteger((start + length) - 1),
                                                            fun: bramWriter(start, r) }, op);
    return op;
endfunction

function List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) bramHandlerRO(Integer start, Integer elements,
                                        BRAMServerBE#(a, t, n) r, List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) op)
    provisos(Bits#(a, a_sz),
             Bits#(t, axiDataWidth),
             Div#(axiDataWidth, 8, n));
    let length = elements * valueOf(n);
    op = List::cons(tagged ReadRangeDelayed ReadOperationRangeDelayed { index_min: fromInteger(start),
                                                                        index_max: fromInteger((start + length) - 1),
                                                                        fun: bramReader(start, r),
                                                                        ret: bramReaderRet(r) }, op);
    return op;
endfunction

function List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) bramHandlerWO(Integer start, Integer elements, BRAMServerBE#(a, t, n) r, List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) op)
    provisos(Bits#(a, a_sz),
             Bits#(t, axiDataWidth),
             Mul#(8, n, axiDataWidth),
             Div#(axiDataWidth, 8, n));
    let length = elements * valueOf(n);
    op = List::cons(tagged WriteRange WriteOperationRange { index_min: fromInteger(start),
                                                                   index_max: fromInteger((start + length) - 1),
                                                                   fun: bramWriter(start, r) }, op);
    return op;
endfunction

function Action bramWriter(Integer start, BRAMServerBE#(a, b, c) bramPort,
                           Bit#(addr_w) addr, Bit#(b_sz) d, Bit#(c) s, AXI4_Lite_Prot p)
    provisos(Bits#(b, b_sz),
             Bits#(a, a_sz));
    action
        addr = addr - fromInteger(start);
        Bit#(a_sz) regNum = zExtend(addr >> valueOf(TLog#(TDiv#(b_sz, 8))));
        bramPort.request.put(BRAMRequestBE {writeen: s, responseOnWrite: False, address: unpack(regNum), datain: unpack(d)});
    endaction
endfunction

function Action bramReader(Integer start, BRAMServerBE#(a, b, c) bramPort,
                           Bit#(addr_w) addr, AXI4_Lite_Prot p)
    provisos(Bits#(a, a_sz),
             Bits#(b, b_sz));
    action
        addr = addr - fromInteger(start);
        Bit#(a_sz) regNum = zExtend(addr >> valueOf(TLog#(TDiv#(b_sz, 8))));
        bramPort.request.put(BRAMRequestBE {writeen: 0, responseOnWrite: False, address: unpack(regNum), datain: unpack(0)});
    endaction
endfunction

function ActionValue#(Bit#(b_sz)) bramReaderRet(BRAMServerBE#(a, b, c) bramPort)
    provisos(Bits#(b, b_sz));
    actionvalue
        let data <- bramPort.response.get();
        return pack(data);
    endactionvalue
endfunction

function List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) registerHandler(Integer register, Reg#(t) r, List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) op)
    provisos(Mul#(TDiv#(axiDataWidth, 8), 8, axiDataWidth),
             Bits#(t, a__));
    op = List::cons(tagged Read ReadOperation { index: fromInteger(register), fun: registerReader(r) }, op);
    op = List::cons(tagged Write WriteOperation { index: fromInteger(register), fun: registerWriter(r) }, op);
    return op;
endfunction

function List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) registerHandlerRO(Integer register, Reg#(t) r, List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) op)
    provisos(Mul#(TDiv#(axiDataWidth, 8), 8, axiDataWidth),
            Bits#(t, a__));
    op = List::cons(tagged Read ReadOperation { index: fromInteger(register), fun: registerReader(r) }, op);
    return op;
endfunction

function List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) registerHandlerWO(Integer register, Reg#(t) r, List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) op)
    provisos(Mul#(TDiv#(axiDataWidth, 8), 8, axiDataWidth),
            Bits#(t, a__));
    op = List::cons(tagged Write WriteOperation { index: fromInteger(register), fun: registerWriter(r) }, op);
    return op;
endfunction

function List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) valueHandlerRO(Integer register, t r, List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) op)
    provisos(Mul#(TDiv#(axiDataWidth, 8), 8, axiDataWidth),
            Bits#(t, a__));
    op = List::cons(tagged Read ReadOperation { index: fromInteger(register), fun: valueReader(r) }, op);
    return op;
endfunction

function Action registerWriter(Reg#(a) r, Bit#(data_width) d, Bit#(TDiv#(data_width, 8)) s, AXI4_Lite_Prot p)
    provisos(Mul#(TDiv#(data_width, 8), 8, data_width),
             Bits#(a, a__));
    action
        Vector#(TDiv#(data_width, 8), Bit#(8)) tr = cExtend(r);
        Vector#(TDiv#(data_width, 8), Bit#(8)) td = unpack(d);
        for(Integer i = 0; i < valueOf(TDiv#(data_width, 8)); i = i + 1) begin
            if(unpack(s[i])) begin
                tr[i] = td[i];
            end
        end
        r <= cExtend(tr);
    endaction
endfunction

function ActionValue#(Bit#(data_width)) registerReader(Reg#(a) r, AXI4_Lite_Prot p)
    provisos(Bits#(a, a__));
    actionvalue
        return cExtend(r);
    endactionvalue
endfunction

function ActionValue#(Bit#(data_width)) valueReader(b r, AXI4_Lite_Prot p)
    provisos(Bits#(b, b_sz));
    actionvalue
        return cExtend(r);
    endactionvalue
endfunction

function Action valueWriter(function Action fun(b d), Bit#(data_width) d, Bit#(TDiv#(data_width, 8)) s, AXI4_Lite_Prot p)
    provisos(Mul#(TDiv#(data_width, 8), 8, data_width),
             Bits#(b, b_sz));
    action
        fun(cExtend(d));
    endaction
endfunction

function Action fifoInterrupt(FIFO#(Bool) r, Bit#(data_width) d, Bit#(TDiv#(data_width, 8)) s, AXI4_Lite_Prot p);
    action
        r.enq(True);
    endaction
endfunction

function Action fifoWriter(FIFO#(t) r, Bit#(data_width) d, Bit#(TDiv#(data_width, 8)) s, AXI4_Lite_Prot p)
    provisos(Bits#(t, t_sz));
    action
        r.enq(unpack(zExtend(d)));
    endaction
endfunction

function ActionValue#(Bit#(t)) fifoReader(FIFO#(Bit#(data_width)) r, AXI4_Lite_Prot p);
    actionvalue
        r.deq();
        return zExtend(r.first());
    endactionvalue
endfunction

interface GenericAxi4LiteSlave#(numeric type axiAddrWidth,
                           numeric type axiDataWidth);
    interface AXI4_Lite_Slave_Rd_Fab#(axiAddrWidth, axiDataWidth) s_rd;
    interface AXI4_Lite_Slave_Wr_Fab#(axiAddrWidth, axiDataWidth) s_wr;
endinterface

module mkGenericAxi4LiteSlave#(List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) operators, Integer readSlaveBuffer, Integer writeSlaveBuffer)
                              (GenericAxi4LiteSlave#(axiAddrWidth,
                                                     axiDataWidth));

    // Find highest used register
    function Bit#(axiAddrWidth) getMax(RegisterOperator#(axiAddrWidth, axiDataWidth) a, Bit#(axiAddrWidth) b);
        let contender = 0;
        case (a) matches
            tagged Write .x: contender = x.index;
            tagged WriteRange .x: contender = x.index_max;
            tagged WriteRangeDelayed .x: contender = x.index_max;
            tagged Read  .x: contender = x.index;
            tagged ReadRange .x: contender = x.index_max;
            tagged ReadRangeDelayed .x: contender = x.index_max;
        endcase
        if(contender > b) return contender;
        else return b;
    endfunction

    function Integer bitToInteger(Bit#(n) x);
       Integer res = 0;
       for (Integer i=0; i<valueOf(n); i=i+1)
          if (x[i] == 1)
             res = res + 2**i;
       return res;
    endfunction

    Integer highestRegister = log2(bitToInteger(List::foldr(getMax, 0, operators)) + 1);

    Bit#(axiAddrWidth) byteEscape = ((1 << valueOf(TLog#(TDiv#(axiDataWidth, 8)))) - 1);
    Bit#(axiAddrWidth) addrEscape = ((1 << highestRegister) - 1) - byteEscape;

    AXI4_Lite_Slave_Rd#(axiAddrWidth, axiDataWidth) readSlave <- mkAXI4_Lite_Slave_Rd(readSlaveBuffer);
    AXI4_Lite_Slave_Wr#(axiAddrWidth, axiDataWidth) writeSlave <- mkAXI4_Lite_Slave_Wr(writeSlaveBuffer);

    function Bool isItem(Bool write, Bit#(axiAddrWidth) i, RegisterOperator#(axiAddrWidth, axiDataWidth) r);
        case (r) matches
            tagged Write .x: return (write && x.index == i);
            tagged WriteRange .x: return (write && i >= x.index_min && i < x.index_max);
            tagged WriteRangeDelayed .x: return (write && i >= x.index_min && i < x.index_max);
            tagged Read  .x: return (!write && x.index == i);
            tagged ReadRange .x: return (!write && i >= x.index_min && i < x.index_max);
            tagged ReadRangeDelayed .x: return (!write && i >= x.index_min && i < x.index_max);
            default: return False;
        endcase
    endfunction

    function Bool getOnlyReadOrWriteItem(Bool write, Bool range, Bool delayed, RegisterOperator#(axiAddrWidth, axiDataWidth) r);
        case (r) matches
            tagged Write .x: return write && !range && !delayed;
            tagged WriteRange .x: return write && range && !delayed;
            tagged WriteRangeDelayed .x: return write && range && delayed;
            tagged Read  .x: return !write && !range && !delayed;
            tagged ReadRange .x: return !write && range && !delayed;
            tagged ReadRangeDelayed .x: return !write && range && delayed;
            default: return False;
        endcase
    endfunction

    Rules readRules = emptyRules();

    Reg#(Bool) readBusy <- mkReg(False);
    Wire#(Bool) readIsHandled <- mkDWire(False);

    for(List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) l = List::filter(getOnlyReadOrWriteItem(False, False, False), operators); l != Nil; l = List::tail(l)) begin
        readRules = rJoinMutuallyExclusive(rules
            rule axiReadSpecial if(List::head(l) matches tagged Read .x &&& isItem(False, readSlave.first().addr & addrEscape, List::head(l)) &&& !readBusy);
                let req <- readSlave.request.get();

                Bit#(axiDataWidth) retVal <- x.fun(req.prot);

                AXI4_Lite_Read_Rs_Pkg#(axiDataWidth) response;
                response.resp = OKAY;
                response.data = retVal;
                readSlave.response.put(response);
                //$display("Special: Read request for register %d: %h %x", req.addr & addrEscape, response.data, addrEscape);
            endrule

            rule axiReadSpecialIsHandled if(List::head(l) matches tagged Read .x &&& isItem(False, readSlave.first().addr & addrEscape, List::head(l)));
                readIsHandled <= True;
            endrule

        endrules, readRules);
    end

    for(List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) l = List::filter(getOnlyReadOrWriteItem(False, True, False), operators); l != Nil; l = List::tail(l)) begin
        readRules = rJoinMutuallyExclusive(rules
            rule axiReadSpecialRange if(List::head(l) matches tagged ReadRange .x &&& isItem(False, readSlave.first().addr & addrEscape, List::head(l)) &&& !readBusy);
                let req <- readSlave.request.get();

                Bit#(axiDataWidth) retVal <- x.fun(readSlave.first().addr, req.prot);

                AXI4_Lite_Read_Rs_Pkg#(axiDataWidth) response;
                response.resp = OKAY;
                response.data = retVal;
                readSlave.response.put(response);
                //$display("SpecialRange: Read request for register %d: %h %x", req.addr & addrEscape, response.data, addrEscape);
            endrule

            rule axiReadSpecialRangeIsHandled if(List::head(l) matches tagged ReadRange .x &&& isItem(False, readSlave.first().addr & addrEscape, List::head(l)));
                readIsHandled <= True;
            endrule
        endrules, readRules);
    end

    for(List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) l = List::filter(getOnlyReadOrWriteItem(False, True, True), operators); l != Nil; l = List::tail(l)) begin
        Reg#(Bool) active <- mkReg(False);
        readRules = rJoinMutuallyExclusive(rules
            rule axiReadSpecialRangeDelayed if(List::head(l) matches tagged ReadRangeDelayed .x &&& isItem(False, readSlave.first().addr & addrEscape, List::head(l)) &&& !readBusy);
                x.fun(readSlave.first().addr, readSlave.first().prot);

                readBusy <= True;
                active <= True;
                //$display("SpecialRangeDelayed: Read request for register %d: %h %x", req.addr & addrEscape, addrEscape);
            endrule

            rule axiReadSpecialRangeDelayedReturn if(readBusy &&& List::head(l) matches tagged ReadRangeDelayed .x &&& active);
                let req <- readSlave.request.get();

                Bit#(axiDataWidth) retVal <- x.ret();
                AXI4_Lite_Read_Rs_Pkg#(axiDataWidth) response;
                response.resp = OKAY;
                response.data = retVal;
                readSlave.response.put(response);
                readBusy <= False;
                active <= False;
            endrule

            rule axiReadSpecialRangeDelayedIsHandled if(List::head(l) matches tagged ReadRangeDelayed .x &&& isItem(False, readSlave.first().addr & addrEscape, List::head(l)));
                readIsHandled <= True;
            endrule
        endrules, readRules);
    end

    readRules = rJoinMutuallyExclusive(readRules, rules
        rule axiReadFallback if(!readIsHandled);
            let req <- readSlave.request.get();
            Bit#(axiDataWidth) retVal;
            retVal = 0;
            AXI4_Lite_Read_Rs_Pkg#(axiDataWidth) response;
            response.resp = OKAY;
            response.data = retVal;
            readSlave.response.put(response);
            //$display("Read request for register %d: %x %x %h", req.addr & addrEscape, addrEscape, byteEscape, response.data);
        endrule
    endrules);

    addRules(readRules);

    Wire#(Bool) writeIsHandled <- mkDWire(False);
    Reg#(Bool) writeBusy <- mkReg(False);

    Rules writeRules = emptyRules();
    for(List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) l = List::filter(getOnlyReadOrWriteItem(True, False, False), operators); l != Nil; l = List::tail(l)) begin
        writeRules = rJoinConflictFree(rules
            rule axiWriteSpecial if(List::head(l) matches tagged Write .x &&& isItem(True, writeSlave.first().addr & addrEscape, List::head(l)) &&& !writeBusy);
                let req <- writeSlave.request.get();

                x.fun(req.data, req.strb, req.prot);
                //$display("Write request for register %b %d: data %h enabled %h", addrEscape, req.addr & addrEscape, req.data, req.strb);

                AXI4_Lite_Write_Rs_Pkg response;
                response.resp = OKAY;
                writeSlave.response.put(response);
            endrule

            rule axiWriteSpecialIsHandled if(List::head(l) matches tagged Write .x &&& isItem(True, writeSlave.first().addr & addrEscape, List::head(l)));
                writeIsHandled <= True;
            endrule
        endrules, writeRules);
    end

    for(List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) l = List::filter(getOnlyReadOrWriteItem(True, True, False), operators); l != Nil; l = List::tail(l)) begin
        writeRules = rJoinConflictFree(rules
            rule axiWriteSpecialRange if(List::head(l) matches tagged WriteRange .x &&& isItem(True, writeSlave.first().addr & addrEscape, List::head(l)) &&& !writeBusy);
                let req <- writeSlave.request.get();

                x.fun(writeSlave.first().addr, req.data, req.strb, req.prot);
                //$display("Write request for register %b %d: data %h enabled %h", addrEscape, req.addr & addrEscape, req.data, req.strb);

                AXI4_Lite_Write_Rs_Pkg response;
                response.resp = OKAY;
                writeSlave.response.put(response);
            endrule

            rule axiWriteSpecialRangeIsHandled if(List::head(l) matches tagged WriteRange .x &&& isItem(True, writeSlave.first().addr & addrEscape, List::head(l)));
                writeIsHandled <= True;
            endrule
        endrules, writeRules);
    end

    for(List#(RegisterOperator#(axiAddrWidth, axiDataWidth)) l = List::filter(getOnlyReadOrWriteItem(True, True, True), operators); l != Nil; l = List::tail(l)) begin
        Reg#(Bool) active <- mkReg(False);
        writeRules = rJoinMutuallyExclusive(rules
            rule axiWriteSpecialRangeDelayed if(List::head(l) matches tagged WriteRangeDelayed .x &&& isItem(True, writeSlave.first().addr & addrEscape, List::head(l)) &&& !writeBusy);
                x.fun(writeSlave.first().addr, writeSlave.first().data, writeSlave.first().strb, writeSlave.first().prot);

                writeBusy <= True;
                active <= True;
                //$display("SpecialRangeDelayed: Write request for register %d: %h %x", req.addr & addrEscape, addrEscape);
            endrule

            rule axiWriteSpecialRangeDelayedReturn if(writeBusy &&& List::head(l) matches tagged WriteRangeDelayed .x &&& active);
                let req <- writeSlave.request.get();
                x.ret();
                AXI4_Lite_Write_Rs_Pkg response;
                response.resp = OKAY;
                writeSlave.response.put(response);
                writeBusy <= False;
                active <= False;
            endrule

            rule axiWriteSpecialRangeDelayedIsHandled if(List::head(l) matches tagged WriteRangeDelayed .x &&& isItem(True, writeSlave.first().addr & addrEscape, List::head(l)));
                writeIsHandled <= True;
            endrule
        endrules, writeRules);
    end

    writeRules = rJoinDescendingUrgency(writeRules, rules
        rule axiWriteFallback if(!writeIsHandled);
            let req <- writeSlave.request.get();

            AXI4_Lite_Write_Rs_Pkg response;
            response.resp = OKAY;
            writeSlave.response.put(response);
        endrule
    endrules);

    addRules(writeRules);

    interface s_rd  = readSlave.fab;
    interface s_wr  = writeSlave.fab;
endmodule

endpackage