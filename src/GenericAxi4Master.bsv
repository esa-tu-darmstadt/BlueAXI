package GenericAxi4Master;

import GetPut :: *;
import ClientServer :: *;
import FIFO :: *;
import FIFOF :: *;
import BRAMFIFO :: *;
import SpecialFIFOs :: *;
import Vector :: *;
import BUtils :: *;

import AXI4_Types :: *;
import AXI4_Master :: *;

typedef struct {
    Bit#(addr_width)  address;
    UInt#(amount_width) bytesToTransfer;
    Bit#(4) region;
} AxiRequest#(numeric type addr_width, numeric type amount_width) deriving(Bits, Eq, FShow);

typedef 4096 TransferBarrier; // Axi does not support transfers over 4096 barriers.

// Should be calculated from max burst length and max outstanding. However, as that's not a numeric type it can't be easily calculated.
// 12 Bits should be easy enough on the logic to not matter and allow for 16 outstanding bursts of 256 beats, which is more than any scenario right now requires.
typedef 12 ENFORCE_BITS;

typedef struct {
        Bit#(data_bytes_addr) bytes_first;
        Bit#(data_bytes_addr) bytes_last;
        Bit#(transfers_total_size) transfers_total;
        Bit#(data_bytes_addr) addr_offset;
        Bool first;
    } Task_data_output#(numeric type transfers_total_size, numeric type data_bytes_addr) deriving(Bits, Eq, FShow);

typedef struct {
        Bit#(9) requests_first;
        Bit#(9) requests_last;
        Bit#(transfers_total_size) requests_total;
        Bit#(addr_aligned) address_static;
        Bit#(amount_aligned) address_dynamic;
        Bit#(TLog#(TransferBarrier)) offset_first;
        Bit#(4) region;
        Bool first;
    } Task_data_requests#(numeric type transfers_total_size, numeric type addr_width, numeric type addr_aligned, numeric type amount_aligned) deriving(Bits, Eq, FShow);

function Bit#(a) extendLSB(Bit#(b) data)
    provisos(Add#(b, diff, a));
    Bit#(a) l = extend(data);
    return l << valueOf(diff);
endfunction

interface RequestGenerator#(numeric type addr_width, numeric type amount_width, numeric type data_bytes, numeric type addr_aligned, numeric type amount_aligned);
    interface Server#(AxiRequest#(addr_width, amount_width), Tuple2#(Task_data_requests#(TSub#(amount_width, TLog#(data_bytes)), addr_width, addr_aligned, amount_aligned), Task_data_output#(TSub#(amount_width, TLog#(data_bytes)), TLog#(data_bytes)))) gen;
endinterface

typedef struct {
        Bit#(data_bytes_addr) bytes_first;
        Bit#(data_bytes_addr) bytes_last;
        Bit#(transfers_total_size) transfers_total;
        AxiRequest#(addrwidth, amount_width) req;
        Bit#(9) requests_first;
    } Intermediate4kBarrier#(numeric type data_bytes_addr,
                             numeric type transfers_total_size,
                             numeric type addrwidth,
                             numeric type amount_width) deriving(Bits, Eq);

module mkRequestGenerator#(String moduleName, Integer burst_length)(RequestGenerator#(addrwidth, amount_width, data_bytes, addr_aligned, amount_aligned))
    provisos(Log#(data_bytes, data_bytes_addr),
            Add#(data_bytes_addr, transfers_total_size, amount_width),
            Log#(TransferBarrier, transfer_barrier_addr),
            Add#(a__, amount_width, addrwidth),
            Add#(b__, data_bytes_addr, addrwidth),
            Add#(addr_aligned, c__, addrwidth),
            Add#(d__, 9, transfers_total_size),
            Add#(e__, 12, addrwidth),
            Add#(g__, amount_aligned, addrwidth)
    );
    function Bit#(data_bytes_addr) calcBytesFirst(Bit#(addrwidth) addr, Bit#(amount_width) bytes_total);
        Bit#(addrwidth) bytes_first = (addr % fromInteger(valueOf(data_bytes)));
        if(bytes_first != 0)
            bytes_first = fromInteger(valueOf(data_bytes)) - bytes_first;
        if(extend(bytes_total) < bytes_first || (bytes_first == 0 && bytes_total < fromInteger(valueOf(data_bytes))))
            bytes_first = extend(bytes_total);
        return truncate(bytes_first);
    endfunction

    function Bit#(data_bytes_addr) calcBytesLast(Bit#(amount_width) bytes_total, Bit#(data_bytes_addr) bytes_first);
        Bit#(data_bytes_addr) bytes_last = truncate((bytes_total - extend(bytes_first)) % fromInteger(valueOf(data_bytes)));
        return bytes_last;
    endfunction

    function Bit#(addr_aligned) escapeAddress(Bit#(addrwidth) addr);
        return truncateLSB(addr);
    endfunction

    function Bit#(amount_aligned) dynamicPartAddress(Bit#(addrwidth) addr);
        let b = addr >> fromInteger(valueOf(transfer_barrier_addr));
        return truncate(b);
    endfunction

    function Bit#(data_bytes_addr) addressOffset(Bit#(addrwidth) addr);
        return truncate(addr % fromInteger(valueOf(data_bytes)));
    endfunction

    function Bit#(transfers_total_size) calcTransfersTotal(Bit#(amount_width) bytes_total, Bit#(data_bytes_addr) bytes_first, Bit#(data_bytes_addr) bytes_last);
        let transfers_total = (bytes_total - extend(bytes_first) - extend(bytes_last)) / fromInteger(valueOf(data_bytes));
        if(bytes_first != 0)
            transfers_total = transfers_total + 1;
        if(bytes_last != 0)
            transfers_total = transfers_total + 1;
        return truncate(transfers_total);
    endfunction

    function Bool crossesBarrier(Bit#(addrwidth) addr, Bit#(9) burst_length);
        Bit#(TLog#(TransferBarrier)) addr_last = truncate(addr) + extend(burst_length) * fromInteger(valueOf(data_bytes));
        addr_last = addr_last - 1;
        Bit#(TLog#(TransferBarrier)) addr_first = truncate(addr);
        Bool ret = False;
        if(addr_last <= addr_first)
            ret = True;
        return ret;
    endfunction

    function Bit#(9) calcFirstRequest(Bit#(addrwidth) addr, Bit#(9) burst_length);
        Bit#(9) requests_first = 0;
        if(crossesBarrier(addr, burst_length)) begin
            Bit#(TAdd#(1, TLog#(TransferBarrier))) tmp = fromInteger(valueOf(TransferBarrier));
            Bit#(TLog#(TransferBarrier)) addr_t = truncate(addr);
            Bit#(TLog#(TransferBarrier)) barrier_bytes = truncate(tmp - extend(addr_t));
            Bit#(TLog#(TransferBarrier)) barrier_transfers = barrier_bytes / fromInteger(valueOf(data_bytes));
            requests_first = truncate(barrier_transfers);
        end
        return requests_first;
    endfunction

    FIFO#(AxiRequest#(addrwidth, amount_width)) incomingBuffer <- mkFIFO();
    FIFO#(Tuple3#(Bit#(data_bytes_addr), Bit#(data_bytes_addr), AxiRequest#(addrwidth, amount_width))) intermediateBuffer <- mkFIFO();
    FIFO#(Tuple4#(Bit#(data_bytes_addr), Bit#(data_bytes_addr), Bit#(transfers_total_size), AxiRequest#(addrwidth, amount_width))) intermediateBuffer2 <- mkFIFO();
    FIFO#(Intermediate4kBarrier#(data_bytes_addr, transfers_total_size, addrwidth, amount_width)) intermediateBuffer3 <- mkFIFO();
    FIFO#(Tuple2#(Task_data_requests#(transfers_total_size, addrwidth, addr_aligned, amount_aligned), Task_data_output#(transfers_total_size, data_bytes_addr))) outgoingBuffer <- mkFIFO();

    rule processRequest;
        let req = incomingBuffer.first();
        incomingBuffer.deq();
        let bytes_first = calcBytesFirst(req.address, pack(req.bytesToTransfer));
        let bytes_last = calcBytesLast(pack(req.bytesToTransfer), bytes_first);

        intermediateBuffer.enq(tuple3(bytes_first, bytes_last, req));
    endrule

    rule calculateTransfersTotal;
        let req_in = intermediateBuffer.first(); intermediateBuffer.deq();
        let bytes_first = tpl_1(req_in);
        let bytes_last = tpl_2(req_in);
        let req = tpl_3(req_in);
        let transfers_total = calcTransfersTotal(pack(req.bytesToTransfer), bytes_first, bytes_last);
        intermediateBuffer2.enq(tuple4(bytes_first, bytes_last, transfers_total, req));
    endrule

    rule check4kBarrier;
        let req_in = intermediateBuffer2.first(); intermediateBuffer2.deq();
        let bytes_first = tpl_1(req_in);
        let bytes_last = tpl_2(req_in);
        let transfers_total = tpl_3(req_in);
        let req = tpl_4(req_in);

        Bit#(9) burst_length_t = fromInteger(burst_length);
        if(transfers_total < fromInteger(burst_length))
            burst_length_t = truncate(transfers_total);

        Bit#(9) requests_first = calcFirstRequest(req.address, burst_length_t);

        let req_out = Intermediate4kBarrier {
            bytes_first: bytes_first,
            bytes_last: bytes_last,
            transfers_total: transfers_total,
            req: req,
            requests_first: requests_first
        };

        intermediateBuffer3.enq(req_out);
    endrule

    rule finishRequest;
        let req_in = intermediateBuffer3.first(); intermediateBuffer3.deq();

        let request_output = Task_data_output {
            bytes_first: req_in.bytes_first,
            bytes_last: req_in.bytes_last,
            transfers_total: req_in.transfers_total,
            addr_offset: addressOffset(req_in.req.address),
            first: True
        };

        let transfers_total_t = req_in.transfers_total - extend(req_in.requests_first);
        Bit#(9) requests_last = truncate(transfers_total_t % fromInteger(burst_length));
        Bit#(transfers_total_size) requests_total = truncate(transfers_total_t / fromInteger(burst_length));

        if(burst_length == 1) begin
            requests_total = req_in.transfers_total;
            req_in.requests_first = 0;
            requests_last = 0;
        end else begin
            if(requests_last != 0)
                requests_total = requests_total + 1;

            if(req_in.requests_first != 0)
                requests_total = requests_total + 1;
        end

        Task_data_requests#(transfers_total_size, addrwidth, addr_aligned, amount_aligned) request_data
        = Task_data_requests {
            requests_first: req_in.requests_first,
            requests_last: requests_last,
            requests_total: requests_total,
            address_static: escapeAddress(req_in.req.address),
            address_dynamic: dynamicPartAddress(req_in.req.address),
            offset_first: truncate(req_in.req.address),
            region : req_in.req.region,
            first: True
        };

        outgoingBuffer.enq(tuple2(request_data, request_output));

        //$display("[" + moduleName + "] request:");
        //$display("[" + moduleName + "] bytes_first: %d", request_output.bytes_first);
        //$display("[" + moduleName + "] bytes_last: %d", request_output.bytes_last);
        //$display("[" + moduleName + "] transfers_total: %d", request_output.transfers_total);
        //$display("[" + moduleName + "] addr_offset: %d", request_output.addr_offset);
        //$display("[" + moduleName + "] requests_first: %d", request_data.requests_first);
        //$display("[" + moduleName + "] requests_last: %d", request_data.requests_last);
        //$display("[" + moduleName + "] requests_total: %d", request_data.requests_total);
        //$display("[" + moduleName + "] address_static: %x", request_data.address_static);
        //$display("[" + moduleName + "] address_dynamic: %x", request_data.address_dynamic);
    endrule

    interface Server gen;
        interface Put request = toPut(incomingBuffer);
        interface Get response = toGet(outgoingBuffer);
    endinterface
endmodule

interface Axi4MasterRead#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width, numeric type amount_width);
    interface AXI4_Master_Rd_Fab#(addrwidth, datawidth, id_width, user_width) fab;

    interface Server#(AxiRequest#(addrwidth, amount_width), Bit#(datawidth)) server;

    method Bool active();

    method UInt#(amount_width) status_4kCrossings();
    method Bit#(amount_width) delayBetweenGets();
    method Bit#(amount_width) lastRead();
    method Integer getBurstLength();
endinterface

module mkAxi4MasterRead#(Integer requestBuffer, Integer dataBuffer, Bool bram, Integer burst_length, Bool checkBarrier, Integer maxOutstanding, Bool enforce_outstanding, Bool buffer_outstanding)(Axi4MasterRead#(addrwidth, datawidth, id_width, user_width, amount_width))
    provisos(Mul#(data_bytes, 8, datawidth),
             Add#(data_bytes_addr, transfers_total_size, amount_width),
             Log#(data_bytes, data_bytes_addr),
             Mul#(burst_length_max, data_bytes, TransferBarrier),
             Log#(TransferBarrier, transfer_barrier_addr),
             Add#(amount_width, addr_aligned_p1, addrwidth),
             Add#(1, addr_aligned, addr_aligned_p1),
             Add#(transfer_barrier_addr, amount_aligned_m1, amount_width),
             Add#(1, amount_width, amount_width_p1),
             Add#(1, amount_aligned_m1, amount_aligned),
             Add#(b__, data_bytes_addr, addrwidth),
             Add#(addr_aligned, c__, addrwidth),
             Add#(d__, 9, transfers_total_size),
             Add#(e__, 12, addrwidth),
             Add#(a__, amount_aligned, addrwidth),
             Add#(1, f__, datawidth)
             );

    Integer burst_length_used = burst_length;
    Bool always_offset = False;

    if(burst_length > valueOf(burst_length_max)) begin
        burst_length_used = valueOf(burst_length_max);
        messageM("Burst_length " + integerToString(burst_length) + " is larger than AXI maximum of 4096 bytes, will reset burst_length to " + integerToString(burst_length_used));
    end

    if(burst_length_used != valueOf(burst_length_max)) begin
        messageM("Burst_length is less than AXI maximum of 4096 bytes. This leads to more hardware resource usage for extra counters.");
        always_offset = True;
    end

    Integer max_outstanding_beats = maxOutstanding * burst_length_used;
    Reg#(UInt#(ENFORCE_BITS)) outstanding_beats[2] <- mkCReg(2, fromInteger(max_outstanding_beats));

    function Bool canPlaceBurst(Bit#(8) beatsThisRequest);
        return !enforce_outstanding || outstanding_beats[0] >= (cExtend(beatsThisRequest) + 1);
    endfunction

    function Action decreaseOutstanding(Bit#(8) beatsThisRequest);
        action
            outstanding_beats[0] <= outstanding_beats[0] - (cExtend(beatsThisRequest) + 1);
        endaction
    endfunction

    function Action increaseOutstanding();
        action
            outstanding_beats[1] <= outstanding_beats[1] + 1;
        endaction
    endfunction

    function Bit#(TLog#(TransferBarrier)) bytesPerFullBurst();
        return fromInteger(valueOf(data_bytes) * burst_length_used);
    endfunction

    RequestGenerator#(addrwidth, amount_width, data_bytes, addr_aligned, amount_aligned) reqGen <- mkRequestGenerator("READ", burst_length_used);

    // Axi related data
    AXI4_Master_Rd#(addrwidth, datawidth, id_width, user_width) master_rd <- mkAXI4_Master_Rd(maxOutstanding, dataBuffer, bram);

    Reg#(Task_data_requests#(transfers_total_size, addrwidth, addr_aligned, amount_aligned)) task_data_requests_reg <- mkReg(unpack(0));

    FIFO#(Bool) maxOutstandingFIFO <- mkSizedFIFO(maxOutstanding);
    FIFO#(AXI4_Read_Rq#(addrwidth, id_width, user_width)) read_rq_out <- mkFIFO();

    // Place requests to Bus
    rule placeRequest if(task_data_requests_reg.requests_total != 0);
        let task_data_requests_reg_new = task_data_requests_reg;
        Bit#(addrwidth) address_this = {task_data_requests_reg.address_static, task_data_requests_reg.address_dynamic, task_data_requests_reg.offset_first};

        Bit#(8) beatsThisRequest = fromInteger(burst_length_used - 1);
        if(task_data_requests_reg.requests_total == 1 && task_data_requests_reg.requests_last != 0)
            beatsThisRequest = truncate(task_data_requests_reg.requests_last - 1); // AXI 0 == 1 beat etc
        else if(task_data_requests_reg.first && task_data_requests_reg.requests_first != 0)
            beatsThisRequest = truncate(task_data_requests_reg.requests_first - 1);


        let request = AXI4_Read_Rq {id: 0, addr: address_this, burst_length: unpack(beatsThisRequest), burst_size: bitsToBurstSize(valueOf(datawidth))
                                    , burst_type: INCR, lock: NORMAL, cache: NORMAL_NON_CACHEABLE_BUFFERABLE,  prot: UNPRIV_SECURE_DATA
                                    , qos: 0, region: task_data_requests_reg.region, user: 0
                                    };
        read_rq_out.enq(request);

        task_data_requests_reg_new.requests_total = task_data_requests_reg.requests_total - 1;

        maxOutstandingFIFO.enq(True);

        if(always_offset) begin
            task_data_requests_reg_new.offset_first = task_data_requests_reg.offset_first + bytesPerFullBurst();
            if(task_data_requests_reg_new.offset_first <= task_data_requests_reg.offset_first) begin
                task_data_requests_reg_new.address_dynamic = task_data_requests_reg.address_dynamic + 1;
            end
        end else begin
            task_data_requests_reg_new.offset_first = 0;
            task_data_requests_reg_new.address_dynamic = task_data_requests_reg.address_dynamic + 1;
        end
        task_data_requests_reg_new.first = False;

        task_data_requests_reg <= task_data_requests_reg_new;

        //$display("%d", outstanding_beats[1]);
        //$display("(%0d) Processing READ request: Address: %x, beats this request %d, requests_total %d", $time, address_this, beatsThisRequest + 1, task_data_requests_reg.requests_total);
    endrule

    rule placeAXI;
        let request = read_rq_out.first();
        if(canPlaceBurst(pack(request.burst_length))) begin
            //$display("(%0d) Read Out %d", $time, outstanding_beats[0]);
            read_rq_out.deq();
            master_rd.request.put(request);
            decreaseOutstanding(pack(request.burst_length));
        end
    endrule

    FIFO#(Bit#(datawidth)) outgoingBuffer = ?;
    if(buffer_outstanding) begin
        outgoingBuffer <- mkSizedBRAMFIFO(max_outstanding_beats);
    end else begin
        outgoingBuffer <- mkPipelineFIFO();
    end

    FIFO#(Bit#(datawidth)) relaxationBuffer <- mkFIFO();

    Reg#(Task_data_output#(transfers_total_size, data_bytes_addr)) task_data_output_reg <- mkReg(unpack(0));
    rule forwardData if(task_data_output_reg.transfers_total != 0);
        //$display("Forward Read Data %d", clkCntr);
        let r <- master_rd.response.get();

        task_data_output_reg.transfers_total <= task_data_output_reg.transfers_total - 1;
        if(r.last)
            maxOutstandingFIFO.deq();

        outgoingBuffer.enq(r.data);
    endrule

    rule forwardRelaxation;
        let r = outgoingBuffer.first(); outgoingBuffer.deq();
        relaxationBuffer.enq(r);
        increaseOutstanding();
        //$display("(%d) Read In %d", $time, outstanding_beats[1]);
    endrule

    function Bool isBusy();
        return task_data_output_reg.transfers_total != 0 || task_data_requests_reg.requests_total != 0;
    endfunction

    rule fillBuffer if(!isBusy());
        let t <- reqGen.gen.response.get();
        task_data_requests_reg <= tpl_1(t);
        task_data_output_reg <= tpl_2(t);
    endrule

    interface Server server;
        interface Put request;
            method Action put(AxiRequest#(addrwidth, amount_width) req);
                reqGen.gen.request.put(req);
            endmethod
        endinterface
        interface Get response = toGet(relaxationBuffer);
    endinterface

    method UInt#(amount_width) status_4kCrossings();
        return 0;
    endmethod

    method Bit#(amount_width) delayBetweenGets();
        return 0;
    endmethod

    method Bit#(amount_width) lastRead();
        return 0;
    endmethod

    interface active = isBusy();
    interface fab = master_rd.fab;
    interface getBurstLength = burst_length_used;
endmodule

import StmtFSM :: *;
import AXI4_Slave :: *;
import Connectable :: *;

module mkReadTest(Empty);

    Axi4MasterRead#(32, 32, 1, 1, 31) master_rd <- mkAxi4MasterRead(2, 2, True, 256, True, 2, False, False);
    AXI4_Slave_Rd#(32, 32, 1, 1) slave_rd <- mkAXI4_Slave_Rd(2, 2);
    mkConnection(master_rd.fab, slave_rd.fab);

    Reg#(UInt#(32)) readCntr <- mkReg(0);
    Reg#(UInt#(32)) readCurCntr <- mkReg(0);
    Reg#(AXI4_Read_Rq#(32, 1, 1)) request <- mkRegU();

    rule handleReadRequest if(readCurCntr == 0);
        let r <- slave_rd.request.get();
        readCntr <= readCntr + extend(r.burst_length) + 1;
        readCurCntr <= extend(r.burst_length) + 1;
        request <= r;
        $display("Request for %x %d", r.addr, r.burst_length);
    endrule

    rule returnReadValue if(readCurCntr != 0);
        slave_rd.response.put(AXI4_Read_Rs {data: pack(readCurCntr), id: request.id, resp: OKAY, last: readCurCntr == 1, user: 0});
        readCurCntr <= readCurCntr - 1;
    endrule

    Reg#(UInt#(32)) responseCntr <- mkReg(0);
    rule readResponse;
        let r <- master_rd.server.response.get();
        responseCntr <= responseCntr + 1;
        $display("Got response %d: %d", responseCntr, r);
    endrule

    Reg#(UInt#(32)) clkCntr <- mkReg(0);
    Reg#(UInt#(32)) startTime <- mkRegU;

    rule cnt;
        clkCntr <= clkCntr + 1;
    endrule

    Stmt fsm = {seq
            $display("Hello World!");
            action
                master_rd.server.request.put(AxiRequest {address: 16380, bytesToTransfer: 130, region: 0});
                startTime <= clkCntr;
            endaction
            await(master_rd.active);
            await(!master_rd.active);
            $display("Done at %d", clkCntr - startTime);
            delay(1000);
        endseq
    };

    mkAutoFSM(fsm);
endmodule

interface Axi4MasterWrite#(numeric type addrwidth, numeric type datawidth, numeric type id_width, numeric type user_width, numeric type amount_width);
    interface AXI4_Master_Wr_Fab#(addrwidth, datawidth, id_width, user_width) fab;

    interface Put#(AxiRequest#(addrwidth, amount_width)) request;
    interface Put#(Bit#(datawidth)) data;

    method Bool active();

    method UInt#(amount_width) status_4kCrossings();
    method Bit#(amount_width) delayBetweenPuts();
    method Bit#(amount_width) lastWritten();
    method Integer getBurstLength();
endinterface

module mkAxi4MasterWrite#(Integer requestBuffer, Integer dataBuffer, Bool bram, Integer burst_length, Bool checkBarrier, Integer maxOutstanding, Bool enforce_outstanding, Integer fillGradeShift)(Axi4MasterWrite#(addrwidth, datawidth, id_width, user_width, amount_width))
    provisos(Mul#(data_bytes, 8, datawidth),
             Add#(data_bytes_addr, transfers_total_size, amount_width),
             Log#(data_bytes, data_bytes_addr),
             Mul#(burst_length_max, data_bytes, 4096),
             Log#(TransferBarrier, transfer_barrier_addr),
             Add#(amount_width, addr_aligned_p1, addrwidth),
             Add#(1, addr_aligned, addr_aligned_p1),
             Add#(transfer_barrier_addr, amount_aligned_m1, amount_width),
             Add#(1, amount_width, amount_width_p1),
             Add#(1, amount_aligned_m1, amount_aligned),
             Add#(b__, data_bytes_addr, addrwidth),
             Add#(addr_aligned, c__, addrwidth),
             Add#(d__, 9, transfers_total_size),
             Add#(e__, 12, addrwidth),
             Add#(a__, amount_aligned, addrwidth),
             Bits#(Vector::Vector#(data_bytes, Bool), TDiv#(datawidth, 8)),
             Add#(1, f__, datawidth)
             );

    Integer burst_length_used = burst_length;
    Bool always_offset = False;

    if(burst_length > valueOf(burst_length_max)) begin
        burst_length_used = valueOf(burst_length_max);
        messageM("Burst_length " + integerToString(burst_length) + " is larger than AXI maximum of 4096 bytes, will reset burst_length to " + integerToString(burst_length_used));
    end

    if(burst_length_used != valueOf(burst_length_max)) begin
        messageM("Burst_length is less than AXI maximum of 4096 bytes. This leads to more hardware resource usage for extra counters.");
        always_offset = True;
    end

    Reg#(UInt#(ENFORCE_BITS)) outstanding_beats[2] <- mkCReg(2, 0);
    Integer max_outstanding_beats = maxOutstanding * burst_length_used;

    function Bool canPlaceBurst(Bit#(8) beatsThisRequest);
        return !enforce_outstanding || (outstanding_beats[0] >= ((cExtend(beatsThisRequest) + 1) >> fromInteger(fillGradeShift)));
    endfunction

    function Action decreaseOutstanding(Bit#(8) beatsThisRequest);
        action
            outstanding_beats[0] <= outstanding_beats[0] - (cExtend(beatsThisRequest) + 1);
        endaction
    endfunction

    function Action increaseOutstanding();
        action
            outstanding_beats[1] <= outstanding_beats[1] + 1;
        endaction
    endfunction

    function Bit#(TLog#(TransferBarrier)) bytesPerFullBurst();
        return fromInteger(valueOf(data_bytes) * burst_length_used);
    endfunction

    RequestGenerator#(addrwidth, amount_width, data_bytes, addr_aligned, amount_aligned) reqGen <- mkRequestGenerator("WRITE", burst_length_used);

    // Axi related data
    AXI4_Master_Wr#(addrwidth, datawidth, id_width, user_width) master_wr <- mkAXI4_Master_Wr(requestBuffer, dataBuffer, 2, bram);

    Reg#(Task_data_requests#(transfers_total_size, addrwidth, addr_aligned, amount_aligned)) task_data_requests_reg <- mkReg(unpack(0));

    FIFO#(Bit#(8)) beatsPerRequestFIFO <- mkSizedFIFO(maxOutstanding);
    FIFO#(Bit#(datawidth)) incomingBuffer = ?;
    if(enforce_outstanding) begin
        incomingBuffer <- mkSizedBRAMFIFO(max_outstanding_beats);
    end else begin
        incomingBuffer <- mkPipelineFIFO();
    end

    FIFO#(AXI4_Write_Rq_Addr#(addrwidth, id_width, user_width)) write_rq_out <- mkFIFO();

    // Place requests to Bus
    rule placeRequest if(task_data_requests_reg.requests_total != 0);
        let task_data_requests_reg_new = task_data_requests_reg;

        Bit#(addrwidth) address_this = {task_data_requests_reg.address_static, task_data_requests_reg.address_dynamic, task_data_requests_reg.offset_first};

        Bit#(8) beatsThisRequest = fromInteger(burst_length_used - 1);
        if(task_data_requests_reg.requests_total == 1 && task_data_requests_reg.requests_last != 0)
            beatsThisRequest = truncate(task_data_requests_reg.requests_last - 1); // AXI 0 == 1 beat etc
        else if(task_data_requests_reg.first && task_data_requests_reg.requests_first != 0)
            beatsThisRequest = truncate(task_data_requests_reg.requests_first - 1);

        let request = AXI4_Write_Rq_Addr {id: 0, addr: address_this, burst_length: unpack(beatsThisRequest), burst_size: bitsToBurstSize(valueOf(datawidth))
                                    , burst_type: INCR, lock: NORMAL, cache: NORMAL_NON_CACHEABLE_BUFFERABLE,  prot: UNPRIV_SECURE_DATA
                                    , qos: 0, region: task_data_requests_reg.region, user: 0
                                    };
        write_rq_out.enq(request);

        beatsPerRequestFIFO.enq(beatsThisRequest);

        task_data_requests_reg_new.requests_total = task_data_requests_reg.requests_total - 1;
        if(always_offset) begin
            task_data_requests_reg_new.offset_first = task_data_requests_reg.offset_first + bytesPerFullBurst();
            if(task_data_requests_reg_new.offset_first <= task_data_requests_reg.offset_first) begin
                task_data_requests_reg_new.address_dynamic = task_data_requests_reg.address_dynamic + 1;
            end
        end else begin
            task_data_requests_reg_new.offset_first = 0;
            task_data_requests_reg_new.address_dynamic = task_data_requests_reg.address_dynamic + 1;
        end
        task_data_requests_reg_new.first = False;

        task_data_requests_reg <= task_data_requests_reg_new;

        //$display("(%0d) Processing WRITE request: Address: %x, beats this request %d, requests_total %d", $time, address_this, beatsThisRequest + 1, task_data_requests_reg.requests_total);
    endrule

    Reg#(UInt#(6)) outstanding_writes[2] <- mkCReg(2, 0);
    Reg#(UInt#(6)) outstanding_writes_out[2] <- mkCReg(2, 0);

    rule placeAXI;
        let request = write_rq_out.first();
        if(canPlaceBurst(pack(request.burst_length))) begin
            //$display("(%0d) Write Out %d", $time, outstanding_beats[0]);
            write_rq_out.deq();
            master_wr.request_addr.put(request);
            decreaseOutstanding(pack(request.burst_length));
            outstanding_writes[1] <= outstanding_writes[1] + 1;
            outstanding_writes_out[1] <= outstanding_writes_out[1] + 1;
        end
    endrule

    rule discardWriteResponses;
        let d <- master_wr.response.get();
        outstanding_writes[0] <= outstanding_writes[0] - 1;
    endrule

    Reg#(Bit#(8)) beatsThisRequestCntr <- mkReg(0);

    FIFO#(Bit#(datawidth)) relaxationBuffer <- mkFIFO();

    Reg#(Task_data_output#(transfers_total_size, data_bytes_addr)) task_data_output_reg <- mkReg(unpack(0));
    rule forwardData if(outstanding_writes_out[0] != 0 && task_data_output_reg.transfers_total != 0);
        let d = incomingBuffer.first(); incomingBuffer.deq();

        Vector#(data_bytes, Bool) strb = unpack(0);
        Bit#(TAdd#(data_bytes_addr, 1)) startByte = 0;
        Bit#(TAdd#(data_bytes_addr, 1)) endByte = fromInteger(valueOf(data_bytes));
        if(task_data_output_reg.first && task_data_output_reg.bytes_first != 0) begin
            startByte = extend(task_data_output_reg.addr_offset);
            endByte = extend(task_data_output_reg.addr_offset) + extend(task_data_output_reg.bytes_first);
        end else if(task_data_output_reg.transfers_total == 1) begin
            if(task_data_output_reg.bytes_last != 0)
                endByte = extend(task_data_output_reg.bytes_last);
        end

        for(Integer i = 0; i < valueOf(data_bytes); i = i + 1) begin
            if(startByte <= fromInteger(i) && fromInteger(i) < endByte) begin
                strb[i] = True;
            end
        end

        let last = False;
        let beatsThisRequestCntrT = beatsThisRequestCntr + 1;
        if(beatsThisRequestCntr == beatsPerRequestFIFO.first()) begin
            last = True;
            beatsPerRequestFIFO.deq();
            beatsThisRequestCntrT = 0;
            outstanding_writes_out[0] <= outstanding_writes_out[0] - 1;
        end
        beatsThisRequestCntr <= beatsThisRequestCntrT;

        master_wr.request_data.put(AXI4_Write_Rq_Data {data: d, strb: pack(strb), last: last, user: 0});

        let task_data_output_reg_new = task_data_output_reg;

        task_data_output_reg_new.transfers_total = task_data_output_reg.transfers_total - 1;
        task_data_output_reg_new.first = False;

        task_data_output_reg <= task_data_output_reg_new;
        //$display("[WRITE] data: %x, strb: %b, last: %d %d %d", d, strb, last, startByte, endByte);
    endrule

    rule forwardRelaxation;
        let r = relaxationBuffer.first(); relaxationBuffer.deq();
        incomingBuffer.enq(r);
        increaseOutstanding();
        //$display("(%0d) Write In %d", $time, outstanding_beats[1]);
    endrule

    function Bool isBusy();
        return task_data_output_reg.transfers_total != 0 || task_data_requests_reg.requests_total != 0 || outstanding_writes[0] != 0;
    endfunction

    rule fillBuffer if(!isBusy());
        let t <- reqGen.gen.response.get();
        task_data_requests_reg <= tpl_1(t);
        task_data_output_reg <= tpl_2(t);
        beatsThisRequestCntr <= 0;
    endrule

    interface Put request;
        method Action put(AxiRequest#(addrwidth, amount_width) req);
            reqGen.gen.request.put(req);
        endmethod
    endinterface

    interface Put data = toPut(relaxationBuffer);

    method UInt#(amount_width) status_4kCrossings();
        return 0;
    endmethod

    method Bit#(amount_width) delayBetweenPuts();
        return 0;
    endmethod

    method Bit#(amount_width) lastWritten();
        return 0;
    endmethod

    interface active = isBusy();
    interface fab = master_wr.fab;
    interface getBurstLength = burst_length_used;
endmodule

module mkWriteTest(Empty);

    Axi4MasterWrite#(32, 32, 1, 1, 31) master_wr <- mkAxi4MasterWrite(2, 2, True, 256, True, 2, True, 1);
    AXI4_Slave_Wr#(32, 32, 1, 1) slave_wr <- mkAXI4_Slave_Wr(2, 2, 2);
    mkConnection(master_wr.fab, slave_wr.fab);

    Reg#(UInt#(32)) writeCntr <- mkReg(0);
    Reg#(UInt#(32)) writeCurCntr <- mkReg(0);
    Reg#(Maybe#(AXI4_Write_Rq_Addr#(32, 1, 1))) request <- mkReg(tagged Invalid);

    rule handleWriteRequest if(request matches tagged Invalid);
        let r <- slave_wr.request_addr.get();
        request <= tagged Valid r;
        writeCntr <= writeCntr + extend(r.burst_length) + 1;
        writeCurCntr <= extend(r.burst_length) + 1;
        $display("Request for %x %d", r.addr, r.burst_length);
    endrule

    rule returnReadValue if(request matches tagged Valid .v);
        let r <- slave_wr.request_data.get();
        $display("Got write request %x %b", r.data, r.strb);
        if(writeCurCntr == 1) begin
            request <= tagged Invalid;
            if(r.last != True) begin
                $display("ERROR: Last missing");
            end
            slave_wr.response.put(AXI4_Write_Rs {id: 0, resp: OKAY, user:0});
        end
        writeCurCntr <= writeCurCntr - 1;
    endrule

    Reg#(UInt#(32)) clkCntr <- mkReg(0);
    Reg#(UInt#(32)) startTime <- mkRegU;

    rule cnt;
        clkCntr <= clkCntr + 1;
    endrule

    rule putRandomData;
        master_wr.data.put(pack(clkCntr));
    endrule

    Stmt fsm = {seq
            $display("Hello World!");
            action
                master_wr.request.put(AxiRequest {address: 16342, bytesToTransfer: 128, region: 0});
                startTime <= clkCntr;
            endaction
            await(master_wr.active);
            await(!master_wr.active);
            $display("Done at %d", clkCntr - startTime);
            delay(1000);
        endseq
    };

    mkAutoFSM(fsm);
endmodule


endpackage
