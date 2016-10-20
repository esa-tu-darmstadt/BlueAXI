package AXI4_Types;

import DefaultValue :: *;

/*
=============
	Types
=============
*/

typedef enum {
		FIXED = 2'b00,
		INCR = 2'b01,
		WRAP = 2'b10,
		RESERVED = 2'b11
	} AXI4_BurstType deriving(Bits, Eq, FShow);

instance DefaultValue#(AXI4_BurstType);
	defaultValue = FIXED;
endinstance

typedef enum {
		B1 = 3'b000,
		B2 = 3'b001,
		B4 = 3'b010,
		B8 = 3'b011,
		B16 = 3'b100,
		B32 = 3'b101,
		B64 = 3'b110,
		B128 = 3'b111
	} AXI4_BurstSize deriving(Bits, Eq, FShow);

function AXI4_BurstSize bitsToBurstSize(Integer bits);
	Integer bytes = bits / 8;
	return unpack(fromInteger(log2(bytes)));
endfunction

typedef enum {
		OKAY = 2'b00,
		EXOKAY = 2'b01,
		SLVERR = 2'b10,
		DECERR = 2'b11
	} AXI4_Response deriving(Bits, Eq, FShow);

instance DefaultValue#(AXI4_Response);
	defaultValue = OKAY;
endinstance

typedef enum {
		DEVICE_NON_BUFFERABLE = 4'b0000,
		DEVICE_BUFFERABLE = 4'b0001,
		NORMAL_NON_CACHEABLE_NON_BUFFERABLE = 4'b0010,
		NORMAL_NON_CACHEABLE_BUFFERABLE = 4'b0011,
		WRITE_THROUGH_READ_ALLOCATE = 4'b1110,
		WRITE_THROUGH_WRITE_ALLOCATE = 4'b1010,
		WRITE_BACK_READ_ALLOCATE = 4'b1111,
		WRITE_BACK_WRITE_ALLOCATE = 4'b1011
	} AXI4_Read_Cache deriving(Bits, Eq, FShow);

instance DefaultValue#(AXI4_Read_Cache);
	defaultValue = DEVICE_NON_BUFFERABLE;
endinstance

typedef enum {
		DEVICE_NON_BUFFERABLE = 4'b0000,
		DEVICE_BUFFERABLE = 4'b0001,
		NORMAL_NON_CACHEABLE_NON_BUFFERABLE = 4'b0010,
		NORMAL_NON_CACHEABLE_BUFFERABLE = 4'b0011,
		WRITE_THROUGH_READ_ALLOCATE = 4'b0110,
		WRITE_THROUGH_WRITE_ALLOCATE = 4'b1110,
		WRITE_BACK_READ_ALLOCATE = 4'b0111,
		WRITE_BACK_WRITE_ALLOCATE = 4'b1111
	} AXI4_Write_Cache deriving(Bits, Eq, FShow);

instance DefaultValue#(AXI4_Write_Cache);
	defaultValue = DEVICE_NON_BUFFERABLE;
endinstance

typedef enum {
		UNPRIV_SECURE_DATA = 3'b000,
		UNPRIV_SECURE_INSTRUCTION = 3'b001,
		UNPRIV_INSECURE_DATA = 3'b010,
		UNPRIV_INSECURE_INSTRUCTION = 3'b011,
		PRIV_SECURE_DATA = 3'b100,
		PRIV_SECURE_INSTRUCTION = 3'b101,
		PRIV_INSECURE_DATA = 3'b110,
		PRIV_INSECURE_INSTRUCTION = 3'b111
	} AXI4_Prot deriving(Bits, Eq, FShow);

instance DefaultValue#(AXI4_Prot);
	defaultValue = UNPRIV_SECURE_DATA;
endinstance

typedef enum {
		NORMAL = 1'b0,
		EXCLUSIVE = 1'b1
	} AXI4_Lock deriving(Bits, Eq, FShow);

instance DefaultValue#(AXI4_Lock);
	defaultValue = NORMAL;
endinstance

typedef struct {
		Bit#(id_width) id;
		Bit#(addrwidth) addr;
		UInt#(8) burst_length;
		AXI4_BurstSize burst_size;
		AXI4_BurstType burst_type;
		AXI4_Lock lock;
		AXI4_Read_Cache cache;
		AXI4_Prot prot;
		Bit#(4) qos;
		Bit#(4) region;
		Bit#(user_width) user;
	} AXI4_Read_Rq#(numeric type addrwidth, numeric type id_width, numeric type user_width) deriving(Bits, Eq, FShow);

instance DefaultValue#(AXI4_Read_Rq#(addrwidth, id_width, user_width));
	defaultValue = AXI4_Read_Rq {
		id: 0,
		addr: 0,
		burst_length: 0,
		burst_size: bitsToBurstSize(valueOf(addrwidth)),
		burst_type: defaultValue,
		lock: defaultValue,
		cache: defaultValue,
		prot: defaultValue,
		qos: 0,
		region: 0,
		user: 0
	};
endinstance

typedef struct {
		Bit#(id_width) id;
		Bit#(datawidth) data;
		AXI4_Response resp;
		Bool last;
		Bit#(user_width) user;
		} AXI4_Read_Rs#(numeric type datawidth, numeric type id_width, numeric type user_width) deriving(Bits, Eq, FShow);

instance DefaultValue#(AXI4_Read_Rs#(datawidth, id_width, user_width));
	defaultValue = AXI4_Read_Rs {
		id: 0,
		data: 0,
		resp: defaultValue,
		last: True,
		user: 0
	};
endinstance

typedef struct {
		Bit#(id_width) id;
		Bit#(addrwidth) addr;
		UInt#(8) burst_length;
		AXI4_BurstSize burst_size;
		AXI4_BurstType burst_type;
		AXI4_Lock lock;
		AXI4_Write_Cache cache;
		AXI4_Prot prot;
		Bit#(4) qos;
		Bit#(4) region;
		Bit#(user_width) user;
	} AXI4_Write_Rq_Addr#(numeric type addrwidth, numeric type id_width, numeric type user_width) deriving(Bits, Eq, FShow);

instance DefaultValue#(AXI4_Write_Rq_Addr#(addrwidth, id_width, user_width));
	defaultValue = AXI4_Write_Rq_Addr {
		id: 0,
		addr: 0,
		burst_length: 0,
		burst_size: bitsToBurstSize(valueOf(addrwidth)),
		burst_type: defaultValue,
		lock: defaultValue,
		cache: defaultValue,
		prot: defaultValue,
		qos: 0,
		region: 0,
		user: 0
	};
endinstance

typedef struct {
		Bit#(datawidth) data;
		Bit#(TDiv#(datawidth, 8)) strb;
		Bool last;
		Bit#(user_width) user;
	} AXI4_Write_Rq_Data#(numeric type datawidth, numeric type user_width) deriving(Bits, Eq, FShow);

instance DefaultValue#(AXI4_Write_Rq_Data#(datawidth, user_width));
	defaultValue = AXI4_Write_Rq_Data {
		data: 0,
		strb: unpack(-1),
		last: True,
		user: 0
	};
endinstance

typedef struct {
		Bit#(id_width) id;
		AXI4_Response resp;
		Bit#(user_width) user;
	} AXI4_Write_Rs#(numeric type id_width, numeric type user_width) deriving(Bits, Eq, FShow);

instance DefaultValue#(AXI4_Write_Rs#(id_width, user_width));
	defaultValue = AXI4_Write_Rs {
		id: 0,
		resp: defaultValue,
		user: 0
	};
endinstance

endpackage