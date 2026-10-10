(* --TYPE DECLARATIONS-- *)
type reg = REG of int

(* --HELPER FUNCTIONS-- *)
let get_value_from_reg (r : reg) : int = match r with REG i -> i

(* --EMIT IMPLEMENTATION-- *)

(* data *)
let mov_imm (rdest : reg) (imm : int) : bytes =
  (* 
31       30 29 28 27 26 25 24 23 22 21 20              5 4       0
┌──┬───────────────────────┬────┬──┬──────────────────┬──────────┐
│sf│       100101          │    │hw│      imm16        │    Rd    │
└──┴───────────────────────┴────┴──┴──────────────────┴──────────┘
  *)
  let buf = Bytes.create 4 in

  (* bits 0-4 = rdest *)
  Bytes.set_uint8 buf 0 (get_value_from_reg rdest land 0b11111);

  (* bits 5-7 = lower 3 bits of imm *)
  let byte0 = Bytes.get_uint8 buf 0 lor ((imm land 0b111) lsl 5) in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-15 = next 8 bits of imm *)
  Bytes.set_uint8 buf 1 ((imm lsr 3) land 0b11111111);

  (* bits 16-20 = upper 5 bits of imm *)
  Bytes.set_uint8 buf 2 ((imm lsr 11) land 0b11111 lor 0x80);

  Bytes.set_uint8 buf 3 0b11010010;

  buf

let mov_reg (rdest : reg) (rsrc : reg) : bytes =
  (* 
31          24 23 22 21 20    16 15       10 9     5 4     0
┌─────────────┬──┬──┬──┬────────┬──────────┬───────┬───────┐
│  10101010   │0 │0 │0 │   Rm   │  000000  │ Rn=31 │  Rd   │
└─────────────┴──┴──┴──┴────────┴──────────┴───────┴───────┘
    fixed        │  │  │    5       6          5       5
      
  *)
  let buf = Bytes.create 4 in

  (* bits 0-4 = rdest *)
  Bytes.set_uint8 buf 0 (get_value_from_reg rdest land 0b11111);

  (* bits 5-7 = lower 3 bits 31 *)
  let byte0 = Bytes.get_uint8 buf 0 lor (0b111 lsl 5) in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-9 = upper 2 bits of 31 *)
  Bytes.set_uint8 buf 1 0b11;

  (* bits 16-20 = rsrc *)
  Bytes.set_uint8 buf 2 (get_value_from_reg rsrc land 0b11111);

  (* bits 24-31 *)
  Bytes.set_uint8 buf 3 0b10101010;

  buf

let load (rdest : reg) (rsrc : reg) (offset : int) : bytes =
  (*
Bits 31–22    Bits 21–10    Bits 9–5    Bits 4–0
┌──────────┬──────────────┬───────────┬─────────┐
│ 1111100101│    imm12     │    Rn     │   Rt    │
└──────────┴──────────────┴───────────┴─────────┘
   10 bits      12 bits      5 bits     5 bits
  *)
  (* instructionx scale offset by 8, so we adjust it *)
  let offset = offset / 8 in
  let buf = Bytes.create 4 in

  (* bits 0-4 = rdest *)
  Bytes.set_uint8 buf 0 (get_value_from_reg rdest land 0b11111);

  (* bits 5-7 = lower 3 bits of rsrc *)
  let byte0 =
    Bytes.get_uint8 buf 0 lor ((get_value_from_reg rsrc land 0b111) lsl 5)
  in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-9 = upper 2 bits of rsrc *)
  Bytes.set_uint8 buf 1 ((get_value_from_reg rsrc lsr 3) land 0b11);

  (* bits 10-15 = lower 6 bits of offset *)
  let byte1 = Bytes.get_uint8 buf 1 lor ((offset land 0b111111) lsl 2) in
  Bytes.set_uint8 buf 1 byte1;

  (* bits 16-21 = upper 6 bits of offset *)
  Bytes.set_uint8 buf 2 ((offset lsr 6) land 0b111111);

  (* bits 22-23 *)
  let byte2 = Bytes.get_uint8 buf 2 lor (0b01 lsl 6) in
  Bytes.set_uint8 buf 2 byte2;
  (* bits 24-31 *)
  Bytes.set_uint8 buf 3 0b11111001;
  buf

let store (rsrc : reg) (rdest : reg) (offset : int) : bytes =
  (*
 31                         0
 ┌──────────┬────────────┬───────┬───────┐
 │ 1111100100│   imm12    │  Rn   │  Rt   │
 └──────────┴────────────┴───────┴───────┘
     10 bits    12 bits    5 bits  5 bits
  *)

  (* instruction scale offset by 8, so we adjust it *)
  let offset = offset / 8 in

  let buf = Bytes.create 4 in

  (* bits 0-4 = rdest *)
  Bytes.set_uint8 buf 0 (get_value_from_reg rsrc land 0b11111);

  (* bits 5-7 = lower 3 bits of rsrc *)
  let byte0 =
    Bytes.get_uint8 buf 0 lor ((get_value_from_reg rdest land 0b111) lsl 5)
  in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-9 = upper 2 bits of rsrc *)
  Bytes.set_uint8 buf 1 ((get_value_from_reg rdest lsr 3) land 0b11);

  (* bits 10-15 = lower 6 bits of offset *)
  let byte1 = Bytes.get_uint8 buf 1 lor ((offset land 0b111111) lsl 2) in
  Bytes.set_uint8 buf 1 byte1;

  (* bits 16-21 = upper 6 bits of offset *)
  Bytes.set_uint8 buf 2 ((offset lsr 6) land 0b111111);

  (* bits 24-31 *)
  Bytes.set_uint8 buf 3 0b11111001;
  buf

(* arithmetic *)
let add (rdest : reg) (rsrc1 : reg) (rsrc2 : reg) : bytes =
  (* 
31                24 23 22 21 20    16 15      10 9     5 4     0
┌──────────────────┬──┬──┬──┬────────┬──────────┬───────┬───────┐
│   10001011       │0 │0 │0 │   Rm   │  000000  │  Rn   │  Rd   │
└──────────────────┴──┴──┴──┴────────┴──────────┴───────┴───────┘
        8 bits       1  1  1    5 bits    6 bits    5 bits   5 bits
  *)
  let buf = Bytes.create 4 in

  (* bits 0-4 = rdest *)
  Bytes.set_uint8 buf 0 (get_value_from_reg rdest land 0b11111);

  (* bits 5-7 = lower 3 bits of rsrc1 *)
  let byte0 =
    Bytes.get_uint8 buf 0 lor ((get_value_from_reg rsrc1 land 0b111) lsl 5)
  in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-9 = upper 2 bits of rsrc1 *)
  Bytes.set_uint8 buf 1 ((get_value_from_reg rsrc1 lsr 3) land 0b11);

  (* bits 16-20 = rsrc2 *)
  Bytes.set_uint8 buf 2 (get_value_from_reg rsrc2 land 0b11111);

  (* bits 24-31 *)
  Bytes.set_uint8 buf 3 0b10001011;

  buf

let add_imm (rdest : reg) (rsrc : reg) (imm : int) : bytes =
  (* 
31       24 23 22 21 20          10 9       5 4       0
┌──────────┬──┬──┬───────────────┬─────────┬─────────┐
│ 10010001 │sh│  │    imm12      │   Rn    │   Rd    │
└──────────┴──┴──┴───────────────┴─────────┴─────────┘
  *)
  let buf = Bytes.create 4 in

  (* bits 0-4 = rdest *)
  Bytes.set_uint8 buf 0 (get_value_from_reg rdest land 0b11111);

  (* bits 5-7 = lower 3 bits of rsrc *)
  let byte0 =
    Bytes.get_uint8 buf 0 lor ((get_value_from_reg rsrc land 0b111) lsl 5)
  in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-9 = upper 2 bits of rsrc *)
  Bytes.set_uint8 buf 1 ((get_value_from_reg rsrc lsr 3) land 0b11);

  (* bits 10-15 = lower 6 bits of imm *)
  let byte1 = Bytes.get_uint8 buf 1 lor ((imm land 0b111111) lsl 2) in
  Bytes.set_uint8 buf 1 byte1;

  (* bits 16-21 = upper 6 bits of imm *)
  Bytes.set_uint8 buf 2 ((imm lsr 6) land 0b111111);

  (* bits 24-31 *)
  Bytes.set_uint8 buf 3 0b10010001;

  buf

let sub_imm (rdest : reg) (rsrc : reg) (imm : int) : bytes =
  (* 
31       24 23 22 21 20          10 9       5 4       0
┌──────────┬──┬──┬───────────────┬─────────┬─────────┐
│ 10010001 │sh│  │    imm12      │   Rn    │   Rd    │
└──────────┴──┴──┴───────────────┴─────────┴─────────┘
  *)
  let buf = Bytes.create 4 in

  (* bits 0-4 = rdest *)
  Bytes.set_uint8 buf 0 (get_value_from_reg rdest land 0b11111);

  (* bits 5-7 = lower 3 bits of rsrc *)
  let byte0 =
    Bytes.get_uint8 buf 0 lor ((get_value_from_reg rsrc land 0b111) lsl 5)
  in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-9 = upper 2 bits of rsrc *)
  Bytes.set_uint8 buf 1 ((get_value_from_reg rsrc lsr 3) land 0b11);

  (* bits 10-15 = lower 6 bits of imm *)
  let byte1 = Bytes.get_uint8 buf 1 lor ((imm land 0b111111) lsl 2) in
  Bytes.set_uint8 buf 1 byte1;

  (* bits 16-21 = upper 6 bits of imm *)
  Bytes.set_uint8 buf 2 ((imm lsr 6) land 0b111111);

  (* bits 24-31 *)
  Bytes.set_uint8 buf 3 0b11010001;

  buf

let mul (rdest : reg) (rsrc1 : reg) (rsrc2 : reg) : bytes =
  (*
  31    24  23  21 20       16 15       10 9        5 4        0
┌──────────┬─────┬───────────┬───────────┬──────────┬──────────┐
│ 10011011 │ 000 │    Rm     │  011111   │   Rn     │   Rd     │
└──────────┴─────┴───────────┴───────────┴──────────┴──────────┘
  *)
  let buf = Bytes.create 4 in

  (* bits 0-4 = rdest *)
  Bytes.set_uint8 buf 0 (get_value_from_reg rdest land 0b11111);

  (* bits 5-7 = lower 3 bits of rsrc1 *)
  let byte0 =
    Bytes.get_uint8 buf 0 lor ((get_value_from_reg rsrc1 land 0b111) lsl 5)
  in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-9 = upper 2 bits of rsrc1 *)
  Bytes.set_uint8 buf 1 ((get_value_from_reg rsrc1 lsr 3) land 0b11);

  (* bits 10-15 = 011111 *)
  let byte1 = Bytes.get_uint8 buf 1 lor 0b011111 in
  Bytes.set_uint8 buf 1 byte1;

  (* bits 16-20 = rsrc2 *)
  Bytes.set_uint8 buf 2 (get_value_from_reg rsrc2 land 0b11111);

  (* bits 24-31 *)
  Bytes.set_uint8 buf 3 0b10011011;

  buf

let div_signed (rdest : reg) (rsrc1 : reg) (rsrc2 : reg) : bytes =
  (*
31  30 29       24 23       16 15       10 9        5 4        0
┌───┬───┬─────────┬───────────┬───────────┬──────────┬──────────┐
│sf │ 0 │ 0011011 │    Rm     │ 000011    │   Rn     │   Rd     │
└───┴───┴─────────┴───────────┴───────────┴──────────┴──────────┘
*)
  let buf = Bytes.create 4 in

  (* bits 0-4 = rdest *)
  Bytes.set_uint8 buf 0 (get_value_from_reg rdest land 0b11111);

  (* bits 5-7 = lower 3 bits of rsrc1 *)
  let byte0 =
    Bytes.get_uint8 buf 0 lor ((get_value_from_reg rsrc1 land 0b111) lsl 5)
  in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-9 = upper 2 bits of rsrc1 *)
  Bytes.set_uint8 buf 1 ((get_value_from_reg rsrc1 lsr 3) land 0b11);

  (* bits 10-15 = 000011 *)
  let byte1 = Bytes.get_uint8 buf 1 lor 0b000011 in
  Bytes.set_uint8 buf 1 byte1;

  (* bits 16-23 = rsrc2 *)
  Bytes.set_uint8 buf 2 (get_value_from_reg rsrc2 land 0b11111111);

  (* bits 24-31 *)
  Bytes.set_uint8 buf 3 0b10011010;

  buf

let ret () : bytes =
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0b11000000;
  Bytes.set_uint8 buf 1 0b00000011;
  Bytes.set_uint8 buf 2 0b01011111;
  Bytes.set_uint8 buf 3 0b11010110;
  buf
