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

  (* bits 5-7 = imm[0:3] *)
  let byte0 = Bytes.get_uint8 buf 0 lor ((imm land 0b111) lsl 5) in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-15 = imm[3:11] *)
  Bytes.set_uint8 buf 1 ((imm lsr 3) land 0b11111111);

  (* bits 16-20 = imm[11:16] *)
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

let sub (rdest : reg) (rsrc1 : reg) (rsrc2 : reg) : bytes =
  (* 
31               24 23 22 21 20    16 15      10 9     5 4     0
┌──────────────────┬──┬──┬──┬────────┬──────────┬───────┬───────┐
│     11001011     │0 │0 │0 │   Rm   │  000000  │  Rn   │  Rd   │
└──────────────────┴──┴──┴──┴────────┴──────────┴───────┴───────┘
       8 bits       1  1  1   5 bits    6 bits   5 bits  5 bits
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
  Bytes.set_uint8 buf 3 0b11001011;

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

(* comparison *)
let cmp (rsrc1 : reg) (rsrc2 : reg) : bytes =
  (*
31       24 23 22 21 20      16 15        10 9       5 4        0
┌──────────┬───┬───┬───────────┬───────────┬──────────┬──────────┐
│ 11001011 │ 0 │ 1 │    Rm     │ 000000    │   Rn     │ 11111    │
└──────────┴───┴───┴───────────┴───────────┴──────────┴──────────┘
*)
  let buf = Bytes.create 4 in

  (* bits 0-4 = 11111 *)
  let byte0 = Bytes.get_uint8 buf 0 lor 0b11111 in
  Bytes.set_uint8 buf 0 byte0;

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

  (* bits 16-20 = rsrc2; bit 21 = 1 *)
  Bytes.set_uint8 buf 2 (get_value_from_reg rsrc2 land 0b11111 lor 0b000001);

  (* bits 24-31 *)
  Bytes.set_uint8 buf 3 0b11001011;

  buf

let cmp_imm (rsrc : reg) (imm : int) : bytes =
  (*
31                24 23 22 21 20             10 9      5 4      0
┌──────────────────┬──┬──┬──┬─────────────────┬────────┬────────┐
│     11110010     │ 1│ 0│ 1│      imm12      │  Rn    │  11111 │
└──────────────────┴──┴──┴──┴─────────────────┴────────┴────────┘
       8 bits       1  1  1      12 bits       5 bits   5 bits
*)
  let buf = Bytes.create 4 in

  (* bits 0-4 = 11111 *)
  let byte0 = Bytes.get_uint8 buf 0 lor 0b11111 in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 5-7 = lower 3 bits of rsrc1 *)
  let byte0 =
    Bytes.get_uint8 buf 0 lor ((get_value_from_reg rsrc land 0b111) lsl 5)
  in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-9 = upper 2 bits of rsrc1 *)
  Bytes.set_uint8 buf 1 ((get_value_from_reg rsrc lsr 3) land 0b11);

  (* bits 10-15 = lower 6 bits of imm *)
  let byte1 = Bytes.get_uint8 buf 1 lor ((imm land 0b111111) lsl 2) in
  Bytes.set_uint8 buf 1 byte1;

  (* bits 16-21 = upper 6 bits of imm *)
  Bytes.set_uint8 buf 2 ((imm lsr 6) land 0b111111);

  (* bit 21 = 1; bit 23 = 1 *)
  let byte2 = Bytes.get_uint8 buf 2 lor (0b101 lsl 5) in
  Bytes.set_uint8 buf 2 byte2;

  (* bits 24-31 *)
  Bytes.set_uint8 buf 3 0b11110010;

  buf

let conditional_set (rdest : reg) (op : Ast.logical_compare) : bytes =
  (*
31       24 23  21 20       16 15  12 11 10 9           5 4        0
┌──────────┬─────┬───────────┬───────┬───┬───┬──────────┬──────────┐
│ 10011010 │ 100 │   11111   │ 1111  │ 0 │ 1 │ 11111    │   Rd     │
└──────────┴─────┴───────────┴───────┴───┴───┴──────────┴──────────┘
*)
  let buf = Bytes.create 4 in

  (* bits 0-4 = rdest *)
  Bytes.set_uint8 buf 0 (get_value_from_reg rdest land 0b11111);

  (* bits 5-7 = 111  *)
  let byte0 = Bytes.get_uint8 buf 0 lor (0b111 lsl 5) in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-9 = 11; bit 10 = 1  *)
  let byte1 = Bytes.get_uint8 buf 1 lor 0b0111 in
  Bytes.set_uint8 buf 1 byte1;

  (* bits 10-15 = 000011 *)
  let byte1 = Bytes.get_uint8 buf 1 lor 0b000011 in
  Bytes.set_uint8 buf 1 byte1;

  (* bits 12-15 = depends on operation *)
  (match op with
  | EQUALS ->
      (* bits 12-15 = 0001 *)
      let byte1 = Bytes.get_uint8 buf 1 lor (0b0001 lsl 4) in
      Bytes.set_uint8 buf 1 byte1
  | NOT_EQUALS ->
      (* bits 12-15 = 0000 *)
      let byte1 = Bytes.get_uint8 buf 1 lor (0b0000 lsl 4) in
      Bytes.set_uint8 buf 1 byte1
  | GREATER_THAN ->
      (* bits 12-15 = 1101 *)
      let byte1 = Bytes.get_uint8 buf 1 lor (0b1101 lsl 4) in
      Bytes.set_uint8 buf 1 byte1
  | GREATER_THAN_EQUALS ->
      (* bits 12-15 = 1011 *)
      let byte1 = Bytes.get_uint8 buf 1 lor (0b1011 lsl 4) in
      Bytes.set_uint8 buf 1 byte1
  | LESS_THAN ->
      (* bits 12-15 = 1010 *)
      let byte1 = Bytes.get_uint8 buf 1 lor (0b1010 lsl 4) in
      Bytes.set_uint8 buf 1 byte1
  | LESS_THAN_EQUALS ->
      (* bits 12-15 = 1100 *)
      let byte1 = Bytes.get_uint8 buf 1 lor (0b1100 lsl 4) in
      Bytes.set_uint8 buf 1 byte1);

  (* bits 16-20 = 11111; bits 21-23 = 100 *)
  let byte2 = Bytes.get_uint8 buf 2 lor 0b10011111 in
  Bytes.set_uint8 buf 2 byte2;

  (* bits 24-31 *)
  Bytes.set_uint8 buf 3 0b10011010;

  buf

(* logical *)
let bitwise (rdest : reg) (rsrc1 : reg) (rsrc2 : reg)
    (op : Ast.logical_combination) : bytes =
  (*
31 30 29  24 23 22 21–16 15–10 9–5  4–0
┌──┬──┬──────┬──┬──┬─────┬─────┬───┬───┐
sf│opc│ 01010│N │ 0│ Rm  │imm6 │Rn │Rd │
└──┴──┴──────┴──┴──┴─────┴─────┴───┴───┘

31 | 30-29 | 28-24 | 23-22 | 21 | 20-16 | 15-10 | 9-5  | 4-0
sf |  opc  | 01010 | shift |  N |  Rm   | imm6  |  Rn  | Rd
 1 |  00   | 01010 |  00   |  0 | 00010 | 000000| 00001| 00000
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

  (* bits 24-28 = 01010; bit 31 = 1 *)
  Bytes.set_uint8 buf 3 0b10001010;

  (* bits 29-30 = depends on operation *)
  (match op with
  | LOGICAL_AND ->
      (* bits 29-30 = 00 *)
      ()
  | LOGICAL_OR ->
      (* bits 29-30 = 01 *)
      let byte3 = Bytes.get_uint8 buf 3 lor (0b01 lsl 5) in
      Bytes.set_uint8 buf 3 byte3);

  buf

(* control flow *)
let unconditional_jmp (imm : int) : bytes =
  (*
  31               26 25                              0
┌──────────────────┬──────────────────────────────────┐
│      000101      │              imm26               │
└──────────────────┴──────────────────────────────────┘
       6 bits                    26 bits
  *)
  let buf = Bytes.create 4 in

  (* bits 0-7 = imm[:8] *)
  let byte0 = Bytes.get_uint8 buf 0 lor (imm land 0b11111111) in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-15 = imm[8:16] *)
  let byte1 = Bytes.get_uint8 buf 1 lor ((imm lsl 8) land 0b11111111) in
  Bytes.set_uint8 buf 1 byte1;

  (* bits 16-23 = imm[16:24] *)
  let byte2 = Bytes.get_uint8 buf 2 lor ((imm lsl 16) land 0b11111111) in
  Bytes.set_uint8 buf 2 byte2;

  (* bits 24-25 = imm[24:26] *)
  Bytes.set_uint8 buf 3 ((imm lsr 24) land 0b11);

  (* bits 26-31 = 000101 *)
  let byte3 = Bytes.get_uint8 buf 3 lor (0b000101 lsl 2) in
  Bytes.set_uint8 buf 3 byte3;

  buf

let conditional_jmp (cond : Ast.logical_compare) (imm : int) : bytes =
  (*
31                24 23                         5 4       0
┌──────────────────┬─────────────────────────────┬─────────┐
│     01010100     │            imm19            │  0 cond │
└──────────────────┴─────────────────────────────┴─────────┘
       8 bits                  19 bits              1+4 bits
*)
  let buf = Bytes.create 4 in

  (* bits 0-4 = depend on cond *)
  (match cond with
  | EQUALS -> ( (* bits 0-4 = 0000 *) )
  | NOT_EQUALS ->
      (* bits 0-4 = 0001 *)
      Bytes.set_uint8 buf 0 0b0001
  | GREATER_THAN ->
      (* bits 0-4 = 1100 *)
      Bytes.set_uint8 buf 0 0b1100
  | GREATER_THAN_EQUALS ->
      (* bits 0-4 = 1010 *)
      Bytes.set_uint8 buf 0 0b1010
  | LESS_THAN ->
      (* bits 0-4 = 1011 *)
      Bytes.set_uint8 buf 0 0b1011
  | LESS_THAN_EQUALS ->
      (* bits 0-4 = 1101 *)
      Bytes.set_uint8 buf 0 0b1101);

  (* bits 5-7 = imm[0:3] *)
  let byte0 = Bytes.get_uint8 buf 0 lor ((imm land 0b111) lsl 5) in
  Bytes.set_uint8 buf 0 byte0;

  (* bits 8-15 = imm[3:11] *)
  Bytes.set_uint8 buf 1 ((imm lsr 3) land 0b11111111);

  (* bits 16-20 = imm[11:16] *)
  Bytes.set_uint8 buf 2 ((imm lsr 11) land 0b11111);

  (* bits 24-31 *)
  Bytes.set_uint8 buf 3 0b01010100;

  buf

let ret () : bytes =
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0b11000000;
  Bytes.set_uint8 buf 1 0b00000011;
  Bytes.set_uint8 buf 2 0b01011111;
  Bytes.set_uint8 buf 3 0b11010110;
  buf
