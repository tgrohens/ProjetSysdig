type instr =
  |Adc of (cond * bool (* S *) * register (*Rn*) * register (*Rd*) * shifter)
  |Add of (cond * bool * register * register * shifter)
  |And of (cond * bool * register * register * shifter)
  |Bic of (cond * bool * register * register * shifter)
  |Eor of (cond * bool * register * register * shifter)
  |Orr of (cond * bool * register * register * shifter)
  |Rsb of (cond * bool * register * register * shifter)
  |Rsc of (cond * bool * register * register * shifter)
  |Sbc of (cond * bool * register * register * shifter)
  |Sub of (cond * bool * register * register * shifter)
  |Mov of (cond * bool * register (*Rd*) * shifter)
  |Mvn of (cond * bool * register (*Rd*) * shifter)

  |Branch of (cond * bool (*L*) * label)
  |Cmp of (cond * register * shifter)
  |Cmn of (cond * register * shifter)

  |Swi of (cond * int)

  |Clz of (cond * register (*Rd*) * register (*Rm*))

  |Load_store_offset of (cond * bool (* U *) * bool (* L *) * register (*Rn*) * register (*Rn*) * int)
  |Load_store_register of (cond * bool (* U *) * bool (* L *) * register (*Rn*) * register (*Rn*) * register (*Rm*))

  |Mul of (cond * bool (* S *) * register (*Rd*) * register (*Rm*) * register (*Rs*))
  |Mla of (cond * bool (* S *) * register (*Rd*) * register (*Rm*) * register (*Rs*) * register (*Rn*))

  |Mrs of (cond * bool (* R *) * register (*Rd*))
  |Msr of (cond * bool (* R *) * int list (*field_mask*) * shifter)

and cond = 
  |Eq |Ne |Cs |Cc |Mi |Pl |Vs |Vc |Hi |Ls |Ge |Lt |Gt |Le |Al |Nv

and register = int

and shifter =
  |Imm of int
  |Reg of register
  |Lsl_by_i of (register * int)
  |Lsl_by_r of (register * register)
  |Lsr_by_i of (register * int)
  |Lsr_by_r of (register * register)
  |Asr_by_i of (register * int)
  |Asr_by_r of (register * register)
  |Ror_by_i of (register * int)
  |Ror_by_r of (register * register)

and label = string
