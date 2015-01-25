_bissextile:
	str lr, [sp, #-4]
	str r7, [sp, #-4]	
	mov	r7, sp
	sub	sp, sp, #8
	mov	r1, #4
	bl	___modsi3
	cmp	r0, #0
	beq	LBB0_2

	mov	r0, #0
	str	r0, [sp, #4]
	b	LBB0_6
LBB0_2:
	mov	r1, #100
	ldr	r0, [sp]
	bl	___modsi3
	cmp	r0, #0
	bne	LBB0_5

	mov	r1, #400
	ldr	r0, [sp]
	bl	___modsi3
	cmp	r0, #0
	beq	LBB0_5

	mov	r0, #0
	str	r0, [sp, #4]
	b	LBB0_6
LBB0_5:
	mov	r0, #1
	str	r0, [sp, #4]
LBB0_6:
	ldr	r0, [sp, #4]
	mov	sp, r7
	ldr r7, [sp], #4
	ldr lr, [sp], #4
	b	lr


_main:
	str lr, [sp, #-4]
	str r7, [sp, #-4]
	mov	r7, sp
	sub	sp, sp, #32
	mov	r0, #0
	str	r0, [r7, #-4]
	str	r0, [r7, #-8]
	str	r0, [r7, #-12]
	str	r0, [sp, #16]
	str	r0, [sp, #12]
	str	r0, [sp, #8]
	str	r0, [sp, #4]
LBB1_1:
	mov	r1, #60
	ldr	r0, [r7, #-8]
	add	r0, r0, #1
	str	r0, [r7, #-8]
	ldr	r0, [r7, #-8]
	bl	___modsi3
	cmp	r0, #0
	bne	LBB1_3

	ldr	r0, [r7, #-12]
	add	r0, r0, #1
	str	r0, [r7, #-12]
LBB1_3:
	ldr	r0, [r7, #-12]
	cmp	r0, #60
	bne	LBB1_5

	mov	r0, #0
	str	r0, [r7, #-12]
	ldr	r0, [sp, #16]
	add	r0, r0, #1
	str	r0, [sp, #16]
LBB1_5:
	ldr	r0, [sp, #16]
	cmp	r0, #24
	bne	LBB1_7

	mov	r0, #0
	str	r0, [sp, #16]
	ldr	r0, [sp, #12]
	add	r0, r0, #1
	str	r0, [sp, #12]
LBB1_7:
	ldr	r0, [sp, #8]
	cmp	r0, #0
	bne	LBB1_9

	ldr	r0, [sp, #12]
	cmp	r0, #32
	beq	LBB1_31
LBB1_9:
	ldr	r0, [sp, #8]
	cmp	r0, #1
	bne	LBB1_11

	ldr	r0, [sp, #12]
	ldr	r1, [sp, #4]
	str	r0, [sp]
	mov	r0, r1
	bl	_bissextile
	add	r0, r0, #29
	ldr	r1, [sp]
	cmp	r1, r0
	beq	LBB1_31
LBB1_11:
	ldr	r0, [sp, #8]
	cmp	r0, #2
	bne	LBB1_13

	ldr	r0, [sp, #12]
	cmp	r0, #32
	beq	LBB1_31
LBB1_13:
	ldr	r0, [sp, #8]
	cmp	r0, #3
	bne	LBB1_15

	ldr	r0, [sp, #12]
	cmp	r0, #31
	beq	LBB1_31
LBB1_15:
	ldr	r0, [sp, #8]
	cmp	r0, #4
	bne	LBB1_17

	ldr	r0, [sp, #12]
	cmp	r0, #32
	beq	LBB1_31
LBB1_17:
	ldr	r0, [sp, #8]
	cmp	r0, #5
	bne	LBB1_19

	ldr	r0, [sp, #12]
	cmp	r0, #31
	beq	LBB1_31
LBB1_19:
	ldr	r0, [sp, #8]
	cmp	r0, #6
	bne	LBB1_21

	ldr	r0, [sp, #12]
	cmp	r0, #32
	beq	LBB1_31
LBB1_21:
	ldr	r0, [sp, #8]
	cmp	r0, #7
	bne	LBB1_23

	ldr	r0, [sp, #12]
	cmp	r0, #32
	beq	LBB1_31
LBB1_23:
	ldr	r0, [sp, #8]
	cmp	r0, #8
	bne	LBB1_25

	ldr	r0, [sp, #12]
	cmp	r0, #31
	beq	LBB1_31
LBB1_25:
	ldr	r0, [sp, #8]
	cmp	r0, #9
	bne	LBB1_27

	ldr	r0, [sp, #12]
	cmp	r0, #32
	beq	LBB1_31
LBB1_27:
	ldr	r0, [sp, #8]
	cmp	r0, #10
	bne	LBB1_29

	ldr	r0, [sp, #12]
	cmp	r0, #31
	beq	LBB1_31
LBB1_29:
	ldr	r0, [sp, #8]
	cmp	r0, #11
	bne	LBB1_32

	ldr	r0, [sp, #12]
	cmp	r0, #32
	bne	LBB1_32
LBB1_31:
	mov	r0, #0
	str	r0, [sp, #12]
	ldr	r0, [sp, #8]
	add	r0, r0, #1
	str	r0, [sp, #8]
LBB1_32:
	ldr	r0, [sp, #8]
	cmp	r0, #12
	bne	LBB1_34

	mov	r0, #0
	str	r0, [sp, #8]
	ldr	r0, [sp, #4]
	add	r0, r0, #1
	str	r0, [sp, #4]
LBB1_34:
	b	LBB1_1
