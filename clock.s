main:
	MOV SP, #65536
	SUB SP, SP, #4
	MOV R4, #55
	MOV R5, #59
	MOV R6, #23
	MOV R7, #30
	MOV R8, #2
	MOV R9, #2000
	ADD R9, R9, #15
	B fin
clock:
	ADD R4, R4,	#1 @R4 : secondes
	CMP R4, #60
	BNE fin
	MOV R4, #0

	ADD R5, R5, #1 @R5 : minutes (modulo 60)
	CMP R5, #60
	BNE fin
	MOV R5, #0

	ADD R6, R6, #1 @R6 : heures (modulo 24)
	CMP R6, #24
	BNE fin
	MOV R6, #0

	ADD R7, R7, #1 @R7 : jours (le cas chiant)
	CMP R8, #0
	BNE fev
	CMP R7, #31
	BNE fin
	MOV R7, #0
	B mois
fev:
	CMP R8, #1
	BNE mar
	MOV R0, R9
	BL bis
	ADD R0, R0, #28
	CMP R7, R0
	BNE fin
	MOV R7, #0
	B mois
mar:
	CMP R8, #2
	BNE avr
	CMP R7, #31
	BNE fin
	MOV R7, #0
	B mois
avr:
	CMP R8, #3
	BNE mai
	CMP R7, #30
	BNE fin
	MOV R7, #0
	B mois
mai:
	CMP R8, #4
	BNE juin
	CMP R7, #31
	BNE fin
	MOV R7, #0
	B mois
juin:
	CMP R8, #5
	BNE juil
	CMP R7, #30
	BNE fin
	MOV R7, #0
	B mois
juil:
	CMP R8, #6
	BNE aout
	CMP R7, #31
	BNE fin
	MOV R7, #0
	B mois
aout:
	CMP R8,	#7
	BNE sep
	CMP R7, #31
	BNE fin
	MOV R7, #0
	B mois
sep:
	CMP R8, #8
	BNE oct
	CMP R7, #30
	BNE fin
	MOV R7, #0
	B mois
oct:
	CMP R8, #9
	BNE nov
	CMP R7, #31
	BNE fin
	MOV R7, #0
	B mois
nov:
	CMP R8, #10
	BNE dec
	CMP R7, #30
	BNE fin
	MOV R7, #0
	B mois
dec:
	CMP R7, #31
	BNE fin
	MOV R7, #0

mois:
	ADD R8, R8, #1
	CMP R8, #12 
	BNE fin
	MOV R8, #0

	ADD R9, R9, #1

fin:
	MOV R0, #0
	STR R4, [R0, #0]
	STR R5, [R0, #4]
	STR R6, [R0, #8]
	STR R7, [R0, #12]
	STR R8, [R0, #16]
	STR R9, [R0, #20] 
	MOV R10, #1 
	STR R10, [R0, #24] @dit au simulateur d'afficher
	STR R0, [R0, #24]
	MOV R10, #0
	B clock

div:
	MOV R2, R0
	MOV R0, #0
loop:
	CMP R2, R1
	ADDGE R0, R0, #1
	SUBGE R2, R2, R1
	BGE loop
	MOV PC, LR

mod:
	STR LR, [SP, #0]
	SUB SP, SP, #4
	STR R0, [SP, #0]
	SUB SP, SP, #4

	BL div

	MUL R2, R0, R1
	LDR R0, [SP], #4
	SUB R0, R0, R2
	
	LDR PC, [SP], #4


bis:
	STR LR, [SP, #0]
	SUB SP, SP, #4
	MOV R3, R0
	
	TST R0, #3 @ R0 % 4 == 0 ?
	BNE non
	
	MOV R1, #100 @ R0 % 100 != 0 ?
	BL mod
	CMP R0, #0
	BNE oui

	MOV R0, R3 @ R0 % 400 == 0 ?
	MOV R1, #400
	BL mod
	CMP R0, #0
	BNE non

oui:
	MOV R0, #1
	LDR PC, [SP], #4
non:
	MOV R0, #0
	LDR PC, [SP], #4

