
; Se debe generar el ejecutable .COM con los siguientes comandos:
; tasm int66ADP.asm
; tlink /t int66ADP.obj

.8086
.model tiny   
.code
   org 100h   
start:
  jmp main   ; Comienza con un salto para dejar la parte residente primero


;- Part que queda residente en memoria y contine las ISR
;- de la interrupcion capturada

Funcion PROC FAR
  ;Requiere pasarle un seed por AX
  ; la sumatoria/secuencia de weyl por SI
  ; el numero random anterior por DI
  ;Devuelve el numero random por AX
  ; la sumatoria/secuencia de weyl por SI
  ; el numero random anterior por DI
  sti
  push dx
  pushf

  add si, ax    ;Le sumo el seed original a la secuencia de Weyl

  mov ax, di    ;Pongo el numero random anterior en AX
  mul ax        ;Elevo el numero al cuadrado (multiplico por si mismo)
  add ax, si    ;Le sumo lo que hay en SI
  mov dl, ah    ;Intercambio las dos mitades del numero
  mov ah, al
  mov al, dl

  popf
  pop dx
  iret
endp

; Datos usados dentro de la ISR ya que no hay DS dentro de una ISR
DespIntXX dw 0
SegIntXX  dw 0

FinResidente LABEL BYTE   ; Marca el fin de la porción a dejar residente

; Datos a ser usados por el Instalador

Cartel    DB "--> LA ISR FUE INSTALADA <--",0dh, 0ah, '$'

main:         ; Se apuntan todos los registros de segmentos al mismo lugar CS.
  mov ax,CS
  mov DS,ax
  mov ES,ax

InstalarInt:
  mov AX,3566h        ; Obtiene la ISR que esta instalada en la interrupcion
  int 21h    
       
  mov DespIntXX,BX    
  mov SegIntXX,ES

  mov AX,2566h    ; Coloca la nueva ISR en el vector de interrupciones
  mov DX,Offset Funcion 
  int 21h

MostrarCartel:
  mov dx, offset Cartel
  mov ah,9
  int 21h

DejarResidente:   
  Mov     AX,(15 + offset FinResidente) ;Sumo 15 para asegurarme un parágrafo más
  Shr     AX,1            
  Shr     AX,1        ;Se obtiene la cantidad de parágrafos
  Shr     AX,1    ;ocupado por el codigo dividiendo por 16
  Shr     AX,1
  Mov     DX,AX           
  Mov     AX,3100h    ;y termina sin error 0, dejando el
  Int     21h         ;programa residente
end start
