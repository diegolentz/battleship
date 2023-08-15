.8086
.model small
.stack 100h
.data

  msjInicio   db "Las posiciones se determinan por fila (A-J) y columna (0-9)", 0dh, 0ah, 24h
  msjError    db "No es posible ubicar, revisar coordenadas / orientacion. Intente nuevamente.", 0dh, 0ah, 24h
  msjBarcos   db "Ubique el barco ", 24h
  msjTam      db ", el tamanio del barco es: ", 24h
  msjIngreso  db "Ingrese coordenada desde  fila (A-J) y columna (0-9): ", 24h
  msjOrientacion db "Elija  H->  horizontal || V -> vertical: ", 24h
  msjMalPos   db "Posicion incorrecta, ingrese (Esc)->  salir || (Enter) -> seguir", 0dh, 0ah, 24h
  msjPosErr   db "Ya se disparo en esa coordenada", 0dh, 0ah, 24h
  msjFin      db "Fin del juego", 0dh, 0ah, 24h
  msjDisparoMaq db "The enemy is using colossus to locate your boats.  (enter) -> continuar: ", 24h
  msjHundido  db "El barco ha sido destruido! ", 24h
  msjContadorBarcos db ", quedan en total: ", 24h
  msjGanaste  db "Felicitaciones!", 0dh, 0ah,24h
  msjPerdiste  db "Has sido derrotado!!", 0dh, 0ah, 24h
  msjSurrender db "La proxima tendras suerte!!", 0dh, 0ah, 24h
  salto db 0dh, 0ah, 24h

  bandera db 0
  Intentos db 17
  IntentosMaq db 17

  ;para generacion de numero aleatorio
  seed        dw 0
  weylseq     dw 0
  prevRandInt dw 0

            ;tableros que ve el usuario
  tablero   db "x------German army-----x", 0dh, 0ah ;Tablero enemigo, solo disparos
            db "|  0 1 2 3 4 5 6 7 8 9 |", 0dh, 0ah
            db "|a . . . . . . . . . . |", 0dh, 0ah
            db "|b . . . . . . . . . . |", 0dh, 0ah
            db "|c . . . . . . . . . . |", 0dh, 0ah
            db "|d . . . . . . . . . . |", 0dh, 0ah
            db "|e . . . . . . . . . . |", 0dh, 0ah
            db "|f . . . . . . . . . . |", 0dh, 0ah
            db "|g . . . . . . . . . . |", 0dh, 0ah
            db "|h . . . . . . . . . . |", 0dh, 0ah
            db "|i . . . . . . . . . . |", 0dh, 0ah
            db "|j . . . . . . . . . . |", 0dh, 0ah
tableroUser db "x-------US Army--------x", 0dh, 0ah ;Tablero usuario
            db "|  0 1 2 3 4 5 6 7 8 9 |", 0dh, 0ah
            db "|a . . . . . . . . . . |", 0dh, 0ah
            db "|b . . . . . . . . . . |", 0dh, 0ah
            db "|c . . . . . . . . . . |", 0dh, 0ah
            db "|d . . . . . . . . . . |", 0dh, 0ah
            db "|e . . . . . . . . . . |", 0dh, 0ah
            db "|f . . . . . . . . . . |", 0dh, 0ah
            db "|g . . . . . . . . . . |", 0dh, 0ah
            db "|h . . . . . . . . . . |", 0dh, 0ah
            db "|i . . . . . . . . . . |", 0dh, 0ah
            db "|j . . . . . . . . . . |", 0dh, 0ah
            db "x----------------------x", 0dh,0ah,24h


                ;Tablero  maquina. está invisible tiene sus barcos ya posicionados
tableroMaquina  db "x----------------------x", 0dh, 0ah
                db "|  0 1 2 3 4 5 6 7 8 9 |", 0dh, 0ah
                db "|a . . . . . . . . . . |", 0dh, 0ah
                db "|b . . . . . . . . . . |", 0dh, 0ah
                db "|c . . . . . . . . . . |", 0dh, 0ah
                db "|d . . . . . . . . . . |", 0dh, 0ah
                db "|e . . . . . . . . . . |", 0dh, 0ah
                db "|f . . . . . . . . . . |", 0dh, 0ah
                db "|g . . . . . . . . . . |", 0dh, 0ah
                db "|h . . . . . . . . . . |", 0dh, 0ah
                db "|i . . . . . . . . . . |", 0dh, 0ah
                db "|j . . . . . . . . . . |", 0dh, 0ah
                db "x----------------------x", 24h

  letrasBarcos db "ABCDE" ;simbolos de los barcos
  tamBarcos db "54233" ;coordenadas de los barcos
  contadorBarcos db '5' ;cuenta  cuantos barcos hay sin hundir
  chars db 26  ;Cantidad de caracteres por fila
  colW db 2   ;Cantidad de caracteres por columna del tablero

.code

;se importan las funciones
  extrn comprobar_lugar:proc
  extrn obtenerIndice:proc
  extrn seedInicial:proc
  extrn disparar:proc
  extrn ubicarBarco:proc
  extrn ponerBarco:proc

main proc
    mov ax, @data
    mov ds, ax

    call seedInicial;obtiene en ax la sumatoria
    
    mov seed, ax
    mov weylseq, ax
    mov prevRandInt, ax
    call Clearscreen    ;bios

    mov ah, 9
    mov dx, offset msjInicio
    int 21h
    
  ;ubicacion de barcos del usuario

    mov di, 0
    mov bx, offset tableroUser
    ;muestro el tablero
    ubicarUnBarco:
      cmp di, 5 ;contador barcos
      je inicio
      mov ah, 9
      mov dx, bx ;Muevo el offset del tablero usuario
      int 21h
      call pedirBarco
      inc di ;Incremento el contador de barcos
      call Clearscreen
      jmp ubicarUnBarco

inicio:
    xor ax, ax
    xor bx, bx
    xor cx, cx
    xor dx, dx
    xor si, si
    xor di, di
    
    mov bx, offset tableroMaquina
    
;UBICACION DE BARCOS DE LA MAQUINA

  ;barco A
  ubicarA:
    mov bandera[0], 0 ;Reinicio la bandera
    call generarFyC
    call ElijeHoV
    mov si, ax ;Muevo el valor de la orientacion devuelta por ElijeHoV
    mov al, "A" ;Quiero un barco A
    mov cl, colW ;Muevo la cantidad de caracteres por columna del tablero
    mov ch, chars ;Muevo la cantidad de caracteres por fila
    call ubicarBarco
    mov si, offset bandera ;Muevo el offset de la bandera para que pueda activarla, en caso de necesitarlo
    call ponerBarco ;Tambien le paso el indice por DI
    cmp bandera[0], 1 ;Comparo si se activo la bandera de error
    je ubicarA ;Si es asi, entonces se ubica de nuevo

  ;barco B
  ubicarB:
    mov bandera[0], 0
    call generarFyC
    call ElijeHoV
    mov si, ax
    mov al, "B" 
    mov cl, colW
    mov ch, chars
    call ubicarBarco
    mov si, offset bandera
    call ponerBarco
    cmp bandera[0], 1
    je ubicarB

  ;barco C
  ubicarC:
    mov bandera[0], 0
    call generarFyC
    call ElijeHoV
    mov si, ax
    mov al, "C" 
    mov cl, colW
    mov ch, chars
    call ubicarBarco
    mov si, offset bandera
    call ponerBarco
    cmp bandera[0], 1
    je ubicarC

  ;barco D
  ubicarD:
    mov bandera[0], 0
    call generarFyC 
    call ElijeHoV
    mov si, ax
    mov al, "D"
    mov cl, colW
    mov ch, chars
    call ubicarBarco
    mov si, offset bandera
    call ponerBarco
    cmp bandera[0], 1
    je ubicarD

  ;barco E
  ubicarE:
    mov bandera[0], 0
    call generarFyC
    call ElijeHoV
    mov si, ax
    mov al, "E" ;Quiero un crucero
    mov cl, colW
    mov ch, chars
    call ubicarBarco
    mov si, offset bandera
    call ponerBarco
    cmp bandera[0], 1
    je ubicarE
  
imprimir:
    ;Una vez ubicados los barcos, imprimo el tablero
    mov ah, 9
    mov dx, offset tablero
    int 21h

;DISPAROS DEL USUARIO   

ingresoPos:
    ;Pido que ingrese una coordenada
    mov ah, 9
    mov dx, offset msjIngreso
    int 21h
    ;Leo el primer caracter
    mov ah, 1
    int 21h
    ;Chequeo que sea válido
    cmp al, "a"
    jb checkLetra
    cmp al, "j"
    ja malPos_auxiliar
    ;Es una letra minuscula, mayusculizo
    sub al, 20h

checkLetra:
    cmp al, "A"
    jb malPos_auxiliar
    cmp al, "J"
    ja malPos_auxiliar
    ;Es una letra valida, la guardo el DH
    mov dh, al
    ;Leo el siguiente caracter
    mov ah, 1
    int 21h
    ;Chequeo que sea un numero
    cmp al, "0"
    jb malPos_auxiliar
    cmp al, "9"
    ja malPos_auxiliar
    jmp letra_valida

  malPos_auxiliar:
    ;Salto intermedio
    jmp malPos

letra_valida:
    ;Es un valor valido, lo guardo en DL
    mov dl, al
    ;Ahora tengo la posicion guardada en DX

    mov cl, colW
    mov ch, chars
    ;Muevo las variables necesarias para las funciones
    call obtenerIndice
    mov bx, offset tablero
    mov si, offset tableroMaquina
    call disparar
    
    ;Chequeo si el disparo fue exitoso 
    cmp al, 0 ;Hubo un error, no se puede disparar en esa posicion
    je nuevoIntento
    
    cmp al, 1 ;Dio a agua
    je continuar

    ;Si llega hasta aca, le dio a un barco

    ;Comparo a que barco dio el disparo
    mov bx, offset tableroMaquina
    mov si, 0 ;Uso SI para recorrer la variable de simbolos de los barcos

  revisoBarcos:
    mov dl, letrasBarcos[si]
    cmp byte ptr [bx + di], dl ;Comparo si el disparo dio en este barco
    je decBarcos 
    inc si ;Leo el siguiente barco
  jmp revisoBarcos

  decBarcos:
    dec tamBarcos[si] ;Decremento lo que es el tamanio de ese barco para ver cuantos intentos le quedan
    cmp tamBarcos[si], 30h ;Veo si el barco fue hundido
    jne decIntentos ;Si no lo fue, me salteo el imprimir el cartel

    dec contadorBarcos[0] ;Decremento el contador de barcos

    mov ah, 9
    mov dx, offset salto
    int 21h

    mov ah, 9
    mov dx, offset msjHundido
    int 21h

    mov ah, 2
    mov dl, letrasBarcos[si]
    int 21h

    mov ah, 9
    mov dx, offset msjContadorBarcos
    int 21h

    mov ah, 2
    mov dl, contadorBarcos[0]
    int 21h

  decIntentos:
    dec Intentos[0]  ;Intentos guarda la cantidad de disparos efectivos restante para ganar. 
    

    cmp Intentos[0], 0  ;Si Intentos llega a 0, el jugador obtendra la victoria
    je fin_auxiliar

    jmp continuar

  fin_auxiliar:
    ;Salto intermedio
    jmp fin

nuevoIntento:
    mov ah, 9                 
    mov dx, offset salto
    int 21h
    mov ah, 9
    mov dx, offset msjPosErr
    int 21h
    jmp ingresoPos

  continuar:
    ;Vuelvo a pedir una nueva coordenada
    mov ah, 08h ;Espero otra tecla sin imprimir en pantalla
    int 21h
    jmp seguir

    ;No se ingreso una posicion valida
malPos:
    mov ah, 9
    mov dx, offset salto
    int 21h
    ;Informo que no lo era
    mov ah, 9
    mov dx, offset msjMalPos
    int 21h

leerTecla:
    ;Espero confirmacion del usuario para seguir o salir
    mov ah, 08h ;Leer sin eco en la pantalla
    int 21h
    ;Si apreto Esc termino el programa
    cmp al, 1Bh
    je fin
    ;Si apreto Enter vuelvo a pedir que ingrese una coordenada
    cmp al, 0dh
    je vuelta
    ;Si ingreso cualquier otra cosa sigo esperando
    jmp leerTecla


seguir:
    call Clearscreen
    ;Imprimir tablero
    mov ah, 9
    mov dx, offset tablero
    int 21h

    mov ah, 9
    mov dx, offset msjDisparoMaq ;mensaje para avisar que la maquina esta efectuando un disparo
    int 21h
    

;DISPAROS DE LA MAQUINA

disparoMaq:
    call generarFyC
    mov cl, colW
    mov ch, chars
    call obtenerIndice
    mov bx, offset tableroUser
    mov si, offset tableroUser
    call disparar

    cmp al, 0
    je disparoMaq

    cmp al, 1
    je aguaMaq

    dec IntentosMaq[0]
    cmp IntentosMaq[0], 0
    je fin
    
  aguaMaq:
    ;Espero otra tecla sin imprimir en pantalla para que pueda leer la posicion ingresada
    mov ah, 08h
    int 21h


vuelta:
    ;Limpio la pantalla antes de volver a mostrar el tablero
    call Clearscreen
    jmp imprimir

fin:
    call Clearscreen

    cmp Intentos[0], 0 ;Veo si los intentos de la maquina es 0 (ya no tiene barcos)
    je ganaste 
    ;Si la maquina no es 0 (ganamos), entonces el nuestro es 0 (perdimos) 
    cmp IntentosMaq[0], 0
    jne surrender

    mov ah, 9
    mov dx, offset msjPerdiste
    int 21h
    mov ah, 9
    mov dx, offset msjSurrender
    int 21h
    jmp fin1

  ganaste:
    mov ah, 9
    mov dx, offset msjGanaste
    int 21h
    jmp fin1

  surrender:
    mov ah, 9
    mov dx, offset msjSurrender
    int 21h

  fin1:
    mov ah, 9
    mov dx, offset msjFin
    int 21h
    mov ax, 4c00h
    int 21h
main endp


;Funciones

Clearscreen proc
  push ax
  push es
  push cx
  push di

    mov ax,3
    int 10h
    mov ax,0b800h
    mov es,ax
    mov cx,1000
    mov ax,7
    mov di,ax
    cld
    rep stosw

  pop di
  pop cx
  pop es
  pop ax
  ret 
Clearscreen endp


;Funcion para generar filas y columnas 
;Recibe por AX EL seed o 0 para usar el existente
  ; por SI el offset de la secuencia de weyl
  ;y por DI el offset del numero random anterior
;devuelvo en DX la posicion random
generarFyC proc

  push ax
  push bx
  push si
  push di
  pushf

    xor dx, dx

    mov ax, seed
    mov si, weylseq
    mov di, prevRandInt
    int 66h   ;Genero un numero random
    ;Recibo por AX el numero random
    mov weylseq, si
    mov prevRandInt, ax

    xor ah, ah  ;Limpio AH para la división de 8bits
    mov bl, 0Ah
    div bl

    add ah, 41h    ;Convierto a numero ASCII el resto
    mov dh, ah

    xor ah, ah  ;Limpio AH para la división de 8bits
    mov bl, 0Ah
    div bl

    add ah, 30h    ;Convierto a ASCII el resto
    mov dl, ah

  ;Devuelvo el entorno
  popf
  pop di
  pop si
  pop bx
  pop ax
  ret
generarFyC endp
  

;Elige de forma aleatoria si el barco sera vertical u horizontal
;Recibe por AX un seed
  ; por SI la secuencia de weyl
  ; y por DI el numero random anterior
  ;devuelvo en AL la posicion random
ElijeHoV proc

  push si
  push dx
  push bx
  pushf

    mov ax, seed
    mov si, weylseq
    mov di, prevRandInt
    int 66h   ;Genero un numero random
    ;Recibo por AX el numero random
    mov weylseq, si
    mov prevRandInt, ax

    xor ah, ah  ;Limpio AH para la división de 8bits
    mov bl, 2
    div bl

    mov al, ah
    xor ah, ah

  popf
  pop bx
  pop dx
  pop si
  ret
ElijeHoV endp


;Pide los barcos para ser ubicados por el usuario
; Recibe por BX el offset del tablero
; por DI el indice de la letra del barco
pedirBarco proc
 push ax
push bx
push cx
push dx
push di
push si

ubicarBarcoUser:
    mov bandera[0], 0 ; Reinicio la bandera

    mov ah, 9
    mov dx, offset msjBarcos
    int 21h

    mov ah, 2
    mov dl, letrasBarcos[di] ; Simbolos de los barcos
    int 21h

    mov ah, 9
    mov dx, offset msjTam
    int 21h

    mov ah, 2
    mov dl, tamBarcos[di] ; Tamanios de los barcos
    int 21h

    mov ah, 9
    mov dx, offset salto
    int 21h
  
    mov ah, 9
    mov dx, offset msjIngreso ; Pido que ingrese 
    int 21h

    mov ah, 1
    int 21h
    mov dh, al ; La fila está en DH
   
    mov ah, 1
    int 21h
    mov dl, al ; La columna está en DL
   
    ; Transformamos a mayúscula
    cmp dh, 4Ah ; Comparo con J
    jl prosigo
    sub dh, 20h

prosigo:
    push dx
    mov ah, 9
    mov dx, offset salto
    int 21h

    mov ah, 9
    mov dx, offset msjOrientacion
    int 21h

    pop dx
    mov ah, 1
    int 21h
    xor ah, ah
    cmp al, 'H'
    je valido
    cmp al, 'h'
    je valido
    cmp al, 'V'
    je valido
    cmp al, 'v'
    je valido
    
    jmp denuevo

denuevo:
    xor ah, ah
    mov ah, 9
    lea dx, msjError
    int 21h

    jmp ubicarBarcoUser
    
valido:
    xor ah, ah
    mov si, ax ; La orientacion esta en SI
    ; add si, 30h

    mov al, letrasBarcos[di] ; La letra queda guardada en AL
    mov cl, colW
    mov ch, chars
    push di
    call ubicarBarco

    mov si, offset bandera
    call ponerBarco ; Tambien le paso el indice por DI
    pop di

    cmp bandera[0], 1 ; Comparo si hubo error a la hora de ubicar los barcos
    je mensaje
    jmp finPedirBarco

mensaje:
    mov ah, 9
    mov dx, offset salto
    int 21h
    mov ah, 9
    mov dx, offset msjError
    int 21h
    jmp ubicarBarcoUser

finPedirBarco:
pop si
pop di
pop dx
pop cx
pop bx
pop ax
ret
pedirBarco endp

end main