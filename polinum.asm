;--------------------------------------------------------------
;----------------PORTADA-------------
; Tecnologico de Costa Rica - Arquitectura de Computadoras - G2
; Realizado por: Randall Madriz Coto
; Carne: 2021439687
; TAREA: Numeros Multidioma
; Entrega: 28 / 8 / 2022
;-------------------------------------
;-------------MANUAL DE USUARIO--------------

;Este programa se encarga de desplegar el nombre de un numero pasado en linea de comandos en distintos idiomas,
;teniendo ademas la capacidad de traducir desde distintas bases

; Para ejecutar de forma eficiente el programa, debe pasar a linea de comandos una cadena que consista en:
;                                       BaseIdioma numero
; La base es una sola letra al igual que el idioma, seguido de un espacio en blanco. 
;Despues, las iniciales de los nombres de los numeros de las distintas representaciones, a excepcion de decimal

;Bases disponibles:
;D: Decimal
;N: Numedrez
;P: Pecumeros
;S: Planetumeros

;Idiomas disponibles:
;E: Espannol
;J: Japones


; Ejemplo:
;         DE 343
;         trescientos cuarenta y tres


;----------------------------------------------------
;---------------------------------ANALISIS DE RESULTADOS--------------------------------

; Documentacion -> A
; Acerca de ->     A
; Conversion de bases a decimal -> A
; Impresiones en español -> A
; Impresiones en japones -> A
; Impresiones en comodin -> E

;----------------------------------------------------------------------------------------

dataS segment

  ;---------------------------- Program needed variables
  CapitalLString db 128 dup (?)

  Base db ?
  BaseInt db ?
  Language db ?

  PassedValidation db ?
  NumberString db 10 dup (?)
  NumberLength db ?

  DecimalNumberString db 4 dup (?)

  RealNumberString db 10 dup (?)

  DecimalNumber dw ?

  FormatError db "El formato pasado por linea de comandos es incorrecto.",10,13
              db "Formato correcto: BaseIdioma_numero$"
  
  NumberErrorV db "Un digito del numero en linea de comandos no corresponde a los de su base.",10,13,10,13
                db "Digitos disponibles:",10,13 
                db "Base decimal: 1234567890",10,13
                db "Numedrez: PACTDR",10,13
                db "Pecumeros: PLIEGAS",10,13
                db "Planetumeros: MVTRJSUNP$"

  Uni db "Tecnologico de Costa Rica - Arquitectura de Computadoras - G2",10,13
      db "Randall Madriz Coto, carne: 2021439687",10,13,10,13,'$'

  ;-------------------------------------------------------
  ;---------------------------- Print variables
  ;----------------Spanish-------------
  milD db "Mil$"
  dosmilD db "Dos mil$"
  tresmilD db "Tres mil$"
  cuatromilD db "Cuatro mil$"
  cincomilD db "Cinco mil$"
  seismilD db "Seis mil$"
  sietemilD db "Siete mil$"
  ochomilD db "Ocho mil$"
  nuevemilD db "Nueve mil$"

  cienD db " cien $"
  doscientosD db " doscientos $"
  trescientosD db " trescientos $"
  cuatroscientosD db " cuatroscientos $"
  quinientosD db " quinientos $"
  seiscientosD db " seiscientos $"
  setecientosD db " setecientos $"
  ochocientosD db " ochocientos $"
  novecientosD db " novecientos $"

  diezD db "diez $"
  onceD db "once $"
  doceD db "doce $"
  treceD db "trece $"
  catorceD db "catorce $"
  quinceD db "quince $"
  dieciseisD db "dieciseis $"
  diecisieteD db "diecisiete $"
  dieciochoD db "dieciocho $"
  diecinueveD db "diecinueve $"

  veinteD db "veinte $"
  treintaD db "treinta $"
  cuarentaD db "cuarenta $"
  cincuentaD db "cincuenta $"
  sesentaD db "sesenta $"
  setentaD db "setenta $"
  ochentaD db "ochenta $"
  noventaD db "noventa $"

  unoD db "y uno$"
  dosD db "y dos$"
  tresD db "y tres$"
  cuatroD db "y cuatro$"
  cincoD db "y cinco$"
  seisD db "y seis$"
  sieteD db "y siete$"
  ochoD db "y ocho$"
  nueveD db "y nueve$"

  ;---------------------------------JAPANESE
  oneJ db " ichi$"
  twoJ db " ni$"
  threeJ db " san$"
  fourJ db " shi$"
  fiveJ db " go$"
  sixJ db " roku$"
  sevenJ db " shichi$"
  eightJ db " hachi$"
  nineJ db " kyu$"

  tenJ db " ju$"
  hundredJ db " hyaku$"
  thousandJ db " sen$"

  
dataS endS

stackS segment stack 'stack'
   dw 256 dup(?)
stackS endS



codeS segment
       Assume CS:codeS,DS:dataS,SS:stackS



CLineCapitalL proc 
  ;Command Line to Capital Letter
  ;converts the command line arguments to capital letter and puts them in a variable
  ;It also saves the lenght of the number in a variable
      xor si, si
      mov si, 80h
      
      
      lea di, CapitalLString

      xor ax, ax
      mov al, byte ptr es:[si]
      mov byte ptr CapitalLString[di], al

      xor cx, cx
      mov cl, byte ptr es:[si]

      ;saves the length of the number in a variable
      push cx
      sub cl, 4
      mov NumberLength, cl
      pop cx

    cicle:
      inc si
      inc di
      mov al, byte ptr es:[si]
      cmp al, 5Ah
      jg CapitalLRefactor

      mov byte ptr CapitalLString[di], al
      loop cicle
      jmp Exitcicle

    CapitalLRefactor:
      sub al, 20h
      dec cx
      mov byte ptr CapitalLString[di], al
      cmp cx, 0
      je Exitcicle
      jmp cicle

    Exitcicle: 
      ret

CLineCapitalL endP

;-----------------------------SAVENUMBER PROCEDURE--------------
SaveNumber proc
  ;saves a variable with only the number digits
  xor si, si
  xor di, di

  lea si, CapitalLString
  lea di, NumberString
  xor di, di

  add si, 5
      
  xor ax, ax
  xor cx, cx
  mov cl, NumberLength

  ;mov al, byte ptr CapitalLString[si]
  ;mov byte ptr NumberString[di], al

  saveNCycle:
    mov al, byte ptr CapitalLString[si]
    mov byte ptr NumberString[di], al
    inc si
    inc di
        
    loop saveNCycle

  ret
SaveNumber endP

GetFirstDigitDx proc ;given a number, it gets the first digit and stores it on dx
  mov ax, DecimalNumber
  mov bx, 10
  div bx
  xor ax, ax
GetFirstDigitDx endP

GetSecondDigitDx proc  ;given a number, it gets the second digit and stores it on dx
  mov ax, DecimalNumber
  mov bx, 100
  div bx
  mov bx, 10
  mov ax, dx
  div bx
  xor ax, ax
GetSecondDigitDx endP

GetThirdDigitDx proc    ;given a number, it gets the third digit and stores it on dx
  mov ax, DecimalNumber
  mov bx, 1000
  div bx
  mov ax, dx ;Rnn
  mov bx, 100
  div bl
  xor ah, ah
  mov dx, ax
GetThirdDigitDx endP

GetFourthDigitDx proc    ;given a number, it gets the fourth digit and stores it on dx
   mov ax, DecimalNumber
    mov bx, 1000
    div bx
    mov dx, ax
GetFourthDigitDx endP

;-----------------------------VALIDATIONS PROCEDURES------------------------
FormatValidationProc proc ;verifies the parameters to have the correct format
      lea si, CapitalLString
      xor si, si
      inc si
      inc si


      cmp byte ptr CapitalLString[si], 'D'
      je ValidLang
      
      cmp byte ptr CapitalLString[si], 'N'
      je ValidLang

      cmp byte ptr CapitalLString[si], 'P'
      je ValidLang

      cmp byte ptr CapitalLString[si], 'S'
      je ValidLang

      jmp FormNotPassedValid1

      ValidLang:
      inc si
      cmp byte ptr CapitalLString[si], 'E'
      je FormValidPassed

      cmp byte ptr CapitalLString[si], 'J'
      je FormValidPassed

      cmp byte ptr CapitalLString[si], 'X'
      je FormValidPassed

      jmp FormNotPassedValid2


      FormNotPassedValid1: ;not passed because of Base
      mov PassedValidation, 0
      ret


      FormNotPassedValid2: ;not passed because of Language
      mov PassedValidation, 0
      ret


      FormValidPassed:
      mov PassedValidation, 1
      ret


FormatValidationProc endP

DecimalValidationProc proc ; verifies all numbers in the parameters have the correct decimal format. Builds a string
  ;variables with the digits
  lea si, NumberString
  xor si, si
  lea di, RealNumberString
  xor di,di
  xor cx, cx
  xor bx, bx
  mov cl, NumberLength

  DecValiCycle:
    cmp byte ptr NumberString[si], 30h
    jb DecValidNotPassed

    cmp byte ptr NumberString[si], 39h
    jg DecValidNotPassed

    mov bl, byte ptr NumberString[si]
    mov byte ptr RealNumberString[di], bl
    inc di

    inc si
    

    loop DecValiCycle

    mov PassedValidation, 1
    ret

  DecValidNotPassed:
    mov PassedValidation, -1
    ret

DecimalValidationProc endP

;----------------------------Numedrez string validation----
NumedrezValidationProc proc ; verifies all numbers in the parameters have the correct numedrez format. Builds a string
  ;variables with the digits
  lea si, NumberString
  xor si, si
  lea di, RealNumberString
  xor di, di
  xor bx,bx      ;limite
  xor cx, cx     ;cuenta
  mov bl, NumberLength

  NumValidCycle:
  cmp cl, bl
  je NumValidPassedValid

  jmp NumP

  NumP:
  cmp byte ptr NumberString[si], 'P'
  je NumedGoodLetterP

  jmp NumA

  NumA:
  cmp byte ptr NumberString[si], 'A'
  je NumedGoodLetterA

  jmp NumC

  NumC:
  cmp byte ptr NumberString[si], 'C'
  je NumedGoodLetterC

  jmp NumT

  NumT:
  cmp byte ptr NumberString[si], 'T'
  je NumedGoodLetterT

  jmp NumD

  NumD:
  cmp byte ptr NumberString[si], 'D'
  je NumedGoodLetterD

  jmp NumR

  NumR:
  cmp byte ptr NumberString[si], 'R'
  je NumedGoodLetterR

  jmp NumValidNotPassed

  NumedGoodLetterP:
  mov byte ptr RealNumberString[di], '0'
  jmp NumedInc

  NumedGoodLetterA:
  mov byte ptr RealNumberString[di], '1'
  jmp NumedInc

  NumedGoodLetterC:
  mov byte ptr RealNumberString[di], '2'
  jmp NumedInc

  NumedGoodLetterT:
  mov byte ptr RealNumberString[di], '3'
  jmp NumedInc

  NumedGoodLetterD:
  mov byte ptr RealNumberString[di], '4'
  jmp NumedInc

  NumedGoodLetterR:
  mov byte ptr RealNumberString[di], '5'
  jmp NumedInc

  NumedInc:
  inc si
  inc di
  inc cl
  jmp NumValidCycle


  NumValidNotPassed:
  mov PassedValidation, 0
  ret

  NumValidPassedValid:
  mov PassedValidation, 1
  ret

NumedrezValidationProc endP
;--------------------------- Pecumero string validation
PecumeroValidationProc proc
  ; Validate letter by letter the Pecumero digits.
  ; If wrong, PassedValidation variable is left with 0 as a value
  ; If right, PassedValidation variable is left with 1 as a value
  lea si, NumberString
  xor si, si
  lea di, RealNumberString
  xor di, di
  xor ax, ax
  xor bx,bx      ;limite
  xor cx, cx     ;cuenta
  mov bl, NumberLength

  PecValidCycle:
  cmp cl, bl
  je PecValidPassedValidJump

  mov al, byte ptr NumberString[si]

  jmp PecP

  PecValidPassedValidJump:
  jmp PecValidPassedValid

  PecP:
  cmp al, 'P'
  je PecuGoodLetterP

  jmp PecL

  PecL:
  cmp al, 'L'
  je PecuGoodLetterL

  jmp PecI

  PecI:
  cmp al, 'I'
  je PecuGoodLetterI

  jmp PecE

  PecE:
  cmp al, 'E'
  je PecuGoodLetterE

  jmp PecG

  PecG:
  cmp al, 'G'
  je PecuGoodLetterG

  jmp PecA

  PecA:
  cmp al, 'A'
  je PecuGoodLetterA

  jmp PecS

  PecS:
  cmp al, 'S'
  je PecuGoodLetterS

  jmp PecValidNotPassed


  PecuGoodLetterP:
  mov byte ptr RealNumberString[di], '0'
  jmp PecInc

  PecuGoodLetterL:
  mov byte ptr RealNumberString[di], '1'
  jmp PecInc

  PecuGoodLetterI:
  mov byte ptr RealNumberString[di], '2'
  jmp PecInc

  PecuGoodLetterE:
  mov byte ptr RealNumberString[di], '3'
  jmp PecInc

  PecuGoodLetterG:
  mov byte ptr RealNumberString[di], '4'
  jmp PecInc

  PecuGoodLetterA:
  mov byte ptr RealNumberString[di], '5'
  jmp PecInc

  PecuGoodLetterS:
  mov byte ptr RealNumberString[di], '6'
  jmp PecInc



  PecInc:
  inc si
  inc di
  inc cl
  jmp PecValidCycle


  PecValidNotPassed:
  mov PassedValidation, 0
  ret

  PecValidPassedValid:
  mov PassedValidation, 1
  ret


PecumeroValidationProc endP
;--------------------------- Planetumero string validation
PlanetumeroValidationProc proc
    ; Validate letter by letter the Pecumero digits.
  ; If wrong, PassedValidation variable is left with 0 as a value
  ; If right, PassedValidation variable is left with 1 as a value
  lea si, NumberString
  xor si, si
  lea di, RealNumberString
  xor di, di
  xor bx,bx      ;limite
  xor cx, cx     ;cuenta
  mov bl, NumberLength

  PlanValidCycle:
  cmp cl, bl
  je PlanValidPassedValidJump

  jmp PlanM

  PlanValidPassedValidJump:
  jmp PlanValidPassedValid


  PlanM:
  cmp byte ptr NumberString[si], 'M'
  je PlanGoodLetterM

  jmp PlanV

  PlanV:
  cmp byte ptr NumberString[si], 'V'
  je PlanGoodLetterV

  jmp PlanT

  PlanT:
  cmp byte ptr NumberString[si], 'T'
  je PlanGoodLetterT

  jmp PlanR

  PlanR:
  cmp byte ptr NumberString[si], 'R'
  je PlanGoodLetterR

  jmp PlanJ

  PlanJ:
  cmp byte ptr NumberString[si], 'J'
  je PlanGoodLetterJ

  jmp PlanS

  PlanS:
  cmp byte ptr NumberString[si], 'S'
  je PlanGoodLetterS

  jmp PlanU

  PlanU:
  cmp byte ptr NumberString[si], 'U'
  je PlanGoodLetterU

  jmp PlanN

  PlanN:
  cmp byte ptr NumberString[si], 'N'
  je PlanGoodLetterN

  jmp PlanP

  PlanP:
  cmp byte ptr NumberString[si], 'P'
  je PlanGoodLetterP

  jmp PlanValidNotPassed



  PlanGoodLetterM:
  mov byte ptr RealNumberString[di], '0'
  jmp PlanInc

  PlanGoodLetterV:
  mov byte ptr RealNumberString[di], '1'
  jmp PlanInc

  PlanGoodLetterT:
  mov byte ptr RealNumberString[di], '2'
  jmp PlanInc

  PlanGoodLetterR:
  mov byte ptr RealNumberString[di], '3'
  jmp PlanInc

  PlanGoodLetterJ:
  mov byte ptr RealNumberString[di], '4'
  jmp PlanInc

  PlanGoodLetterS:
  mov byte ptr RealNumberString[di], '5'
  jmp PlanInc

  PlanGoodLetterU:
  mov byte ptr RealNumberString[di], '6'
  jmp PlanInc

  PlanGoodLetterN:
  mov byte ptr RealNumberString[di], '7'
  jmp PlanInc

  PlanGoodLetterP:
  mov byte ptr RealNumberString[di], '8'
  jmp PlanInc


  PlanInc:
  inc si
  inc di
  inc cl
  jmp PlanValidCycle


  PlanValidNotPassed:
  mov PassedValidation, 0
  ret

  PlanValidPassedValid:
  mov PassedValidation, 1
  ret
PlanetumeroValidationProc endP

;----------------------------------- MAIN CODE ---------------------------
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
main: mov ax, stackS
      mov ss, ax

      mov ax, dataS
      mov ds, ax

      xor ax, ax
      mov ah, 09h
      lea dx, Uni
      int 21h

      xor dx,dx

      call CLineCapitalL ;converts the command line arguments to capital letter and puts them in a 
                         ;variable, and saves the length of the number in a variable

      call FormatValidationProc
      cmp PassedValidation, 0
      je EndProgramFormat  ;did it passed format validation?

      mov PassedValidation, 0


      ;*********************************************************************************************

      
      call SaveNumber  ;saves a variable with only the number digits


      ;--------------Saves the Base and the Language in variables
      lea si, CapitalLString
      xor si, si
      add si, 2
      mov al, byte ptr CapitalLString[si]
      mov Base, al
      inc si
      mov al, byte ptr CapitalLString[si]
      mov Language, al
      ;--------------------------------------------

      cmp Base, 'D'
      je ValidateDecimal

      cmp Base, 'N'
      je ValidateNumedrez

      cmp Base, 'P'
      je ValidatePecumeros

      cmp Base, 'S'
      je ValidatePlanetumeros

      jmp EndProgram

      EndProgramFormat:   ;prints error message
        mov ah, 09h
        lea dx, FormatError
        int 21h
        jmp EndProgram


      ValidateDecimal:    ;validates correct decimal number
        call DecimalValidationProc
        cmp PassedValidation, 1
        je DecimalTraduction
        jmp NumberError
        

      ValidateNumedrez: ;validates correct numedrez number
        call NumedrezValidationProc
        cmp PassedValidation, 1
        je NumedrezTraduction
        jmp NumberError
        ;---Sale jajajaj

      ValidatePecumeros: ;validates correct pecumero number
        call PecumeroValidationProc
        cmp PassedValidation, 1
        je PecumerosTraduction
        jmp NumberError


      ValidatePlanetumeros:  ;validates correct planetumero number
        call PlanetumeroValidationProc
        cmp PassedValidation, 1
        je PlanetumerosTraduction
        jmp NumberError


      NumberError: ;prints error message
      mov ah, 09h
      lea dx, NumberErrorV
      int 21h
      jmp EndProgram




      DecimalTraduction:
      mov BaseInt, 10
      jmp Traduction

      NumedrezTraduction:
      mov BaseInt, 6
      jmp Traduction
      ;call 
      ;mov dx, 5

      PecumerosTraduction:
      mov BaseInt, 7
      jmp Traduction
      ;call PecumeroValidationProc
      ;mov dx, 5

      PlanetumerosTraduction:
      mov BaseInt, 9
      jmp Traduction
  

      Traduction:
        xor ax, ax         ; ax tiene la potencia de la base
        inc ax             ; guardo un 1 como primera base

        xor bx, bx         ; en bx voy guardando el resultado
        xor dx, dx         ; en dx guardo el digito del string

        xor cx, cx
        mov cl, NumberLength

        lea di, RealNumberString
        xor di, di

        mov bl, NumberLength
        add di, bx       ;deberia llegar al final del string
        dec di 

        xor bx, bx

        TraductionCycle:
        push ax  ;guardo la potencia

        mov dl, byte ptr RealNumberString[di] ;save the char from the string
        ;and convert it into integer
        sub dl, 30h ;dl has the integer about to be multiplied

        mul dx

        add bx, ax   ;bx has the result. Ax the multiplication
        dec di

        pop ax
        mul BaseInt

        loop TraductionCycle ;it will repeat depending on number length
        mov DecimalNumber, bx
        xor ax, ax
        xor dx, dx
        xor bx, bx

      cmp Language, 'E'
      je SpanishPrints

      cmp Language, 'J'
      je JapanesePrintsJump

      jmp EndProgramFormat


      JapanesePrintsJump:
      jmp JapanesePrints

      EndProgramJump:
      jmp EndProgram


      SpanishPrints:
      xor cx, cx
      mov cl, NumberLength

      SpanishPrintsCycle: ;***************************** START OF SPANISH CYCLE **********
      cmp cl, 0
      je EndProgramJump

      cmp cl, 4
      je ThousandsPrintsSpanish

      cmp cl, 3
      je HundredsPrintsSpanishJump

      cmp cl, 2
      je TensPrintsSpanishJump

      cmp cl, 1
      je UnitsPrintsSpanishJump

      jmp EndProgram

      HundredsPrintsSpanishJump:
      jmp HundredsPrintsSpanish

      TensPrintsSpanishJump:
      jmp TensPrintsSpanish

      UnitsPrintsSpanishJump:
      jmp UnitsPrintsSpanish

    ThousandsPrintsSpanish: ;-------------------THOUSANDS DECIMAL PRINTS
      xor ax, ax
      xor dx, dx
      call GetFourthDigitDx

      cmp dx, 1
      je milPrint

      cmp dx, 2
      je dosmilPrint

      cmp dx, 3
      je tresmilPrint

      cmp dx, 4
      je cuatromilPrint

      cmp dx, 5
      je cincomilPrint

      cmp dx, 6
      je seismilPrint

      cmp dx, 7
      je sietemilPrint

      cmp dx, 8
      je ochomilPrint

      cmp dx, 9
      je nuevemilPrint

      dec cx
      jmp SpanishPrintsCycle


      milPrint:
      mov ah, 09h
      lea dx, milD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      dosmilPrint:
      mov ah, 09h
      lea dx, dosmilD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      tresmilPrint:
      mov ah, 09h
      lea dx, tresmilD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      cuatromilPrint:
      mov ah, 09h
      lea dx, cuatromilD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      cincomilPrint:
      mov ah, 09h
      lea dx, cincomilD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      seismilPrint:
      mov ah, 09h
      lea dx, seismilD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      sietemilPrint:
      mov ah, 09h
      lea dx, sietemilD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      ochomilPrint:
      mov ah, 09h
      lea dx, ochomilD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      nuevemilPrint:
      mov ah, 09h
      lea dx, nuevemilD
      int 21h
      dec cx
      jmp SpanishPrintsCycle
      
      ;************************HUNDREDS SPANISH ********
    HundredsPrintsSpanish: ;-------------------HUNDREDS DECIMAL PRINTS
      xor ax, ax
      xor dx, dx

      ;call GetThirdDigitDx

      ;------------------
      mov ax, DecimalNumber
      mov bx, 1000
      div bx
      mov ax, dx ;Rnn
      mov bx, 100
      div bl
      xor ah, ah
      mov dx, ax
      ;------------------


      cmp dx, 1
      je cienDPrint

      cmp dx, 2
      je doscientosDPrint

      cmp dx, 3
      je trescientosDPrint

      cmp dx, 4
      je cuatrocientosDPrint

      cmp dx, 5
      je quinientosDPrint

      cmp dx, 6
      je seicientosDPrint

      cmp dx, 7
      je setecientosDPrint

      cmp dx, 8
      je ochocientosDPrint

      cmp dx, 9
      je novecientosDPrint

      dec cx
      jmp SpanishPrintsCycle

      cienDPrint:
      mov ah, 09h
      lea dx, cienD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      doscientosDPrint:
      mov ah, 09h
      lea dx, doscientosD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      trescientosDPrint:
      mov ah, 09h
      lea dx, trescientosD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      cuatrocientosDPrint:
      mov ah, 09h
      lea dx, cuatroscientosD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      quinientosDPrint:
      mov ah, 09h
      lea dx, quinientosD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      seicientosDPrint:
      mov ah, 09h
      lea dx, seiscientosD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      setecientosDPrint:
      mov ah, 09h
      lea dx, setecientosD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      ochocientosDPrint:
      mov ah, 09h
      lea dx, ochocientosD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      novecientosDPrint:
      mov ah, 09h
      lea dx, novecientosD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

    TensPrintsSpanish: ;-------------------TENS DECIMAL PRINTS
      xor ax, ax
      xor dx, dx

      ;call GetThirdDigitDx

      ;------------------
      mov ax, DecimalNumber
      mov bx, 100
      div bx
      mov ax, dx ;Rnn
      mov bx, 10
      div bl
      xor ah, ah
      mov dx, ax
      ;------------------
      
      cmp dx, 1
      je diezDPrint

      cmp dx, 2
      je veinteDPrint

      cmp dx, 3
      je treintaDPrint

      cmp dx, 4
      je cuarentaDPrint

      cmp dx, 5
      je cincuentaDPrint

      cmp dx, 6
      je sesentaDPrint

      cmp dx, 7
      je setentaDPrint

      cmp dx, 8
      je ochentaDPrint

      cmp dx, 9
      je noventaDPrint

      dec cx
      jmp SpanishPrintsCycle

      diezDPrint:
      mov ah, 09h
      lea dx, diezD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      veinteDPrint:
      mov ah, 09h
      lea dx, veinteD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      treintaDPrint:
      mov ah, 09h
      lea dx, treintaD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      cuarentaDPrint:
      mov ah, 09h
      lea dx, cuarentaD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      cincuentaDPrint:
      mov ah, 09h
      lea dx, cincuentaD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      sesentaDPrint:
      mov ah, 09h
      lea dx, sesentaD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      setentaDPrint:
      mov ah, 09h
      lea dx, setentaD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      ochentaDPrint:
      mov ah, 09h
      lea dx, ochentaD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      noventaDPrint:
      mov ah, 09h
      lea dx, noventaD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

    UnitsPrintsSpanish: ;-------------------TENS DECIMAL PRINTS
      xor ax, ax
      xor dx, dx

      ;call GetThirdDigitDx

      ;------------------
      mov ax, DecimalNumber
      mov bx, 10
      div bx
      mov ax, dx 
      mov bx, 1
      div bl
      xor ah, ah
      mov dx, ax
      ;------------------
      cmp dx, 1
      je UnoDPrint

      cmp dx, 2
      je DosDPrint

      cmp dx, 3
      je TresDPrint

      cmp dx, 4
      je CuatroDPrint

      cmp dx, 5
      je CincoDPrint

      cmp dx, 6
      je SeisDPrint

      cmp dx, 7
      je SieteDPrint

      cmp dx, 8
      je OchoDPrint

      cmp dx, 9
      je NueveDPrint

      dec cx
      jmp SpanishPrintsCycle

      UnoDPrint:
      mov ah, 09h
      lea dx, unoD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      DosDPrint:
      mov ah, 09h
      lea dx, dosD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      TresDPrint:
      mov ah, 09h
      lea dx, tresD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      CuatroDPrint:
      mov ah, 09h
      lea dx, cuatroD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      CincoDPrint:
      mov ah, 09h
      lea dx, cincoD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      SeisDPrint:
      mov ah, 09h
      lea dx, seisD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      SieteDPrint:
      mov ah, 09h
      lea dx, sieteD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      OchoDPrint:
      mov ah, 09h
      lea dx, ochoD
      int 21h
      dec cx
      jmp SpanishPrintsCycle

      NueveDPrint:
      mov ah, 09h
      lea dx, nueveD
      int 21h
      dec cx
      jmp SpanishPrintsCycle


    JapanesePrints:

      xor cx, cx
      mov cl, NumberLength

      JapanesePrintsCycle: ;***************************** START OF SPANISH CYCLE **********
      cmp cl, 0
      je EndProgramJumpJ

      cmp cl, 4
      je ThousandsPrintsJapanese

      cmp cl, 3
      je HundredsPrintsJapaneseJump

      cmp cl, 2
      je TensPrintsJapaneseJump

      cmp cl, 1
      je UnitsPrintsJapaneseJump

      EndProgramJumpJ:
      jmp EndProgram

;---------------
      HundredsPrintsJapaneseJump:
      jmp HundredsPrintsJapanese

      TensPrintsJapaneseJump:
      jmp TensPrintsJapanese

      UnitsPrintsJapaneseJump:
      jmp UnitsPrintsJapanese
;---------------


    ThousandsPrintsJapanese:
      xor ax, ax
      xor dx, dx
      call GetFourthDigitDx

      cmp dx, 1
      je milJPrint

      cmp dx, 2
      je dosmilJPrint

      cmp dx, 3
      je tresmilJPrint

      cmp dx, 4
      je cuatromilJPrint

      cmp dx, 5
      je cincomilJPrint

      cmp dx, 6
      je seismilJPrint

      cmp dx, 7
      je sietemilJPrint

      cmp dx, 8
      je ochomilJPrint

      cmp dx, 9
      je nuevemilJPrint

      dec cx
      jmp JapanesePrintsCycle

      milJPrint:
      mov ah, 09h
      lea dx, thousandJ
      int 21h
      dec cx
      jmp JapanesePrintsCycle

      dosmilJPrint:
      mov ah, 09h
      lea dx, twoJ
      int 21h
      jmp ThousandPrintJ

      tresmilJPrint:
      mov ah, 09h
      lea dx, threeJ
      int 21h
      jmp ThousandPrintJ

      cuatromilJPrint:
      mov ah, 09h
      lea dx, fourJ
      int 21h
      jmp ThousandPrintJ

      cincomilJPrint:
      mov ah, 09h
      lea dx, fiveJ
      int 21h
      jmp ThousandPrintJ

      seismilJPrint:
      mov ah, 09h
      lea dx, sixJ
      int 21h
      jmp ThousandPrintJ

      sietemilJPrint:
      mov ah, 09h
      lea dx, sevenJ
      int 21h
      jmp ThousandPrintJ

      ochomilJPrint:
      mov ah, 09h
      lea dx, eightJ
      int 21h
      jmp ThousandPrintJ

      nuevemilJPrint:
      mov ah, 09h
      lea dx, nineJ
      int 21h
      jmp ThousandPrintJ

      ThousandPrintJ:
      lea dx, thousandJ
      int 21h
      dec cx
      jmp JapanesePrintsCycle


    HundredsPrintsJapanese:
      xor ax, ax
      xor dx, dx

      

      ;------------------ hundred digit
      mov ax, DecimalNumber
      mov bx, 1000
      div bx
      mov ax, dx ;Rnn
      mov bx, 100
      div bl
      xor ah, ah
      mov dx, ax
      ;------------------
      cmp dx, 1
      je cienJPrint

      cmp dx, 2
      je doscientosJPrint

      cmp dx, 3
      je trescientosJPrint

      cmp dx, 4
      je cuatrocientosJPrint

      cmp dx, 5
      je quinientosJPrint

      cmp dx, 6
      je seiscientosJPrint

      cmp dx, 7
      je setecientosJPrint

      cmp dx, 8
      je ochocientosJPrint

      cmp dx, 9
      je novecientosJPrint

      dec cx
      jmp JapanesePrintsCycle


      cienJPrint:
      mov ah, 09h
      lea dx, hundredJ
      int 21h
      dec cx
      jmp JapanesePrintsCycle

      doscientosJPrint:
      mov ah, 09h
      lea dx, twoJ
      int 21h
      jmp hundredPrintJ

      trescientosJPrint:
      mov ah, 09h
      lea dx, threeJ
      int 21h
      jmp hundredPrintJ

      cuatrocientosJPrint:
      mov ah, 09h
      lea dx, fourJ
      int 21h
      jmp hundredPrintJ

      quinientosJPrint:
      mov ah, 09h
      lea dx, fiveJ
      int 21h
      jmp hundredPrintJ

      seiscientosJPrint:
      mov ah, 09h
      lea dx, sixJ
      int 21h
      jmp hundredPrintJ

      setecientosJPrint:
      mov ah, 09h
      lea dx, sevenJ
      int 21h
      jmp hundredPrintJ

      ochocientosJPrint:
      mov ah, 09h
      lea dx, eightJ
      int 21h
      jmp hundredPrintJ

      novecientosJPrint:
      mov ah, 09h
      lea dx, nineJ
      int 21h
      jmp hundredPrintJ

      hundredPrintJ:
      lea dx, hundredJ
      int 21h
      dec cx
      jmp JapanesePrintsCycle






    TensPrintsJapanese:
      xor ax, ax
      xor dx, dx

      ;call GetThirdDigitDx

      ;------------------
      mov ax, DecimalNumber
      mov bx, 100
      div bx
      mov ax, dx ;Rnn
      mov bx, 10
      div bl
      xor ah, ah
      mov dx, ax
      ;------------------
      cmp dx, 1
      je diezJPrint

      cmp dx, 2
      je veinteJPrint

      cmp dx, 3
      je treintaJPrint

      cmp dx, 4
      je cuarentaJPrint

      cmp dx, 5
      je cincuentaJPrint

      cmp dx, 6
      je sesentaJPrint

      cmp dx, 7
      je setentaJPrint

      cmp dx, 8
      je ochentaJPrint

      cmp dx, 9
      je noventaJPrint

      dec cx
      jmp JapanesePrintsCycle



      diezJPrint:
      mov ah, 09h
      lea dx, tenJ
      int 21h
      dec cx
      jmp JapanesePrintsCycle

      veinteJPrint:
      mov ah, 09h
      lea dx, twoJ
      int 21h
      jmp tenPrintJ

      treintaJPrint:
      mov ah, 09h
      lea dx, threeJ
      int 21h
      jmp tenPrintJ

      cuarentaJPrint:
      mov ah, 09h
      lea dx, fourJ
      int 21h
      jmp tenPrintJ

      cincuentaJPrint:
      mov ah, 09h
      lea dx, fiveJ
      int 21h
      jmp tenPrintJ

      sesentaJPrint:
      mov ah, 09h
      lea dx, sixJ
      int 21h
      jmp tenPrintJ

      setentaJPrint:
      mov ah, 09h
      lea dx, sevenJ
      int 21h
      jmp tenPrintJ

      ochentaJPrint:
      mov ah, 09h
      lea dx, eightJ
      int 21h
      jmp tenPrintJ

      noventaJPrint:
      mov ah, 09h
      lea dx, nineJ
      int 21h
      jmp tenPrintJ

      tenPrintJ:
      lea dx, tenJ
      int 21h
      dec cx
      jmp JapanesePrintsCycle


    UnitsPrintsJapanese:

      xor ax, ax
      xor dx, dx

      ;call GetThirdDigitDx

      ;------------------
      mov ax, DecimalNumber
      mov bx, 10
      div bx
      mov ax, dx 
      mov bx, 1
      div bl
      xor ah, ah
      mov dx, ax
      ;------------------

      cmp dx, 1
      je unoJPrint

      cmp dx, 2
      je dosJPrint

      cmp dx, 3
      je tresJPrint

      cmp dx, 4
      je cuatroJPrint

      cmp dx, 5
      je cincoJPrint

      cmp dx, 6
      je seisJPrint

      cmp dx, 7
      je sieteJPrint

      cmp dx, 8
      je ochoJPrint

      cmp dx, 9
      je nueveJPrint

      dec cx
      jmp JapanesePrintsCycle




      unoJPrint:
      mov ah, 09h
      lea dx, oneJ
      jmp tensPrint

      dosJPrint:
      mov ah, 09h
      lea dx, twoJ
      jmp tensPrint

      tresJPrint:
      mov ah, 09h
      lea dx, threeJ
      jmp tensPrint

      cuatroJPrint:
      mov ah, 09h
      lea dx, fourJ
      jmp tensPrint

      cincoJPrint:
      mov ah, 09h
      lea dx, fiveJ
      jmp tensPrint

      seisJPrint:
      mov ah, 09h
      lea dx, sixJ
      jmp tensPrint

      sieteJPrint:
      mov ah, 09h
      lea dx, sevenJ
      jmp tensPrint

      ochoJPrint:
      mov ah, 09h
      lea dx, eightJ
      jmp tensPrint

      nueveJPrint:
      mov ah, 09h
      lea dx, nineJ
      jmp tensPrint

      tensPrint:
      int 21h
      dec cx
      jmp JapanesePrintsCycle


      jmp EndProgram


      



EndProgram:
      mov ax, 4C00h
      int 21h 


codeS ends

end main
