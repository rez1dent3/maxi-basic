uses crt
	, SysUtils;
	
{
	
	@Developer: Babichev Maxim
	@KSU, year:	2013 - 2014
	
	@Program:	MaxiBasic
	@Version:	0.5
	
}

Const
	Shift = $100;
	DetermineTheTypesOf : packed 
		array[1..4] of 
			packed record
				Name : String[20];
				Size : Byte;
				Arithmetic : boolean;
			end = (
				(Name : 'INTEGER'; 	Size: 2; Arithmetic: TRUE),
				(Name : 'WORD'; 	Size: 2; Arithmetic: TRUE),
				(Name : 'BYTE'; 	Size: 1; Arithmetic: TRUE),
				(Name : 'CHAR'; 	Size: 1; Arithmetic: FALSE)
			);
	
	Digits: set of Char = ['0'..'9'];
	Liters: set of Char = ['A'..'Z'];
	Dividers: set of Char = ['+', '-', '*', '/', ':', '.', ';', '=', '[', ']', '(', ')', '<', '>', '"'];
	
Type
	TLexer = object

				procedure TWrite(Bt : byte);
				procedure TWrite(Bt1, Bt2 : byte);
				procedure TWrite(Bt1, Bt2, Bt3 : byte);
				procedure TWrite(Bt1, Bt2, Bt3, Bt4 : byte);
				
				procedure CheckForErrors(Code : Byte; Msg : String);
				procedure Prog;
				procedure Stmt;
				procedure Opers;
				procedure XPRS;
				procedure CompilationExpressions1;
				procedure CompilationExpressions2;
				procedure Vars;
				procedure OutInt;
				procedure ReadInt;
				procedure ToStack(Param : Word);
				procedure MakeCompare;
			
				function FromStack : Word;
				function NextWord : String;
				function InId(Name : String) : Word;

		end;
		
var
	InpStr : packed array[0..4096] of Char;
	OutStr : packed array[0..4096] of Byte;
	InpF, OutF : File;
	OutPtr : Word = 0;
	Lexer : TLexer;
	InpSize, InpPtr : Word;
	VarTabl : packed record
		Field : packed array[0..1000] of packed record
			Name : String[20];
			Tip : Byte;
			Size : Word;
			Adr : Word;
		end;
		FieldNum : Word;
	end;
	Address : Word = 0;
	Stack : packed array[0..65534] of Word;
	StackPtr : Word = 0;
	DataBase : Word = 0;
	TmpInt : Integer = 0;
	ReadAdr : Word = 0;
	StrNum : Word = 1;
	CurseWord : String = '';

procedure TLexer.MakeCompare;
var First, Second : Char;
begin
	XPRS;
	TWrite($50); 
    if CurseWord[1] in ['<', '>', '='] then 
		first := CurseWord[1];
		
    CurseWord := NextWord;
    
    Second := ' ';
    
    if CurseWord[1] in ['<','>','='] then begin
		Second := CurseWord[1];
		CurseWord := NextWord;
	end;
	
    XPRS;
    TWrite($5B);
    TWrite($93);
    TWrite($3B, $C3);
    Case First of
		'=':TWrite($74, $03);
		'>':Case Second of
			' ':TWrite($7F, $03);
			'=':TWrite($7D, $03);
		end;
		'<':case Second of
			' ':TWrite($7C, $03);
			'=':TWrite($7E, $03);
			'>':TWrite($75, $03);
		end;
	end;
end;

procedure TLexer.Prog;
var retpoint : Word;
begin
	CurseWord := NextWord;
	if CurseWord = 'DEFINE' then begin
		Vars;
		DataBase := $FFFE - Address;
		TWrite($BC, Lo(DataBase), Hi(DataBase));
		TWrite($E9, $00, $00);
		retpoint := OutPtr;
		OutInt;
		ReadAdr := OutPtr;
		ReadInt;
		OutStr[retpoint - 2] := Lo(OutPtr - RetPoint);
		OutStr[retpoint - 1] := Hi(OutPtr - RetPoint);

		if CurseWord = 'BEGIN' then begin
			Stmt;
			if CurseWord <> 'END' then 
				CheckForErrors(3, 'end');
				
			if NextWord <> '.' then 
				CheckForErrors(3, '.');
		end else CheckForErrors(3, ' begin');
		
	end else CheckForErrors(3, ' define');

   TWrite($90);
   TWrite($B8, $00, $4C);
   TWrite($CD, $21);
end;

procedure TLexer.Stmt;
begin
  CurseWord := NextWord;
  While CurseWord <> 'END' do
	OPERS;
end;

procedure TLexer.Opers;
var NewName : String;
    Tmp,NewNum : Word;
    StrBeg : Word;
    IfPtr, ElsePtr, WhilePtr1, WhilePtr2 : Word;
    ToEnd, ToBegin : Word;
begin
	if CurseWord = 'PRINTI' then begin
		CurseWord := NextWord;
		XPRS;
		TWrite($B6, $01);
		TmpInt := -(OutPtr - 3);
		TWrite($E8, Lo(TmpInt), Hi(TmpInt));
	end else if CurseWord = 'PRINTW' then begin
		CurseWord := NextWord;
		XPRS;     
		TWrite($B6, $00);
		TmpInt := -(OutPtr - 3);
		TWrite($E8, Lo(TmpInt), Hi(TmpInt));
	end else if CurseWord = 'PRINTS' then begin 
	
		If NextWord <> '(' then 
			CheckForErrors(2, '(');
			
		If NextWord <> '"' then 
			CheckForErrors(2,'"');
			
		Inc(OutPtr,2);
		StrBeg := OutPtr;
		 
		while InpStr[InpPtr] <> '"' do begin 
			TWrite(Ord(InpStr[InpPtr]));
			Inc(InpPtr)
		end;
		
		inc(InpPtr);
		CurseWord := NextWord;
		
		if CurseWord = 'L' then begin
			CurseWord := NextWord;
			TWrite(13, 10);
		end;
		
		TWrite(Ord('$'));
		
		if CurseWord <> ')' then 
			CheckForErrors(2, ')'); 
		
		OutStr[StrBeg - 2] := $EB;             
		OutStr[StrBeg - 1]:= OutPtr - StrBeg;
		TWrite($BA, Lo(StrBeg + shift), Hi(StrBeg + shift));
		TWrite($B8, $00, $09); 
		TWrite($CD, $21);
		CurseWord := NextWord;
	end else if CurseWord = 'BEGIN' then begin
		CurseWord := NextWord;
		while CurseWord <> 'END' do
			Opers; 
		CurseWord := NextWord;
	end else if CurseWord = 'IF' then begin
		CurseWord := NextWord;
		MakeCompare; 
		TWrite($E9, 0, 0);
		IfPtr := OutPtr;
		
		If CurseWord <> 'THEN' then 
			CheckForErrors(3, 'then');
			
		CurseWord := NextWord;
		Opers; 
		
		if CurseWord = 'ELSE' then begin
			TWrite($E9, 0, 0);
			ElsePtr := OutPtr;
		end else ElsePtr := 0;
		
		OutStr[IfPtr - 2] := Lo(OutPtr - IfPtr);
		OutStr[IfPtr - 1] := Hi(OutPtr - IfPtr);
		
		if ElsePtr <> 0 then begin
			CurseWord := NextWord;
			Opers; 
			OutStr[ElsePtr - 2] := Lo(OutPtr - ElsePtr);
			OutStr[ElsePtr - 1] := Hi(OutPtr - ElsePtr);
		end;
		 
	end else if CurseWord = 'WHILE' then begin
		CurseWord := NextWord;
		WhilePtr1 := OutPtr;
		MakeCompare; 
		TWrite($E9, 0, 0);
		WhilePtr2 := OutPtr;
		
		If CurseWord <> 'DO' then 
			CheckForErrors(3,'do');
    
		CurseWord := NextWord;
		
		Opers; 
		TmpInt := -(OutPtr - WhilePtr1 + 3);
		TWrite($E9, Lo(TmpInt), Hi(TmpInt));
		OutStr[WhilePtr2 - 2] := Lo(OutPtr - WhilePtr2);
		OutStr[WhilePtr2 - 1] := Hi(OutPtr - WhilePtr2);
   end else if CurseWord = 'FOR' then begin
		CurseWord := NextWord;
		NewName := CurseWord;
		NewNum := InId(CurseWord); 
		
		if NewNum = 0 then 
			CheckForErrors(8, CurseWord);
				
		if VarTabl.Field[NewNum].Tip <> 1 then 
			CheckForErrors(9, 'integer');
			
		if NextWord <> '=' then 
			CheckForErrors(2, '=');
			
		CurseWord := NextWord;
		XPRS; 
		Tmp := VarTabl.Field[NewNum].Adr;
		TWrite($A3, Lo(DataBase + Tmp), Hi(DataBase + Tmp)); 
		
		if CurseWord <> 'TO' then 
			CheckForErrors(3,'to');
			
		ToBegin := OutPtr;
		CurseWord := NextWord;
		XPRS; 
		TWrite($3B, $06, Lo(DataBase + Tmp), Hi(DataBase + Tmp));
		TWrite($7D, $03);
		TWrite($E9,0,0);
		ToEnd := OutPtr;
		
		if CurseWord <> 'DO' then 
			CheckForErrors(3,'do'); 
			
		CurseWord := NextWord;
		Opers; 
		TWrite($FF, $06, Lo(DataBase + Tmp), Hi(DataBase + Tmp)); 
		TmpInt := -(OutPtr - ToBegin + 3);
		TWrite($E9, Lo(TmpInt), Hi(TmpInt));
		OutStr[ToEnd - 2] := Lo(OutPtr - ToEnd);
		OutStr[ToEnd - 1] := Hi(OutPtr - ToEnd);
	end else if InId(CurseWord) <> 0 then begin 
		NewName := CurseWord;
		with DetermineTheTypesOf[VarTabl.Field[InId(NewName)].Tip] do begin
			CurseWord := NextWord;
			if CurseWord = '[' then begin
				CurseWord := NextWord;
				XPRS;
				TWrite($2D, $01, $00);
				if Size = 2 then 
					TWrite($D1, $E0);  
					
				if CurseWord <> ']' then 
					CheckForErrors(2, ']');
					
				CurseWord := NextWord;
			end else TWrite($2B, $C0);
			
			if CurseWord <> '=' then 
				CheckForErrors(2, '=');
				
			CurseWord := NextWord;
			TWrite($50);  

			if not Arithmetic then 
				if CurseWord <> '"' then 
					CheckForErrors(2, '"'); 
					
			XPRS;
			
			TWrite($5B); 
			Tmp := VarTabl.Field[InId(NewName)].Adr;
			
			if Size = 1 then
				TWrite($88, $87, Lo(DataBase + Tmp), Hi(DataBase + Tmp))
			else
				TWrite($89, $87, Lo(DataBase + Tmp), Hi(DataBase + Tmp));
				
		end;
   end else
		CheckForErrors(7, CurseWord);
		
end;

procedure TLexer.XPRS;
begin
	CompilationExpressions2;
	while CurseWord[1] in ['+', '-'] do begin
		TWrite($50);	
		if not (CurseWord[1] in ['+','-']) then
			CheckForErrors(2,'"+" или "-"');
		ToStack(Ord(CurseWord[1]));
		CurseWord := NextWord;
		CompilationExpressions2;
		TWrite($5B); 
		case Chr(Lo(FromStack)) of
			'+': TWrite($03,$C3);
			'-': TWrite($93,$2B,$C3);
		end;
	end;
end;

procedure TLexer.CompilationExpressions1;
var
	Code, I, TmpInt : Integer;
	Tmp : Word;
	NewName : String;
	
begin
	if CurseWord = 'CON' then begin
		TmpInt := -(OutPtr - ReadAdr + 3);
		TWrite($E8, Lo(TmpInt), Hi(TmpInt));
		CurseWord := NextWord;
	end else if CurseWord[1] = '"' then begin
		CurseWord := NextWord;
		TWrite($2A, $E4, $B0, Ord(InpStr[InpPtr - 1]));
		
		if CurseWord <> '"' then 
			CheckForErrors(2, '"');
		
		CurseWord := NextWord;
	end else if (CurseWord[1] in ['0'..'9']) then begin
		Val(CurseWord, I, Code);
		
		if Code <> 0 then 
			CheckForErrors(6, 'числа');
			
		TWrite($B8, Lo(I), Hi(I));
		CurseWord := NextWord;
	end else if CurseWord = '(' then begin
		CurseWord := NextWord;
		XPRS;
		
		if CurseWord <> ')' then
			CheckForErrors(2, ')');
			
		CurseWord := NextWord;
	end else if InId(CurseWord) <> 0 then begin
		NewName := CurseWord;
		CurseWord := NextWord;
		
		if CurseWord = '[' then begin
			TWrite($50);
			CurseWord := NextWord;
			XPRS;
			TWrite($2D, $01, $00);
			
			if DetermineTheTypesOf[VarTabl.Field[InId(NewName)].Tip].Size = 2 then
				TWrite($D1,$E0);    
				
			TWrite($8B,$D8);             
			TWrite($58);                          
			
			if CurseWord <> ']' then 
				CheckForErrors(2, ']');
			
			CurseWord:=NextWord;
		end else TWrite($2B,$DB);      
		
		Tmp:=VarTabl.Field[InId(NewName)].Adr;
		
		if DetermineTheTypesOf[VarTabl.Field[InId(NewName)].Tip].Size = 1 then begin
			TWrite($8A, $87, Lo(DataBase + Tmp), Hi(DataBase + Tmp));
			TWrite($B4, $00);                              
       end else TWrite($8B, $87, Lo(DataBase + Tmp), Hi(DataBase + Tmp));
	end else 
		CheckForErrors(8, CurseWord);
	
end;

function TLexer.InId(Name : String) : Word;
var i : Word;
begin
	InId:=0;
	for i := 1 to VarTabl.FieldNum do
		if VarTabl.Field[i].Name = Name then 
			InId := i;
end;

function TLexer.NextWord : String;
var tmp:string = '';
begin

	while (InpStr[InpPtr] in [#10, #13, ' ', '}', '{']) do begin
	
		while (InpStr[InpPtr] in [#10, #13, ' ', '}']) do begin
			if InpStr[InpPtr] = #10 then 
				Inc(StrNum);
			inc (InpPtr);
		end;
		
		if InpStr[InpPtr]='{' then
			while InpStr[InpPtr] <> '}' do begin
				if InpStr[InpPtr] = #10 then 
					Inc(StrNum);
				inc(InpPtr);
			end;
	end;
	
	if InpStr[InpPtr] in Digits then
		while InpStr[InpPtr] in Digits do begin
			tmp += InpStr[InpPtr];
			inc(InpPtr);
		end
	else if (InpStr[InpPtr] in Dividers) then begin
		tmp := InpStr[InpPtr];
		inc(InpPtr);
	end else if UpCase(InpStr[InpPtr]) in Liters then
		while UpCase(InpStr[InpPtr]) in Digits + Liters do begin
			tmp += UpCase(InpStr[InpPtr]);
			inc(InpPtr);
		end
	else 
		CheckForErrors(10, InpStr[InpPtr]);
		
	NextWord := tmp;
		
end;

procedure TLexer.CompilationExpressions2;
begin
	CompilationExpressions1;
	
	While CurseWord[1] in ['*', '/'] do begin
		TWrite($50);
		if not (CurseWord[1] in ['*', '/']) then
			CheckForErrors(2, '"*" или "/"');

		ToStack(ord(CurseWord[1]));

		CompilationExpressions1;

		TWrite($5B);
		case chr(Lo(FromStack)) of
			'*' : TWrite($F7,$E3);	
			'/' : TWrite($93,$F6,$F3);
		end;
	end;
end;

procedure TLexer.ToStack(Param : Word);
begin
	Stack[StackPtr] := Param;
	Inc(StackPtr);
end;
				
function TLexer.FromStack : Word;
begin
	Dec(StackPtr);
	FromStack := Stack[StackPtr];
end;

procedure TLexer.TWrite(Bt : byte);
begin
	OutStr[OutPtr] := Bt;
	Inc(OutPtr);
end;

procedure TLexer.TWrite(Bt1, Bt2 : byte);
begin
	OutStr[OutPtr] := Bt1;
	OutStr[OutPtr + 1] := Bt2;
	Inc(OutPtr, 2);
end;

procedure TLexer.TWrite(Bt1, Bt2, Bt3 : byte);
begin
	OutStr[OutPtr] := Bt1;
	OutStr[OutPtr + 1] := Bt2;
	OutStr[OutPtr + 2] := Bt3;
	Inc(OutPtr, 3);
end;

procedure TLexer.TWrite(Bt1, Bt2, Bt3, Bt4:byte);
begin
	OutStr[OutPtr] := Bt1;
	OutStr[OutPtr + 1]:=Bt2;
	OutStr[OutPtr + 2] := Bt3;
	OutStr[OutPtr + 3] := Bt4;
	Inc(OutPtr, 4);
end;

procedure TLexer.ReadInt;
var BufAdr : Word;
begin
	TWrite($EB, $0A);
	
	BufAdr := OutPtr + shift;
	
	TWrite($07, $2E, $2E); 
	TWrite($2E, $2E, $2E, $2E);
	TWrite($2E, $2E, $90);

	TWrite($BA, Lo(BufAdr), Hi(BufAdr)); 
	TWrite($B0, $0A);
	TWrite($B4, $0C);
	TWrite($CD, $21);
	
	TWrite($B8, 0, 0);
	TWrite($B9, $0A, $00);
	TWrite($BB, $02, $00);
	TWrite($8A, $97, Lo(BufAdr), Hi(BufAdr));
	TWrite($80, $FA, $2D);
	TWrite($75, $01);
	TWrite($43);
	TWrite($80, $BF, Lo(BufAdr), Hi(BufAdr));
	TWrite($0D);
	TWrite($74, $10);
	TWrite($F7, $E1);
	TWrite($8A, $97, Lo(BufAdr), Hi(BufAdr));
	TWrite($80, $F2, $30);
	TWrite($B6, $00);
	TWrite($03, $C2);
	TWrite($43);
	TWrite($EB, $E9);
	TWrite($80, $3E, Lo(BufAdr + 2), Hi(BufAdr + 2));
	TWrite($2D);
	TWrite($75, $02);
	TWrite($F7, $D8);
	TWrite($C3);
end;

procedure TLexer.OutInt;
begin
	TWrite($50);
	TWrite($BF, $41, $01);
	TWrite($B9, $06, $00);
	TWrite($B0, $20);
	TWrite($F3, $AA);
	TWrite($58);
	TWrite($BF, $46, $01);
	TWrite($B9, $0A, $00);
	TWrite($B3, $20);
	TWrite($80, $FE, $00);
	TWrite($74, $09);
	TWrite($3D, $00, $00);
	TWrite($79, $04);
	TWrite($B3, $2D);
	TWrite($F7, $D8);
	TWrite($BA, $00, $00);
	TWrite($F7, $F1);
	TWrite($80, $CA, $30);
	TWrite($88, $15);
	TWrite($4F);
	TWrite($3C, $00);
	TWrite($75, $F1);
	TWrite($88, $1D);
	TWrite($BA, $41, $01);
	TWrite($B4, $09);
	TWrite($CD, $21);
	TWrite($C3);
	TWrite($2B, $2B, $2B);
	TWrite($2B, $2B, $2B, $24);
	TWrite($00);
end;

procedure TLexer.Vars;
var	I : Integer;
	NewName : String;
	NewNum : Byte;
	Code, NewSize : Integer;
	
begin
	CurseWord := NextWord;
	
	if InId(CurseWord) <> 0 then 
		CheckForErrors(1, CurseWord)
	else begin
		NewName := CurseWord;
		
		if NextWord <> ':' then 
			CheckForErrors(2, ':');
			
		CurseWord := NextWord;
		NewNum := 0;
		
		for i:=1 to 4 do
			if CurseWord = DetermineTheTypesOf[i].Name then 
				NewNum := i; 
			
		if NewNum = 0 then 
			CheckForErrors(4, 'тип');
		
		CurseWord := NextWord;
    
		if CurseWord <> ';' then
		
			if CurseWord <> '[' then 
				CheckForErrors(2, '[') 
			else begin
				CurseWord := NextWord;
				Val(CurseWord, NewSize, Code);
				
				if (Code <> 0) or (NewSize < 1) then 
					CheckForErrors(5, 'натуральное число');
					
				If NextWord <> ']' then 
					CheckForErrors(2, ']');
					
				If NextWord <> ';' then 
					CheckForErrors(2, ';');
					
			end else NewSize := 1;
			
		with VarTabl do begin
			Inc(FieldNum); 
			Field[FieldNum].Name := NewName; 
			Field[FieldNum].Tip := NewNum; 
			Field[FieldNum].Size := NewSize;
			Field[FieldNum].Adr := Address;
			Inc(Address, Field[FieldNum].Size * DetermineTheTypesOf[Field[FieldNum].Tip].Size);
		end;
		
	end;
	
	CurseWord := NextWord; 
	if CurseWord = 'DEFINE' then 
		Vars;
		
end;

procedure TLexer.CheckForErrors(Code : Byte; Msg : String);
begin
	
	Case Code Of
		1:	Write('[1] Повторное определение переменной ', Msg);
		2:	Write('[2] Ожидается символ "', Msg, '"');
		3:	Write('[3] Ожидается слово ', Msg);
		4:	Write('[4] Неправильно указан ', Msg);
		5:	Write('[5] Ожидается ', Msg);
		6:	Write('[6] Неверный формат ', Msg);
		7:	Write('[7] Неверный тип операнда');
		8:	Write('[8] Неизвестный идентификатор ', Msg);
		9:	Write('[9] Ожидается переменная типа ', Msg);
		10:	Write('[10] Неизвестный символ: "', Msg, '"');
	end;
	
	WriteLn(', ( cтрока: ', StrNum, ' )');
	WriteLn('последнее считанное слово: ', CurseWord);
	Halt(1);
	
end;

begin
	writeln('--------------------------------------');
	writeln(' @Author: Babichev Maxim (REZ1DENT3)');
	writeln(' @Compiler: MaxiBasic, version: 0.5');
	writeln('--------------------------------------');
	
	if ParamCount < 1 then begin
		WriteLn('[fne] Передайте файл компилятору');
		Halt(1);
	end;
	
	if (not FileExists(ParamStr(1))) then begin
		WriteLn('[fn] Файла не существует');
		Halt(1);
	end;
	
	VarTabl.FieldNum := 0;
	
	Assign(InpF, ParamStr(1));
	Reset(InpF, 1);
	BlockRead(InpF, InpStr, SizeOf(InpStr), InpSize);
	Close(InpF);
	Lexer.Prog;
	
	writeln('[c] Компиляция успешно завершена');
	
	Assign(OutF, copy(ParamStr(1), 0, pos('.', ParamStr(1)) - 1) + '.com');
	Rewrite(OutF, 1);
	BlockWrite(OutF, OutStr, OutPtr); 
	Close(OutF);
 
end.