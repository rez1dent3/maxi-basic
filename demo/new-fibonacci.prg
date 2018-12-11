define m : integer;
define i : integer;
define s : word[3];

begin
 s[1] = 0
 s[2] = 1
 s[3] = 0
 m = con
 i = 1
 if m <= 1 then
  printi(1)
 else begin
  while i < m do begin
   s[3] = s[1] + s[2]
   s[1] = s[2]
   s[2] = s[3]
   i = i + 1
  end
  printw(s[3])
 end
END.
