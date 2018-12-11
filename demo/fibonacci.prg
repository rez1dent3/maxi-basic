define m : integer;
define i : integer;
define s : integer;
define a : integer;
define b : integer;

begin
 a = 0
 b = 1
 s = 0
 m = 8
 i = 2
 while i < m do begin
  s = a + b
  a = b
  b = s
  i = i + 1
 end
 printi(s)
END.
