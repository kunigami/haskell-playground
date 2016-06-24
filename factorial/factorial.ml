let rec factorial n =
  if n == 0 then 1
  else n * (factorial (n -1));;

try
  while true do
    let line = input_line stdin in
    let n = int_of_string line in
    Printf.printf "%d\n" (factorial n)
  done;
  None
with
  End_of_file -> None
;;
