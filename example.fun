fib = \n -> if eq n 0 then 0 else (if eq n 1 then 1 else (add (fib (min n 1)) (fib (min n 2))));

const = add 10 10;

main = (\x -> \y -> if x then (add const const) else mul y (add y y)) 17 Yes;

