simple = \x -> if eq x 0 then 42 else 43;

fib = \n -> if eq n 0 then 0 else (if eq n 1 then 1 else (add (fib (min n 1)) (fib (min n 2))));

const = add 10 10;

myadd = \x -> \y -> add x y;

diff = \x -> if lte x 0 then (\y -> min y x) else (\y -> min x y);

main2 = (\x -> \y -> if x then (add const const) else mul y (add y y)) 17 Yes;

main = diff 100 10;
