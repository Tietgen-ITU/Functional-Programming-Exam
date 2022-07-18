module Exam2022
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2022 = 
 *)

(* 1: Grayscale images *)

    type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale
    
    let img = 
      Quad (Square 255uy, 
            Square 128uy, 
            Quad(Square 255uy, 
                 Square 128uy, 
                 Square 192uy,
                 Square 64uy),
            Square 0uy)
    
(* Question 1.1 *)
    let rec countWhite = function
        | Square x when x = 255uy -> 1
        | Square _ -> 0
        | Quad (x, r ,z ,u) -> (countWhite x) + (countWhite r) + (countWhite z) + (countWhite u)
        
    
(* Question 1.2 *)
    let rec rotateRight = function
        | Square x -> Square x
        | Quad (x, r, z, u) -> Quad ((rotateRight u), (rotateRight x), (rotateRight r), (rotateRight z))

(* Question 1.3 *)
    let rec map f = function
        | Square x -> f x
        | Quad (x, y, z, u) -> Quad ((map f x), (map f y), (map f z), (map f u)) 
    
    let bitmap a = 
        let convertBlackOrWhite = function
            | x when x <= 127uy -> Square(0uy)
            | x -> Square(255uy)

        map convertBlackOrWhite a

(* Question 1.4 *)

    let rec fold folder acc = function 
        | Square x -> folder acc x
        | Quad (x, y, z, u) -> 
            fold folder acc x 
            |> fun accu -> fold folder accu y
            |> fun accu -> fold folder accu z
            |> fun accu -> fold folder accu u
    
    let countWhite2 a = 
        let white acc = function 
            | x when x = 255uy -> acc + 1
            | _ -> acc

        fold white 0 a

(* 2: Code Comprehension *)
    let rec foo = function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"

    let rec bar =
        function
        | []      -> []
        | x :: xs -> (foo x) :: (bar xs)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: 
        foo: int -> string
        bar: list<int> -> list<string> 


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: bar goes through one item at a time and converts it to a string by calling foo

    Q: What does the function foo do.

    A: foo applies the modulo operation in order to convert the int to a binary operation recursively

    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: 
        foo: intToBinaryString
        bar: listOfIntsToListOfBinaryStrings
        
    Q: The function foo does not return reasonable results for all possible inputs.
       What requirements must we have on the input to foo in order to get reasonable results?
    
    A: So the issue is if you input with a negtive integer to begin with. Because the case is when you say -25 % 2 then it will return -1.
        This is a case that we do not take into consideration. So if we want to use integers then we have to create a case for it or else 
        we could use uint32.
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo compiles with a warning. 

    
    Q: What warning and why?

    A: It compiles with a warning of incomplete pattern matching. The reason is that as described above we do not take every possible scenario into consideration.
       The specific scenario is for example when -25 % 2 = -1.

       What it wants us to do is to catch when no one of the above restrictions become true. 
       Since we do a modulo operation then by removing "when x % 2 = 1" then it should stop complaining.
    *)

    let rec foo2 = function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x -> foo (x / 2) + "1"

(* Question 2.3 *) 

    let bar2 xs = List.foldBack (fun x acc -> (foo2 x)::acc) xs [] 

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: foo is not tail recursive because it needs to wait for the recursive calls to be finished. 
        As shown below:

        foo 25
        -> (foo 12) + "1"
        -> (((foo 6) + "0") + "1")
        -> (((foo 3 + "0") + "0") + "1") 
        -> (((((foo 1) + "1") + "0") + "0") + "1") 
        -> (((((foo 0 + "1") + "1") + "0") + "0") + "1") 
        -> ((((("" + "1") + "1") + "0") + "0") + "1") 
        -> (((("1" + "1") + "0") + "0") + "1") 
        -> (((("11") + "0") + "0") + "1") 
        -> ((("110") + "0") + "1") 
        -> (("1100") + "1") 
        -> "11001" 
    
    Q: Even though neither `foo` nor `bar` is tail recursive only one of them runs the risk of overflowing the stack.
       Which one and why does  the other one not risk overflowing the stack?

    A: The one that risks overflowing the stack is bar. The amount of recursive calls is defined by the items of the list... which could be a lot.
        foo does not risk overflowing the stack because it devides the number that it goes through with 2. So it does not have to go through 
        as many recursive calls.

    *)
(* Question 2.5 *)

    let fooTail x = 
        let rec aux x acc = 
            match x with 
            | 0 -> acc
            | x when x % 2 = 0 -> aux (x / 2) ("0" + acc)
            | x -> aux (x / 2) ("1" + acc)

        aux x ""

(* Question 2.6 *)
    let barTail xs = 
        let rec aux x cont = 
            match x with 
            | []      -> cont []
            | y :: ys -> aux ys (fun r -> cont((fooTail y)::r))

        aux xs id

(* 3: Matrix operations *)

    type matrix = int[,]

    let init f rows cols = Array2D.init rows cols f

    let numRows (m : matrix) = Array2D.length1 m
    let numCols (m : matrix) = Array2D.length2 m

    let get (m : matrix) row col = m.[row, col]
    let set (m : matrix) row col v = m.[row, col] <- v

    let print (m : matrix) =
        for row in 0..numRows m - 1 do
            for col in 0..numCols m - 1 do
                printf "%d\t" (get m row col)
            printfn ""

(* Question 3.1 *)

    let failDimensions m1 m2 = 
        let (m1Row, m1Col) = (numRows m1, numCols m1)
        let (m2Row, m2Col) = (numRows m2, numCols m2)

        failwith (sprintf $"Invalid matrix dimensions: m1 rows = {m1Row}, m1 columns = {m1Col}, m2 roms = {m2Row}, m2 columns = {m2Col}")

(* Question 3.2 *)

    let add m1 m2 = 
        let m1RowAndCol = (numRows m1, numCols m1)
        let m2RowAndCol = (numRows m2, numCols m2)

        match m1RowAndCol, m2RowAndCol with 
        | (m1Row, m1Col), (m2Row, m2Col) when not(m1Row = m2Row) || not(m1Col = m2Col) -> failDimensions m1 m2
        | (m1Row, m1Col), _ -> init (fun row col -> (get m1 row col) + (get m2 row col)) m1Row m1Col 

(* Question 3.3 *)
    
    let m1 = (init (fun i j -> i * 3 + j + 1) 2 3) 
    let m2 = (init (fun j k -> j * 2 + k + 1) 3 2)

    let dotProduct m1' m2' row col = 
        [0.. (numCols m1')-1] 
        |> List.map (fun i -> (get m1' row i) * (get m2' i col)) 
        |> List.sum


    let mult m1 m2 = 
        let m1Col = numCols m1
        let m2Row = numRows m2
        let m2Col = numCols m2

        match m1Col, m2Row with 
        | (col, row) when not(col = row) -> failDimensions m1 m2
        | _, _ -> init (fun row col -> dotProduct m1 m2 row col) m2Col m2Col

(* Question 3.4 *)
    let parInit f row col = 
        let matrix = init (fun _ _ -> 0) row col

        let rec asyncComp r c = seq {
            match r, c with
            | 0, 0 ->
                yield async {
                    f 0 0 |> set matrix 0 0 |> ignore
                }
            | r, c when c = 0 ->
                yield! asyncComp (r-1) (col-1)
                yield async {
                    f r c |> set matrix r c |> ignore
                }
            | r, c -> 
                yield! asyncComp r (c-1)
                yield async {
                    f r c |> set matrix r c |> ignore
                }
        }

        asyncComp (row-1) (col-1)
        |> Async.Parallel
        |> Async.RunSynchronously 
        |> ignore ; matrix
        
(* 4: Stack machines *)

    type cmd = Push of int | Add | Mult
    type stackProgram = cmd list

(* Question 4.1 *)

    type stack = List<int> (* replace this entire type with your own *)
    let emptyStack () = List.empty<int>

(* Question 4.2 *)

    let runStackProg prog = 
        let pop = function
            | [] -> failwith "empty stack"
            | _::[] -> failwith "empty stack"
            | x::y::xs -> ((x,y), xs)
        
        let push x xs = x::xs

        let rec aux st = function
            | [] -> List.item 0 st
            | Push(x)::xs -> push x st |> fun st' -> aux st' xs
            | Add::xs -> pop st |> fun ((x,y), st') -> push (x+y) st' |> fun st' -> aux st' xs
            | Mult::xs -> pop st |> fun ((x,y), st') -> push (x*y) st' |> fun st' -> aux st' xs

        aux (emptyStack ()) prog

(* Question 4.3 *)
    
    type StateMonad<'a> = SM of (stack -> ('a * stack) option)

    let ret x = SM (fun s -> Some (x, s))
    let fail  = SM (fun _ -> None)
    let bind f (SM a) : StateMonad<'b> = 
        SM (fun s -> 
            match a s with 
            | Some (x, s') -> 
                let (SM g) = f x             
                g s'
            | None -> None)
        
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM (SM f) = f (emptyStack ())

    let push x = SM(fun s -> Some((), x::s))

    let pop = 
        SM(fun s -> 
            match s with 
            | [] -> None
            | x::xs -> Some(x, xs))

(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let binop f a b = a >>= (fun x -> b >>= fun y -> ret (f x y))

    let runStackProg2 stProg = 

        let rec aux sm stp =
            match stp with
            | [] -> sm
            | Push(x)::xs -> aux (sm >>>= push x) xs
            | Add::xs -> aux (sm >>>= (binop (+) pop pop) >>= push) xs
            | Mult::xs -> aux (sm >>>= (binop (*) pop pop) >>= push) xs

        (aux (SM(fun x -> Some((),x))) stProg) >>>= pop
    
(* Question 4.5 *)
    
    open JParsec.TextParser

    let spaces = many (pchar ' ')
    let newline = pstring System.Environment.NewLine
    

    let parseStackProg = 
        let parseAdd = pstring "ADD" |>> (fun a -> Add)
        let parsePush = pstring "PUSH" >>. between spaces spaces pint32 |>> (fun a -> Push a)
        let parseMult = pstring "MULT" >>. between spaces spaces pint32 |>> (fun a -> Push a)

        let parseCmd = choice (Seq.ofList [parsePush; parseAdd; parseMult]) 
        let parseCmdLine = spaces >>. parseCmd .>> newline

        parseCmdLine
        