open System
 
type vec = int*int
type color = System.ConsoleColor
type symbol = char*color*color
 
let o = (0,0)
let dn : vec = (0,1)
let up : vec = (0,-1)
let rt : vec = (1,0)
let lt : vec = (-1,0)
 
//Scale a vector A by a length B
let (+*) (A:vec) (B:int) : vec =
    (fst A * B, snd A * B)
 
//Add two vectors A & B
let (++) (A:vec) (B:vec) : vec =
    (fst A + fst B, snd A + snd B)
 
//Subtract two vectors A & B
let (+-) (A:vec) (B:vec) : vec =
    (fst A - fst B, snd A - snd B)
 
//Get length of a vector
let length (A:vec) : float =
    sqrt (float(fst A)**2. + float(snd A)**2.)
 
//Get vector in same direction but with length 1
let normalize (A:vec) : vec =
    let len = length A
    (fst A / int(len), snd A / int(len))
 
let defaultSym = ('.', color.Black, color.DarkGreen)
 
[<AbstractClassAttribute>]
type Entity (pos : vec, look : (char*color)) =
    let mutable pos : vec = pos
    let mutable look : (char*color) = look
    let mutable solid = true
 
    member val Pos = pos with get, set
    member val Look = look with get, set
    member val Solid = solid with get, set
 
    member this.Move (dir : vec) = 
        pos <- pos ++ dir
 
    abstract member TakeTurn : unit -> unit
 
type Canvas(width:int, height:int) = 
    let width = width;
    let height = height;
    let mutable chars : symbol array = Array.init (width*height) (fun x -> defaultSym)
    member this.Set (point : vec, s : symbol) : unit = 
        chars.[(snd point) * width + (fst point)] <- s
 
    member this.SetEntity (ent : Entity) : unit = 
        let i = (snd ent.Pos) * width + (fst ent.Pos)
 
        let (_,_,bgCol) = chars.[i]
 
        chars.[i] <- (fst ent.Look, snd ent.Look, bgCol)
 
 
    member this.Show () : unit = 
        Console.Clear()
        for i in [0..(chars.Length-1)] do
            let (c, f, b) = chars.[i]
            Console.BackgroundColor <- b;
            Console.ForegroundColor <- f;
            Console.Write(c.ToString() + " ")
            if ((i+1) % (width) = 0) then
                Console.ResetColor()
                Console.Write("\n")
            else ()
 
 
 
 
let c = new Canvas (10,10)
// let snabelA = ('@', color.Red, color.DarkBlue)
// c.Set((4,4), snabelA)
// c.Show()
 
 
type Player (pos : vec, look : (char*color)) =
 
    inherit Entity (pos, look)
 
    member this.KeyMove () = 
        let k = Console.ReadKey(true)
        match k.Key with
            | ConsoleKey.RightArrow -> this.Pos <- this.Pos ++ rt
            | ConsoleKey.UpArrow -> this.Pos <- this.Pos ++ up
            | ConsoleKey.DownArrow -> this.Pos <- this.Pos ++ dn
            | ConsoleKey.LeftArrow -> this.Pos <- this.Pos ++ lt
            | _ -> this.KeyMove()
 
    override this.TakeTurn () =
        this.KeyMove()
        ()
 
 
// type Item (pos : vec, look : (char*color)) =
    //inherit Entity (pos, look)
 
 
let p1 = new Player(o, ('1', color.Red))
 
 
let entities : Entity list = [p1]
 
let gameRunning : bool = true;
 
while (gameRunning) do
 
 
 
    //Draw background
 
 
 
    //Draw entities
 
    for i in [0..(entities.Length-1)] do
 
        let ent = entities.[i]
 
        c.SetEntity(ent)
 
    c.Show()
 
    p1.TakeTurn()