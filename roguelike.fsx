open System
 
type vec = int * int
type color = System.ConsoleColor
type symbol = string * color * color option
 
let o = (0, 0)
let dn: vec = (0, 1)
let up: vec = (0, -1)
let rt: vec = (1, 0)
let lt: vec = (-1, 0)
 
///<summary>Returns a pseudo-random number.</summary>
///<param name ="max">The maximum value to be returned</param>
///<returns>A "random" int from [0..max]</returns>
let randN (min: int) (max: int): int =
    let rnd = System.Random()
    rnd.Next(max + min) - min
///<summary>Creates a new random vector</summary>
    ///<param name="()">A unit for running the method</param>
    ///<returns>Returns a randomized vector using the randN function</returns>
let randomVec (): vec = (randN -1 1, randN -1 1)
 
//Scale a vector A by a length B
let (+*) (A: vec) (B: int): vec = (fst A * B, snd A * B)
 
//Add two vectors A & B
let (++) (A: vec) (B: vec): vec = (fst A + fst B, snd A + snd B)
 
//Subtract two vectors A & B
let (+-) (A: vec) (B: vec): vec = (fst A - fst B, snd A - snd B)
 
//Get length of a vector
let length (A: vec): float =
    sqrt (float (fst A) ** 2. + float (snd A) ** 2.)
 
//Get vector in same direction but with length 1
let normalize (A: vec): vec =
 
    let xs = if fst A <> 0 then sign (fst A) else 0
    let ys = if snd A <> 0 then sign (snd A) else 0
    (xs, ys)
 
let defaultSym1 = ("░░", color.DarkGray, Some color.Black)
let defaultSym2 = ("  ", color.DarkGray, Some color.Black)
 
///<summary>Creates a string which is assigned int duplicaitons of character</summary>
///<param name="c: char, amount:int">C defines which character to iterate over and amount is number of loops</param>
///<returns>A string with the number of characters repeated</returns>

let repeatChar (c: char) (amount: int): string =
    let mutable s: string = ""
    for i in [ 1 .. amount ] do
        s <- s + c.ToString()
    s
///<summary>Repeats the string a select number of times, and assigns it to a new string</summary>
///<param name="c: char, amount:int">C defines which character to iterate over and amount is number of loops</param>
///<returns>Returns a new string with the given string multipled</returns>
let repeatString (c: string) (amount: int): string =
    let mutable s: string = ""
    for i in [ 1 .. amount ] do
        s <- s + c
    s

///<summary>Runs through a whole list of integers and returns the highest number</summary>
///<param name="xs">A given list</param>
///<returns>Returns the highest integer in the list</returns>
let rec maxOfList xs =
        match xs with
        | [] -> invalidArg "xs" "Empty list"
        | [x] -> x
        | x1::x2::xs' -> maxOfList((max x1 x2)::xs')
 
let inline fst3 (a,_,_) = a
let inline snd3 (_,b,_) = b
let inline third3 (_,_,c) = c
 
 
type Canvas(width: int, height: int) =
 
    member val Chars: symbol array = Array.init (width * height) (fun _ -> defaultSym1) with get, set
    member this.Width = width
    member this.Height = height
 
    ///<summary>Sets a string of length 2 and a char color onto the canvas</summary>
    ///<param name="point: vec, s:symbol"></param>

    member this.Set(point: vec, s: symbol): unit =
        if ((fst point) < 0
            || (fst point) > width - 1
            || (snd point) < 0
            || (snd point) > height - 1) then
            ()
        else
            this.Chars.[(snd point) * width + (fst point)] <- s
    
    ///<summary>Renders the pixels onto the canvas </summary>
    member this.Show(): unit =
        Console.Clear()
        for i in [ 0 .. (this.Chars.Length - 1) ] do
            Console.ResetColor()
            let (c, f, b) = this.Chars.[i]
            Console.ForegroundColor <- f
            match b with | None -> () | Some c -> Console.BackgroundColor <- c
            Console.Write(c.[0..1])
            if ((i + 1) % (width) = 0) then
                Console.ResetColor()
                Console.Write("\n")
            else
                ()
    ///<summary>Fills the canvas with default background grid</summary>
    member this.Reset(): unit =
        // this.Chars <- Array.init (this.Width*this.Height) (fun _ -> defaultSym1) //WHY DOESN'T THIS WORK??
 
        for i in [ 0 .. this.Chars.Length - 1 ] do
            let (x, y) = (i % this.Width, i / this.Width)
            this.Chars.[i] <- if x % 2 = y % 2 then defaultSym1 else defaultSym2
 
[<AbstractClassAttribute>]
type Entity(pos: vec, look: symbol, solid: bool) =
 
    //will be true when this object is to be taken out of simulation
    member val isDeleted = false with get, set
    member this.Solid: bool = solid
    member val Pos = pos with get, set
    member val Look = look with get, set
    /// <summary> returns the look of the object when in data view mode </summary>
    abstract GetDataLook : unit -> symbol
    default this.GetDataLook () : symbol = this.Look
    ///<summary>Returns the new position in the world</summary>
    ///<param name="vec">A given vector to relocate to</param>
    abstract Move: vec -> World -> unit
    default this.Move (dir: vec) (w: World) = ()
 
    ///<summary>A method for stopping the function call InteractWith if the actor is deleted</summary>
    abstract AttemptReactTo : Actor -> unit
    //stops interactions if the entity is deleted
    default this.AttemptReactTo(a: Actor): unit =
        if this.isDeleted then 
            () 
        else this.ReactTo a;
 
    ///<summary>Provides interaction to other part</summary>
    abstract ReactTo: Actor -> unit

    default this.ReactTo (a: Actor) = ()
 
    //Called when we want to render this entity - can be overridden by subclasses
    ///<summary>RenderOn takes the canvas and a boolean whereafter returns a unit, takes the array of symbol and sets its own symbol on the array</summary>
    ///<param name="showData">A boolean which toggles the graphics/info as false/true</param>
    abstract RenderOn: Canvas -> bool -> unit
    default this.RenderOn(c: Canvas)(showData : bool) =
        if ((fst this.Pos) < 0
            || (fst this.Pos) > c.Width - 1
            || (snd this.Pos) < 0
            || (snd this.Pos) > c.Height - 1) then
            ()
        else
            let i = (snd this.Pos) * c.Width + (fst this.Pos)
            let (    _,     _, c_bgc) = c.Chars.[i]
            let (e_str, e_fgc, e_bgc) = if showData then this.GetDataLook () else this.Look
 
            c.Chars.[i] <- (e_str, e_fgc, match e_bgc with | None -> c_bgc | Some _ -> e_bgc)
 
    ///<summary>Checks whether with-in hitbox of enemy</summary>
    abstract InHitBox: vec -> bool
    ///<summary>Default HitBox assumes you are the exact size of 1x1 and do not have a special HitBox</summary>
    ///<param name="pos">The position of the actor</param>
    default this.InHitBox(pos: vec): bool = pos = this.Pos
 
 
and [<AbstractClassAttribute>] Actor(HP: int, pos: vec, look: symbol, ?speed : int, ?solid : bool) =
    inherit Entity(pos, look, match solid with | None -> true | Some b -> b)
 
    let mutable hp = HP
 
    member this.HP
        with get () = hp
        and set (v) =
            if v <= 0 then
                hp <- 0
                this.onDeath ()
            else
                hp <- v
 
    member val DMG = 1 with get, set
 
    member val isDead = false with get, set
 
    member val MaxTurns = match speed with
                            | None -> 1
                            | Some s -> s 
                          with get,set
    member val TurnsLeft = match speed with
                            | None -> 1
                            | Some s -> s 
                          with get, set
 
 
    //called when hp is set to 0 or less, can also be called manually
    ///<summary>A method called based on health, which calls the death event</summary>
    ///<param name="()">An event called when HP is <= 0</param>
    abstract onDeath: unit -> unit
    default this.onDeath(): unit =
        this.isDead <- true
        this.isDeleted <- true
        this.TurnsLeft <- 0
 
    //stops interactions if the entity is deleted
    override this.AttemptReactTo(a: Actor): unit =
        if this.isDeleted then 
            () 
        else 
            this.ReactTo a
            a.OnProvoke (this)
 
    //called in game loop
    ///<summary>Starts a new turn based on if the actor is still active in the game</summary>
    ///<param name="w">Takes a world, w, and returns a unit</param>
    member this.StartTurn(w: World): unit =
        if this.isDeleted || this.TurnsLeft < 1 then () 
        else 
            this.TakeTurn(w); 
            this.TurnsLeft <- this.TurnsLeft - 1
 
    ///<summary>Performs action onto world</summary>
    abstract TakeTurn: World -> unit
    default this.TakeTurn(w: World): unit = ()
 
    //override of move that interacts with other entities if moving into or over them
    override this.Move (dir: vec) (w: World): unit =
        if not this.Solid then
            this.Pos <- this.Pos ++ dir
            ()
        else
            match w.EntityAt(this.Pos ++ dir) with
            | None -> this.Pos <- this.Pos ++ dir
            | Some e ->
                if e.Solid then
                    e.AttemptReactTo(this)
                    this.OnCollide e
                else
                    this.Pos <- this.Pos ++ dir
                    e.AttemptReactTo(this)
 
    ///<summary>Called on collision with solid entity</summary>
    abstract OnCollide : Entity -> unit
    default this.OnCollide (e : Entity) = ()
 
    //by default will be damages by other
    override this.ReactTo(a: Actor): unit = this.HP <- this.HP - a.DMG
 
    override this.GetDataLook () = (sprintf "%iT" this.TurnsLeft , color.Black, Some (snd3 this.Look))
 
    ///<summary>Called on interaction started with other</summary>
    abstract OnProvoke : Actor -> unit
    default this.OnProvoke (a : Actor) = ()
 
 
and [<AbstractClassAttribute>] Item(pos: vec, look: symbol, solid: bool) =
    inherit Entity(pos, look, solid)
 
 
and World(c: Canvas) =
 
    member val c = c
 
    member val Player: Player = Player(5, 5)
    member val ItemList: Item list = [] with get, set
    member val ActorList: Actor list = [] with get, set
    member val EntityList: Entity list = [] with get, set
    member val globalTurnsFinished = 0 with get, set
 
    member val showEntityInfo = false with get, set
 
    ///<summary>Creates a module to AddItems to the world</summary>
    ///<param name="start, dir, len">The start position, direction and length</param>
    ///<returns>Returns a unit adding a new class to the ItemList and EntityList</returns>
    member this.AddItemLine<'T when 'T :> Item and 'T : (new : unit -> 'T)> (start : vec, dir : vec, len : int) : unit =
        for i in [0.. len-1] do
            let item : 'T = new 'T()
            item.Pos <- start ++ (dir +* i)
            this.AddItem (item)
    
    ///<summary>Takes an Item to be added to the Item and EntityList</summary>
    ///<param name="item">The type of Item to be added</param>
    ///<returns>A unit, adding the Item to both lists.</returns>
    member this.AddItem(item: Item) =
        this.ItemList <- this.ItemList @ [ item ]
        this.EntityList <- this.EntityList @ [ item ]
    ///<summary>Takes an Actor and adds it to the Actor and EntityList</summary>
    ///<param name="actor">The type of Actor to be added</param>
    ///<returns>A unit, adding the Actor to both lists.</returns>
    member this.AddActor(actor: Actor) =
        this.ActorList <- this.ActorList @ [ actor ]
        this.EntityList <- this.EntityList @ [ actor ]
    ///<summary>Checks if there is an entity to interact with nearby</summary>
    ///<param name="pos">The given position</param>
    ///<returns>An entity option.</returns>
    member this.EntityAt(pos: vec): Entity option =
        let mutable e: Entity option = None
        for ent in this.EntityList do
            if not ent.isDeleted && (ent.InHitBox pos) then e <- Some ent else ()
        e
 
    member this.RenderWorld() : unit = 
 
        //remove deleted objects
        this.EntityList <- List.filter (fun e -> not e.isDeleted) this.EntityList
        this.ItemList <- List.filter (fun e -> not e.isDeleted) this.ItemList
        this.ActorList <- List.filter (fun e -> not e.isDeleted) this.ActorList
 
        this.c.Reset ()
 
        printfn "Turn: %3i" this.globalTurnsFinished
 
        //Draw entities
        for ent : Entity in this.EntityList do
            ent.RenderOn this.c this.showEntityInfo
 
        this.c.Show()
 
        printfn "%s" (this.Player.Status())
    ///<summary>Plays the game</summary>
    member this.Play() = ()
 
    member val Background: symbol list = []
 
//Cool chars: ☺ ෴ ♥ •
and Player(pos: vec) =
    inherit Actor(3, pos, ("O>", color.White, None), speed = 2)
 
    let mutable lookDir : vec = (1,0)
 
    override this.TakeTurn(w: World) =
        printf "Your Turn:"
        let k = Console.ReadKey(true)
        printfn ""
        match k.Key with
        | ConsoleKey.RightArrow -> this.MoveTurn w rt
        | ConsoleKey.UpArrow -> this.MoveTurn w up
        | ConsoleKey.DownArrow -> this.MoveTurn w dn
        | ConsoleKey.LeftArrow -> this.MoveTurn w lt
        | ConsoleKey.Spacebar -> this.ShootTurn w
        | ConsoleKey.OemMinus -> () //wait a turn
        | ConsoleKey.Tab -> w.showEntityInfo <- not w.showEntityInfo; w.RenderWorld (); this.TakeTurn w
        | _ -> this.TakeTurn(w)

    ///<summary>Renders all the looks dynamically based on turns and direction</summary>
    ///<param name="w, dir">The world to Move in and direction facing</param>
    ///<returns>A unit, switching look</returns>
    member private this.MoveTurn (w: World) (dir : vec) =
        this.Move (dir) w
 
        let sword = if fst dir > 0 then lookDir <- rt; ">" 
                    else if fst dir < 0 then lookDir <- lt; "<" 
                        else if snd dir > 0 then lookDir <- dn; "v" 
                            else lookDir <- up; "^"
 
        this.Look <- if fst lookDir = -1 then (sword + "O", snd3 this.Look, None) else ("O" + sword, snd3 this.Look, None)
    
    ///<summary>Adds an arrow whenever ShootTurn has been called</summary>
    ///<param name="w">The world to Shoot in</param>
    ///<returns>A turn of shooting an arrow in the position and look direction of the player</returns>
    member private this.ShootTurn (w: World) =
        let arrow = FlyingArrow(this.Pos, lookDir)
        w.AddActor(arrow)
        arrow.TakeTurn w
    ///<summary>Shows the status bar below the game rendering</summary>
    ///<returns>A string consisting of the players health points</returns>
    member this.Status(): string =
        sprintf "[HP:%-9s]" (repeatString " <3" this.HP)
 
and FlyingArrow (pos: vec, dir : vec) =
    inherit Actor (1, pos, ("AR", color.Green, None), speed = 2)
    member val dir = dir with get, set

    ///<summary>Finds the correct string to place based on direction of Entity</summary>
    ///<returns>The string matching the given position</returns>
    member private this.getString () : string =
        match this.dir with
            | (1,0) -> "=>"
            | (-1,0) -> "<="
            | (0,-1) -> "/\\"
            | (0,1) -> "\\/"
            | (1,1) -> " \\"
            | (-1,1) -> "/ "
            | (1,-1) -> " /"
            | (-1,-1) -> "\\ "
            | _  -> "AR"
        // sprintf "%2i" <| this.HP
 
    override this.TakeTurn (w : World) =
        this.Move this.dir w
        this.Look <- (this.getString (), snd3 this.Look, None)
        // this.Move dir w
        // this.Look <- (this.getString (), snd this.Look)
 
    override this.ReactTo (a : Actor) =
 
        this.dir <- this.Pos +- a.Pos 
        this.Look <- (this.getString (), snd3 this.Look, None)
        //this.MaxTurns <- this.MaxTurns + 1
        ()
 
    override this.OnProvoke (a : Actor) = 
        //this.Look <- (this.getString (), snd this.Look)
        //this.HP <- this.HP - a.DMG
        ()
 
    override this.OnCollide (e : Entity) =
        this.MaxTurns <- 0
        this.HP <- 0
        ()
        //drop arrow
 
 
and FloorArrow (pos : vec, amount : int) =
    inherit Item (pos, ("-» ", color.Green, None), false)
    new() = FloorArrow(randomVec () +* randN 0 10, 1)
 
    override this.ReactTo (a : Actor) = 
        this.isDeleted <- true
 
    override this.GetDataLook () = (sprintf "%iA" amount, color.Black, Some color.Green)
 
 
type Monster(HP: int, pos: vec, look: symbol) =
    inherit Actor(HP, pos, look)
 
    member val aggressive = true with get, set
 
    override this.TakeTurn(w: World): unit =
        if this.aggressive then
            this.Move (normalize (w.Player.Pos +- this.Pos)) w
            ()
        else
            this.Move (randomVec ()) w
            ()
 
 
type Gollum(pos: vec) =
    inherit Monster(2, pos, ("█>", color.DarkMagenta, None))
    let mutable innerClock = randN 0 1
    let mutable lookDir : vec  = (-1,0)
    ///<summary>The figure of the character</summary>
    member private this.Body : string = "█" 
    new() = Gollum(randomVec () +* randN 0 10)
 
 
    override this.TakeTurn(w: World): unit =
 
        if this.aggressive then
 
            let dir: vec = normalize (w.Player.Pos +- this.Pos)
 
            if randN 0 10 > 6 then
                lookDir <- dir
                let arrow = FlyingArrow(this.Pos, lookDir)
                w.AddActor(arrow)
                arrow.TakeTurn w
            else
 
                this.Move (dir) w
 
                let dirIndicator : string = if fst dir > 0 then ">" 
                                                else if fst dir < 0 then "<" 
                                                    else if snd dir > 0 then "v" 
                                                        else "^"
                lookDir <- dir
 
                this.Look <- if fst lookDir = -1 then (dirIndicator + this.Body, snd3 this.Look, None) else (this.Body + dirIndicator, snd3 this.Look, None)
                ()
        else
            let dir: vec = randomVec ()
            this.Move (dir) w
            ()


type AngryPlant(pos: vec) =
    inherit Item(pos, ("##", color.Green, Some color.DarkBlue), false)
    let damage = 1 
    new() = AngryPlant(randomVec () +* randN 0 10)
    override this.ReactTo(a: Actor) =
            a.HP <- a.HP - damage
 
    override this.GetDataLook () = (sprintf "+%i" damage , color.White, Some (snd3 this.Look))

type HealthPotion(pos: vec) =
    inherit Item(pos, ("<3", color.Red, None), false)
    let healAmount = 1
    new() = HealthPotion(randomVec () +* randN 0 10)
    override this.ReactTo(a: Actor) =
            a.HP <- a.HP + 1
            this.isDeleted <- true
 
    override this.GetDataLook () = (sprintf "+%i" healAmount , color.White, Some (snd3 this.Look))
 
type Wall(pos: vec) =
    inherit Item(pos, ("██", color.Yellow, None), true)
    new() = Wall(randomVec () +* randN 0 10)
 

type WeakWall(pos:vec) =
    inherit Item(pos, ("▓▒", color.Yellow, Some color.DarkGray), true)
    new() = WeakWall(randomVec () +* randN 0 10)
 
    override this.ReactTo(a: Actor) =
            printfn "<!>"
            System.Threading.Thread.Sleep(100)
            printfn "The old wall crumbles from the impact"
            System.Threading.Thread.Sleep(100)
            this.isDeleted <- true
            printf "> continue "
            ignore <| Console.ReadKey(true)
            printfn ""
            ()
 
 
 
let W = World(Canvas(16, 10))
W.AddActor(W.Player)
 
W.AddItemLine<Wall>(o, rt, 15)          //top wall
W.AddItemLine<Wall>(o, dn, 10)          //left
W.AddItemLine<Wall>(dn +* 9, rt, 15)    //bottom
W.AddItemLine<Wall>(rt +* 14, dn, 3)    //right top
W.AddItemLine<Wall>(rt +* 14 ++ (dn +* 4), dn, 6)   //right bottom
 
W.AddItem(WeakWall(rt +* 14 ++ (dn +* 3)))
 
W.AddItem(AngryPlant ((7, 3)))
W.AddItem(HealthPotion ((15, 1)))
W.AddItem(HealthPotion ((9, 2)))
//W.AddItem(FloorArrow ((3,3), 3))
 
W.AddActor(Gollum((7, 8)))
W.AddActor(Gollum((13, 5)))
// W.AddActor(Gollum((3, 3)))
 
let gameRunning : bool = true
 
 
while (gameRunning) do
 
    //remove deleted entities
    // W.EntityList <- List.filter (fun e -> not e.isDeleted) W.EntityList
    // W.ItemList <- List.filter (fun e -> not e.isDeleted) W.ItemList
    // W.ActorList <- List.filter (fun e -> not e.isDeleted) W.ActorList
 
    //maxOfList <| List.map (fun (act : Actor) -> act.MaxTurns) W.ActorList
    // let MaxTurns : int = List.sumBy (fun (act : Actor) -> act.MaxTurns) W.ActorList
    // let mutable TurnsLeft = MaxTurns
 
    //while we have turns left
    while (List.filter (fun (act : Actor) -> act.TurnsLeft > 0) W.ActorList <> []) do
 
        W.RenderWorld ()
 
        //Take turns
 
        for act in W.ActorList do
 
            act.StartTurn(W)
            //System.Threading.Thread.Sleep(100)
 
            // TurnsLeft <- TurnsLeft - 1
 
 
    //afterwards reset all turns
    for act in W.ActorList do
        act.TurnsLeft <- act.MaxTurns
 
    W.globalTurnsFinished <- W.globalTurnsFinished + 1