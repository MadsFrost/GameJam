module roguelike
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
///<summary>Create a new random vector</summary>
    ///<returns>Returns a random vector pointing from middle of 3x3 grid to any of those 9</returns>
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
 
 
//CANVAS
 
type CanvasSection = All | Nothing | Area of vec * vec
 
type Canvas(size : vec, position : vec, layer : int) =
 
    member val renderSection : CanvasSection = All with get,set
    member val position = position with get,set
    member val Layer = layer with get, set 
    member val Chars: symbol array = Array.init (fst size * snd size) (fun _ -> defaultSym1) with get, set
    member this.Width = fst size
    member this.Height = snd size
 
    member this.printIndex () =
 
        for i in [ 0 .. (this.Chars.Length - 1) ] do
            Console.Write(sprintf "%3i" i)
            if ((i + 1) % (this.Width) = 0) then
                Console.Write("\n")
            else
                ()
        ()
 
    ///<summary>Sets a string of length 2 and a char color onto the canvas</summary>
    ///<param name="point: vec, s:symbol"></param>
    member this.Set (s: symbol) (point: vec) : unit =
        if ((fst point) < 0
            || (fst point) > this.Width - 1
            || (snd point) < 0
            || (snd point) > this.Height - 1) then
            ()
        else
            this.Chars.[(snd point) * this.Width + (fst point)] <- s
 
    member this.SetLine (s:symbol) (start:vec) (dest:vec) =
 
        let mutable point = start
        let amount = int(length(dest +- start))
        for i in [0..amount] do
 
            if ((fst point) < 0
                || (fst point) > this.Width - 1
                || (snd point) < 0
                || (snd point) > this.Height - 1) then
                ()
            else
                this.Chars.[(snd point) * this.Width + (fst point)] <- s
            point <- point ++ normalize(dest +- start)
 
    ///<summary>Spells the string onto the canvas</summary>
    member this.Spell (s:string) (pos : vec) (colors : color*(color option)) : unit =
        let symbols = ceil <| (float(s.Length) / 2.)
 
        for i in [0..(int <| (symbols - 1.))] do
            this.Set ((s + " ").[2*i..2*i+1], fst colors, snd colors) (pos ++ (rt +* i))
            ()  
 
    ///<summary>Fills the canvas with default background grid</summary>
    abstract Reset : unit -> unit
    default this.Reset(): unit =
        // this.Chars <- Array.init (this.Width*this.Height) (fun i -> ((sprintf "%2s" name).[0..1]), color.White, Some color.Black)
 
        for i in [ 0 .. this.Chars.Length - 1 ] do
            let (x, y) = (i % this.Width, i / this.Width)
            this.Chars.[i] <- if x % 2 = y % 2 then defaultSym1 else defaultSym2
 
type CanvasRenderer (size : vec, cList : Canvas list) =
    member val Chars: symbol array = Array.init (fst size * snd size) (fun _ -> ("  ", color.Black, Some color.DarkGray)) with get, set
    member val canvasList = cList with get, set
    member val size = size with get, set
    member this.Width = fst size
    member this.Height = snd size
 
 
 
    member this.Show() =
 
        this.Chars <- Array.init (fst size * snd size) (fun _ -> ("  ", color.Black, Some color.Black))
 
        ///<summary> transforms from index of canvas to screenSpace position </summary>
        let indexToPos (i : int) (c : Canvas) : vec option =
                if i > c.Chars.Length || i < 0 then
                    None
                else 
                    let pos = (i % c.Width, i / c.Width)
                    Some (pos ++ c.position)
        ///<summary>Checks pos on on canvas</summary>
        ///<param name ="pos, c">The position in vector form and canvas</param>
        ///<returns>An integer option</returns>
        let posToIndexOn (pos : vec) (c:Canvas)  : int option =
            let transformedPos = pos
            if ((fst transformedPos) < 0
                || (fst transformedPos) > c.Width - 1
                || (snd transformedPos) < 0
                || (snd transformedPos) > c.Height - 1) then
                None
            else 
                let i = (fst transformedPos + snd transformedPos * c.Width)
                Some i
 
        ///<summary>Checks position to a certain index</summary>
        ///<param name ="pos">The position in vector form</param>
        ///<returns>An integer option</returns>
        let posToThisIndex (pos : vec)  : int option =
            let transformedPos = pos
            if ((fst transformedPos) < 0
                || (fst transformedPos) > this.Width - 1
                || (snd transformedPos) < 0
                || (snd transformedPos) > this.Height - 1) then
                None
            else 
                let i = (fst transformedPos + snd transformedPos * this.Width)
                Some i
 

        let sortedList = List.sortBy (fun (c:Canvas) -> c.Layer) this.canvasList
        for c in sortedList do
            match c.renderSection with
            | Nothing -> ()
            | All ->
 
                for i in [0..c.Chars.Length-1] do
 
                    match Option.bind (posToThisIndex) (indexToPos i c) with
                        | None -> ()
                        | Some screenSpaceIndex -> 
                            let (    _, c_fgc, c_bgc) = this.Chars.[screenSpaceIndex]
                            let (e_str, e_fgc, e_bgc) = c.Chars.[i]
 
                            let pixel = (e_str, e_fgc, match e_bgc with | None -> c_bgc | Some c -> e_bgc)
                            this.Chars.[screenSpaceIndex] <- pixel
 
            | Area (TL, BR) -> 
                let width = abs(fst (TL +- BR))
                let height = abs(snd (TL +- BR))
                let area =  width * height
 
                for i in [0..area-1] do
                    let screenSpacePoint = (i % width, i / width) ++ c.position
                    let canvasPoint = screenSpacePoint ++ TL
                    let canvasIndex = posToIndexOn canvasPoint c
 
                    match posToThisIndex screenSpacePoint  with
                        | None -> ()
                        | Some screenSpaceIndex -> 
                            let (c_str, c_fgc, c_bgc) = this.Chars.[screenSpaceIndex]
 
                            let (e_str, e_fgc, e_bgc)  = match canvasIndex with 
                                                            | None -> ("  ", color.Black, Some color.Black)
                                                            | Some canvasI -> c.Chars.[canvasI]
 
                            let pixel = (e_str, e_fgc, match e_bgc with | None -> c_bgc | Some c -> e_bgc)
 
                            //if e_str = "  " then pixel <- (c_str, color.Gray, Some color.DarkGray) else ()
                            this.Chars.[screenSpaceIndex] <- pixel
                ()
 
        Console.Clear()
        for i in [ 0 .. (this.Chars.Length - 1) ] do
            Console.ResetColor()
            let (c, f, b) = this.Chars.[i]
            Console.ForegroundColor <- f
            match b with | None -> () | Some c -> Console.BackgroundColor <- c
            Console.Write(c.[0..1])
            if ((i + 1) % (this.Width) = 0) then
                Console.ResetColor()
                Console.Write("\n")
            else
                ()
 
 
 
//WORLD AND GAME
 
///<summary>A class holding entities and actors, used simulate windows, UI, and game map.</summary>
and World (c : Canvas, player : Playable, canvasRenderer : CanvasRenderer) =
 
    member val worldCanvas = c.Reset(); c
    member val canvasRenderer = canvasRenderer
    member val Player: Playable = player
    member val ItemList: Item list = [] with get, set
    member val ActorList: Actor list = [] with get, set
    member val EntityList: Entity list = [] with get, set
    member val showEntityInfo = false with get, set
 
    ///<summary>A shortcut to adding multiple items to the world in a straight line</summary>
    ///<param name="start, dir, len">The start position, direction and length</param>
    member this.AddItemLine<'T when 'T :> Item and 'T : (new : unit -> 'T)> (start : vec, dir : vec, len : int) : unit =
        for i in [0.. len-1] do
            let item : 'T = new 'T()
            item.Pos <- start ++ (dir +* i)
            this.AddItem (item)
 
    ///<summary>Takes an Item to be added to the Item and EntityList</summary>
    ///<param name="item">The type of Item to be added</param>
 
    member this.AddItem(item: Item) =
        this.ItemList <- this.ItemList @ [ item ]
        this.EntityList <- this.EntityList @ [ item ]
    ///<summary>Takes an Actor and adds it to the Actor and EntityList</summary>
    ///<param name="actor">The type of Actor to be added</param>
 
    member this.AddActor(actor: Actor) =
        this.ActorList <- this.ActorList @ [ actor ]
        this.EntityList <- this.EntityList @ [ actor ]
        actor.world <- Some this
    ///<summary>Checks if there is an entity to interact with nearby</summary>
    ///<param name="pos">The given position</param>
    ///<returns>An entity option.</returns>
    member this.EntityAt(pos: vec): Entity option =
        let mutable e: Entity option = None
        for ent in this.EntityList do
            if not ent.isDeleted && (ent.InHitBox pos) then e <- Some ent else ()
        e
    abstract member Update : unit -> unit
    default this.Update () = 
        this.worldCanvas.Reset()
        //remove deleted objects
        this.EntityList <- List.filter (fun e -> not e.isDeleted) this.EntityList
        this.ItemList <- List.filter (fun e -> not e.isDeleted) this.ItemList
        this.ActorList <- List.filter (fun e -> not e.isDeleted) this.ActorList
 
        //Draw entities
        for ent : Entity in this.EntityList do
            ent.RenderOn this.worldCanvas this.showEntityInfo
 
        let healthSpot = (-player.Status().Length/4 - 1, -7)
        this.worldCanvas.Spell (player.Status()) (player.Pos++healthSpot) (color.Red, None)
 
    ///<summary>Renders the world</summary>
    member this.RenderWorld() : unit = 
 
        this.Update()
        this.canvasRenderer.Show()
    ///<summary>Performs the turns in the game</summary>
    member this.PerformGameLoop () =
 
        //while we have turns left
            while (List.filter (fun (act : Actor) -> act.TurnsLeft > 0) this.ActorList <> []) do
 
                this.RenderWorld()
 
                //Take turns
                for act in this.ActorList do
 
                    act.StartTurn this
 
            //afterwards reset all turns
            for act in this.ActorList do
                act.TurnsLeft <- act.MaxTurns
 
    ///<summary> What to do when "leaving" or exiting this world" </summary>
    abstract member Quit : unit -> unit
    default this.Quit () = ()
 
    abstract member Init : unit -> unit
    default this.Init () = ()
 
 
and GameWorld(size : vec, canvasRenderer : CanvasRenderer, player : Adventurer) =
 
    inherit World (Canvas((fst size, snd size), o, 1), player, canvasRenderer)
 
    override this.Init() = 
 
        let lines = System.IO.File.ReadAllLines("worldMap.txt")
 
        this.AddActor(this.Player)
 
        for y in [0.. lines.Length-1] do
            for x in [0.. lines.[y].Length-1] do
                match lines.[y].[x] with
                            | '#' -> this.AddItem(Wall ((x,y)))
                            | 'A' -> this.AddActor(Archer ((x,y)))
                            | 'B' -> this.AddActor(Brute  ((x,y)))
                            | 'P' -> player.Pos <- (x,y);
                                     let renderSize = (-7,-7)
                                     this.worldCanvas.renderSection <- Area (player.Pos ++ renderSize, player.Pos +- renderSize)
                            | 'H' -> this.AddItem(HealthPotion ((x,y)))
                            | 'W' -> this.AddItem(Water ((x,y)))
                            | '!' -> this.AddItem(Fire ((x,y)))
                            | 'F' -> this.AddItem(FleshEatingPlant ((x,y)))
                            | ':' -> this.AddItem(WeakWall ((x,y)))
                            | 'S' -> this.AddItem(Secret ((x,y)))
                            | 'X' -> this.AddItem <| Exit ((x,y), ("[]",color.DarkYellow, Some color.Black))
                            | _ -> ()
 
                ()
 
 
 
    override this.Quit() =
 
 
        let renderSize = (-7,-7)
        this.worldCanvas.renderSection <- Area (player.Pos ++ renderSize, player.Pos +- renderSize)
        let textPos = player.Pos ++ (-7, -4)
        let col = (color.Gray, Some color.Black)
        this.worldCanvas.Reset()
        this.worldCanvas.Spell ("ESCAPE") (textPos++rt++rt++rt++rt++rt++up++up) (color.DarkGreen, Some color.Black)
        this.worldCanvas.Spell (" You've escaped the prison") textPos col
        this.worldCanvas.Spell (sprintf " with %i of your health left" player.HP) (textPos++dn) col
 
        this.worldCanvas.Spell ("Remaining hiding archers") (textPos++(dn +* 3)++rt) col
        this.worldCanvas.Spell ("run to the exit, but you") (textPos++(dn +* 4)++rt) col
        this.worldCanvas.Spell (" swiftly lock the door. ") (textPos++(dn +* 5)++rt) col
 
        this.worldCanvas.Spell (" Did you find all of the") (textPos++(dn +* 7)++rt) col
        this.worldCanvas.Spell ("secrets?") (textPos++(dn +* 8)++(rt +* 5)) (color.DarkCyan, Some color.Black)
 
        this.canvasRenderer.Show()
 
        System.Threading.Thread.Sleep(System.Threading.Timeout.Infinite)
 
 
and Game () =
    let renderer = new CanvasRenderer((20,13),[])
    let inventory = new Inventory((20,13), renderer)
    let player = new Adventurer (o, inventory)
    let gameWorld = new GameWorld((90,30), renderer, player)
    let mutable gameOver = false
    ///<summary>Renders the game on a canvas and loops the game turns until game over</summary>
    member this.Play () = 
        renderer.canvasList <- [gameWorld.worldCanvas; inventory.worldCanvas]
        gameWorld.Init()
        inventory.Init()
        while not gameOver do
            gameWorld.PerformGameLoop()
 
 
//INVENTORY
 
and InventoryScreen (size:vec, position:vec, layer: int) =
    inherit Canvas (size, position, layer)
 
    override this.Reset () : unit =
 
        this.Chars <- Array.init (this.Width*this.Height) (fun _ -> ("__", color.DarkGray, Some color.DarkGray))
 
        let boxPos = (3,5)
        let topL = (0,1) ++ boxPos
        let topR = (8, 1) ++ boxPos
        let botL = (0, 5) ++ boxPos
        let botR = (8, 5) ++ boxPos
 
        let col = color.DarkYellow
        let bgCol = Some color.Black
 
        this.Spell " INVENTORY" (5,4) (color.DarkYellow, Some color.Black)  
        this.SetLine ("║║", col, bgCol) topL botL
        this.SetLine ("║║", col, bgCol) topR botR
        this.SetLine ("══", col, bgCol) topL topR 
        this.SetLine ("══", col, bgCol) botL botR
        for i in [1..(snd (botL +- topL))-1] do
            let offset = dn +* i
            this.SetLine ("  ", color.DarkGray, Some color.Black) (topL++offset++rt) (topR++ offset ++ lt)
        //╩ ╦ ╔ ╚ ╗ ╝
        this.Set ("╦╗", col, bgCol) topR
        this.Set ("╔╦", col, bgCol) topL
        this.Set ("╚╩", col, bgCol) botL
        this.Set ("╩╝", col, bgCol) botR
 
 
and Inventory(size:vec, canvasRenderer : CanvasRenderer) =
 
    inherit World (
        (let is = InventoryScreen((fst size, snd size), o, 2)
         is.renderSection <- Nothing
         is), new InvMarker (o), canvasRenderer)
 
    let boxPos = (3,5)
    let topL = (0,1) ++ boxPos
    let topR = (8, 1) ++ boxPos
    let botL = (0, 5) ++ boxPos
    let botR = (8, 5) ++ boxPos
 
    member val isOpen : bool = false with get, set 
 
 
    override this.Init () =
 
        this.AddItemLine<Barrier> (topL, rt, fst (botR +- botL))
        this.AddItemLine<Barrier> (topL, dn, snd (botL +- topL))
        this.AddItemLine<Barrier> (botL, rt, fst (topR +- topL))
        this.AddItemLine<Barrier> (topR, dn, snd (botL +- topL))
 
        this.AddItem(new Exit (topR ++ dn ++ lt, (" X", color.Red, None)))
 
        this.Player.Pos <- topR ++ dn ++ lt
        this.AddActor(this.Player)
        this.AddItem (new InfinityArrow(topL ++ rt ++dn))
        this.AddItem (new TutorialPaper (botR ++ lt ++ up, "Space to shoot"))
        this.AddItem (new TutorialPaper (botR ++ lt ++ lt ++ up, "Tab for data"))
        this.AddItem (new TutorialPaper (botR ++ lt ++ lt ++ lt ++ up, "Minus to wait"))
 
        this.AddItem (new Key (topL ++ rt ++ rt ++rt ++dn))
 
    override this.Update () =
        this.worldCanvas.Reset()
        base.Update()
 
    ///<summary>Opens the canvas and starts the game</summary>
    member this.Open () =
        this.isOpen <- true
        this.worldCanvas.renderSection <- All
        //this.Update()
        while this.isOpen do
            this.PerformGameLoop()
        ()
        this.worldCanvas.renderSection <- Nothing
 
    override this.Quit () = 
        this.isOpen <- false
        this.RenderWorld (); 
        System.Threading.Thread.Sleep(500);
 
 
 
//ENTITY
 
and [<AbstractClassAttribute>] Entity(pos: vec, look: symbol, solid: bool) =
 
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
 
    ///<summary>A method for stopping the function call ReactTo if the actor is deleted</summary>
    abstract AttemptReactTo : Actor -> unit
    //stops interactions if the entity is deleted
    default this.AttemptReactTo(a: Actor): unit =
        if this.isDeleted then 
            () 
        else this.ReactTo a;
    ///<summary>Checks pos on on canvas</summary>
    ///<param name ="pos, c">The position and canvas</param>
    ///<returns>An integer option</returns>
    member this.AttemptInspect (a : Actor) : string =
        if this.isDeleted then 
            "" 
        else this.Inspect a
 
    abstract Inspect : Actor -> string
    default this.Inspect (a : Actor) : string = this.GetType().ToString()
 
    ///<summary>Provides interaction to other part</summary>
    abstract ReactTo : Actor -> unit
 
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
 
 
 
 
//ACTORS
 
and [<AbstractClassAttribute>] Actor(HP: int, pos: vec, look: symbol, ?world : World, ?speed : int, ?solid : bool) =
    inherit Entity(pos, look, match solid with | None -> true | Some b -> b)
 
 
 
    let mutable hp = HP
 
    member this.HP
        with get () = hp
        and set (v) =
            if v <= 0 then
                hp <- 0
                this.onDeath ()
            else
                if v >= 15 then
                    hp <- 15
                else 
                    hp <- v
 
    member val world = world with get, set
 
    member val DMG = 5 with get, set
 
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
            this.TurnsLeft <- this.TurnsLeft - 1
            this.TakeTurn(w); 
 
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
 
 
and Playable(pos: vec, look : symbol) =
    inherit Actor(10, pos, look, speed = 2)
 
    ///<summary>Shows the status bar below the game rendering</summary>
    ///<returns>A string consisting of the players health points</returns>
    abstract Status : unit -> string
    default this.Status(): string =
        sprintf "[HP:%-5s]" (repeatString "*" this.HP)
 
and Adventurer (pos: vec, inv : Inventory) =
    inherit Playable(pos, ("O>", color.White, None))
 
    let mutable lookDir : vec = (1,0)
 
    member val inventory : Inventory = inv
 
    override this.TakeTurn(w: World) =
 
        let k = Console.ReadKey(true)
 
        match k.Key with
        | ConsoleKey.RightArrow -> this.MoveTurn w rt
        | ConsoleKey.UpArrow -> this.MoveTurn w up
        | ConsoleKey.DownArrow -> this.MoveTurn w dn
        | ConsoleKey.LeftArrow -> this.MoveTurn w lt
        | ConsoleKey.Spacebar -> this.ShootTurn w
        | ConsoleKey.OemMinus -> () //wait a turn
        | ConsoleKey.I -> this.EnterInventory (); w.RenderWorld(); this.TakeTurn w;
        | ConsoleKey.Tab -> w.showEntityInfo <- not w.showEntityInfo; w.RenderWorld (); this.TakeTurn w;
        | _ -> this.TakeTurn(w);
 
    ///<summary>Renders all the looks dynamically based on turns and direction</summary>
    ///<param name="w, dir">The world to Move in and direction facing</param>
    ///<returns>A unit, switching look</returns>
    member private this.MoveTurn (w: World) (dir : vec) =
        this.Move (dir) w
 
        let renderSize = (-7,-7)
        w.worldCanvas.renderSection <- Area (this.Pos ++ renderSize, this.Pos +- renderSize)
 
        let sword = if fst dir > 0 then lookDir <- rt; ">" 
                    else if fst dir < 0 then lookDir <- lt; "<" 
                        else if snd dir > 0 then lookDir <- dn; "v" 
                            else lookDir <- up; "^"
 
        this.Look <- if fst lookDir = -1 then (sword + "O", snd3 this.Look, None) else ("O" + sword, snd3 this.Look, None)
 
    ///<summary>Adds an arrow whenever ShootTurn has been called</summary>
    ///<param name="w">The world to Shoot in</param>
    ///<returns>A turn of shooting an arrow in the position and look direction of the player</returns>
    member private this.ShootTurn (w: World) =
        let arrow = FlyingArrow(this.Pos, lookDir, color.Green)
        w.AddActor(arrow)
        arrow.TakeTurn w
 
    ///<summary>Opens the player inventory</summary>
    member private this.EnterInventory () =
        this.inventory.Open()
 
    override this.onDeath () =
        let textPos = this.Pos ++ (-6, -5)
        let col = (color.Gray, Some color.Black)
        match this.world with
            | None -> ()
            | Some w ->
                let renderSize = (-8,-7)
                w.worldCanvas.renderSection <- Area (this.Pos ++ renderSize, this.Pos +- renderSize)
                w.worldCanvas.Reset()
                w.worldCanvas.Spell ("DEFEAT") (textPos++(rt +* 5)++up++up) (color.DarkRed, Some color.Black)
                w.worldCanvas.Spell ("You have succumbed to") (textPos++rt) col
                w.worldCanvas.Spell ("your numerous wounds.") (textPos++dn++rt) col
 
                w.worldCanvas.Spell ("As the light fades from") (textPos++(dn +* 3)++rt) col
                w.worldCanvas.Spell ("your eyes, all you can") (textPos++(dn +* 4)++rt) col
                w.worldCanvas.Spell ("think of is your family,") (textPos++(dn +* 5)++rt) col
                w.worldCanvas.Spell ("and how they will never") (textPos++(dn +* 6)++rt) col
                w.worldCanvas.Spell ("get to know what happened.") (textPos++(dn +* 7)++rt) col
 
                w.worldCanvas.Spell ("Try again? -> Restart") (textPos++(dn +* 9)++rt) col
                w.worldCanvas.Spell ("Tread carefully") (textPos++(dn +* 10)++(rt +* 3)) (color.DarkCyan, Some color.Black)
                w.canvasRenderer.Show()
 
        System.Threading.Thread.Sleep(System.Threading.Timeout.Infinite)
 
and InvMarker (pos: vec) =
    inherit Playable(pos, ("  ", color.Black, Some color.DarkGray))
 
    override this.TakeTurn(w: World) =
        let k = Console.ReadKey(true)
        match k.Key with
        | ConsoleKey.RightArrow -> this.Move rt w
        | ConsoleKey.UpArrow ->    this.Move up w 
        | ConsoleKey.DownArrow ->  this.Move dn w
        | ConsoleKey.LeftArrow ->  this.Move lt w
        | _ -> this.TakeTurn(w);
 
    override this.Status () = ""
 
    override this.Move (dir : vec) (w : World) =
        if not this.Solid then
            this.Pos <- this.Pos ++ dir
            ()
        else
            match w.EntityAt(this.Pos ++ dir) with
            | None -> this.Pos <- this.Pos ++ dir
            | Some e ->
                if e.Solid then
                    //e.AttemptReactTo(this)
                    this.OnCollide e
                else
                    this.Pos <- this.Pos ++ dir
                    w.Update()
                    match e.AttemptInspect(this) with
                        | "" -> ()
                        | s -> w.worldCanvas.Spell s (e.Pos++up) (snd3 e.Look, Some color.Black)
                               w.canvasRenderer.Show()
                               ignore <| Console.ReadKey(true)
                               ()
 
    override this.RenderOn (c:Canvas) (_:bool) =
        if ((fst this.Pos) < 0
            || (fst this.Pos) > c.Width - 1
            || (snd this.Pos) < 0
            || (snd this.Pos) > c.Height - 1) then
            ()
        else
            let i = (snd this.Pos) * c.Width + (fst this.Pos)
            let (c_str, c_fgc, c_bgc) = c.Chars.[i]
            let (e_str, e_fgc, e_bgc) = this.Look
 
            c.Chars.[i] <- (c_str, c_fgc, e_bgc)
 
 
and FlyingArrow (pos: vec, dir : vec, col : color) =
    inherit Actor (1, pos, ("AR", col, None), speed = 2)
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
 
and Monster(HP: int, pos: vec, look: symbol) =
    inherit Actor(HP, pos, look)
 
    member val aggressive = true with get, set
    member val detectDist = 7.
 
    override this.TakeTurn(w: World) =
        if length (w.Player.Pos +- this.Pos) < this.detectDist then
            this.aggressive <- true
        else 
            this.aggressive <- false
 
 
and Brute(pos: vec) =
    inherit Monster(10, pos, ("█-", color.DarkMagenta, None))
    let mutable lookDir : vec  = (-1,0)
 
    ///<summary>The figure of the character</summary>
    member private this.Body : string = "█" 
    new() = Brute(randomVec () +* randN 0 10)
 
 
    override this.TakeTurn(w: World) =
 
        base.TakeTurn(w)
 
        if this.aggressive then
 
            let dir: vec = normalize (w.Player.Pos +- this.Pos)
 
            this.Move (dir) w 
            let dirIndicator : string = if fst dir > 0 then ">" 
                                            else if fst dir < 0 then "<" 
                                                else if snd dir > 0 then "v" 
                                                    else "^"
            lookDir <- dir
            this.Look <- if fst lookDir = -1 then (dirIndicator + this.Body, snd3 this.Look, None) else (this.Body + dirIndicator, snd3 this.Look, None)
 
        else
            ()
 
and Archer(pos: vec) =
    inherit Monster(5, pos, ("A}", color.DarkMagenta, None))
    let mutable innerClock = randN 0 1
    let mutable lookDir : vec  = (-1,0)
    ///<summary>The figure of the character</summary>
    member private this.Body : string = "A" 
    new() = Archer(randomVec () +* randN 0 10)
 
    override this.TakeTurn(w: World) =
 
        base.TakeTurn(w)
 
        if this.aggressive then
            innerClock <- innerClock + 1
            let dir: vec = normalize (w.Player.Pos +- this.Pos)
            if true then //innerClock % 2 = 0
                lookDir <- dir
                let arrow = FlyingArrow(this.Pos, lookDir, color.DarkMagenta)
                arrow.MaxTurns <- 2
                w.AddActor(arrow)
                arrow.TakeTurn w
                arrow.TurnsLeft <- 1
 
            else
            ()
                // this.Move (dir) w 
                // let dirIndicator : string = if fst dir > 0 then "}" 
                //                                 else if fst dir < 0 then "{" else "}"
 
                // lookDir <- dir
                // this.Look <- if fst lookDir = -1 then (dirIndicator + this.Body, snd3 this.Look, None) else (this.Body + dirIndicator, snd3 this.Look, None)
 
        else
            ()
 
 
 
//ITEMS
 
and [<AbstractClassAttribute>] Item(pos: vec, look: symbol, solid: bool) =
    inherit Entity(pos, look, solid)
 
and Fire(pos: vec) =
    inherit Item(pos, (match randN 0 1 with
                                | 0 -> ("/\\", color.Red, Some color.Black)
                                | 1 -> ("/\\", color.DarkRed, Some color.Black)
                                | _ -> (":D", color.Red, Some color.Black)
                      ), false)
    let mutable fireLeft = 5
    let damage = 1
    new() = Fire(randomVec () +* randN 0 10)
    override this.ReactTo(a: Actor) = 
            a.HP <- a.HP - damage
            a.Look <- (fst3 a.Look, color.Red, third3 a.Look)
            fireLeft <- fireLeft - 1
            if fireLeft < 1 then
                this.isDeleted <- true
            else 
                ()
 
    override this.GetDataLook () = (sprintf "-%i" damage, color.Black, Some (snd3 this.Look))
 
and FleshEatingPlant(pos: vec) =
    inherit Item(pos, (match randN 0 3 with
                            | 0 -> "<^"
                            | 1 -> "v^"
                            | 2 -> "<>"
                            | 3 -> "^v"
                            | _ -> ":D"
                            , color.DarkGreen, None), false)
    let damage = 5 
    new() = FleshEatingPlant(randomVec () +* randN 0 10)
    override this.ReactTo(a: Actor) =
            a.HP <- a.HP - damage
 
    override this.GetDataLook () = (sprintf "-%i" damage , color.Black, Some (snd3 this.Look))
 
and HealthPotion(pos: vec) =
    inherit Item(pos, ("<3", color.Red, None), false)
    let healAmount = 5
    new() = HealthPotion(randomVec () +* randN 0 10)
    override this.ReactTo(a: Actor) = 
            a.HP <- a.HP + healAmount
            this.isDeleted <- true
 
    override this.GetDataLook () = (sprintf "+%i" healAmount , color.Black, Some (snd3 this.Look))
 
and Water(pos: vec) =
    inherit Item(pos, ("~~", color.Black, Some color.DarkCyan), false)
    let healAmount = 2
    new() = Water(randomVec () +* randN 0 10)
    override this.ReactTo(a: Actor) = 
            a.HP <- a.HP + healAmount
            this.isDeleted <- true
 
    override this.GetDataLook () = (sprintf "+%i" healAmount , color.Black, third3 this.Look)
 
and Wall(pos: vec) =
    inherit Item(pos, ("▓▓", color.Gray, None), true)
    new() = Wall(randomVec () +* randN 0 10)
 
and WeakWall(pos:vec) =
    inherit Item(pos, ("▒▓", color.Gray, None), true)
    new() = WeakWall(randomVec () +* randN 0 10)
 
    override this.ReactTo(a: Actor) =
            this.isDeleted <- true
            // printfn "<!>"
            // printfn "The old wall crumbles from the impact"
            // printf "> continue "
            // ignore <| Console.ReadKey(true)
            ()
 
and Exit (pos : vec, look : symbol) =
    inherit Item(pos, look, false)
 
 
    override this.ReactTo(a: Actor) =
            this.Quit (a)
 
    override this.Inspect (a:Actor) : string =
            this.Quit(a)
            ""
 
    ///<summary>Quits the game</summary>
    ///<param name ="a">An actor</param>
    member this.Quit (a:Actor) : unit =
        match a.world with
            | None -> failwith "Error: Exit couldn't quit world because world was None"
            | Some w -> 
                        if a.HP < 5 then
 
                            let renderSize = (-7,-7)
                            w.worldCanvas.renderSection <- Area (a.Pos ++ renderSize, a.Pos +- renderSize)
                            let textPos = a.Pos ++ (-7, -4)
                            let col = (color.Gray, Some color.Black)
                            w.worldCanvas.Reset()
                            w.worldCanvas.Spell ("TOO WEAK") (textPos++rt++rt++rt++rt++rt++up++up) (color.DarkRed, Some color.Black)
                            w.worldCanvas.Spell ("You reach the door, but") (textPos++rt) col
                            w.worldCanvas.Spell (sprintf "with only %i health left" a.HP) (textPos++dn++rt) col
 
                            w.worldCanvas.Spell ("Less than 5 makes you too") (textPos++(dn +* 3)++rt) col
                            w.worldCanvas.Spell ("weak. The door is stuck.") (textPos++(dn +* 4)++rt) col
 
                            w.worldCanvas.Spell ("An arrow whizzes into") (textPos++(dn +* 6)++rt) col
                            w.worldCanvas.Spell ("your aching back.") (textPos++(dn +* 7)++rt) col
 
                            w.worldCanvas.Spell ("You die.") (textPos++(dn +* 8 )++rt) col
                            w.worldCanvas.Spell ("Explore for health!") (textPos++(dn +* 9)++(rt +* 2)) (color.DarkCyan, Some color.Black)
 
                            w.canvasRenderer.Show()
 
                            System.Threading.Thread.Sleep(System.Threading.Timeout.Infinite)
                        else
                            w.Quit()
                            a.TurnsLeft <- 0
        ()
 
 
and Barrier (pos : vec) =
    inherit Item (pos, ("  ", color.Black, None), true)
    new () = new Barrier(o)
    override this.RenderOn (_ : Canvas) (_ : bool) = ()
 
and Secret(pos: vec) =
    inherit Item(pos, ("$$", color.DarkCyan, None), false)
    new() = Secret(randomVec () +* randN 0 10)
    override this.ReactTo(a: Actor) = 
 
            match a.world with
             | None -> ()
             | Some w -> 
                         w.Update()
                         w.worldCanvas.Spell "Secret found" (this.Pos++up++lt++lt++lt) (snd3 this.Look, Some color.Black)
                         w.canvasRenderer.Show()
                         ignore <| Console.ReadKey(true)
                         w.RenderWorld()
 
            this.isDeleted <- true
 
 
    override this.GetDataLook () = ("ST", color.Black, Some (snd3 this.Look))
 
 
//INVENTORY ITEMS
 
and Key (pos : vec) =
    inherit Item (pos, ("o-", color.DarkYellow, None), false)
 
    override this.Inspect (a:Actor) : string =
            "Key for exit"
 
and TutorialPaper (pos : vec, text : string) =
 
    inherit Item (pos, ("[?", color.DarkCyan, None), false)
 
    override this.Inspect (a:Actor) : string =
        text
 
and InfinityArrow (pos : vec) =
    inherit Item (pos, ("=> ", color.Green, None), false)
    new() = InfinityArrow(randomVec () +* randN 0 10)
 
    override this.ReactTo (a : Actor) = 
 
        this.isDeleted <- true
 
    override this.GetDataLook () = ("1A", color.Black, Some color.Green)
 
    override this.Inspect (a : Actor) =
        "Infinity Arrow"