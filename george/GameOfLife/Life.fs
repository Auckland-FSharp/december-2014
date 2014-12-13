module Life 
    
open System

// A cell is either alive or dead
type Cell = Dead | Alive

// A point is x & y coordinates
type Point = 
    struct
        val x : int
        val y : int
    end
    new(x : int, y : int) = { x = x; y = y; }

    // We can add or subtract points:
    static member (+) (p1 : Point, p2 : Point) = Point (p1.x + p2.x, p1.y + p2.y)
    static member (-) (p1 : Point, p2 : Point) = Point (p1.x - p2.x, p1.y - p2.y)

    override this.ToString() = sprintf "(%d, %d)" this.x this.y

// A board is an array of arrays of cells
type Board = 
    private
    | Board of Cell[,]

    static member mapPoints f (Board b) = seq {
        let minY = Array2D.base1 b
        let maxY = Array2D.length1 b + minY - 1
        let minX = Array2D.base2 b
        let maxX = Array2D.length2 b + minX - 1 
        
        for y in [minY..maxY] do
            for x in [minX..maxX] do
                yield f (Point(x, y))
        }

    static member iterPoints f (Board b) =
        let minY = Array2D.base1 b
        let maxY = Array2D.length1 b + minY - 1
        let minX = Array2D.base2 b
        let maxX = Array2D.length2 b + minX - 1 

        for y in [minY..maxY] do
            for x in [minX..maxX] do
                f (Point(x, y))

    static member iterPointsValues f (Board b) =
        let minY = Array2D.base1 b
        let maxY = Array2D.length1 b + minY - 1
        let minX = Array2D.base2 b
        let maxX = Array2D.length2 b + minX - 1 

        for y in [minY..maxY] do
            for x in [minX..maxX] do
                f (Point(x, y)) b.[y, x]
        
let width (Board b) = Array2D.length2 b
let height (Board b) = Array2D.length1 b
let fromCells = Board << Array2D.copy // we want to know that only we can touch the array, so copy it
let topLeft (Board b) = Point(Array2D.base2 b, Array2D.base1 b)
let bottomRight (Board b) = Point(Array2D.base2 b + Array2D.length2 b, Array2D.base1 b + Array2D.length1 b)
    
// Check if a point is valid on a board:
let private inBounds b (p : Point) = 
    p.y < (Array2D.length1 b + Array2D.base1 b) && p.y >= Array2D.base1 b &&
    p.x < (Array2D.length2 b + Array2D.base2 b) && p.x >= Array2D.base2 b

// We can access the point on a board (safely)
let (@) (Board b) p = if inBounds b p then b.[p.y, p.x] else Dead

// All points currently available on the board.
let points = Board.mapPoints id

// We can check if a particular cell is alive
let private isAlive board point = board @ point = Alive

// Get all the neighbours for a particular point:
let private neighbours p = seq { 
    for y in [-1..1] do
      for x in [-1..1] do
        if not (x = 0 && y = 0) 
          then yield p + Point(x, y)
    }

// Count the number of neighbours that are alive for a point on a board:
let private aliveNeighbours b = neighbours >> Seq.sumBy (isAlive b >> Convert.ToInt32)

// Calculate the new state for a particular point:
let private newState b p = 
    if isAlive b p 
      then 
        match aliveNeighbours b p
          with
          | 2 | 3 -> Alive
          | _ -> Dead
      else
        match aliveNeighbours b p
         with
         | 3 -> Alive
         | _ -> Dead

// Copies a board with new boundaries (anything outside original board is considered dead)
let private withBounds b i1 i2 j1 j2 = 
    Board <| Array2D.initBased i1 j1 (i2-i1+1) (j2-j1+1)
        (fun i j -> if inBounds b (Point(j, i)) then b.[i, j] else Dead)

let private orElse v = function
    | Some v' -> v'
    | None -> v

let private existsCol j p a = 
    let startI = Array2D.base1 a
    let endI = Array2D.length1 a + startI - 1

    let rec loopI i = i < endI && (p a.[i, j] || loopI (i+1))
    loopI startI

let private existsRow i p a = // delicious
    let startJ = Array2D.base2 a
    let endJ = Array2D.length2 a + startJ - 1

    let rec loopJ j = j < endJ && (p a.[i, j] || loopJ (j+1))
    loopJ startJ

// Adjusts a board so that it will always have 'room to grow'
let private adjustBounds (Board b) =
    let isColAlive i = existsCol i (fun s -> s = Alive) b
    let isRowAlive i = existsRow i (fun s -> s = Alive) b

    let firstX = Array2D.base2 b
    let lastX = Array2D.length2 b + firstX - 1
    let firstY = Array2D.base1 b
    let lastY = Array2D.length1 b + firstY - 1

    let minX = [firstX..lastX] |> Seq.tryFind isColAlive |> orElse firstX
    let maxX = [lastX.. -1 ..firstX] |> Seq.tryFind isColAlive |> orElse firstX
    let minY = [firstY..lastY] |> Seq.tryFind isRowAlive |> orElse firstY
    let maxY = [lastY.. -1 ..firstY] |> Seq.tryFind isRowAlive |> orElse firstY
    withBounds b (minY-1) (maxY+1) (minX-1) (maxX+1)

type IBoardDiffer =
    abstract cellChanged : Point -> Cell -> unit

// Mutably builds a board from an old one and a diff:
type private BoardBuilder (basedOn : Board) = 
    let (Board b) = basedOn
    let myBoard = Array2D.copy b
    member private this.set (p : Point) state = myBoard.[p.y, p.x] <- state
    interface IBoardDiffer with
        member this.cellChanged p s = this.set p s
    member this.Built() = Board <| myBoard

// Run 2 differs at once
let private biDiffer (l : IBoardDiffer, r: IBoardDiffer) = { 
        new IBoardDiffer with
        member this.cellChanged p s = l.cellChanged p s ; r.cellChanged p s
    } 

let private actionDiffer (action : Point -> Cell -> Unit) = {
        new IBoardDiffer with
        member this.cellChanged p s = action p s
    }

// Generates a new board based on an old one, and emits what changed:
// TODO: rework this to not use mutation and instead emit a list of changes
let generateNextBoard (b : Board) (differ : Point -> Cell -> Unit) : Board = 
    let b' = adjustBounds b
    let builder = BoardBuilder(b')
    let differ' = biDiffer(builder, actionDiffer(differ))
    Board.iterPointsValues (fun p old ->
        match newState b' p with
            | next when next = old -> ()
            | next -> differ'.cellChanged p next
            ) b'
    adjustBounds (builder.Built())