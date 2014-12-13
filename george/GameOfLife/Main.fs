module Main

open Life

// Here's an example board:
let board1 = fromCells <| array2D (List.replicate 3 [| Dead; Alive; Dead |])

let glider = 
    fromCells <| array2D [|
        [| Dead; Alive; Dead |]
        [| Dead; Dead; Alive |]
        [| Alive; Alive; Alive |]
    |]

let rpentomino = 
    fromCells <| array2D [|
        [| Dead; Alive; Alive |]
        [| Alive; Alive; Dead |]
        [| Dead; Alive; Dead |]
    |]

let diehard = 
    fromCells <| array2D [|
        [| Dead; Dead; Dead; Dead; Dead; Dead; Alive; Dead |]
        [| Alive; Alive; Dead; Dead; Dead; Dead; Dead; Dead |]
        [| Dead; Alive; Dead; Dead; Dead; Alive; Alive; Alive |]
    |]

open System
open System.Collections.Generic
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Media.Animation
open System.Windows.Shapes

type GraphicalBoard (c: Canvas) =
    let boxSize = 10.0
    let boxCenter = boxSize / 2.0

    let canvasOffset = TranslateTransform()
    let canvasScale = ScaleTransform()

    let mutable myBoard = fromCells <| array2D [| [| |] |]
    let children = Dictionary<Life.Point, Rectangle>()
    let border = Rectangle() // Stroke = Brushes.AliceBlue if you want to see the board outline
    let rand = Random()

    do
        let transform = TransformGroup()
        transform.Children.Add(canvasOffset)
        transform.Children.Add(canvasScale)
        c.RenderTransform <- transform

    member this.reCenter() =
        let topLeft = topLeft myBoard
        let bottomRight = bottomRight myBoard
        let (x, y) = (float topLeft.x * boxSize, float topLeft.y * boxSize)
        let (x', y') = (float bottomRight.x * boxSize, float bottomRight.y * boxSize)

        border.Width <- (x' - x)
        border.Height <- (y' - y)
        Canvas.SetLeft(border, x)
        Canvas.SetTop(border, y)

        let centerX = (x' - x) / 2.0 + x
        let centerY = (y' - y) / 2.0 + y

        let offsetX = c.ActualWidth / 2.0 - centerX
        let offsetY = c.ActualHeight / 2.0 - centerY
        let xAnimation = DoubleAnimation(offsetX, Duration.Automatic, EasingFunction = CubicEase())
        canvasOffset.BeginAnimation(TranslateTransform.XProperty, xAnimation)
        let yAnimation = DoubleAnimation(offsetY, Duration.Automatic, EasingFunction = CubicEase())
        canvasOffset.BeginAnimation(TranslateTransform.YProperty, yAnimation)

        let scaleX = c.ActualWidth / (x' - x)
        let scaleY = c.ActualHeight / (y' - y)
        let scale = min scaleX scaleY * 0.85
        canvasScale.CenterX <- c.ActualWidth / 2.0
        canvasScale.CenterY <- c.ActualHeight / 2.0
        let xAnimation = DoubleAnimation(scale, Duration.Automatic, EasingFunction = BackEase(EasingMode = EasingMode.EaseOut))
        canvasScale.BeginAnimation(ScaleTransform.ScaleXProperty, xAnimation)
        let yAnimation = DoubleAnimation(scale, Duration.Automatic, EasingFunction = BackEase(EasingMode = EasingMode.EaseOut))
        canvasScale.BeginAnimation(ScaleTransform.ScaleYProperty, yAnimation) 

    member this.initChildren() =
        children.Clear()
        c.Children.Clear()
        c.Children.Add(border) |> ignore
        Board.iterPointsValues (fun p s -> if s = Alive then this.addChild p else ()) myBoard
        
    member this.addChild (p : Life.Point) : unit =
        let colorBase = Colors.CornflowerBlue
        let brush = SolidColorBrush(colorBase)
        let r = Rectangle(
                    Fill = brush,
                    SnapsToDevicePixels = true,
                    Width = boxSize,
                    Height = boxSize)
                    
        let appear = ScaleTransform(0.0, 0.0, CenterX = boxCenter, CenterY = boxCenter) :> Transform
        r.RenderTransform <- appear
        
        let anim = DoubleAnimation(0.0, 1.0, Duration(TimeSpan.FromSeconds(0.1)), EasingFunction = BackEase(EasingMode = EasingMode.EaseOut))
        appear.BeginAnimation(ScaleTransform.ScaleXProperty, anim)
        anim.Completed.Add(fun _ -> if Object.ReferenceEquals(r.RenderTransform, appear) then r.RenderTransform <- null)
        appear.BeginAnimation(ScaleTransform.ScaleYProperty, anim)

        Canvas.SetLeft (r, boxSize * float p.x)
        Canvas.SetTop (r, boxSize * float p.y)
        
        children.Add(p, r)
        c.Children.Add(r) |> ignore

    member private this.removeChild (p : Life.Point) : unit =
        let child = children.[p]
        children.Remove p |> ignore
        
        let shrink = ScaleTransform(CenterX = boxCenter, CenterY = boxCenter) :> Transform
        let rotate = RotateTransform(0.0, CenterX = boxCenter, CenterY = boxCenter) :> Transform
        let group = TransformGroup()
        group.Children.Add(rotate)
        group.Children.Add(shrink)

        child.RenderTransform <- group

        let duration = Duration(TimeSpan.FromSeconds(rand.NextDouble()/3.0+0.2))
        let anim = DoubleAnimation(0.0, duration, EasingFunction = QuadraticEase(EasingMode = EasingMode.EaseIn))
        shrink.BeginAnimation(ScaleTransform.ScaleXProperty, anim)
        anim.Completed.Add(fun _ -> c.Children.Remove(child))
        shrink.BeginAnimation(ScaleTransform.ScaleYProperty, anim)

        let rotAnim = DoubleAnimation(rand.NextDouble() * 360.0 - 180.0, duration, EasingFunction = QuadraticEase(EasingMode = EasingMode.EaseIn))
        rotate.BeginAnimation(RotateTransform.AngleProperty, rotAnim)

        let colorAnim = ColorAnimation(Colors.White, duration)
        child.Fill.BeginAnimation(SolidColorBrush.ColorProperty, colorAnim)
        

    member private this.cellChanged (p : Life.Point) = function
        | Dead -> this.removeChild p
        | Alive -> this.addChild p

    member this.ProcessKeyDown (keys : Input.KeyEventArgs) = 
        myBoard <- generateNextBoard myBoard this.cellChanged
        this.reCenter()

    member this.initialize(b : Board) =
        myBoard <- b
        this.initChildren()
        this.reCenter()

    member this.Resized (args : SizeChangedEventArgs) =
        this.reCenter()


let canvas = Canvas()
let graphical = GraphicalBoard(canvas)
let window = Window(Content = canvas)
window.Loaded.Add(fun _ -> graphical.initialize(diehard))
window.KeyDown.Add(graphical.ProcessKeyDown)
window.SizeChanged.Add(graphical.Resized)

[<STAThread>]
Application().Run window |> ignore