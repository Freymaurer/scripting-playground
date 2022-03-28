#r "nuget: SixLabors.ImageSharp, 2.0.0"
#r "nuget: SixLabors.ImageSharp.Drawing, 1.0.0-beta14"
// #r "nuget: SixLabors.Fonts, 1.0.0-beta16"

let p = @"C:\Users\Kevin\source\repos\nfdi-helpdesk\src\Server\test.jpg"

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Processing;
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Drawing.Processing;
open SixLabors
open SixLabors.Fonts

let fontArr = [|"Arial"; "Verdana"; "Times New Roman"|]
let colorArr = Color.WebSafePalette.ToArray()
let charArr = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890"
let rand = System.Random()
let backcolor = Color.AliceBlue
let textColor = Color.Black
let fontsize = float32 29
let rotationDegree = 30

let createCaptchaString(length:int, charArr:string) =
    String.init length (fun i -> string charArr[rand.Next(0,charArr.Length-1)])
    |> Seq.map string
    |> String.concat " "

let createCaptcha() = 
    let captcha = createCaptchaString(7, charArr)
    let font = Fonts.SystemFonts.CreateFont(fontArr.[rand.Next(0,fontArr.Length-1)], fontsize, Fonts.FontStyle.Regular)
    let stringHeight, stringWidth = 
        let m = TextMeasurer.Measure(captcha, TextOptions(font))
        m.Height, m.Width
    let imgHeight, imgWidth = stringHeight * float32 1.5, stringWidth * float32 2
    let startX = 
        let halfStringWidth = stringWidth / float32 2
        let halfWidth = float imgWidth/2.
        float32 halfWidth - halfStringWidth
    let startY = 
        let halfStringHeight = stringHeight / float32 2
        let halfHeight = float imgHeight/2.
        float32 halfHeight - halfStringHeight
    let img = new Image<Rgba32>(int imgWidth,int imgHeight)
    img.Mutate(
            fun x -> x.BackgroundColor(backcolor) |> ignore
        )
    let mutable position = startX

    for c in captcha do
        let imgCharacter = new Image<Rgba32>(int imgWidth, int imgHeight)
        let font = Fonts.SystemFonts.CreateFont(fontArr.[rand.Next(0,fontArr.Length-1)], fontsize, Fonts.FontStyle.Regular)
        let location = new PointF(position, startY);
        position <- position + TextMeasurer.Measure(c.ToString(), TextOptions(font)).Width
        let color = textColor //colors.[rand.Next(0,colors.Length-1)]
        imgCharacter.Mutate(
            fun x -> x.DrawText(c.ToString(), font, color, location) |> ignore
        )

        let builder = new AffineTransformBuilder()        

        imgCharacter.Mutate(fun x ->
            x
                .Transform(
                    builder.PrependRotationDegrees(float32 <| rand.Next(-rotationDegree,rotationDegree), new PointF(position,startY))
                )
                // .Transform(
                //     builder.PrependTranslation(new PointF(position,yPosition))
                // )
                |> ignore
        )

        img.Mutate(fun x -> x.DrawImage(imgCharacter, float32 1) |> ignore ) 
        imgCharacter.Dispose()


    img.SaveAsJpeg(p)

createCaptcha()