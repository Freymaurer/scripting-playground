#r "nuget: Markdig, 0.30.2"

open Markdig
open Markdig.Renderers.Html
open Markdig.Syntax

// https://odetocode.com/blogs/scott/archive/2020/01/23/a-custom-renderer-extension-for-markdig.aspx
// https://github.com/arthurrump/MarkdigExtensions/blob/master/src/MarkdigExtensions.ImageAsFigure/ImageAsFigure.fs

open Markdig
open Markdig.Renderers
open Markdig.Renderers.Html
open Markdig.Syntax
open Markdig.Syntax.Inlines
open Markdig
open Markdig.Renderers
open Markdig.Renderers.Html
open Markdig.Syntax

open System

type NFDIHeaderRenderer() =
    inherit HeadingRenderer()

    override __.Write(renderer : HtmlRenderer, hb : HeadingBlock ) =

        let headingTexts = [|
            "nfdi-h1";
            "nfdi-h2";
            "nfdi-h3";
            "nfdi-h4";
            "nfdi-h5";
            "nfdi-h6";
        |]
        let index = hb.Level - 1
        let headingText =
            if index < headingTexts.Length then
                headingTexts[index]
            else
                headingTexts.[headingTexts.Length-1]
        printfn "%A" headingText

        if (renderer.EnableHtmlForBlock) then
            renderer.Write('<') |> ignore
            renderer.Write(headingText) |> ignore
            renderer.WriteAttributes(hb) |> ignore
            renderer.Write('>') |> ignore
        
        renderer.WriteLeafInline(hb) |> ignore

        if (renderer.EnableHtmlForBlock) then
            renderer.Write("</") |> ignore
            renderer.Write(headingText) |> ignore
            renderer.WriteLine('>') |> ignore
 
        renderer.EnsureLine() |> ignore

/// An extension for Markdig that highlights syntax in fenced code blocks
type SyntaxHighlightingExtension() =

    interface IMarkdownExtension with

        member __.Setup(_) = ()

        member __.Setup(_, renderer) = 
            renderer.ObjectRenderers.ReplaceOrAdd<CodeBlockRenderer>(new NFDIHeaderRenderer()) |> ignore

open System.Runtime.CompilerServices

[<Extension>]
type MarkdownPipelineBuilderExtensions() =
    [<Extension>]
    // <summary>Highlight code in fenced code blocks</summary>
    // <param name="pipeline">The Markdig <see cref="MarkdownPipelineBuilder"/> to add the extension to</param>
    static member UseNFDIHeader(pipeline : MarkdownPipelineBuilder) =
        pipeline.Extensions.Add(SyntaxHighlightingExtension())
        pipeline

let pipeline = 
    let builder = new MarkdownPipelineBuilder()
    builder
        .UseAdvancedExtensions()
        .UseEmojiAndSmiley()
        .UseNFDIHeader()
        .Build()

let markdown = """
# Start testing!

This is a text with some *emphasis* :tada:

![Test](https://upload.wikimedia.org/wikipedia/en/9/95/Test_image.jpg)
"""

let result = Markdown.ToHtml(markdown, pipeline)
result