#r "nuget: Expecto, 9.0.4"
#r "nuget: Markdig, 0.30.2"

// https://odetocode.com/blogs/scott/archive/2020/01/23/a-custom-renderer-extension-for-markdig.aspx
// https://github.com/arthurrump/MarkdigExtensions/blob/master/src/MarkdigExtensions.ImageAsFigure/ImageAsFigure.fs

open System.Text
open Markdig.Parsers;
open Markdig.Renderers;
open Markdig.Renderers.Html;
open Markdig.Syntax;
open Markdig

// https://github.com/ilich/Markdig.Prism/blob/main/src/Markdig.Prism/PrismCodeBlockRenderer.cs
type NFDICodeBlockRenderer() =
    inherit HtmlObjectRenderer<CodeBlock>()

    let extractSourcecode (node: LeafBlock) = 
        let code = new StringBuilder()
        let lines = node.Lines.Lines
        let totalLines = lines.Length
        let rec appendLines (counter: int) (c: StringBuilder) =
            if counter >= totalLines then
                c
            else
                let line = lines[counter]
                let slice = line.Slice
                if isNull slice.Text then
                    appendLines (counter + 1) c
                else
                    let lineText = slice.Text.Substring(slice.Start, slice.Length);
                    if counter > 0 then 
                        appendLines (counter+1) (c.AppendLine().Append(lineText))
                    else
                        appendLines (counter+1) (c.Append(lineText))
        appendLines 0 code
        |> fun x -> x.ToString()

    override this.Write(renderer : HtmlRenderer , cb : CodeBlock ) =

        if cb :? FencedCodeBlock && cb.Parser :? FencedCodeBlockParser then
            let fcb = cb :?> FencedCodeBlock
            let parser = cb.Parser :?> FencedCodeBlockParser
            let languageCode = fcb.Info.Replace(parser.InfoPrefix, "").Trim()
            let code = extractSourcecode(cb)
            if languageCode = "" then
                renderer
                    .Write("<nfdi-code>")
                    .Write(code)
                    .Write("</nfdi-code>")
                |> ignore
            else
                let attributes = new HtmlAttributes()
                attributes.AddClass($"language-{languageCode}")
                renderer
                    .Write("<nfdi-code")
                    .WriteAttributes(attributes)
                    .Write(">")
                    .Write(code)
                    .Write("</nfdi-code>")
                |> ignore
            renderer.EnsureLine() |> ignore
        else
            // let codeBlockRenderer = new CodeBlockRenderer()
            renderer.Write(cb) |> ignore


/// An extension for Markdig that highlights syntax in fenced code blocks
type SyntaxHighlightingExtension() =

    interface IMarkdownExtension with

        member __.Setup(_) = ()

        member __.Setup(_, renderer) = 
            renderer.ObjectRenderers.ReplaceOrAdd<CodeBlockRenderer>(new NFDICodeBlockRenderer()) |> ignore

open System.Runtime.CompilerServices

[<Extension>]
type MarkdownPipelineBuilderExtensions() =
    [<Extension>]
    // <summary>Highlight code in fenced code blocks</summary>
    // <param name="pipeline">The Markdig <see cref="MarkdownPipelineBuilder"/> to add the extension to</param>
    static member UseNFDICodeBlock(pipeline : MarkdownPipelineBuilder) =
        pipeline.Extensions.Add(SyntaxHighlightingExtension())
        pipeline

let pipeline = 
    let builder = new MarkdownPipelineBuilder()
    builder
        .UseNFDICodeBlock()
        .Build()

let markdown = 
    """# Start testing!

This is just some markdown text, ignore this!

```css
body {
    background-color: #f0f0f0;
    color: #000;
}
```
"""

let result = Markdown.ToHtml(markdown, pipeline)

result

open System.IO

let mdFile = File.ReadAllText("files/nfdi-code.test.md") 

Markdown.ToHtml(mdFile, pipeline)

open Expecto

[<Tests>]
let tests = 
    testList "UseNFDICodeBlock" [
        test "basic case" {
            let markdown = """Here is some inline `code`!"""
            let result = Markdown.ToHtml(markdown, pipeline)
            Expect.equal result $"""<p>Here is some inline <code>code</code>!</p>{'\010'}""" ""
        }
        test "Fenced code block, no language" {
            let markdown = """Test line
```
let someCode = 42
```
"""
            let result = Markdown.ToHtml(markdown, pipeline)
            let expected = $"""<p>Test line</p>
<nfdi-code>let someCode = 42</nfdi-code>{'\010'}"""
            Expect.equal result expected ""
        }
        test "Fenced code block" {
            let markdown = """Test line
```fsharp
let someCode = 42
```
"""
            let result = Markdown.ToHtml(markdown, pipeline)
            let expected = $"""<p>Test line</p>
<nfdi-code class="language-fsharp">let someCode = 42</nfdi-code>{'\010'}"""
            Expect.equal result expected ""
        }
    ]
Expecto.Tests.runTestsWithCLIArgs [] [||] tests